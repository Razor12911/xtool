{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00                                        }
{   File name:        flcStringPatternMatcher.pas                              }
{   File version:     5.06                                                     }
{   Description:      String pattern matchers.                                 }
{                                                                              }
{   Copyright:        Copyright (c) 2000-2018, David J Butler                  }
{                     All rights reserved.                                     }
{                     Redistribution and use in source and binary forms, with  }
{                     or without modification, are permitted provided that     }
{                     the following conditions are met:                        }
{                     Redistributions of source code must retain the above     }
{                     copyright notice, this list of conditions and the        }
{                     following disclaimer.                                    }
{                     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND   }
{                     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED          }
{                     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED   }
{                     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A          }
{                     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL     }
{                     THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,    }
{                     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR             }
{                     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,    }
{                     PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF     }
{                     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)         }
{                     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER   }
{                     IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING        }
{                     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE   }
{                     USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE             }
{                     POSSIBILITY OF SUCH DAMAGE.                              }
{                                                                              }
{   Github:           https://github.com/fundamentalslib                       }
{   E-mail:           fundamentals.library at gmail.com                        }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   2000/09/04  1.01  Added MatchFileMask.                                     }
{   2001/05/13  1.02  Added simple regular expression matching.                }
{   2002/02/14  2.03  Added MatchPattern.                                      }
{   2016/04/13  5.04  Change pattern match functions to RawByteString.         }
{   2017/10/07  5.05  Move from flcStrings unit.                               }
{   2018/08/12  5.06  String type changes.                                     }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Delphi 10 Win32                     5.05  2016/01/09                       }
{   Delphi 10 Win64                     5.05  2016/01/09                       }
{                                                                              }
{******************************************************************************}

{$INCLUDE ..\flcInclude.inc}

{$IFDEF FREEPASCAL}
  {$WARNINGS OFF}
  {$HINTS OFF}
{$ENDIF}

{$IFDEF DEBUG}
{$IFDEF TEST}
  {$DEFINE STRING_PAT_MATCH_TEST}
{$ENDIF}
{$ENDIF}

unit flcStringPatternMatcher;

interface

uses
  { Fundamentals }
  flcStdTypes;



{                                                                              }
{ Pattern Matcher                                                              }
{                                                                              }
{   Matches a subset of regular expressions (* . ? and [])                     }
{       '*' Matches zero or more of any character                              }
{       '.' Matches exactly one character                                      }
{       '?' Matches zero or one character                                      }
{       '[' <char set> ']' Matches character from <char set>                   }
{       '[^' <char set> ']' Matches character not in <char set>                }
{       <char set> can include multiple ranges and escaped characters          }
{       '\n' matches NewLine (#10)                                             }
{       '\r' matches Return (#13)                                              }
{       '\\' matches a slash ('\')                                             }
{   StrZMatchPattern returns the number of matched characters,                 }
{     or < 0 if no match.                                                      }
{   StrPosPattern returns the index of matched pattern (F) in string S,        }
{     or 0 if not found. Len is the length of the matched pattern.             }
{                                                                              }
type
  TMatchPatternGreed = (
    mpgLazy,
    mpgGreedy,
    mpgGreedyNoBacktrack);

{$IFDEF SupportAnsiString}
function  StrZMatchPatternA(M, S: PAnsiChar; const G: TMatchPatternGreed = mpgLazy): Integer;
{$ENDIF}
function  StrZMatchPatternW(M, S: PWideChar; const G: TMatchPatternGreed = mpgLazy): Integer;

{$IFDEF SupportAnsiString}
function  StrEqualPatternA(const M, S: RawByteString; const G: TMatchPatternGreed = mpgLazy): Boolean;
{$ENDIF}
function  StrEqualPatternU(const M, S: UnicodeString; const G: TMatchPatternGreed = mpgLazy): Boolean;
function  StrEqualPattern(const M, S: String; const G: TMatchPatternGreed = mpgLazy): Boolean;

{$IFDEF SupportAnsiString}
function  StrPosPatternA(const F, S: RawByteString; var Len: Integer;
          const StartIndex: Integer = 1; const G: TMatchPatternGreed = mpgLazy): Integer;
{$ENDIF}
function  StrPosPatternU(const F, S: UnicodeString; var Len: Integer;
          const StartIndex: Integer = 1; const G: TMatchPatternGreed = mpgLazy): Integer;



{                                                                              }
{ File Mask Matcher                                                            }
{                                                                              }
{   Matches classic file mask type regular expressions.                        }
{     ? = matches one character (or zero if at end of mask)                    }
{     * = matches zero or more characters                                      }
{                                                                              }
{$IFDEF SupportAnsiString}
function  MatchFileMaskB(const Mask, Key: RawByteString;
          const AsciiCaseSensitive: Boolean = False): Boolean;
{$ENDIF}



{                                                                              }
{ Fast abbreviated regular expression matcher                                  }
{                                                                              }
{   Matches regular expressions of the form: (<charset><quant>)*               }
{     where <charset> is a character set and <quant> is one of the quantifiers }
{     (mnOnce, mnOptional = ?, mnAny = *, mnLeastOnce = +).                    }
{                                                                              }
{   Supports deterministic/non-deterministic, greedy/non-greedy matching.      }
{   Returns first MatchPos (as opposed to longest).                            }
{   Uses an NFA (Non-deterministic Finite Automata).                           }
{                                                                              }
{   For example:                                                               }
{     I := 1                                                                   }
{     S := 'a123'                                                              }
{     MatchQuantSeq(I, [['a'..'z'], ['0'..9']], [mqOnce, mqAny], S) = True     }
{                                                                              }
{     is the same as matching the regular expression [a-z][0-9]*               }
{                                                                              }
type
  TMatchQuantifier = (
      mqOnce,
      mqAny,
      mqLeastOnce,
      mqOptional);
  TMatchQuantSeqOptions = Set of (
      moDeterministic,
      moNonGreedy);

{$IFDEF SupportAnsiString}
function  MatchQuantSeqB(var MatchPos: Integer;
          const MatchSeq: array of ByteCharSet; const Quant: array of TMatchQuantifier;
          const S: RawByteString; const MatchOptions: TMatchQuantSeqOptions = [];
          const StartIndex: Integer = 1; const StopIndex: Integer = -1): Boolean;
{$ENDIF}



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF STRING_PAT_MATCH_TEST}
procedure Test;
{$ENDIF}



implementation

uses
  { Fundamentals }
  flcUtils,
  flcASCII,
  flcStrings;



{                                                                              }
{ MatchPattern                                                                 }
{                                                                              }
{$IFDEF SupportAnsiString}
function StrZMatchPatternA(M, S: PAnsiChar; const G: TMatchPatternGreed): Integer;

  function EscapedChar(const C: AnsiChar): AnsiChar;
  begin
    case C of
      'b' : Result := AsciiBS;
      'e' : Result := AsciiESC;
      'f' : Result := AsciiFF;
      'n' : Result := AsciiLF;
      'r' : Result := AsciiCR;
      't' : Result := AsciiHT;
      'v' : Result := AsciiVT;
      else Result := C;
    end;
  end;

var A, C, D : AnsiChar;
    N, R    : Boolean;
    L, I    : Integer;
begin
  Result := 0;
  repeat
    case M^ of
      #0 : // end of pattern
        exit;
      '.' : // match exactly one
        if S^ = #0 then
          begin
            Result := -1; // no match
            exit;
          end
        else
          begin
            Inc(M);
            Inc(S);
            Inc(Result);
          end;
      '\' : // escaped character
        if S^ = #0 then
          begin
            Result := -1; // no match
            exit;
          end
        else
          begin
            Inc(M);
            C := M^;
            if C = #0 then
              begin
                Result := -2; // format error
                exit;
              end;
            C := EscapedChar(C);
            if S^ <> C then
              begin
                Result := -1; // no match
                exit;
              end else
              begin
                Inc(M);
                Inc(S);
                Inc(Result);
              end;
          end;
      '?' : // match zero or one
        begin
          Inc(M);
          if M^ = #0 then
            begin
              if G in [mpgGreedy, mpgGreedyNoBacktrack] then
                if S^ <> #0 then
                  Inc(Result);
              break;
            end;
          if S^ <> #0 then
            begin
              if G = mpgGreedyNoBacktrack then
                begin
                  Inc(S);
                  Inc(Result);
                end
              else
              if G = mpgGreedy then
                begin
                  Inc(S);
                  if S^ = #0 then
                    L := -1
                  else
                    begin
                      L := StrZMatchPatternA(M, S, G); // try one
                      if L >= 0 then
                        Inc(Result, L + 1);
                    end;
                  if L < 0 then
                    begin
                      Dec(S);
                      L := StrZMatchPatternA(M, S, G); // try zero
                      if L > 0 then
                        Inc(Result, L);
                    end;
                  if L < 0 then
                    Result := -1; // no match
                  exit;
                end
              else
                begin // lazy
                  L := StrZMatchPatternA(M, S, G); // try zero
                  if L >= 0 then
                    Inc(Result, L)
                  else
                    begin
                      Inc(S);
                      if S <> #0 then
                        begin
                          L := StrZMatchPatternA(M, S, G); // try one
                          if L >= 0 then
                            Inc(Result, L + 1);
                        end;
                    end;
                  if L < 0 then
                    Result := -1; // no match
                  exit;
                end;
            end;
        end;
      '*' :
        begin
          Inc(M);
          if M^ = #0 then // always match at end of mask
            begin
              if G in [mpgGreedy, mpgGreedyNoBacktrack] then
                while S^ <> #0 do
                  begin
                    Inc(S);
                    Inc(Result);
                  end;
              break;
            end
          else
            if S^ <> #0 then
              if G = mpgGreedyNoBacktrack then
                while S^ <> #0 do
                  begin
                    Inc(S);
                    Inc(Result);
                  end
              else
              if G = mpgGreedy then
                begin
                  // move to end
                  I := 0;
                  while S^ <> #0 do
                    begin
                      Inc(S);
                      Inc(Result);
                      Inc(I);
                    end;
                  // match from back
                  L := 0;
                  while I >= 0 do
                    begin
                      L := StrZMatchPatternA(M, S, G);
                      if L >= 0 then
                        break
                      else
                        begin
                          Dec(S);
                          Dec(Result);
                          Dec(I);
                        end;
                    end;
                  if L < 0 then
                    Result := -1 // no match
                  else
                    Inc(Result, L);
                  exit;
                end
              else
                // lazy
                begin
                  // match from front
                  L := 0;
                  while S^ <> #0 do
                    begin
                      L := StrZMatchPatternA(M, S, G);
                      if L >= 0 then
                        break
                      else
                        begin
                          Inc(S);
                          Inc(Result);
                        end;
                    end;
                  if L < 0 then
                    Result := -1 // no match
                  else
                    Inc(Result, L);
                  exit;
                end;
        end;
      '[' : // character class
        begin
          A := S^;
          if A = #0 then
            begin
              Result := -1; // no match
              exit;
            end;
          Inc(M);
          C := M^;
          N := C = '^';
          if N then
            begin
              Inc(M);
              C := M^;
            end;
          R := False;
          while C <> ']' do
            begin
              if C = #0 then
                begin
                  Result := -2; // format error
                  exit;
                end;
              Inc(M);
              if C = '\' then // escaped character
                begin
                  C := M^;
                  if C = #0 then
                    begin
                      Result := -2; // format error
                      exit;
                    end;
                  C := EscapedChar(C);
                  Inc(M);
                end;
              D := M^;
              if D = '-' then // match range
                begin
                  Inc(M);
                  D := M^;
                  if D = #0 then
                    begin
                      Result := -2; // format error
                      exit;
                    end;
                  if D = '\' then // escaped character
                    begin
                      Inc(M);
                      D := M^;
                      if D = #0 then
                        begin
                          Result := -2; // format error
                          exit;
                        end;
                      D := EscapedChar(D);
                      Inc(M);
                    end;
                  if (A >= C) and (A <= D) then
                    begin
                      R := True;
                      break;
                    end;
                  Inc(M);
                  C := M^;
                end
              else
                begin // match single character
                  if A = C then
                    begin
                      R := True;
                      break;
                    end;
                  C := D;
                end;
            end;
          if (N and R) or
             (not N and not R) then
            begin
              Result := -1; // no match
              exit;
            end;
          Inc(Result);
          Inc(S);
          // locate closing bracket
          while M^ <> ']' do
            if M^ = #0 then
              begin
                Result := -2; // format error
                exit;
              end
            else
              Inc(M);
          Inc(M);
        end;
    else // single character match
      if M^ <> S^ then
        begin
          Result := -1; // no match
          exit;
        end else
        begin
          Inc(M);
          Inc(S);
          Inc(Result);
        end;
    end;
  until False;
end;
{$ENDIF}

function StrZMatchPatternW(M, S: PWideChar; const G: TMatchPatternGreed): Integer;

  function EscapedChar(const C: WideChar): WideChar;
  begin
    case C of
      'b' : Result := WideBS;
      'e' : Result := WideESC;
      'f' : Result := WideFF;
      'n' : Result := WideLF;
      'r' : Result := WideCR;
      't' : Result := WideHT;
      'v' : Result := WideVT;
      else Result := C;
    end;
  end;

var A, C, D : WideChar;
    N, R    : Boolean;
    L, I    : Integer;
begin
  Result := 0;
  repeat
    case M^ of
      #0 : // end of pattern
        exit;
      '.' : // match exactly one
        if S^ = #0 then
          begin
            Result := -1; // no match
            exit;
          end
        else
          begin
            Inc(M);
            Inc(S);
            Inc(Result);
          end;
      '\' : // escaped character
        if S^ = #0 then
          begin
            Result := -1; // no match
            exit;
          end
        else
          begin
            Inc(M);
            C := M^;
            if C = #0 then
              begin
                Result := -2; // format error
                exit;
              end;
            C := EscapedChar(C);
            if S^ <> C then
              begin
                Result := -1; // no match
                exit;
              end else
              begin
                Inc(M);
                Inc(S);
                Inc(Result);
              end;
          end;
      '?' : // match zero or one
        begin
          Inc(M);
          if M^ = #0 then
            begin
              if G in [mpgGreedy, mpgGreedyNoBacktrack] then
                if S^ <> #0 then
                  Inc(Result);
              break;
            end;
          if S^ <> #0 then
            begin
              if G = mpgGreedyNoBacktrack then
                begin
                  Inc(S);
                  Inc(Result);
                end
              else
              if G = mpgGreedy then
                begin
                  Inc(S);
                  if S^ = #0 then
                    L := -1
                  else
                    begin
                      L := StrZMatchPatternW(M, S, G); // try one
                      if L >= 0 then
                        Inc(Result, L + 1);
                    end;
                  if L < 0 then
                    begin
                      Dec(S);
                      L := StrZMatchPatternW(M, S, G); // try zero
                      if L > 0 then
                        Inc(Result, L);
                    end;
                  if L < 0 then
                    Result := -1; // no match
                  exit;
                end
              else
                begin // lazy
                  L := StrZMatchPatternW(M, S, G); // try zero
                  if L >= 0 then
                    Inc(Result, L)
                  else
                    begin
                      Inc(S);
                      if S^ <> #0 then
                        begin
                          L := StrZMatchPatternW(M, S, G); // try one
                          if L >= 0 then
                            Inc(Result, L + 1);
                        end;
                    end;
                  if L < 0 then
                    Result := -1; // no match
                  exit;
                end;
            end;
        end;
      '*' :
        begin
          Inc(M);
          if M^ = #0 then // always match at end of mask
            begin
              if G in [mpgGreedy, mpgGreedyNoBacktrack] then
                while S^ <> #0 do
                  begin
                    Inc(S);
                    Inc(Result);
                  end;
              break;
            end
          else
            if S^ <> #0 then
              if G = mpgGreedyNoBacktrack then
                while S^ <> #0 do
                  begin
                    Inc(S);
                    Inc(Result);
                  end
              else
              if G = mpgGreedy then
                begin
                  // move to end
                  I := 0;
                  while S^ <> #0 do
                    begin
                      Inc(S);
                      Inc(Result);
                      Inc(I);
                    end;
                  // match from back
                  L := 0;
                  while I >= 0 do
                    begin
                      L := StrZMatchPatternW(M, S, G);
                      if L >= 0 then
                        break
                      else
                        begin
                          Dec(S);
                          Dec(Result);
                          Dec(I);
                        end;
                    end;
                  if L < 0 then
                    Result := -1 // no match
                  else
                    Inc(Result, L);
                  exit;
                end
              else
                // lazy
                begin
                  // match from front
                  L := 0;
                  while S^ <> #0 do
                    begin
                      L := StrZMatchPatternW(M, S, G);
                      if L >= 0 then
                        break
                      else
                        begin
                          Inc(S);
                          Inc(Result);
                        end;
                    end;
                  if L < 0 then
                    Result := -1 // no match
                  else
                    Inc(Result, L);
                  exit;
                end;
        end;
      '[' : // character class
        begin
          A := S^;
          if A = #0 then
            begin
              Result := -1; // no match
              exit;
            end;
          Inc(M);
          C := M^;
          N := C = '^';
          if N then
            begin
              Inc(M);
              C := M^;
            end;
          R := False;
          while C <> ']' do
            begin
              if C = #0 then
                begin
                  Result := -2; // format error
                  exit;
                end;
              Inc(M);
              if C = '\' then // escaped character
                begin
                  C := M^;
                  if C = #0 then
                    begin
                      Result := -2; // format error
                      exit;
                    end;
                  C := EscapedChar(C);
                  Inc(M);
                end;
              D := M^;
              if D = '-' then // match range
                begin
                  Inc(M);
                  D := M^;
                  if D = #0 then
                    begin
                      Result := -2; // format error
                      exit;
                    end;
                  if D = '\' then // escaped character
                    begin
                      Inc(M);
                      D := M^;
                      if D = #0 then
                        begin
                          Result := -2; // format error
                          exit;
                        end;
                      D := EscapedChar(D);
                      Inc(M);
                    end;
                  if (A >= C) and (A <= D) then
                    begin
                      R := True;
                      break;
                    end;
                  Inc(M);
                  C := M^;
                end
              else
                begin // match single character
                  if A = C then
                    begin
                      R := True;
                      break;
                    end;
                  C := D;
                end;
            end;
          if (N and R) or
             (not N and not R) then
            begin
              Result := -1; // no match
              exit;
            end;
          Inc(Result);
          Inc(S);
          // locate closing bracket
          while M^ <> ']' do
            if M^ = #0 then
              begin
                Result := -2; // format error
                exit;
              end
            else
              Inc(M);
          Inc(M);
        end;
    else // single character match
      if M^ <> S^ then
        begin
          Result := -1; // no match
          exit;
        end else
        begin
          Inc(M);
          Inc(S);
          Inc(Result);
        end;
    end;
  until False;
end;

{$IFDEF SupportAnsiString}
function StrEqualPatternA(const M, S: RawByteString; const G: TMatchPatternGreed): Boolean;
begin
  Result := StrZMatchPatternA(PAnsiChar(M), PAnsiChar(S), G) = Length(S);
end;
{$ENDIF}

function StrEqualPatternU(const M, S: UnicodeString; const G: TMatchPatternGreed): Boolean;
begin
  Result := StrZMatchPatternW(PWideChar(M), PWideChar(S), G) = Length(S);
end;

function StrEqualPattern(const M, S: String; const G: TMatchPatternGreed): Boolean;
begin
  {$IFDEF StringIsUnicode}
  Result := StrZMatchPatternW(PWideChar(M), PWideChar(S), G) = Length(S);
  {$ELSE}
  Result := StrZMatchPatternA(PAnsiChar(M), PAnsiChar(S), G) = Length(S);
  {$ENDIF}
end;

{$IFDEF SupportAnsiString}
function StrPosPatternA(const F, S: RawByteString; var Len: Integer; const StartIndex: Integer; const G: TMatchPatternGreed): Integer;
var P : PAnsiChar;
    M : PAnsiChar;
    I : Integer;
begin
  P := PAnsiChar(S);
  M := PAnsiChar(F);
  for I := MaxInt(1, StartIndex) to Length(S) do
    begin
      Len := StrZMatchPatternA(M, P, G);
      if Len >= 0 then
        begin
          Result := I;
          exit;
        end
      else
        Inc(P);
    end;
  Len := 0;
  Result := 0;
end;
{$ENDIF}

function StrPosPatternU(const F, S: UnicodeString; var Len: Integer; const StartIndex: Integer; const G: TMatchPatternGreed): Integer;
var P : PWideChar;
    M : PWideChar;
    I : Integer;
begin
  P := PWideChar(S);
  M := PWideChar(F);
  for I := MaxInt(1, StartIndex) to Length(S) do
    begin
      Len := StrZMatchPatternW(M, P, G);
      if Len >= 0 then
        begin
          Result := I;
          exit;
        end
      else
        Inc(P);
    end;
  Len := 0;
  Result := 0;
end;



{                                                                              }
{ MatchFileMask                                                                }
{                                                                              }
{$IFDEF SupportAnsiString}
function MatchFileMaskB(const Mask, Key: RawByteString; const AsciiCaseSensitive: Boolean): Boolean;
var ML, KL : Integer;

  function MatchAt(MaskPos, KeyPos: Integer): Boolean;
  begin
    while (MaskPos <= ML) and (KeyPos <= KL) do
      case Mask[MaskPos] of
        '?' :
          begin
            Inc(MaskPos);
            Inc(KeyPos);
          end;
        '*' :
          begin
            while (MaskPos <= ML) and (Mask[MaskPos] = '*') do
              Inc(MaskPos);
            if MaskPos > ML then
              begin
                Result := True;
                exit;
              end;
            repeat
              if MatchAt(MaskPos, KeyPos) then
                begin
                  Result := True;
                  exit;
                end;
              Inc(KeyPos);
            until KeyPos > KL;
            Result := False;
            exit;
          end;
        else
          if not CharMatchB(Mask[MaskPos], Key[KeyPos], AsciiCaseSensitive) then
            begin
              Result := False;
              exit;
            end
          else
            begin
              Inc(MaskPos);
              Inc(KeyPos);
            end;
      end;
    while (MaskPos <= ML) and (Mask[MaskPos] in ['?', '*']) do
      Inc(MaskPos);
    if (MaskPos <= ML) or (KeyPos <= KL) then
      begin
        Result := False;
        exit;
      end;
    Result := True;
  end;

begin
  ML := Length(Mask);
  if ML = 0 then
    begin
      Result := True;
      exit;
    end;
  KL := Length(Key);
  Result := MatchAt(1, 1);
end;
{$ENDIF}



{                                                                              }
{ Abbreviated regular expression matcher                                       }
{                                                                              }
{$IFDEF SupportAnsiString}
function MatchQuantSeqB(var MatchPos: Integer; const MatchSeq: array of ByteCharSet;
    const Quant: array of TMatchQuantifier; const S: RawByteString;
    const MatchOptions: TMatchQuantSeqOptions;
    const StartIndex: Integer; const StopIndex: Integer): Boolean;

var Stop          : Integer;
    Deterministic : Boolean;
    NonGreedy     : Boolean;

  function MatchAt(MPos, SPos: Integer; var MatchPos: Integer): Boolean;

    function MatchAndAdvance: Boolean;
    var I : Integer;
    begin
      I := SPos;
      Result := S[I] in MatchSeq[MPos];
      if Result then
        begin
          MatchPos := I;
          Inc(SPos);
        end;
    end;

    function MatchAndSetResult(var Res: Boolean): Boolean;
    begin
      Result := MatchAndAdvance;
      Res := Result;
      if not Result then
        MatchPos := 0;
    end;

    function MatchAny: Boolean;
    var I, L : Integer;
        P : PAnsiChar;
    begin
      L := Stop;
      if Deterministic then
        begin
          while (SPos <= L) and MatchAndAdvance do ;
          Result := False;
        end
      else
      if NonGreedy then
        repeat
          Result := MatchAt(MPos + 1, SPos, MatchPos);
          if Result or not MatchAndAdvance then
            exit;
        until SPos > L
      else
        begin
          I := SPos;
          P := Pointer(S);
          Inc(P, I - 1);
          while (I <= L) and (P^ in MatchSeq[MPos]) do
            begin
              Inc(I);
              Inc(P);
            end;
          repeat
            MatchPos := I - 1;
            Result := MatchAt(MPos + 1, I, MatchPos);
            if Result then
              exit;
            Dec(I);
          until SPos > I;
        end;
    end;

  var Q    : TMatchQuantifier;
      L, M : Integer;
  begin
    L := Length(MatchSeq);
    M := Stop;
    while (MPos < L) and (SPos <= M) do
      begin
        Q := Quant[MPos];
        if Q in [mqOnce, mqLeastOnce] then
          if not MatchAndSetResult(Result) then
            exit;
        if (Q = mqAny) or ((Q = mqLeastOnce) and (SPos <= M)) then
          begin
            Result := MatchAny;
            if Result then
              exit;
          end
        else
        if Q = mqOptional then
          if Deterministic then
            MatchAndAdvance
          else
            begin
              if NonGreedy then
                begin
                  Result := MatchAt(MPos + 1, SPos, MatchPos);
                  if Result or not MatchAndSetResult(Result) then
                    exit;
                end
              else
                begin
                  Result := (MatchAndAdvance and MatchAt(MPos + 1, SPos, MatchPos)) or
                            MatchAt(MPos + 1, SPos, MatchPos);
                  exit;
                end;
            end;
        Inc(MPos);
      end;
    while (MPos < L) and (Quant[MPos] in [mqAny, mqOptional]) do
      Inc(MPos);
    Result := MPos = L;
    if not Result then
      MatchPos := 0;
  end;

begin
  Assert(Length(MatchSeq) = Length(Quant));
  if StopIndex < 0 then
    Stop := Length(S)
  else
    Stop := MinInt(StopIndex, Length(S));
  MatchPos := 0;
  if (Length(MatchSeq) = 0) or (StartIndex > Stop) or (StartIndex <= 0) then
    begin
      Result := False;
      exit;
    end;
  NonGreedy := moNonGreedy in MatchOptions;
  Deterministic := moDeterministic in MatchOptions;
  Result := MatchAt(0, StartIndex, MatchPos);
end;
{$ENDIF}



{                                                                              }
{ Tests                                                                        }
{                                                                              }
{$IFDEF STRING_PAT_MATCH_TEST}
procedure Test_MatchFileMask;
begin
  {$IFDEF SupportAnsiString}
  Assert(MatchFileMaskB('*', 'A'), 'MatchFileMask');
  Assert(MatchFileMaskB('?', 'A'), 'MatchFileMask');
  Assert(MatchFileMaskB('', 'A'), 'MatchFileMask');
  Assert(MatchFileMaskB('', ''), 'MatchFileMask');
  Assert(not MatchFileMaskB('X', ''), 'MatchFileMask');
  Assert(MatchFileMaskB('A?', 'A'), 'MatchFileMask');
  Assert(MatchFileMaskB('A?', 'AB'), 'MatchFileMask');
  Assert(MatchFileMaskB('A*B*C', 'ACBDC'), 'MatchFileMask');
  Assert(MatchFileMaskB('A*B*?', 'ACBDC'), 'MatchFileMask');
  {$ENDIF}
end;

procedure Test_MatchPattern;
var
  I : Integer;
begin
  { MatchPattern                                                         }
  {$IFDEF SupportAnsiString}
  Assert(StrZMatchPatternA('', '', mpgLazy) = 0);
  Assert(StrZMatchPatternA('', '', mpgGreedy) = 0);
  Assert(StrZMatchPatternA('', '', mpgGreedyNoBacktrack) = 0);
  Assert(StrZMatchPatternA('', 'a', mpgLazy) = 0);
  {$ENDIF}

  Assert(StrZMatchPatternW('', '', mpgLazy) = 0);

  {$IFDEF SupportAnsiString}
  Assert(StrZMatchPatternA('a', '', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a', 'a', mpgLazy) = 1);
  Assert(StrZMatchPatternA('a', 'b', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a', 'aa', mpgLazy) = 1);
  Assert(StrZMatchPatternA('aa', '', mpgLazy) < 0);
  Assert(StrZMatchPatternA('aa', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('aa', 'aa', mpgLazy) = 2);
  Assert(StrZMatchPatternA('aa', 'aaa', mpgLazy) = 2);
  Assert(StrZMatchPatternA('aa', 'ab', mpgLazy) < 0);

  Assert(StrZMatchPatternA('.', '', mpgLazy) < 0);
  Assert(StrZMatchPatternA('.', 'a', mpgLazy) = 1);
  Assert(StrZMatchPatternA('.', 'aa', mpgLazy) = 1);
  Assert(StrZMatchPatternA('a.', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a.', 'aa', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a.', 'aaa', mpgLazy) = 2);
  Assert(StrZMatchPatternA('.a', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('.a', 'aa', mpgLazy) = 2);
  Assert(StrZMatchPatternA('.a', 'ab', mpgLazy) < 0);

  Assert(StrZMatchPatternA('?', '', mpgLazy) = 0);
  Assert(StrZMatchPatternA('?', 'a', mpgLazy) = 0);
  Assert(StrZMatchPatternA('?', 'aa', mpgLazy) = 0);
  Assert(StrZMatchPatternA('a?', 'a', mpgLazy) = 1);
  Assert(StrZMatchPatternA('a?', 'aa', mpgLazy) = 1);
  Assert(StrZMatchPatternA('a?', 'aaa', mpgLazy) = 1);
  Assert(StrZMatchPatternA('?a', 'a', mpgLazy) = 1);
  Assert(StrZMatchPatternA('?a', 'aa', mpgLazy) = 1);
  Assert(StrZMatchPatternA('?a', 'ab', mpgLazy) = 1);

  Assert(StrZMatchPatternA('?', '', mpgGreedy) = 0);
  Assert(StrZMatchPatternA('?', 'a', mpgGreedy) = 1);
  Assert(StrZMatchPatternA('?', 'aa', mpgGreedy) = 1);
  Assert(StrZMatchPatternA('a?', 'a', mpgGreedy) = 1);
  Assert(StrZMatchPatternA('a?', 'aa', mpgGreedy) = 2);
  Assert(StrZMatchPatternA('a?', 'aaa', mpgGreedy) = 2);
  Assert(StrZMatchPatternA('?a', 'a', mpgGreedy) = 1);
  Assert(StrZMatchPatternA('?a', 'aa', mpgGreedy) = 2);
  Assert(StrZMatchPatternA('?a', 'ab', mpgGreedy) = 1);

  Assert(StrZMatchPatternA('?', '', mpgGreedyNoBacktrack) = 0);
  Assert(StrZMatchPatternA('?', 'a', mpgGreedyNoBacktrack) = 1);
  Assert(StrZMatchPatternA('?', 'aa', mpgGreedyNoBacktrack) = 1);
  Assert(StrZMatchPatternA('a?', 'a', mpgGreedyNoBacktrack) = 1);
  Assert(StrZMatchPatternA('a?', 'aa', mpgGreedyNoBacktrack) = 2);
  Assert(StrZMatchPatternA('a?', 'aaa', mpgGreedyNoBacktrack) = 2);
  Assert(StrZMatchPatternA('?a', 'a', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('?a', 'aa', mpgGreedyNoBacktrack) = 2);
  Assert(StrZMatchPatternA('?a', 'ab', mpgGreedyNoBacktrack) < 0);

  Assert(StrZMatchPatternA('*', '', mpgLazy) = 0);
  Assert(StrZMatchPatternA('*', 'a', mpgLazy) = 0);
  Assert(StrZMatchPatternA('*', 'aa', mpgLazy) = 0);
  Assert(StrZMatchPatternA('a*', '', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a*', 'a', mpgLazy) = 1);
  Assert(StrZMatchPatternA('a*', 'aa', mpgLazy) = 1);
  Assert(StrZMatchPatternA('a*', 'abc', mpgLazy) = 1);
  Assert(StrZMatchPatternA('a*b', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a*b', 'ab', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a*b', 'acb', mpgLazy) = 3);
  Assert(StrZMatchPatternA('a*b', 'acbd', mpgLazy) = 3);
  Assert(StrZMatchPatternA('a*b', 'acccbd', mpgLazy) = 5);
  Assert(StrZMatchPatternA('a*b', 'acbcb', mpgLazy) = 3);
  Assert(StrZMatchPatternA('a*b', 'acbcbd', mpgLazy) = 3);
  Assert(StrZMatchPatternA('a*b', 'bb', mpgLazy) < 0);

  Assert(StrZMatchPatternA('*', '', mpgGreedy) = 0);
  Assert(StrZMatchPatternA('*', 'a', mpgGreedy) = 1);
  Assert(StrZMatchPatternA('*', 'aa', mpgGreedy) = 2);
  Assert(StrZMatchPatternA('a*', '', mpgGreedy) < 0);
  Assert(StrZMatchPatternA('a*', 'a', mpgGreedy) = 1);
  Assert(StrZMatchPatternA('a*', 'aa', mpgGreedy) = 2);
  Assert(StrZMatchPatternA('a*', 'abc', mpgGreedy) = 3);
  Assert(StrZMatchPatternA('a*b', 'a', mpgGreedy) < 0);
  Assert(StrZMatchPatternA('a*b', 'ab', mpgGreedy) = 2);
  Assert(StrZMatchPatternA('a*b', 'acb', mpgGreedy) = 3);
  Assert(StrZMatchPatternA('a*b', 'acbd', mpgGreedy) = 3);
  Assert(StrZMatchPatternA('a*b', 'acccbd', mpgGreedy) = 5);
  Assert(StrZMatchPatternA('a*b', 'acbcb', mpgGreedy) = 5);
  Assert(StrZMatchPatternA('a*b', 'acbcbd', mpgGreedy) = 5);
  Assert(StrZMatchPatternA('a*b', 'bb', mpgGreedy) < 0);

  Assert(StrZMatchPatternA('*', '', mpgGreedyNoBacktrack) = 0);
  Assert(StrZMatchPatternA('*', 'a', mpgGreedyNoBacktrack) = 1);
  Assert(StrZMatchPatternA('*', 'aa', mpgGreedyNoBacktrack) = 2);
  Assert(StrZMatchPatternA('a*', '', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('a*', 'a', mpgGreedyNoBacktrack) = 1);
  Assert(StrZMatchPatternA('a*', 'aa', mpgGreedyNoBacktrack) = 2);
  Assert(StrZMatchPatternA('a*', 'abc', mpgGreedyNoBacktrack) = 3);
  Assert(StrZMatchPatternA('a*b', 'a', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('a*b', 'ab', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('a*b', 'acb', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('a*b', 'acbd', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('a*b', 'acccbd', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('a*b', 'acbcb', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('a*b', 'acbcbd', mpgGreedyNoBacktrack) < 0);
  Assert(StrZMatchPatternA('a*b', 'bb', mpgGreedyNoBacktrack) < 0);

  Assert(StrZMatchPatternA('a[b]', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[b]', 'ab', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[b]', 'abb', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[b]', 'aa', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[^a]', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[^a]', 'ab', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[^a]', 'abb', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[^a]', 'aa', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[0-9]', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[0-9]', 'a0', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[0-9]', 'aa', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[0-9]', 'a99', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[^0-9]', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[^0-9]', 'a0', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[^0-9]', 'aa', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[^0-9]', 'aaa', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[0-9a-z]', 'a', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[0-9a-z]', 'aa', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[0-9a-z]', 'az', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[0-9a-z]', 'a0', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[0-9a-z]', 'a9', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[0-9a-z]', 'a00', mpgLazy) = 2);
  Assert(StrZMatchPatternA('a[0-9a-z]', 'aA', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[0-9a-z]', 'aA0', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[0-9a-z]a', 'aaa', mpgLazy) = 3);
  Assert(StrZMatchPatternA('a[0-9a-z]a', 'aa', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[0-9a-z]a', 'aab', mpgLazy) < 0);
  Assert(StrZMatchPatternA('a[0-9a-z][AB]', 'aaA', mpgLazy) = 3);
  Assert(StrZMatchPatternA('a[0-9a-z][AB]', 'aaC', mpgLazy) < 0);

  Assert(StrZMatchPatternA('\r', #13, mpgLazy) = 1);
  Assert(StrZMatchPatternA('\\', '\', mpgLazy) = 1);
  Assert(StrZMatchPatternA('\.', '.', mpgLazy) = 1);
  Assert(StrZMatchPatternA('[\r]', #13, mpgLazy) = 1);
  Assert(StrZMatchPatternA('[\\]', '\', mpgLazy) = 1);
  Assert(StrZMatchPatternA('[\-]', '-', mpgLazy) = 1);
  Assert(StrZMatchPatternA('[\-]', '\', mpgLazy) < 0);
  Assert(StrZMatchPatternA('[\-\.]', '.', mpgLazy) = 1);

  Assert(StrPosPatternA('', '', I, 1, mpgGreedy) = 0);            Assert(I = 0);
  Assert(StrPosPatternA('', 'a', I, 1, mpgGreedy) = 1);           Assert(I = 0);
  Assert(StrPosPatternA('a', '', I, 1, mpgGreedy) = 0);           Assert(I = 0);
  Assert(StrPosPatternA('a*b', 'xacb', I, 1, mpgGreedy) = 2);     Assert(I = 3);
  Assert(StrPosPatternA('a*b', 'xaccbd', I, 1, mpgGreedy) = 2);   Assert(I = 4);
  Assert(StrPosPatternA('a*b', 'xa', I, 1, mpgGreedy) = 0);       Assert(I = 0);
  {$ENDIF}

  Assert(StrPosPatternU('', '', I, 1, mpgGreedy) = 0);            Assert(I = 0);
  Assert(StrPosPatternU('', 'a', I, 1, mpgGreedy) = 1);           Assert(I = 0);
  Assert(StrPosPatternU('a', '', I, 1, mpgGreedy) = 0);           Assert(I = 0);
  Assert(StrPosPatternU('a*b', 'xacb', I, 1, mpgGreedy) = 2);     Assert(I = 3);
  Assert(StrPosPatternU('a*b', 'xaccbd', I, 1, mpgGreedy) = 2);   Assert(I = 4);
  Assert(StrPosPatternU('a*b', 'xa', I, 1, mpgGreedy) = 0);       Assert(I = 0);
end;

procedure Test_MatchQuantSeq;
var I : Integer;
begin
  {$IFDEF SupportAnsiString}
  Assert(MatchQuantSeqB(I, [csAlpha], [mqOnce], 'a', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqAny], 'a', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqLeastOnce], 'a', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqOptional], 'a', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqOnce], 'ab', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqAny], 'ab', []));
  Assert(I = 2);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqLeastOnce], 'ab', []));
  Assert(I = 2);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqOptional], 'ab', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqOnce], 'abc', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqAny], 'abc', []));
  Assert(I = 3);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqLeastOnce], 'abc', []));
  Assert(I = 3);
  Assert(MatchQuantSeqB(I, [csAlpha], [mqOptional], 'abc', []));
  Assert(I = 1);
  Assert(not MatchQuantSeqB(I, [csAlpha, csNumeric], [mqOnce, mqOnce], 'ab12', []));
  Assert(I = 0);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric], [mqAny, mqOnce], 'abc123', []));
  Assert(I = 4);
  Assert(not MatchQuantSeqB(I, [csAlpha, csNumeric], [mqLeastOnce, mqAny], '123', []));
  Assert(I = 0);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric], [mqOptional, mqAny], '123abc', []));
  Assert(I = 3);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric], [mqOnce, mqAny], 'a123', []));
  Assert(I = 4);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric], [mqAny, mqAny], 'abc123', []));
  Assert(I = 6);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric], [mqLeastOnce, mqOnce], 'ab123', []));
  Assert(I = 3);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric], [mqOptional, mqOptional], '1', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric], [mqOptional, mqOptional], 'a', []));
  Assert(I = 1);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric], [mqOnce, mqOptional], 'ab', []));
  Assert(I = 1);
  Assert(not MatchQuantSeqB(I, [csAlpha, csNumeric], [mqOptional, mqOnce], 'ab', []));
  Assert(I = 0);
  Assert(MatchQuantSeqB(I, [csAlphaNumeric, csNumeric, csAlpha, csNumeric],
                          [mqLeastOnce, mqAny, mqOptional, mqOnce], 'a1b2', []));
  Assert(I = 4);
  Assert(MatchQuantSeqB(I, [csAlphaNumeric, csNumeric, csAlpha, csNumeric],
                          [mqAny, mqOnce, mqOptional, mqOnce], 'a1b2cd3efg4', []));
  Assert(I = 4);
  Assert(MatchQuantSeqB(I, [csAlphaNumeric, csNumeric], [mqAny, mqOptional], 'a1', [moDeterministic]));
  Assert(I = 2);
  Assert(not MatchQuantSeqB(I, [csAlphaNumeric, csNumeric], [mqAny, mqOnce], 'a1', [moDeterministic]));
  Assert(I = 0);
  Assert(MatchQuantSeqB(I, [csAlpha, csNumeric, csAlpha, csAlphaNumeric],
                          [mqAny, mqOnce, mqAny, mqLeastOnce], 'a1b2cd3efg4', [moDeterministic]));
  Assert(I = 11);
  Assert(MatchQuantSeqB(I, [csAlphaNumeric, csNumeric], [mqAny, mqOptional], 'a1', [moNonGreedy]));
  Assert(I = 0);
  Assert(MatchQuantSeqB(I, [csAlphaNumeric, csNumeric], [mqAny, mqLeastOnce], 'a1', [moNonGreedy]));
  Assert(I = 2);
  Assert(not MatchQuantSeqB(I, [csAlphaNumeric, csNumeric], [mqAny, mqOnce], 'abc', [moNonGreedy]));
  Assert(I = 0);
  Assert(MatchQuantSeqB(I, [csAlphaNumeric, csNumeric, csAlpha, csNumeric],
                          [mqAny, mqOnce, mqOnce, mqLeastOnce], 'a1bc2de3g4', [moNonGreedy]));
  Assert(I = 10);
  {$ENDIF}
end;

procedure Test;
begin
  Test_MatchFileMask;
  Test_MatchPattern;
  Test_MatchQuantSeq;
end;
{$ENDIF}



end.

