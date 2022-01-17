{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 5.00 - HTML Parser                          }
{   File name:        flcHTMLReader.pas                                        }
{   File version:     5.02                                                     }
{   Description:      HTML reader utilities                                    }
{                                                                              }
{   Copyright:        Copyright (c) 2000-2020, David J Butler                  }
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
{   2001/04/13  1.01  Part of cHTML unit.                                      }
{   2019/02/21  5.02  Part flcHTMLReader unit.                                 }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcHTML.inc}

unit flcHTMLReader;

interface

uses
  flcStreams,
  flcUnicodeCodecs,
  flcUnicodeReader;



{ Encoding detection functions                                                 }
function  htmlGetUnicodeCodec(const Encoding: RawByteString): TUnicodeCodecClass;
function  htmlDetectEncoding(const DocumentTop: RawByteString): RawByteString;
function  htmlDetectDocumentCodec(const DocumentTop: RawByteString): TUnicodeCodecClass;
function  htmlGetDocumentCodec(const Encoding, DocumentTop: RawByteString): TUnicodeCodecClass;



{ Unicode Document Reader constructors                                         }
function  htmlGetDocumentReader(
          const Reader: AReaderEx; const ReaderOwner: Boolean = True;
          const Encoding: RawByteString = ''): TUnicodeReader;
function  htmlGetDocumentReaderForRawString(
          const Document: RawByteString;
          const Encoding: RawByteString = ''): TUnicodeReader;
function  htmlGetDocumentReaderForFile(
          const FileName: String;
          const Encoding: RawByteString = ''): TUnicodeReader;



implementation

uses
  flcUTF,
  flcStrings;



{ Encoding detection functions                                                 }
function htmlGetUnicodeCodec(const Encoding: RawByteString): TUnicodeCodecClass;
begin
  if Encoding <> '' then
    begin
      Result := GetCodecClassByAliasA(Encoding);
    end
  else
    Result := nil;
end;

function htmlDetectEncoding(const DocumentTop: RawByteString): RawByteString;
var P: PAnsiChar;
    L: Integer;
    R: Boolean;
begin
  L := Length(DocumentTop);
  if L = 0 then
    begin
      Result := '';
      exit;
    end;
  P := Pointer(DocumentTop);
  // check if document is UTF-16 Unicode encoding
  if DetectUTF16BOM(P, L, R) then
    begin
      if not R then
        Result := 'utf16'
      else
        Result := 'utf16le';
      exit;
    end;
  // check document html meta tag
  Result := StrBetweenB(DocumentTop, 'text/html; charset=', [#0..#32, '"', '''', '>', ';'],
      False, True, False);
  if Result <> '' then
    exit;
  // find any charset indicator
  Result := StrTrimB(StrBetweenB(DocumentTop, 'charset=', ['<', '>', ';', ']'],
      False, True, False), [#0..#32, '"', '''']);
  if Result <> '' then
    exit;
end;

function htmlDetectDocumentCodec(const DocumentTop: RawByteString): TUnicodeCodecClass;
begin
  Result := htmlGetUnicodeCodec(htmlDetectEncoding(DocumentTop));
end;

function htmlGetDocumentCodec(const Encoding, DocumentTop: RawByteString): TUnicodeCodecClass;
begin
  // Check specified encoding
  Result := htmlGetUnicodeCodec(Encoding);
  if Assigned(Result) then
    exit;
  // Detect encoding
  Result := htmlDetectDocumentCodec(DocumentTop);
  if Assigned(Result) then
    exit;
  // Use default for HTML: ISO-8859-1 (Latin1)
  Result := TISO8859_1Codec;
end;



{ Unicode Document Reader constructors                                         }
function htmlGetDocumentReader(
         const Reader: AReaderEx; const ReaderOwner: Boolean;
         const Encoding: RawByteString): TUnicodeReader;
const
  DocumentSampleSize = 4096;
var
  C : TUnicodeCodecClass;
  P : Integer;
  T : RawByteString;
begin
  C := htmlGetUnicodeCodec(Encoding);
  if not Assigned(C) then
    begin
      // detect from document top
      P := Reader.Position;
      T := Reader.ReadStrB(DocumentSampleSize);
      Reader.Position := P;
      C := htmlDetectDocumentCodec(T);
    end;
  if not Assigned(C) then
    C := TISO8859_1Codec; // default codec
  Result := TUnicodeReader.Create(Reader, ReaderOwner, C.Create, True);
end;

function htmlGetDocumentReaderForRawString(const Document: RawByteString;
         const Encoding: RawByteString): TUnicodeReader;
begin
  Result := TUnicodeMemoryReader.Create(
      Pointer(Document), Length(Document),
      htmlGetDocumentCodec(Encoding, Document).Create, True);
end;

function htmlGetDocumentReaderForFile(const FileName: String;
    const Encoding: RawByteString): TUnicodeReader;
begin
  Result := htmlGetDocumentReader(
      TFileReader.Create(FileName), True, Encoding);
end;



end.

