{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals TLS                                         }
{   File name:        flcTLSCompress.pas                                       }
{   File version:     5.03                                                     }
{   Description:      TLS compression                                          }
{                                                                              }
{   Copyright:        Copyright (c) 2008-2020, David J Butler                  }
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
{   2008/01/18  0.01  Initial development.                                     }
{   2010/11/30  0.02  Revision.                                                }
{   2018/07/17  5.03  Revised for Fundamentals 5.                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE flcTLS.inc}

{$IFDEF TLS_ZLIB_DISABLE}
  {$UNDEF TLS_COMPRESS_ZLIB}
{$ELSE}
  {$DEFINE TLS_COMPRESS_ZLIB}
{$ENDIF}

unit flcTLSCompress;

interface

uses
  { TLS }

  flcTLSConsts,
  flcTLSErrors,
  flcTLSAlgorithmTypes;



{                                                                              }
{ Fragment compression                                                         }
{                                                                              }
procedure TLSCompressFragment(
          const CompressionMethod: TTLSCompressionMethod;
          const PlainTextBuf; const PlainTextSize: Integer;
          var CompressedBuf; const CompressedBufSize: Integer;
          var CompressedSize: Integer);

procedure TLSDecompressFragment(
          const CompressionMethod: TTLSCompressionMethod;
          const CompressedBuf; const CompressedSize: Integer;
          var PlainTextBuf; const PlainTextBufSize: Integer;
          var PlainTextSize: Integer);



implementation

{$IFDEF TLS_COMPRESS_ZLIB}
uses
  { Utils }
  flcZLib;
{$ENDIF}



{                                                                              }
{ Fragment compression                                                         }
{                                                                              }
procedure TLSCompressFragment(
          const CompressionMethod: TTLSCompressionMethod;
          const PlainTextBuf; const PlainTextSize: Integer;
          var CompressedBuf; const CompressedBufSize: Integer;
          var CompressedSize: Integer);
{$IFDEF TLS_COMPRESS_ZLIB}
var OutBuf : Pointer;
    OutSize : Integer;
{$ENDIF}
begin
  if (PlainTextSize <= 0) or
     (PlainTextSize > TLS_PLAINTEXT_FRAGMENT_MAXSIZE) then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  case CompressionMethod of
    tlscmNull :
      begin
        if CompressedBufSize < PlainTextSize then
          raise ETLSError.Create(TLSError_InvalidBuffer);
        Move(PlainTextBuf, CompressedBuf, PlainTextSize);
        CompressedSize := PlainTextSize;
      end;
    {$IFDEF TLS_COMPRESS_ZLIB}
    tlscmDeflate :
      begin
        ZLibCompressBuf(@PlainTextBuf, PlainTextSize, OutBuf, OutSize, zclDefault);
        if CompressedBufSize < OutSize then
          raise ETLSError.Create(TLSError_InvalidBuffer);
        if OutSize > TLS_COMPRESSED_FRAGMENT_MAXSIZE then
          raise ETLSError.Create(TLSError_InvalidBuffer); // compressed fragment larger than maximum allowed size
        Move(OutBuf^, CompressedBuf, OutSize);
        FreeMem(OutBuf);
        CompressedSize := OutSize;
      end;
    {$ENDIF}
  else
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid compression method');
  end;
end;

procedure TLSDecompressFragment(
          const CompressionMethod: TTLSCompressionMethod;
          const CompressedBuf; const CompressedSize: Integer;
          var PlainTextBuf; const PlainTextBufSize: Integer;
          var PlainTextSize: Integer);
{$IFDEF TLS_COMPRESS_ZLIB}
var OutBuf : Pointer;
    OutSize : Integer;
{$ENDIF}
begin
  if (CompressedSize < 0) or
     (CompressedSize > TLS_COMPRESSED_FRAGMENT_MAXSIZE) then
    raise ETLSError.Create(TLSError_InvalidBuffer);
  case CompressionMethod of
    tlscmNull :
      begin
        if PlainTextBufSize < CompressedSize then
          raise ETLSError.Create(TLSError_InvalidBuffer);
        Move(CompressedBuf, PlainTextBuf, CompressedSize);
        PlainTextSize := CompressedSize;
      end;
    {$IFDEF TLS_COMPRESS_ZLIB}
    tlscmDeflate :
      begin
        ZLibDecompressBuf(@CompressedBuf, CompressedSize, OutBuf, OutSize);
        if PlainTextBufSize < OutSize then
          raise ETLSError.Create(TLSError_InvalidBuffer);
        if OutSize > TLS_PLAINTEXT_FRAGMENT_MAXSIZE then
          raise ETLSError.Create(TLSError_InvalidBuffer); // uncompressed fragment larger than maximum allowed size
        Move(OutBuf^, PlainTextBuf, OutSize);
        FreeMem(OutBuf);
        PlainTextSize := OutSize;
      end;
    {$ENDIF}
  else
    raise ETLSError.Create(TLSError_InvalidParameter, 'Invalid compression method');
  end;
end;



end.

