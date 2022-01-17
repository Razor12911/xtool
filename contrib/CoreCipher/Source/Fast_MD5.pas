{ ****************************************************************************** }
{ * Fast md5                                                                   * }
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
unit Fast_MD5;

{$INCLUDE zDefine.inc}

interface


uses CoreClasses, UnicodeMixedLib;

function FastMD5(const buffPtr: PByte; bufSiz: nativeUInt): TMD5; overload;
function FastMD5(stream: TCoreClassStream; StartPos, EndPos: Int64): TMD5; overload;

implementation

{$IF Defined(MSWINDOWS) and Defined(Delphi)}


uses MemoryStream64;

(*
  fastMD5 algorithm by Maxim Masiutin
  https://github.com/maximmasiutin/MD5_Transform-x64

  delphi imp by 600585@qq.com
  https://github.com/PassByYou888/FastMD5
*)

{$IF Defined(WIN32)}
(*
  ; ==============================================================
  ;
  ; MD5_386.Asm   -  386 optimized helper routine for calculating
  ;                  MD Message-Digest values
  ; written 2/2/94 by
  ;
  ; Peter Sawatzki
  ; Buchenhof 3
  ; D58091 Hagen, Germany Fed Rep
  ;
  ; EMail: Peter@Sawatzki.de
  ; EMail: 100031.3002@compuserve.com
  ; WWW:   http://www.sawatzki.de
  ;
  ;
  ; original C Source was found in Dr. Dobbs Journal Sep 91
  ; MD5 algorithm from RSA Data Security, Inc.
*)
{$L MD5_32.obj}
{$ELSEIF Defined(WIN64)}
(*
  ; MD5_Transform-x64
  ; MD5 transform routine oprimized for x64 processors
  ; Copyright 2018 Ritlabs, SRL
  ; The 64-bit version is written by Maxim Masiutin <max@ritlabs.com>

  ; The main advantage of this 64-bit version is that
  ; it loads 64 bytes of hashed message into 8 64-bit registers
  ; (RBP, R8, R9, R10, R11, R12, R13, R14) at the beginning,
  ; to avoid excessive memory load operations
  ; througout the routine.

  ; To operate with 32-bit values store in higher bits
  ; of a 64-bit register (bits 32-63) uses "Ror" by 32;
  ; 8 macro variables (M1-M8) are used to keep record
  ; or corrent state of whether the register has been
  ; Ror'ed or not.

  ; It also has an ability to use Lea instruction instead
  ; of two sequental Adds (uncomment UseLea=1), but it is
  ; slower on Skylake processors. Also, Intel in the
  ; Optimization Reference Maual discourages us of
  ; Lea as a replacement of two adds, since it is slower
  ; on the Atom processors.

  ; MD5_Transform-x64 is released under a dual license,
  ; and you may choose to use it under either the
  ; Mozilla Public License 2.0 (MPL 2.1, available from
  ; https://www.mozilla.org/en-US/MPL/2.0/) or the
  ; GNU Lesser General Public License Version 3,
  ; dated 29 June 2007 (LGPL 3, available from
  ; https://www.gnu.org/licenses/lgpl.html).

  ; MD5_Transform-x64 is based
  ; on the following code by Peter Sawatzki.

  ; The original notice by Peter Sawatzki follows.
*)
{$L MD5_64.obj}
{$ENDIF}

procedure MD5_Transform(var Accu; const Buf); register; external;

function FastMD5(const buffPtr: PByte; bufSiz: nativeUInt): TMD5;
var
  Digest: TMD5;
  Lo, Hi: Cardinal;
  p: PByte;
  ChunkIndex: Byte;
  ChunkBuff: array [0 .. 63] of Byte;
begin
  Lo := 0;
  Hi := 0;
  PCardinal(@Digest[0])^ := $67452301;
  PCardinal(@Digest[4])^ := $EFCDAB89;
  PCardinal(@Digest[8])^ := $98BADCFE;
  PCardinal(@Digest[12])^ := $10325476;

  inc(Lo, bufSiz shl 3);
  inc(Hi, bufSiz shr 29);

  p := buffPtr;

  while bufSiz >= $40 do
    begin
      MD5_Transform(Digest, p^);
      inc(p, $40);
      dec(bufSiz, $40);
    end;
  if bufSiz > 0 then
      CopyPtr(p, @ChunkBuff[0], bufSiz);

  Result := PMD5(@Digest[0])^;
  ChunkBuff[bufSiz] := $80;
  ChunkIndex := bufSiz + 1;
  if ChunkIndex > $38 then
    begin
      if ChunkIndex < $40 then
          FillPtrByte(@ChunkBuff[ChunkIndex], $40 - ChunkIndex, 0);
      MD5_Transform(Result, ChunkBuff);
      ChunkIndex := 0
    end;
  FillPtrByte(@ChunkBuff[ChunkIndex], $38 - ChunkIndex, 0);
  PCardinal(@ChunkBuff[$38])^ := Lo;
  PCardinal(@ChunkBuff[$3C])^ := Hi;
  MD5_Transform(Result, ChunkBuff);
end;

function FastMD5(stream: TCoreClassStream; StartPos, EndPos: Int64): TMD5;
const
  deltaSize: Cardinal = $40 * $FFFF;

var
  Digest: TMD5;
  Lo, Hi: Cardinal;
  DeltaBuf: Pointer;
  bufSiz: Int64;
  Rest: Cardinal;
  p: PByte;
  ChunkIndex: Byte;
  ChunkBuff: array [0 .. 63] of Byte;
begin
  if StartPos > EndPos then
      Swap(StartPos, EndPos);
  StartPos := umlClamp(StartPos, 0, stream.Size);
  EndPos := umlClamp(EndPos, 0, stream.Size);
  if EndPos - StartPos <= 0 then
    begin
      Result := FastMD5(nil, 0);
      exit;
    end;
{$IFDEF OptimizationMemoryStreamMD5}
  if stream is TCoreClassMemoryStream then
    begin
      Result := FastMD5(Pointer(nativeUInt(TCoreClassMemoryStream(stream).Memory) + StartPos), EndPos - StartPos);
      exit;
    end;
  if stream is TMemoryStream64 then
    begin
      Result := FastMD5(TMemoryStream64(stream).PositionAsPtr(StartPos), EndPos - StartPos);
      exit;
    end;
{$ENDIF}
  //
  Lo := 0;
  Hi := 0;
  PCardinal(@Digest[0])^ := $67452301;
  PCardinal(@Digest[4])^ := $EFCDAB89;
  PCardinal(@Digest[8])^ := $98BADCFE;
  PCardinal(@Digest[12])^ := $10325476;

  bufSiz := EndPos - StartPos;
  Rest := 0;

  inc(Lo, bufSiz shl 3);
  inc(Hi, bufSiz shr 29);

  DeltaBuf := GetMemory(deltaSize);
  stream.Position := StartPos;

  if bufSiz < $40 then
    begin
      stream.read(DeltaBuf^, bufSiz);
      p := DeltaBuf;
    end
  else
    while bufSiz >= $40 do
      begin
        if Rest = 0 then
          begin
            if bufSiz >= deltaSize then
                Rest := deltaSize
            else
                Rest := bufSiz;
            stream.ReadBuffer(DeltaBuf^, Rest);

            p := DeltaBuf;
          end;
        MD5_Transform(Digest, p^);
        inc(p, $40);
        dec(bufSiz, $40);
        dec(Rest, $40);
      end;

  if bufSiz > 0 then
      CopyPtr(p, @ChunkBuff[0], bufSiz);

  FreeMemory(DeltaBuf);

  Result := PMD5(@Digest[0])^;
  ChunkBuff[bufSiz] := $80;
  ChunkIndex := bufSiz + 1;
  if ChunkIndex > $38 then
    begin
      if ChunkIndex < $40 then
          FillPtrByte(@ChunkBuff[ChunkIndex], $40 - ChunkIndex, 0);
      MD5_Transform(Result, ChunkBuff);
      ChunkIndex := 0
    end;
  FillPtrByte(@ChunkBuff[ChunkIndex], $38 - ChunkIndex, 0);
  PCardinal(@ChunkBuff[$38])^ := Lo;
  PCardinal(@ChunkBuff[$3C])^ := Hi;
  MD5_Transform(Result, ChunkBuff);
end;

{$ELSE}


function FastMD5(const buffPtr: PByte; bufSiz: nativeUInt): TMD5;
begin
  Result := umlMD5(buffPtr, bufSiz);
end;

function FastMD5(stream: TCoreClassStream; StartPos, EndPos: Int64): TMD5;
begin
  Result := umlStreamMD5(stream, StartPos, EndPos);
end;

{$ENDIF Defined(MSWINDOWS) and Defined(Delphi)}

end.
