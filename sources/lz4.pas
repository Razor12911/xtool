(*
  LZ4Delphi
  Copyright (C) 2015, Jose Pascoa (atelierwebgm@gmail.com)
  BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

  *************************************************************************
  LZ4 - Fast LZ compression algorithm
  xxHash - Fast Hash algorithm
  LZ4 source repository : http://code.google.com/p/lz4/
  xxHash source repository : http://code.google.com/p/xxhash/
  Copyright (c) 2011-2014, Yann Collet
  BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

  * Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above
  copyright notice, this list of conditions and the following disclaimer
  in the documentation and/or other materials provided with the
  distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  ******************************************************************************
*)

{$POINTERMATH ON}
unit lz4;

interface

uses Windows;

const
  MINMATCH = 4;
  COPYLENGTH = 8;
  LASTLITERALS = 5;
  _MFLIMIT = COPYLENGTH + MINMATCH;
  MAXD_LOG = 16;
  MAX_DISTANCE = (1 shl MAXD_LOG) - 1;
  STEPSIZE = sizeof(size_t);
  ML_BITS = 4;
  ML_MASK = (1 shl ML_BITS) - 1;
  RUN_BITS = 8 - ML_BITS;
  RUN_MASK = (1 shl RUN_BITS) - 1;

type
  ppByte = ^pByte;
  dict_directive = (noDict = 0, withPrefix64k, usingExtDict);
  endCondition_directive = (endOnOutputSize = 0, endOnInputSize = 1);
  earlyEnd_directive = (full = 0, partial = 1);

function LZ4_decompress_generic(const source: pointer; const dest: pointer;
  inputSize: integer; outputSize: integer;
  endOnInput: integer = integer(endOnOutputSize);
  partialDecoding: integer = integer(full); targetOutputSize: integer = 0;
  dict: integer = integer(noDict); const lowPrefix: pByte = nil;
  const dictStart: pByte = nil; const dictSize: size_t = 0): integer;

implementation

function LZ4_read_ARCH(const p: pointer): size_t; inline;
begin
{$IFDEF WIN64}
  result := size_t(pUint64(p)^)
{$ELSE}
  result := size_t(pCardinal(p)^);
{$ENDIF}
end;

function LZ4_read32(const memPtr: pointer): cardinal;
begin
  result := pCardinal(memPtr)^;
end;

{$IFDEF WIN64}

function LZ4_NbCommonBytesx64(value: size_t): cardinal;
asm
  bsf rax, rcx // value comes in rcx register
  shr eax, 3
end;
{$ENDIF}

function LZ4_count(pIn: pByte; pMatch: pByte; const pInLimit: pByte): cardinal;
var
  pStart: pByte;
  diff: size_t;
  incValue: cardinal;

  calcedPByte: pByte;
begin
  pStart := pIn;
  calcedPByte := pInLimit - (STEPSIZE - 1);

  while pIn < calcedPByte do
  begin
    diff := LZ4_read_ARCH(pMatch) xor LZ4_read_ARCH(pIn);
    if (diff = 0) then
    begin
      inc(pIn, STEPSIZE);
      inc(pMatch, STEPSIZE);
      continue;
    end;
{$IFDEF WIN32}
    asm
      bsf eax, diff
      shr eax, 3
      mov incValue, eax
    end;
{$ELSE}
    incValue := LZ4_NbCommonBytesx64(diff);
    // x64 mode does not allow asm inline
{$ENDIF}
    inc(pIn, incValue);
    exit(cardinal(pIn - pStart));
  end;
{$IFDEF WIN64}
  if (pIn < (pInLimit - 3)) and (pCardinal(pMatch)^ = pCardinal(pIn)^) then
  begin
    inc(pIn, 4);
    inc(pMatch, 4);
  end;
{$ENDIF}
  if ((pIn < (pInLimit - 1)) and (pWord(pMatch)^ = pWord(pIn)^)) then
  begin
    inc(pIn, 2);
    inc(pMatch, 2);
  end;
  if ((pIn < pInLimit) and (pMatch^ = pIn^)) then
    inc(pIn);
  result := cardinal(pIn - pStart);
end;

function LZ4_read16(const memPtr: pointer): word; inline;
begin
  result := pWord(memPtr)^;
end;

function LZ4_read64(const memPtr: pointer): uint64; inline;
begin
  result := pUint64(memPtr)^;
end;

{$IFDEF WILDCOPY_ASM}
{$IFDEF WIN32}

procedure LZ4_wildCopy;
// (dstPtr: pointer; const srcPtr: pointer; dstEnd: pointer);
asm
  push edi
  push esi
  mov edi, eax
  mov esi, edx
  // copyCount := (((e - d) - 1) div 8) * 8 + 8;
  sub ecx, eax // (e - d)
  dec ecx		 // e - d) - 1)
  shr ecx, 3   // ((e - d) - 1) div 8)
  shl ecx, 3	 // ((e - d) - 1) div 8) * 8
  add ecx, 8   // ((e - d) - 1) div 8) * 8 + 8

  // if copyCount <= 0 then
  // copyCount := 8;
  mov eax, 8
  cmp ecx, 0
  cmovbe ecx, eax
  shr ecx, 2
  rep movsd
  pop esi
  pop edi
end;

{$ELSE}

procedure LZ4_wildCopy;
// (dstPtr: pointer; const srcPtr: pointer; dstEnd: pointer);
asm
  mov r10, rdi
  mov r11, rsi
  mov rdi, rcx
  mov rsi, rdx
  // copyCount := (((e - d) - 1) div 8) * 8 + 8;
  sub r8, rcx  // (dstEnd - dest)
  mov rax, 8
  dec r8		 // e - d) - 1)
  shr r8, 3	 // ((e - d) - 1) div 8)
  shl r8, 3	 // ((e - d) - 1) div 8) * 8
  add r8, rax	 // ((e - d) - 1) div 8) * 8 + 8
  cmp r8, 0
  cmovbe r8, rax
  mov rcx, r8
  shr rcx, 3
  rep movsq
  mov rdi, r10
  mov rsi, r11
end;
{$ENDIF}
{$ELSE}

procedure LZ4_wildCopy(dstPtr: pointer; const srcPtr: pointer;
  dstEnd: pointer); inline;
var
  d: pByte;
  s: pByte;
  e: pByte;
begin
  d := dstPtr;
  s := srcPtr;
  e := dstEnd;
  repeat
{$IFDEF WIN32}
    // pCardinal(d)[0] := pCardinal(s)[0];
    // pCardinal(d)[1] := pCardinal(s)[1];
    pUint64(d)^ := pUint64(s)^;
{$ELSE}
    pUint64(d)^ := pUint64(s)^;
{$ENDIF}
    inc(d, 8);
    inc(s, 8);
  until not(d < e);
end;

{$ENDIF}

procedure LZ4_writeLE16(memPtr: pointer; value: word); inline;
begin
  pWord(memPtr)^ := value;

end;

function LZ4_decompress_generic(const source: pointer; const dest: pointer;
  inputSize: integer; outputSize: integer; endOnInput: integer;
  partialDecoding: integer; targetOutputSize: integer; dict: integer;
  const lowPrefix: pByte; const dictStart: pByte;
  const dictSize: size_t): integer;
var
  ip: pByte;
  iend: pByte;
  op: pByte;
  oend: pByte;
  cpy: pByte;
  oexit: pByte;
  lowLimit: pByte;
  dictEnd: pByte;
  safeDecode: Boolean;
  checkOffset: Boolean;
  token: cardinal;
  length: size_t;
  match: pByte;
  s: cardinal;
  booleantest: Boolean;
  copySize: size_t;
  endOfMatch: pByte;
  copyFrom: pByte;
  dec64: size_t;
const
  dec32table: array [0 .. 7] of size_t = (4, 1, 2, 1, 4, 4, 4, 4);
  dec64table: array [0 .. 7] of size_t = (0, 0, 0, size_t(-1), 0, 1, 2, 3);
label
  _output_error;
begin
  ip := pByte(source);
  iend := ip + inputSize;
  op := pByte(dest);
  oend := op + outputSize;
  oexit := op + targetOutputSize;
  lowLimit := lowPrefix - dictSize;
  dictEnd := pByte(dictStart) + dictSize;
  safeDecode := (endOnInput = integer(endOnInputSize));
  checkOffset := ((safeDecode) and (dictSize < 65536));

  if (partialDecoding <> 0) and (oexit > oend - _MFLIMIT) then
    oexit := oend - _MFLIMIT;
  if (endOnInput <> 0) and (outputSize = 0) then
  begin
    if (inputSize = 1) and (ip^ = 0) then
      exit(0)
    else
      exit(-1);
  end;
  if (endOnInput = 0) and (outputSize = 0) then
  begin
    if ip^ = 0 then
      exit(1)
    else
      exit(-1);
  end;

  while True do
  begin
    token := ip^;
    inc(ip);
    length := token shr ML_BITS;
    if length = RUN_MASK then
    begin
      while True do
      begin
        s := ip^;
        inc(ip);
        inc(length, s);
        if endOnInput <> 0 then
        begin
          if not(ip < iend - RUN_MASK) then
            break;
        end;
        if s <> 255 then
          break;
      end;
      if safeDecode and (size_t(op + length) < size_t(op)) then
        goto _output_error;
      if safeDecode and (size_t(ip + length) < size_t(ip)) then
        goto _output_error;
    end;
    cpy := op + length;
    if partialDecoding <> 0 then
      booleantest := cpy > oexit
    else
      booleantest := cpy > oend - _MFLIMIT;

    if ((endOnInput <> 0) and ((booleantest) or
      (ip + length > iend - (2 + 1 + LASTLITERALS)))) or
      ((endOnInput = 0) and (cpy > oend - COPYLENGTH)) then
    begin
      if partialDecoding <> 0 then
      begin
        if (cpy > oend) then
          goto _output_error;
        if ((endOnInput <> 0) and (ip + length > iend)) then
          goto _output_error;
      end
      else
      begin
        if ((endOnInput = 0) and (cpy <> oend)) then
          goto _output_error;
        if ((endOnInput <> 0) and ((ip + length <> iend) or (cpy > oend))) then
          goto _output_error;
      end;
      move(ip^, op^, length);
      inc(ip, length);
      inc(op, length);
      break;
    end;
    LZ4_wildCopy(op, ip, cpy);
    inc(ip, length);
    op := cpy;
    match := cpy - LZ4_read16(ip); // LZ4_readLE16 = LZ4_read16 for unaligned
    inc(ip, 2);

    if checkOffset and (match < lowLimit) then
      goto _output_error;

    length := token and ML_MASK;
    if length = ML_MASK then
    begin
      while True do
      begin
        if ((endOnInput <> 0) and (ip > iend - LASTLITERALS)) then
          goto _output_error;
        s := ip^;
        inc(ip);
        inc(length, s);
        if s <> 255 then
          break;
      end;
      if safeDecode and (size_t(op + length) < size_t(op)) then
        goto _output_error;
    end;
    inc(length, MINMATCH);
    if (dict = integer(usingExtDict)) and (match < lowPrefix) then
    begin
      if op + length > oend - LASTLITERALS then
        goto _output_error;
      if (length <= size_t(lowPrefix - match)) then
      begin
        match := dictEnd - (lowPrefix - match);
        move(match^, op^, length);
        inc(op, length);
      end
      else
      begin
        copySize := size_t(lowPrefix - match);
        move((dictEnd - copySize)^, op^, copySize);
        inc(op, copySize);
        copySize := length - copySize;
        if copySize > size_t(op - lowPrefix) then
        begin
          endOfMatch := op + copySize;
          copyFrom := lowPrefix;
          while (op < endOfMatch) do
          begin
            op^ := copyFrom^;
            inc(op);
            inc(copyFrom);
          end;
        end
        else
        begin
          move(lowPrefix^, op^, copySize);
          inc(op, copySize);
        end;
      end;
      continue;
    end;
    cpy := op + length;
    if (op - match) < 8 then
    begin
      dec64 := dec64table[op - match];
      op[0] := match[0];
      op[1] := match[1];
      op[2] := match[2];
      op[3] := match[3];
      inc(match, dec32table[op - match]);
      pCardinal(op + 4)^ := pCardinal(match)^;
      inc(op, 8);
      dec(match, dec64);
    end
    else
    begin
{$IFDEF WIN64}
      pUint64(op)^ := pUint64(match)^;
{$ELSE}
      pCardinal(op)[0] := pCardinal(match)[0];
      pCardinal(op)[1] := pCardinal(match)[1];
{$ENDIF}
      inc(op, 8);
      inc(match, 8);
    end;

    if cpy > oend - 12 then
    begin
      if (cpy > oend - LASTLITERALS) then
        goto _output_error;
      if (op < oend - 8) then
      begin
        LZ4_wildCopy(op, match, oend - 8);
        inc(match, (oend - 8) - op);
        op := oend - 8;
      end;
      while (op < cpy) do
      begin
        op^ := match^;
        inc(op);
        inc(match);
      end;
    end
    else
      LZ4_wildCopy(op, match, cpy);
    op := cpy;
  end;
  if (endOnInput <> 0) then
    result := integer(op - pByte(dest))
  else
    result := integer(ip - pByte(source));
  exit;
_output_error:
  result := -(ip - pByte(source)) - 1;
end;

end.
