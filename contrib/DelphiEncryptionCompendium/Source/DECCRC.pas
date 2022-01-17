{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory
  of this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}

{
  Implementation of threadsafe CRC checksum functions.

  The following standard CRCs are supported:
    CRC-8, CRC-10, CRC-12 (Mobil Telephone),
    CRC-16, CRC-16-CCITT, CRC-16-ZModem,
    CRC-24 (PGP's MIME64 Armor CRC),
    CRC-32, CRC-32-CCITT and CRC-32-ZModem.

  How to use:

  var
    CRC16: UInt16;
  begin
    CRC16 := CRCCalc(CRC_16, Data, SizeOf(Data)); // all in one
  end;

  or

  var
    CRC: TCRCDef;
    CRC32: UInt32;
  begin
    CRCInit(CRC, CRC_32);                         // setup CRC data structure
    CRCCode(CRC, Data, SizeOf(Data));             // calcs CRC for "Data"
    CRCCode(CRC, PChar(string)^, Length(string) * SizeOf(string[1])); // calcs CRC for String
    CRC32 := CRCDone(CRC);                        // returns correct combined CRC for Data and String
    // after CRCDone we can start a new calculation
  end;
}

unit DECCRC;

interface

{$INCLUDE DECOptions.inc}

type
  /// <summary>
  ///   CRC Definition Structure
  /// </summary>
  PCRCDef = ^TCRCDef;

  /// <summary>
  ///   Record with meta data about a single CRC algorithm/polynom
  ///   Do *not* reorder or change this structure
  /// <para>
  ///   SizeOf(TCRCDef) = 1056 = 0420h
  /// </para>
  /// </summary>
  TCRCDef = packed record
    /// <summary>
    ///   Lookup Table, precomputed in CRCSetup
    /// </summary>
    Table       : array[0..255] of UInt32;
    /// <summary>
    ///   Intermediate CRC
    /// </summary>
    CRC         : UInt32;
    /// <summary>
    ///   Is this Polynomial an inverse function?
    /// </summary>
    Inverse     : LongBool;
    /// <summary>
    ///   Shift Value for CRCCode (for more speed)
    /// </summary>
    Shift       : UInt32;
    /// <summary>
    ///   Start Value of CRC cComputation
    /// </summary>
    InitVector  : UInt32;
    /// <summary>
    ///   Final XOR Vector of computed CRC
    /// </summary>
    FinalVector : UInt32;
    /// <summary>
    ///   Precomputed AND Mask of computed CRC
    /// </summary>
    Mask        : UInt32;
    /// <summary>
    ///   Bitsize of CRC
    /// </summary>
    Bits        : UInt32;
    /// <summary>
    ///   Used Polynomial
    /// </summary>
    Polynomial  : UInt32;
  end;

  /// <summary>
  ///   predefined standard CRC Types
  /// </summary>
  TCRCType = (
    CRC_8,
    CRC_10,
    CRC_12,
    CRC_16,
    CRC_16CCITT,
    CRC_16XModem,
    CRC_24,
    CRC_32,
    CRC_32CCITT,
    CRC_32ZModem,
    CRC_8ATMHEC,
    CRC_8SMBus,
    CRC_15CAN,
    CRC_16ZMODEM
  );

type
  /// <summary>
  ///   Callback method used by some CRC calculation routines to fetch the data
  ///   to be processed
  /// </summary>
  /// <param name="Buffer">
  ///   Buffer containing the data to be processed
  /// </param>
  /// <param name="Count">
  ///   Number of bytes of the buffer to be processed
  /// </param>
  /// <returns>
  ///
  /// </returns>
  TReadMethod = function(var Buffer; Count: Int64): Int64 of object;

// initialize CRC Definition with a custom Algorithm

/// <summary>
///   Fills the individual fields of a CRC meta data structure
/// </summary>
/// <param name="CRCDef">
///   Structure whose fields shall be filled
/// </param>
/// <param name="Polynomial">
///   CRC polynome, defining the algorithm
/// </param>
/// <param name="Bits">
///   Size of the CRC value to be computed in bits. Needs to be at least 8
/// </param>
/// <param name="InitVector">
///   Initial value for the vector going into each calculation cycle
/// </param>
/// <param name="FinalVector">
///   Final XOR Vector of computed CRC
/// </param>
/// <param name="Inverse">
///   true if this Polynomial is an inverse function
/// </param>
/// <returns>
///   true on success, false when a number smaller 8 is being passed as Bits parameter
/// </returns>
function CRCSetup(var CRCDef: TCRCDef;
                  Polynomial, Bits, InitVector, FinalVector: UInt32;
                  Inverse: LongBool): Boolean;

/// <summary>
///   Retrieves the necessary meta data and precomputed tables for a given CRC
///   algorithm.
/// </summary>
/// <param name="CRCDef">
///   Record in which the to be retrieved meta data will be returned
/// </param>
/// <param name="CRCType">
///   Specifies the exact CRC type which shall be initialized
/// </param>
/// <returns>
///   true on success
/// </returns>
function CRCInit(var CRCDef: TCRCDef; CRCType: TCRCType): Boolean;

/// <summary>
///   Calculate the CRC of the contents of the passed in buffer.
/// </summary>
/// <param name="CRCDef">
///   Structure with the necessary metadata for the CRC algorithm to be used.
///   CRC processing state is being updated during calculation to enable this
///   structure to be fed in another call to CRCCode if a CRC over multiple
///   buffers has to be calculated.
/// </param>
/// <param name="Buffer">
///   Buffer with the data the CRC shall be calculated from
/// </param>
/// <param name="Size">
///   Number of bytes to calculate the CRC from, starting at the beginning of
///   the buffer
/// </param>
/// <returns>
///   Calculated CRC value, including any necessary correction (like CRCDone).
///   CRCDef.CRC holds the actual computed CRC, additional calls of CRCCode
///   compute the total CRC of split buffers
/// </returns>
function CRCCode(var CRCDef: TCRCDef; const Buffer; Size: UInt32): UInt32; overload;

/// <summary>
///   Calculate the CRC of the contents provided by a given callback
/// </summary>
/// <param name="CRCDef">
///   Structure with the necessary metadata for the CRC algorithm to be used.
///   CRC processing state is being updated during calculation to enable this
///   structure to be fed in another call to CRCCode if a CRC over multiple
///   buffers has to be calculated.
/// </param>
/// <param name="ReadMethod">
///   Callback which is being called to get the data the CRC is processed over,
///   e.g. TStream.Read
/// </param>
/// <param name="Size">
///   Number of bytes over which the CRC will be calculated. The callback will
///   be called until that number of bytes have been processed.
/// </param>
/// <returns>
///   Calculated CRC value, including any necessary correction (like CRCDone).
///   CRCDef.CRC holds the actual computed CRC, additional calls of CRCCode
///   compute the total CRC of split buffers
/// </returns>
function CRCCode(var CRCDef: TCRCDef;
                 ReadMethod: TReadMethod;
                 Size: UInt32 = $FFFFFFFF): UInt32; overload;

{ TODO :
DUnitTests für die Callback-Methoden Varianten von CRCCode und CRCCalc
schreiben }
//
//    CRCInit(CRC, CRC_32);                         // setup CRC data structure
//    CRCCode(CRC, Data, SizeOf(Data));             // calcs CRC for "Data"
//    CRCCode(CRC, PChar(string)^, Length(string) * SizeOf(string[1])); // calcs CRC for String
//    CRC32 := CRCDone(CRC);

// returns corrected CRC as definied in CRCDef and resets CRCDef.CRC to InitVector

/// <summary>
///   Corrects the CRC via the final vector and resets the internal intermediate
///   CRC value to the init vector so the next CRC calculation can start.
/// </summary>
/// <param name="CRCDef">
///   Structure with the current CRC state
/// </param>
/// <returns>
///   Final CRC value
/// </returns>
function CRCDone(var CRCDef: TCRCDef): UInt32;

/// <summary>
///   Calculates a CRC over some Buffer with Size Bytes length. Processing is
///   being done in one single step
/// </summary>
/// <param name="CRCType">
///   Specifies the CRC algorithm to be used
/// </param>
/// <param name="Buffer">
///   Buffer with the data to calculate the CRC from
/// </param>
/// <param name="Size">
///   Number of bytes over which the CRC will be calculated from the beginning
///   of the buffer
/// </param>
function CRCCalc(CRCType: TCRCType; const Buffer; Size: UInt32): UInt32; overload;

/// <summary>
///   Calculates a CRC. Data is passed via callback, which is called repeatedly
///   if necessary
/// </summary>
/// <param name="CRCType">
///   Specifies the CRC algorithm to be used
/// </param>
/// <param name="ReadMethod">
///   Callback which is being called to get the data the CRC is processed over
///   e.g. TStream.Read
/// </param>
/// <param name="Size">
///   Number of bytes over which the CRC will be calculated. The callback will
///   be called until that number of bytes have been processed.
/// </param>
/// <returns>
///   Calculated CRC value.
/// </returns>
function CRCCalc(CRCType    : TCRCType;
                 ReadMethod : TReadMethod;
                 Size       : UInt32 = $FFFFFFFF): UInt32; overload;

/// <summary>
///   Calculates a CRC according a predefined CRC16-Standard over some Buffer
///   with Size Bytes length. Processing is being done in one single step
/// </summary>
/// <remarks>
///   call CRC := CRC16(0, Data, SizeOf(Data));
/// </remarks>
/// <param name="CRC">
///   Specifies the CRC algorithm to be used
/// </param>
/// <param name="Buffer">
///   Buffer with the data to calculate the CRC from
/// </param>
/// <param name="Size">
///   Number of bytes over which the CRC will be calculated from the beginning
///   of the buffer
/// </param>
/// <returns>
///   Calculated CRC16 value
/// </returns>
function CRC16(CRC: UInt16; const Buffer; Size: UInt32): UInt16;

/// <summary>
///   Calculates a CRC according the CRC32-CCITT standard over some Buffer
///   with Size Bytes length. Processing is being done in one single step
/// </summary>
/// <remarks>
///   call CRC := CRC32(0, Data, SizeOf(Data));
/// </remarks>
/// <param name="CRC">
///   Specifies the CRC algorithm to be used
/// </param>
/// <param name="Buffer">
///   Buffer with the data to calculate the CRC from
/// </param>
/// <param name="Size">
///   Number of bytes over which the CRC will be calculated from the beginning
///   of the buffer
/// </param>
/// <returns>
///   Calculated CRC32 value
/// </returns>
function CRC32(CRC: UInt32; const Buffer; Size: UInt32): UInt32;

implementation

{$IFOPT Q+}{$DEFINE RESTORE_OVERFLOWCHECKS}{$Q-}{$ENDIF}
{$IFOPT R+}{$DEFINE RESTORE_RANGECHECKS}{$R-}{$ENDIF}

type
  PCRCTab = ^TCRCTab;
  /// <summary>
  ///   Array type for the meta data definitions of the individual CRC algorithms
  /// </summary>
  TCRCTab = array[TCRCType] of packed record
    Poly, Bits, Init, FInit: UInt32;
    Inverse: LongBool;
  end;

const
  /// <summary>
  ///   Table containing meta data of various well known CRC algorithms/polynoms
  /// </summary>
  CRCTab : TCRCTab = (
    (Poly: $000000D1; Bits: 08; Init: $00000000; FInit: $00000000; Inverse: True),  // CRC_8  GSM/ERR
    (Poly: $00000233; Bits: 10; Init: $00000000; FInit: $00000000; Inverse: True),  // CRC_10 ATM/OAM Cell
    (Poly: $0000080F; Bits: 12; Init: $00000000; FInit: $00000000; Inverse: True),  // CRC_12
    (Poly: $00008005; Bits: 16; Init: $00000000; FInit: $00000000; Inverse: True),  // CRC_16 ARC;IBM;MODBUS RTU
     // Init value of 1D0F instead of FFFF because the code doesn't fill with zeros,
     // which would otherwise be required for the CCITT variant
    (Poly: $00001021; Bits: 16; Init: $00001D0F; FInit: $00000000; Inverse: False), // CRC_16 CCITT ITU
    (Poly: $00008408; Bits: 16; Init: $00000000; FInit: $00000000; Inverse: True),  // CRC_16 XModem
    (Poly: $00864CFB; Bits: 24; Init: $00B704CE; FInit: $00000000; Inverse: False), // CRC_24
    (Poly: $9DB11213; Bits: 32; Init: $FFFFFFFF; FInit: $FFFFFFFF; Inverse: True),  // CRC_32

    (Poly: $04C11DB7; Bits: 32; Init: $FFFFFFFF; FInit: $FFFFFFFF; Inverse: True),  // CRC_32CCITT
    (Poly: $04C11DB7; Bits: 32; Init: $FFFFFFFF; FInit: $00000000; Inverse: True),  // CRC_32ZModem
    (Poly: $00000007; Bits: 08; Init: $00000000; FInit: $00000000; Inverse: True),  // CRC_8ATMHEC
    (Poly: $00000007; Bits: 08; Init: $00000000; FInit: $00000000; Inverse: False), // CRC_8SMBus
    (Poly: $00004599; Bits: 15; Init: $00000000; FInit: $00000000; Inverse: True),  // CRC_15CAN
    (Poly: $00001021; Bits: 16; Init: $00000000; FInit: $00000000; Inverse: False)  // CRC_16ZMODEM
  );

  // some other CRC's, not all yet verfied
  // DD    $00001021, 16, $0000FFFF, $00000000,  0   // CRC_16 CCITT British Aerospace
  // DD    $00004003, 16, $00000000, $00000000, -1   // CRC_16 reversed
  // DD    $00001005, 16, $00000000, $00000000, -1   // CRC_16 X25

  // https://fenix.tecnico.ulisboa.pt/downloadFile/3779571246541/BasicCrd.pdf enthält
  // eine beschreibung dieser BasicCard Smartcard incl. C-CRC Quellcode, aber die
  // Polynome konnte ich so noch nicht überprüfen
  // DD    $00000053, 16, $00000000, $00000000, -1   // BasicCard 16Bit CRC (sparse poly for Crypto MCU)
  // DD    $000000C5, 32, $00000000, $00000000, -1   // BasicCard 32Bit CRC

function CRCSetup(var CRCDef: TCRCDef; Polynomial, Bits, InitVector,
  FinalVector: UInt32; Inverse: LongBool): Boolean;
// initialize CRCDef according to the parameters, calculate the lookup table
{$IFDEF X86ASM}
asm
       CMP   ECX,8
       JB    @@8
       PUSH  EBX
       PUSH  EDI
       PUSH  ESI
       MOV   [EAX].TCRCDef.Polynomial,EDX
       MOV   [EAX].TCRCDef.Bits,ECX
       MOV   EBX,InitVector
       MOV   EDI,FinalVector
       MOV   ESI,Inverse
       MOV   [EAX].TCRCDef.CRC,EBX
       MOV   [EAX].TCRCDef.InitVector,EBX
       MOV   [EAX].TCRCDef.FinalVector,EDI
       MOV   [EAX].TCRCDef.Inverse,ESI
       XOR   EDI,EDI
       LEA   EBX,[ECX - 8]
       SUB   ECX,32
       DEC   EDI
       NEG   ECX
       SHR   EDI,CL
       MOV   [EAX].TCRCDef.Shift,EBX
       MOV   [EAX].TCRCDef.Mask,EDI
       TEST  ESI,ESI
       JZ    @@5
       XOR   EBX,EBX
       MOV   ECX,[EAX].TCRCDef.Bits
@@1:   SHR   EDX,1
       ADC   EBX,EBX
       DEC   ECX
       JNZ   @@1
       NOP
       MOV   ECX,255
       NOP
@@20:  MOV   EDX,ECX
       SHR   EDX,1
       JNC   @@21
       XOR   EDX,EBX
@@21:  SHR   EDX,1
       JNC   @@22
       XOR   EDX,EBX
@@22:  SHR   EDX,1
       JNC   @@23
       XOR   EDX,EBX
@@23:  SHR   EDX,1
       JNC   @@24
       XOR   EDX,EBX
@@24:  SHR   EDX,1
       JNC   @@25
       XOR   EDX,EBX
@@25:  SHR   EDX,1
       JNC   @@26
       XOR   EDX,EBX
@@26:  SHR   EDX,1
       JNC   @@27
       XOR   EDX,EBX
@@27:  SHR   EDX,1
       JNC   @@28
       XOR   EDX,EBX
@@28:  MOV   [EAX + ECX * 4],EDX
       DEC   ECX
       JNL   @@20
       JMP   @@7
@@5:   AND   EDX,EDI
       ROL   EDX,CL
       MOV   EBX,255
// can be coded branchfree
@@60:  MOV   ESI,EBX
       SHL   ESI,25
       JNC   @@61
       XOR   ESI,EDX
@@61:  ADD   ESI,ESI
       JNC   @@62
       XOR   ESI,EDX
@@62:  ADD   ESI,ESI
       JNC   @@63
       XOR   ESI,EDX
@@63:  ADD   ESI,ESI
       JNC   @@64
       XOR   ESI,EDX
@@64:  ADD   ESI,ESI
       JNC   @@65
       XOR   ESI,EDX
@@65:  ADD   ESI,ESI
       JNC   @@66
       XOR   ESI,EDX
@@66:  ADD   ESI,ESI
       JNC   @@67
       XOR   ESI,EDX
@@67:  ADD   ESI,ESI
       JNC   @@68
       XOR   ESI,EDX
@@68:  ROR   ESI,CL
       MOV   [EAX + EBX * 4],ESI
       DEC   EBX
       JNL   @@60
@@7:   POP   ESI
       POP   EDI
       POP   EBX
@@8:   CMC
       SBB   EAX,EAX
       NEG   EAX
end;
{$ELSE !X86ASM}
var
  Value, XorValue, OldValue: UInt32;
  Index: Integer;
  B: Boolean;
  One: Byte;
begin
  if Bits >= 8 then
  begin
    CRCDef.Polynomial := Polynomial;
    CRCDef.Bits := Bits;
    CRCDef.CRC := InitVector;
    CRCDef.InitVector := InitVector;
    CRCDef.FinalVector := FinalVector;
    CRCDef.Inverse := Inverse;
    CRCDef.Shift := Bits - 8;
    Bits := -(Bits - 32);
    CRCDef.Mask := -1 shr Byte(Bits);

    if Inverse then
    begin
      Bits := CRCDef.Bits;
      XorValue := 0;
      repeat
        Inc(XorValue, XorValue + Ord(Polynomial and $1));
        Polynomial := Polynomial shr 1;
        Dec(Bits);
      until Bits = 0;

      One := $1;
      for Index := 255 downto 0 do
      begin
        Value := Index;

        B := Boolean(Value and One); Value := Value shr 1;
        if B then Value := Value xor XorValue;

        B := Boolean(Value and One); Value := Value shr 1;
        if B then Value := Value xor XorValue;

        B := Boolean(Value and One); Value := Value shr 1;
        if B then Value := Value xor XorValue;

        B := Boolean(Value and One); Value := Value shr 1;
        if B then Value := Value xor XorValue;

        B := Boolean(Value and One); Value := Value shr 1;
        if B then Value := Value xor XorValue;

        B := Boolean(Value and One); Value := Value shr 1;
        if B then Value := Value xor XorValue;

        B := Boolean(Value and One); Value := Value shr 1;
        if B then Value := Value xor XorValue;

        B := Boolean(Value and One); Value := Value shr 1;
        if B then Value := Value xor XorValue;

        CRCDef.Table[Index] := Value;
      end;
    end
    else
    begin
      XorValue := Polynomial and CRCDef.Mask;
      XorValue := (XorValue shl Byte(Bits)) or (XorValue shr (32 - Byte(Bits)));
      for Index := 255 downto 0 do
      begin
        B := Boolean(Index and $000000080); Value := Index shl 25;
        if B then Value := Value xor XorValue;

        OldValue := Value; Inc(Value, Value);
        if Value < OldValue then Value := Value xor XorValue;

        OldValue := Value; Inc(Value, Value);
        if Value < OldValue then Value := Value xor XorValue;

        OldValue := Value; Inc(Value, Value);
        if Value < OldValue then Value := Value xor XorValue;

        OldValue := Value; Inc(Value, Value);
        if Value < OldValue then Value := Value xor XorValue;

        OldValue := Value; Inc(Value, Value);
        if Value < OldValue then Value := Value xor XorValue;

        OldValue := Value; Inc(Value, Value);
        if Value < OldValue then Value := Value xor XorValue;

        OldValue := Value; Inc(Value, Value);
        if Value < OldValue then Value := Value xor XorValue;

        Value := (Value shr Byte(Bits)) or (Value shl (32 - Byte(Bits)));
        CRCDef.Table[Index] := Value;
      end;
    end;
    Result := True;
  end
  else
    Result := False;
end;
{$ENDIF !X86ASM}

function CRCInit(var CRCDef: TCRCDef; CRCType: TCRCType): Boolean;
begin
  Result := CRCSetup(CRCDef,
                     PCRCTab(@CRCTab)[CRCType].Poly,
                     PCRCTab(@CRCTab)[CRCType].Bits,
                     PCRCTab(@CRCTab)[CRCType].Init,
                     PCRCTab(@CRCTab)[CRCType].FInit,
                     PCRCTab(@CRCTab)[CRCType].Inverse);
end;

function CRCCode(var CRCDef: TCRCDef; const Buffer; Size: UInt32): UInt32;
// do the CRC computation
{$IFDEF X86ASM}
asm
       JECXZ @@5
       TEST  EDX,EDX
       JZ    @@5
       PUSH  ESI
       PUSH  EBX
       MOV   ESI,EAX
       CMP   [EAX].TCRCDef.Inverse,0
       MOV   EAX,[ESI].TCRCDef.CRC
       JZ    @@2
       XOR   EBX,EBX
@@1:   MOV   BL,[EDX]
       XOR   BL,AL
       SHR   EAX,8
       INC   EDX
       XOR   EAX,[ESI + EBX * 4]
       DEC   ECX
       JNZ   @@1
       JMP   @@4
@@2:   PUSH  EDI
       MOV   EBX,EAX
       MOV   EDI,ECX
       MOV   ECX,[ESI].TCRCDef.Shift
       MOV   EBX,EAX
@@3:   SHR   EBX,CL
       SHL   EAX,8
       XOR   BL,[EDX]
       INC   EDX
       MOVZX EBX,BL
       XOR   EAX,[ESI + EBX * 4]
       DEC   EDI
       MOV   EBX,EAX
       JNZ   @@3
       POP   EDI
@@4:   MOV   [ESI].TCRCDef.CRC,EAX
       XOR   EAX,[ESI].TCRCDef.FinalVector
       AND   EAX,[ESI].TCRCDef.Mask
       POP   EBX
       POP   ESI
       RET
@@5:   MOV   EAX,[EAX].TCRCDef.CRC
end;
{$ELSE !X86ASM}
var
  P: PByte;
  Value: Byte;
begin
  Result := CRCDef.CRC;
  P := @Buffer;
  if (Size <> 0) and (P <> nil) then
  begin
    if CRCDef.Inverse then
    begin
      repeat
        Value := P^ xor Byte(Result);
        Result := (Result shr 8) xor CRCDef.Table[Value];
        Inc(P);
        Dec(Size);
      until Size = 0;
    end
    else
    begin
      Value := Byte(CRCDef.Shift); // move to local variable => cpu register
      repeat
        Result := (Result shl 8) xor CRCDef.Table[Byte(Result shr Value) xor P^];
        Inc(P);
        Dec(Size);
      until Size = 0;
    end;
    CRCDef.CRC := Result;
    Result := (Result xor CRCDef.FinalVector) and CRCDef.Mask;
  end;
end;
{$ENDIF !X86ASM}

function CRCCode(var CRCDef: TCRCDef; ReadMethod: TReadMethod; Size: UInt32 = $FFFFFFFF): UInt32;
var
  Buffer: array[0..1023] of Char;
  Count: Int64;
begin
  repeat
    if Size > SizeOf(Buffer) then
      Count := SizeOf(Buffer)
    else
      Count := Size;
    Count := ReadMethod(Buffer, Count);
    Result := CRCCode(CRCDef, Buffer, Count);
    Dec(Size, Count);
  until (Size = 0) or (Count = 0);
end;

function CRCDone(var CRCDef: TCRCDef): UInt32;
// finalize CRCDef after a computation
{$IFDEF X86ASM}
asm
       MOV   EDX,[EAX].TCRCDef.CRC
       MOV   ECX,[EAX].TCRCDef.InitVector
       XOR   EDX,[EAX].TCRCDef.FinalVector
       MOV   [EAX].TCRCDef.CRC,ECX
       AND   EDX,[EAX].TCRCDef.Mask
       MOV   EAX,EDX
end;
{$ELSE !X86ASM}
begin
  Result := CRCDef.CRC;
  CRCDef.CRC := CRCDef.InitVector;
  Result := (Result xor CRCDef.FinalVector) and CRCDef.Mask;
end;
{$ENDIF !X86ASM}

function CRCCalc(CRCType: TCRCType; const Buffer; Size: UInt32): UInt32;
// inplace calculation
var
  CRC: TCRCDef;
begin
  CRCInit(CRC, CRCType);
  Result := CRCCode(CRC, Buffer, Size);
end;

function CRCCalc(CRCType: TCRCType; ReadMethod: TReadMethod; Size: UInt32): UInt32;
var
  CRC: TCRCDef;
begin
  CRCInit(CRC, CRCType);
  Result := CRCCode(CRC, ReadMethod, Size);
end;

// predefined CRC16/CRC32CCITT, avoid slower lookuptable computation by use of precomputation
var
  FCRC16: PCRCDef = nil;
  FCRC32: PCRCDef = nil;

function CRC16Init: Pointer;
begin
  // Replace GetMem by GetMemory due to C++ Builder compatibility
  // GetMem(FCRC16, SizeOf(TCRCDef));
  FCRC16 := GetMemory(SizeOf(TCRCDef));
  CRCInit(FCRC16^, CRC_16);
  Result := FCRC16;
end;

function CRC16(CRC: UInt16; const Buffer; Size: UInt32): UInt16;
{$IFDEF X86ASM}
asm
       JECXZ @@2
       PUSH  EDI
       PUSH  ESI
       MOV   EDI,ECX
{$IFDEF PIC}
       MOV   ESI,[EBX].FCRC16
{$ELSE !PIC}
       MOV   ESI,FCRC16
{$ENDIF !PIC}
       XOR   ECX,ECX
       TEST  ESI,ESI
       JZ    @@3
@@1:   MOV    CL,[EDX]
       XOR    CL,AL
       SHR   EAX,8
       INC   EDX
       XOR   EAX,[ESI + ECX * 4]
       DEC   EDI
       JNZ   @@1
       POP   ESI
       POP   EDI
@@2:   RET
@@3:   PUSH  EAX
       PUSH  EDX
       CALL  CRC16Init
       MOV   ESI,EAX
       XOR   ECX,ECX
       POP   EDX
       POP   EAX
       JMP   @@1
end;
{$ELSE !X86ASM}
var
  LCRC16: PCRCDef;
  P: PByte;
  CRC32: UInt32;
  Value: Byte;
begin
  if Size <> 0 then
  begin
    LCRC16 := FCRC16;
    if LCRC16 = nil then
      LCRC16 := CRC16Init;

    CRC32 := CRC;
    P := @Buffer;
    repeat
      Value := P^ xor Byte(CRC32);
      CRC32 := (CRC32 shr 8) xor LCRC16.Table[Value];
      Inc(P);
      Dec(Size);
    until Size = 0;
    Result := UInt16(CRC32);
  end
  else
    Result := CRC;
end;
{$ENDIF !X86ASM}

function CRC32Init: Pointer;
begin
  // Replaced for C++ Builder compatibility
  // GetMem(FCRC32, SizeOf(TCRCDef));
  FCRC32 := GetMemory(SizeOf(TCRCDef));
  CRCInit(FCRC32^, CRC_32CCITT);
  Result := FCRC32;
end;

function CRC32(CRC: UInt32; const Buffer; Size: UInt32): UInt32;
{$IFDEF X86ASM}
asm
       JECXZ @@2
       PUSH  EDI
       PUSH  ESI
       NOT   EAX                    // inverse Input CRC
       MOV   EDI,ECX
{$IFDEF PIC}
       MOV   ESI,[EBX].FCRC32
{$ELSE !PIC}
       MOV   ESI,FCRC32
{$ENDIF !PIC}
       XOR   ECX,ECX
       TEST  ESI,ESI
       JZ    @@3
@@1:   MOV    CL,[EDX]
       XOR    CL,AL
       SHR   EAX,8
       INC   EDX
       XOR   EAX,[ESI + ECX * 4]
       DEC   EDI
       JNZ   @@1
       NOT   EAX                    // inverse Output CRC
       POP   ESI
       POP   EDI
@@2:   RET
@@3:   PUSH  EAX
       PUSH  EDX
       CALL  CRC32Init
       MOV   ESI,EAX
       XOR   ECX,ECX
       POP   EDX
       POP   EAX
       JMP   @@1
end;
{$ELSE !X86ASM}
var
  LCRC32: PCRCDef;
  P: PByte;
  CRC32: UInt32;
  Value: Byte;
begin
  if Size <> 0 then
  begin
    LCRC32 := FCRC32;
    if LCRC32 = nil then
      LCRC32 := CRC32Init;

    CRC32 := not CRC; // inverse Input CRC
    P := @Buffer;
    repeat
      Value := P^ xor Byte(CRC32);
      CRC32 := (CRC32 shr 8) xor LCRC32.Table[Value];
      Inc(P);
      Dec(Size);
    until Size = 0;
    Result := not CRC32; // inverse Output CRC
  end
  else
    Result := CRC;
end;
{$ENDIF !X86ASM}

procedure CRCInitThreadSafe;
begin
  CRC16Init;
  CRC32Init;
end;

{$IFDEF RESTORE_RANGECHECKS}{$R+}{$ENDIF}
{$IFDEF RESTORE_OVERFLOWCHECKS}{$Q+}{$ENDIF}

initialization
  CRCInitThreadSafe;

finalization
  if FCRC16 <> nil then
    FreeMem(FCRC16);

  if FCRC32 <> nil then
    FreeMem(FCRC32);
end.
