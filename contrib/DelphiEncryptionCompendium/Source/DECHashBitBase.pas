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

/// <summary>
///   Base unit for all the hash algorithms which can operate on bit sized
///   messsages as well.
/// </summary>
unit DECHashBitBase;

interface

{$INCLUDE DECOptions.inc}

uses
  {$IFDEF FPC}
  SysUtils, Classes,
  {$ELSE}
  System.SysUtils, System.Classes,
  {$ENDIF}
  DECHashAuthentication, DECHashInterface, DECUtil;

type
  /// <summary>
  ///   Base class for all hash algorithms which can operate on bit sized
  ///   messsages as well.
  /// </summary>
  TDECHashBit = class(TDECHashAuthentication, IDECHashBitsized)
  strict private
    /// <summary>
    ///   Returns the number of bits the final byte of the message consists of
    /// </summary>
    function GetFinalByteLength: UInt8;
    /// <summary>
    ///   Sets the number of bits the final byte of the message consists of
    /// </summary>
    procedure SetFinalByteLength(const Value: UInt8);
  public
    /// <summary>
    ///   Setting this to a number of bits allows to process messages which have
    ///   a length which is not a exact multiple of bytes.
    /// </summary>
    property FinalByteLength : UInt8
      read   GetFinalByteLength
      write  SetFinalByteLength;
  end;

implementation

resourcestring
  /// <summary>
  ///   Exception message for the exception raised when a to long final byte
  ///   length is specified.
  /// </summary>
  rFinalByteLengthTooBig = 'Final byte length too big (%0:d) must be 0..7';

{ TDECHashBit }

function TDECHashBit.GetFinalByteLength: UInt8;
begin
  Result := FFinalByteLength;
end;

procedure TDECHashBit.SetFinalByteLength(const Value: UInt8);
begin
  // if length of final byte is 8 this value shall be 0 as the normal specification
  // of message length is good enough then.
  Assert(Value < 8, 'Length of final byte too big, a byte has 8 bit maximum');

  if (Value > 7) then
    raise EDECHashException.CreateFmt(rFinalByteLengthTooBig, [Value]);

  FFinalByteLength := Value;
end;

end.
