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
///   Declarations of various datatypes, some of those have not been
///   declared for certain platforms but are used in DEC and some do change
///   meanings between platforms like PLongWord where LongWord is 32 bit on
///   Windows and Android but 64 bit on iOS for instance
/// </summary>
unit DECTypes;

interface

type
  {$IFNDEF FPC}
    {$IF CompilerVersion <= 20}
    // In D2009 NativeInt was not properly treated by the compiler under certain
    // conditions. See: http://qc.embarcadero.com/wc/qcmain.aspx?d=71292
    NativeInt = Integer;
    {$IFEND}
  {$ENDIF}

  PUInt32Array = ^TUInt32Array;
  TUInt32Array = array[0..1023] of UInt32;

  /// <summary>
  ///   Replacement for PLongWord, as LongWord changes between platforms from
  ///   32 to 64 bit
  /// </summary>
  PUInt32 = ^UINT32;

  PUInt64Array = ^TUInt64Array;
  TUInt64Array = array[0..1023] of UInt64;

implementation

end.

