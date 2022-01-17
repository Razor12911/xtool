{*****************************************************************************
  The DEC team (see file NOTICE.txt) licenses this file
  to you under the Apache License, Version 2.0 (the
  "License"); you may not use this file except in compliance
  with the License. A copy of this licence is found in the root directory of
  this project in the file LICENCE.txt or alternatively at

    http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing,
  software distributed under the License is distributed on an
  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied.  See the License for the
  specific language governing permissions and limitations
  under the License.
*****************************************************************************}
unit TestDECTestDataContainer;

interface

type
  ITestDataInputVector = interface
  ['{CEC7AE49-DA2D-438A-BE8B-2BC2FA1DBCD0}']
    function GetRunCount:UInt32;
    function GetData:RawByteString;

    /// <summary>
    ///   Number of times this test needs to be run to produce the final test data
    /// </summary>
    property RepeatCount:UInt32
      read   GetRunCount;
    /// <summary>
    ///   Input data for the test
    /// </summary>
    property Data:RawByteString
      read   GetData;
  end;

  ITestDataInputVectorList = interface
  ['{34CEDD4B-4249-4C69-A0CC-C89F90A9B4E3}']
    function GetCount:Integer;
    function GetVector(aIndex:Integer):ITestDataInputVector;

    property Count:Integer
      read   GetCount;
    property Vectors[aIndex:integer]:ITestDataInputVector
      read   GetVector; default;

    function AddInputVector(const aData:RawByteString; const aRunCount:UInt32=1;
                            const aConcatCount:UInt32=1;
                            const aFinalBitCount:UInt32 = 0):ITestDataInputVector;
  end;

  ITestDataRow = interface
  ['{A105BADC-46E9-4A1D-B338-5C8E60305823}']
    function GetInputData:RawByteString;
    function GetInputVectors:ITestDataInputVectorList;
    function GetOutputData:RawByteString;
    function GetOutputUTFStrTest:RawByteString;

    property InputData : RawByteString
      read   GetInputData;
    property InputDataVectors : ITestDataInputVectorList
      read   GetInputVectors;
    property ExpectedOutput : RawByteString
      read   GetOutputData;
    property ExpectedOutputUTFStrTest : RawByteString
      read   GetOutputUTFStrTest;
  end;

  ITestDataRowSetup = interface
  ['{DCB0F980-7120-41A6-BD02-118B594E2AB6}']
    procedure SetExpectedOutput(const aValue:RawByteString);
    procedure SetExpectedOutputUTFStrTest(const aValue:RawByteString);

    property ExpectedOutput : RawByteString
      Write  SetExpectedOutput;
    property ExpectedOutputUTFStrTest : RawByteString
      Write  SetExpectedOutputUTFStrTest;

    procedure AddInputVector(const aData:RawByteString; const aRunCount:UInt32=1;
                             const aConcatCount:UInt32=1);
  end;

  ITestDataContainer = interface
  ['{65205874-94D9-424C-8314-4816D33CECA4}']
    function GetCount:Integer;
    property Count:Integer
      read   GetCount;

    procedure Clear;
  end;

  // ---------------------------------------------------------------------------

  IHashTestDataRowSetup = interface(ITestDataRowSetup)
  ['{ADB4AFA2-4199-47F4-86F6-E84A20C3AA8E}']
    /// <summary>
    ///   Specifies the length of the hash value generated for those hash classes
    ///   which support configurable output lengths.
    /// </summary>
    /// <param name="aValue">
    ///   Length of the calculated hash value in byte
    /// </param>
    procedure SetRequiredDigestSize(const aValue:UInt32);
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. The SHA3 tests using this require to specify what this last
    ///   byte contains if it is not contained in the test data itsself already
    ///   (because that would influence the length of that test data)
    /// </summary>
    /// <param name="aValue">
    ///   Value which is used in the finalization of the hash calculation if
    ///   SHA3's feature of bit length specification of the input data is being
    ///   used.
    /// </param>
    procedure SetPaddingByte(const aValue:Byte);
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. This property specifies how many bits of the last byte shall
    ///   be processed.
    /// </summary>
    /// <param name="aValue">
    ///   Number of bits of the last byte within the test data to process
    /// </param>
    procedure SetFinalBitLength(const aValue: Int16);

    /// <summary>
    ///   Specifies the length of the hash value generated for those hash classes
    ///   which support configurable output lengths. The length is specified in
    ///   byte.
    /// </summary>
    property RequiredDigestSize : UInt32
      Write  SetRequiredDigestSize;

    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. This property specifies how many bits of the last byte shall
    ///   be processed.
    /// </summary>
    property PaddingByte        : Byte
      Write  SetPaddingByte;
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. This property specifies how many bits of the last byte shall
    ///   be processed.
    /// </summary>
    property FinalBitLength     : Int16
      write  SetFinalBitLength;
  end;

  IHashTestDataRow = interface(ITestDataRow)
  ['{73ED2877-967A-410B-8493-636F099FBA60}']
    /// <summary>
    ///   Gets the length of the hash value generated for those hash classes
    ///   which support configurable output lengths.
    /// </summary>
    /// <returns>
    ///   Length of the calculated hash value in byte
    /// </returns>
    function GetRequiredDigestSize:UInt32;
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. The SHA3 tests using this require to specify what this last
    ///   byte contains if it is not contained in the test data itsself already
    ///   (because that would influence the length of that test data)
    /// </summary>
    /// <returns>
    ///   Value which is used in the finalization of the hash calculation if
    ///   SHA3's feature of bit length specification of the input data is being
    ///   used.
    /// </returns>
    function GetPaddingByte:Byte;
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. This property specifies how many bits of the last byte shall
    ///   be processed.
    /// </summary>
    function GetFinalBitLength:Int16;

    /// <summary>
    ///   Gets the length in bytes of the hash value generated for those
    ///   hash classes which support configurable output lengths.
    /// </summary>
    property RequiredDigestSize : UInt32
      read   GetRequiredDigestSize;
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. The SHA3 tests using this require to specify what this last
    ///   byte contains if it is not contained in the test data itsself already
    ///   (because that would influence the length of that test data).
    ///
    ///   Value which is used in the finalization of the hash calculation if
    ///   SHA3's feature of bit length specification of the input data is being
    ///   used.
    /// </summary>
    property PaddingByte        : Byte
      read   GetPaddingByte;
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. This property specifies how many bits of the last byte shall
    ///   be processed.
    /// </summary>
    property FinalByteLength     : Int16
      read   GetFinalBitLength;
  end;

  IHashTestDataContainer = interface(ITestDataContainer)
  ['{BDF2082D-3133-48D8-B9AA-87F3485FD91F}']
    function GetRows(aIndex:Integer):IHashTestDataRow;
    property Rows[aIndex:Integer]:IHashTestDataRow
      read   GetRows; default;

    function AddRow:IHashTestDataRowSetup;
  end;

function CreateTestDataContainer:ITestDataContainer;

implementation

uses
  Classes;

type
  TTestDataInputVector = class(TInterfacedObject, ITestDataInputVector)
  private
    FData          : RawByteString;
    FRunCount      : UInt32;
    FFinalBitCount : UInt32;
  protected // ITestDataInputVector
    function GetRunCount:UInt32;
    function GetData:RawByteString;
  public
    constructor Create(const aData:RawByteString; const aRunCount:UInt32; const aFinalBitCOunt: UInt32);
  end;

  /// <summary>
  ///   All methods are protected by design so that nobody directly uses this class.
  ///   It shall be used via the ITestDataInputVectorContainer interface, which
  ///   automatically makes the methods allowed to be used externally public
  /// </summary>
  TTestDataInputVectorList = class(TInterfacedObject, ITestDataInputVectorList)
  private
    /// <summary>
    ///   List of all the input values for the tests
    /// </summary>
    FVectors : TInterfaceList;
  protected // ITestDataInputVectorContainer
    /// <summary>
    ///   Returns the number of test data entries (vectors) stored in the list
    /// </summary>
    function GetCount:Integer;
    /// <summary>
    ///   Returns the test data vector specified by the index
    /// </summary>
    /// <param name="aIndex">
    ///   Index of the vector which shall be returned
    /// </param>
    /// <returns>
    ///   Test data entry
    /// </returns>
    function GetVector(aIndex:Integer):ITestDataInputVector;
    /// <summary>
    ///   Adds an input vector for one test to the list
    /// </summary>
    /// <param name="aData">
    ///   Test data for the vector
    /// </param>
    /// <param name="aRunCount">
    ///   Number of times the test shall be repeated on the data given, default = 1
    /// </param>
    /// <param name="aConcatCount">
    ///   Number of times aData is being concatenated to form the real input data
    ///   for this test vector
    /// </param>
    /// <param name="aFinalBitCount">
{ TODO : Rework comment as far as necessary after implementing this }
    ///   some hash algorithms allow to specify the size of the data to calculate
    ///   the hash from in bits. If this parameter is set to a value > 0 the last
    ///   byte of the data specified in aData is not hashed with the "Calc" method
    ///   but the aFinalBitCount number of bits from it are hashed in Done or so...
    /// </param>
    /// <returns>
    ///   An interface to the generated test vector
    /// </returns>
    function AddInputVector(const aData:RawByteString;
                            const aRunCount:UInt32=1;
                            const aConcatCount:UInt32=1;
                            const aFinalBitCount:UInt32 = 0):ITestDataInputVector;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  THashTestDataRow = class(TInterfacedObject, ITestDataRow, ITestDataRowSetup,
                           IHashTestDataRow, IHashTestDataRowSetup)
  private
    FInputData:RawByteString;
    FInputVectors:ITestDataInputVectorList;
    FOutputData:RawByteString;
    FOutputUTFStrTest:RawByteString;
    FReqDigSize:UInt32;
    FPaddingByte:Byte;
    FFinalBitLength: Int16;
  protected // ITestDataRow
    function GetInputData:RawByteString;
    function GetInputVectors:ITestDataInputVectorList;
    function GetOutputData:RawByteString;
    function GetOutputUTFStrTest:RawByteString;
  protected // ITestDataRowSetup
    procedure SetExpectedOutput(const aValue:RawByteString);
    procedure SetExpectedOutputUTFStrTest(const aValue:RawByteString);
    procedure AddInputVector(const aData:RawByteString; const aRunCount:UInt32=1;
                             const aConcatCount:UInt32=1);
  protected // IHashTestDataRow
    /// <summary>
    ///   Gets the length of the hash value generated for those hash classes
    ///   which support configurable output lengths.
    /// </summary>
    /// <returns>
    ///   Length of the calculated hash value in byte
    /// </returns>
    function GetRequiredDigestSize:UInt32;
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. The SHA3 tests using this require to specify what this last
    ///   byte contains if it is not contained in the test data itsself already
    ///   (because that would influence the length of that test data)
    /// </summary>
    /// <returns>
    ///   Value which is used in the finalization of the hash calculation if
    ///   SHA3's feature of bit length specification of the input data is being
    ///   used.
    /// </returns>
    function GetPaddingByte:Byte;
    function GetFinalBitLength:Int16;
  protected // IHashTestDataRowSetup
    /// <summary>
    ///   Specifies the length of the hash value generated for those hash classes
    ///   which support configurable output lengths.
    /// </summary>
    /// <param name="aValue">
    ///   Length of the calculated hash value in byte
    /// </param>
    procedure SetRequiredDigestSize(const aValue:UInt32);
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. The SHA3 tests using this require to specify what this last
    ///   byte contains if it is not contained in the test data itsself already
    ///   (because that would influence the length of that test data)
    /// </summary>
    /// <param name="aValue">
    ///   Value which is used in the finalization of the hash calculation if
    ///   SHA3's feature of bit length specification of the input data is being
    ///   used.
    /// </param>
    procedure SetPaddingByte(const aValue:Byte);
    /// <summary>
    ///   Required parameter for SHA3 tests: SHA3 allows to specify the number of
    ///   bits of the input bytes which shall be processed. This can lead to the
    ///   situation that the last byte of the input data shall not be processed
    ///   completely. This property specifies how many bits of the last byte shall
    ///   be processed.
    /// </summary>
    /// <param name="aValue">
    ///   Number of bits of the last byte within the test data to process
    /// </param>
    procedure SetFinalBitLength(const aValue: Int16);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  /// <summary>
  ///   List of all the test vectors of a unit test for one of the hash classes
  /// </summary>
  TTestDataList = class(TInterfacedObject, ITestDataContainer, IHashTestDataContainer)
  private
    FDataRows:TInterfaceList;
  protected // ITestDataContainer
    function GetCount:Integer;
    function GetRows(aIndex:Integer):ITestDataRow;
    procedure Clear;
  protected // IHashTestDataContainer
    function HASH_AddRow:IHashTestDataRowSetup;             function IHashTestDataContainer.AddRow = HASH_Addrow;
    function HASH_GetRows(aIndex:Integer):IHashTestDataRow; function IHashTestDataContainer.GetRows = HASH_GetRows;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function CreateTestDataContainer:ITestDataContainer;
begin
  result := TTestDataList.Create;
end;

{ TTestDataContainer }

procedure TTestDataList.Clear;
begin
  FDataRows.Clear;
end;

constructor TTestDataList.Create;
begin
  inherited Create;
  FDataRows := TInterfaceList.Create;
end;

destructor TTestDataList.Destroy;
begin
  FDataRows.Free;
  inherited;
end;

function TTestDataList.GetCount: Integer;
begin
  result := FDataRows.Count;
end;

function TTestDataList.GetRows(aIndex: Integer): ITestDataRow;
begin
  result := FDataRows.Items[aIndex] as ITestDataRow;
end;

function TTestDataList.HASH_AddRow: IHashTestDataRowSetup;
begin
  Result := THashTestDataRow.Create;
  FDataRows.Add(Result);
end;

function TTestDataList.HASH_GetRows(aIndex: Integer): IHashTestDataRow;
begin
  result := FDataRows.Items[aIndex] as IHashTestDataRow;
end;

{ TTestDataRow }

procedure THashTestDataRow.AddInputVector(const aData: RawByteString; const aRunCount, aConcatCount: UInt32);
var
  lData:RawByteString;
  Idx:Integer;
  lVector:ITestDataInputVector;
begin
  lVector := FInputVectors.AddInputVector(aData, aRunCount, aConcatCount);

  lData := '';
  for Idx := 1 to lVector.RepeatCount do
  begin
    lData := lData + lVector.Data;
  end;

  FInputData := FInputData + lData;
end;

constructor THashTestDataRow.Create;
begin
  inherited Create;
  FInputVectors := TTestDataInputVectorList.Create;
end;

destructor THashTestDataRow.Destroy;
begin
  FInputVectors := NIL;
  inherited;
end;

function THashTestDataRow.GetFinalBitLength: Int16;
begin
  result := FFinalBitLength;
end;

function THashTestDataRow.GetInputData: RawByteString;
begin
  result := FInputData;
end;

function THashTestDataRow.GetInputVectors: ITestDataInputVectorList;
begin
  result := FInputVectors;
end;

function THashTestDataRow.GetOutputData: RawByteString;
begin
  result := FOutputData;
end;

function THashTestDataRow.GetOutputUTFStrTest: RawByteString;
begin
  result := FOutputUTFStrTest;
end;

function THashTestDataRow.GetPaddingByte: Byte;
begin
  result := FPaddingByte;
end;

function THashTestDataRow.GetRequiredDigestSize: UInt32;
begin
  result := FReqDigSize;
end;

procedure THashTestDataRow.SetExpectedOutput(const aValue: RawByteString);
begin
  FOutputData := aValue;
end;

procedure THashTestDataRow.SetExpectedOutputUTFStrTest(const aValue: RawByteString);
begin
  FOutputUTFStrTest := aValue;
end;

procedure THashTestDataRow.SetFinalBitLength(const aValue: Int16);
begin
  FFinalBitLength := aValue;
end;

procedure THashTestDataRow.SetPaddingByte(const aValue: Byte);
begin
  FPaddingByte := aValue;
end;

procedure THashTestDataRow.SetRequiredDigestSize(const aValue: UInt32);
begin
  FReqDigSize := aValue;
end;

{ TTestDataInputVectorContainer }

function TTestDataInputVectorList.AddInputVector(const aData:RawByteString;
           const aRunCount:UInt32=1; const aConcatCount:UInt32=1;
           const aFinalBitCount:UInt32 = 0):ITestDataInputVector;
var
  lData : RawByteString;
  Idx   : Integer;
begin
  lData := '';
  for Idx := 1 to aConcatCount do
  begin
    lData := lData + aData;
  end;
  Result := TTestDataInputVector.Create(lData, aRunCount, aFinalBitCount);
  FVectors.Add(Result);
end;

constructor TTestDataInputVectorList.Create;
begin
  inherited Create;
  FVectors := TInterfaceList.Create;
end;

destructor TTestDataInputVectorList.Destroy;
begin
  FVectors.Free;
  inherited;
end;

function TTestDataInputVectorList.GetCount: Integer;
begin
  result := FVectors.Count;
end;

function TTestDataInputVectorList.GetVector(aIndex: Integer): ITestDataInputVector;
begin
  result := FVectors.Items[aIndex] as ITestDataInputVector;
end;

{ TTestDataInputVector }

constructor TTestDataInputVector.Create(const aData: RawByteString;
                                        const aRunCount: UInt32;
                                        const aFinalBitCOunt: UInt32);
begin
  inherited Create;
  FData          := aData;
  FRunCount      := aRunCount;
  FFinalBitCount := aFinalBitCount;
end;

function TTestDataInputVector.GetData: RawByteString;
begin
  result := FData;
end;

function TTestDataInputVector.GetRunCount: UInt32;
begin
  result := FRunCount;
end;

end.
