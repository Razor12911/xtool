{ ****************************************************************************** }
{ * geometry 3D Advance library writen by QQ 600585@qq.com                     * }
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

unit Geometry3DUnit;

{$DEFINE FPC_DELPHI_MODE}
{$INCLUDE zDefine.inc}

interface

uses Types, CoreClasses,
  GeometryLib, Geometry2DUnit, PascalStrings, UnicodeMixedLib;

type
  TMat4 = TMatrix;
  TVec4 = TVector;
  TVec3 = TAffineVector;

  TMatrix4 = record
    Buff: TMat4;
  public
    class operator Equal(const Lhs, Rhs: TMatrix4): Boolean;
    class operator NotEqual(const Lhs, Rhs: TMatrix4): Boolean;
    class operator Multiply(const Lhs, Rhs: TMatrix4): TMatrix4;
    class operator Implicit(Value: TGeoFloat): TMatrix4;
    class operator Implicit(Value: TMat4): TMatrix4;

    function Swap: TMatrix4;
    function Lerp(M: TMatrix4; Delta: TGeoFloat): TMatrix4;
    function AffineMatrix: TAffineMatrix;
    function Invert: TMatrix4;
    function Translate(v: TVec3): TMatrix4;
    function Normalize: TMatrix4;
    function Transpose: TMatrix4;
    function AnglePreservingInvert: TMatrix4;
    function Determinant: TGeoFloat;
    function Adjoint: TMatrix4;
    function Pitch(angle: TGeoFloat): TMatrix4;
    function Roll(angle: TGeoFloat): TMatrix4;
    function Turn(angle: TGeoFloat): TMatrix4;
  end;

  TVector4 = record
    Buff: TVec4;
  private
    function GetVec3: TVec3;
    procedure SetVec3(const Value: TVec3);
    function GetVec2: TVec2;
    procedure SetVec2(const Value: TVec2);
    function GetLinkValue(index: Integer): TGeoFloat;
    procedure SetLinkValue(index: Integer; const Value: TGeoFloat);
  public
    property vec2: TVec2 read GetVec2 write SetVec2;
    property Vec3: TVec3 read GetVec3 write SetVec3;
    property XYZ: TVec3 read GetVec3 write SetVec3;
    property RGB: TVec3 read GetVec3 write SetVec3;
    property Vec4: TVec4 read Buff write Buff;
    property RGBA: TVec4 read Buff write Buff;
    property COLOR: TVec4 read Buff write Buff;
    property LinkValue[index: Integer]: TGeoFloat read GetLinkValue write SetLinkValue; default;

    class operator Equal(const Lhs, Rhs: TVector4): Boolean;
    class operator NotEqual(const Lhs, Rhs: TVector4): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TVector4): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TVector4): Boolean;
    class operator LessThan(const Lhs, Rhs: TVector4): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TVector4): Boolean;

    class operator Add(const Lhs, Rhs: TVector4): TVector4;
    class operator Add(const Lhs: TVector4; const Rhs: TGeoFloat): TVector4;
    class operator Add(const Lhs: TGeoFloat; const Rhs: TVector4): TVector4;

    class operator Subtract(const Lhs, Rhs: TVector4): TVector4;
    class operator Subtract(const Lhs: TVector4; const Rhs: TGeoFloat): TVector4;
    class operator Subtract(const Lhs: TGeoFloat; const Rhs: TVector4): TVector4;

    class operator Multiply(const Lhs, Rhs: TVector4): TVector4;
    class operator Multiply(const Lhs: TVector4; const Rhs: TGeoFloat): TVector4;
    class operator Multiply(const Lhs: TGeoFloat; const Rhs: TVector4): TVector4;
    class operator Multiply(const Lhs: TVector4; const Rhs: TMatrix4): TVector4;
    class operator Multiply(const Lhs: TMatrix4; const Rhs: TVector4): TVector4;
    class operator Multiply(const Lhs: TVector4; const Rhs: TMat4): TVector4;
    class operator Multiply(const Lhs: TMat4; const Rhs: TVector4): TVector4;
    class operator Multiply(const Lhs: TVector4; const Rhs: TAffineMatrix): TVector4;
    class operator Multiply(const Lhs: TAffineMatrix; const Rhs: TVector4): TVector4;

    class operator Divide(const Lhs, Rhs: TVector4): TVector4;
    class operator Divide(const Lhs: TVector4; const Rhs: TGeoFloat): TVector4;
    class operator Divide(const Lhs: TGeoFloat; const Rhs: TVector4): TVector4;

    class operator Implicit(Value: TGeoFloat): TVector4;
    class operator Implicit(Value: TVec4): TVector4;
    class operator Implicit(Value: TVec3): TVector4;
    class operator Implicit(Value: TVec2): TVector4;

    class operator Explicit(Value: TVector4): TVec4;
    class operator Explicit(Value: TVector4): TVec3;
    class operator Explicit(Value: TVector4): TVec2;

    procedure SetRGBA(const r, g, b, a: TGeoFloat); overload;
    procedure SetLocation(const fx, fy, fz, fw: TGeoFloat); overload;
    procedure SetLocation(const fx, fy, fz: TGeoFloat); overload;
    function Distance4D(const v2: TVector4): TGeoFloat;
    function Distance3D(const v2: TVector4): TGeoFloat;
    function Distance2D(const v2: TVector4): TGeoFloat;
    function Lerp(const v2: TVector4; const t: TGeoFloat): TVector4;
    function LerpDistance(const v2: TVector4; const d: TGeoFloat): TVector4;
    function Norm: TGeoFloat;
    function length: TGeoFloat;
    function Normalize: TVector4;
    function Cross(const v2: TVector4): TVector4; overload;
    function Cross(const v2: TVec3): TVector4; overload;
    function Cross(const v2: TVec4): TVector4; overload;
  end;

  TVector3 = record
    Buff: TVec3;
  private
    function GetVec2: TVec2;
    procedure SetVec2(const Value: TVec2);
    function GetLinkValue(index: Integer): TGeoFloat;
    procedure SetLinkValue(index: Integer; const Value: TGeoFloat);
  public
    property vec2: TVec2 read GetVec2 write SetVec2;
    property Vec3: TVec3 read Buff write Buff;
    property XYZ: TVec3 read Buff write Buff;
    property COLOR: TVec3 read Buff write Buff;
    property RGB: TVec3 read Buff write Buff;
    property LinkValue[index: Integer]: TGeoFloat read GetLinkValue write SetLinkValue; default;

    class operator Equal(const Lhs, Rhs: TVector3): Boolean;
    class operator NotEqual(const Lhs, Rhs: TVector3): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TVector3): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TVector3): Boolean;
    class operator LessThan(const Lhs, Rhs: TVector3): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TVector3): Boolean;

    class operator Add(const Lhs, Rhs: TVector3): TVector3;
    class operator Add(const Lhs: TVector3; const Rhs: TGeoFloat): TVector3;
    class operator Add(const Lhs: TGeoFloat; const Rhs: TVector3): TVector3;

    class operator Subtract(const Lhs, Rhs: TVector3): TVector3;
    class operator Subtract(const Lhs: TVector3; const Rhs: TGeoFloat): TVector3;
    class operator Subtract(const Lhs: TGeoFloat; const Rhs: TVector3): TVector3;

    class operator Multiply(const Lhs, Rhs: TVector3): TVector3;
    class operator Multiply(const Lhs: TVector3; const Rhs: TGeoFloat): TVector3;
    class operator Multiply(const Lhs: TGeoFloat; const Rhs: TVector3): TVector3;

    class operator Multiply(const Lhs: TVector3; const Rhs: TMatrix4): TVector3;
    class operator Multiply(const Lhs: TMatrix4; const Rhs: TVector3): TVector3;

    class operator Multiply(const Lhs: TVector3; const Rhs: TMat4): TVector3;
    class operator Multiply(const Lhs: TMat4; const Rhs: TVector3): TVector3;

    class operator Multiply(const Lhs: TVector3; const Rhs: TAffineMatrix): TVector3;
    class operator Multiply(const Lhs: TAffineMatrix; const Rhs: TVector3): TVector3;

    class operator Divide(const Lhs, Rhs: TVector3): TVector3;
    class operator Divide(const Lhs: TVector3; const Rhs: TGeoFloat): TVector3;
    class operator Divide(const Lhs: TGeoFloat; const Rhs: TVector3): TVector3;

    class operator Implicit(Value: TGeoFloat): TVector3;
    class operator Implicit(Value: TVec4): TVector3;
    class operator Implicit(Value: TVec3): TVector3;
    class operator Implicit(Value: TVec2): TVector3;

    class operator Explicit(Value: TVector3): TVec4;
    class operator Explicit(Value: TVector3): TVec3;
    class operator Explicit(Value: TVector3): TVec2;

    procedure SetLocation(const fx, fy, fz: TGeoFloat); overload;
    function Distance3D(const v2: TVector3): TGeoFloat;
    function Distance2D(const v2: TVector3): TGeoFloat;
    function Lerp(const v2: TVector3; const t: TGeoFloat): TVector3;
    function LerpDistance(const v2: TVector3; const d: TGeoFloat): TVector3;
    function Norm: TGeoFloat;
    function length: TGeoFloat;
    function Normalize: TVector3;
    function Cross(const v2: TVector3): TVector3;

    function Vec4(fw: TGeoFloat): TVector4; overload;
    function Vec4: TVector4; overload;
  end;

  TAABB = record
    Min, Max: TAffineVector;
  public
    { : Resize the AABB if necessary to include p. }
    procedure Include(const p: TVector3);
    { : Make an AABB that is formed by sweeping a sphere (or AABB) from Start to Dest }
    procedure FromSweep(const Start, dest: TVector3; const radius: TGeoFloat);
    { : Returns the intersection AABB of two AABBs.<p>
      If the AABBs don't intersect, will return a degenerated AABB (plane, line or point). }
    function Intersection(const aabb2: TAABB): TAABB;
    { : Adds delta to min and max of the AABB. }
    procedure Offset(const Delta: TVector3);
    { : Checks if a point "p" is inside an AABB }
    function PointIn(const p: TVector3): Boolean;
  end;

  TVector2 = record
    Buff: TVec2;
  private
    function GetLinkValue(index: Integer): TGeoFloat;
    procedure SetLinkValue(index: Integer; const Value: TGeoFloat);
  public
    property LinkValue[index: Integer]: TGeoFloat read GetLinkValue write SetLinkValue; default;

    class operator Equal(const Lhs, Rhs: TVector2): Boolean;
    class operator NotEqual(const Lhs, Rhs: TVector2): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TVector2): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TVector2): Boolean;
    class operator LessThan(const Lhs, Rhs: TVector2): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TVector2): Boolean;

    class operator Add(const Lhs, Rhs: TVector2): TVector2;
    class operator Add(const Lhs: TVector2; const Rhs: TGeoFloat): TVector2;
    class operator Add(const Lhs: TGeoFloat; const Rhs: TVector2): TVector2;

    class operator Subtract(const Lhs, Rhs: TVector2): TVector2;
    class operator Subtract(const Lhs: TVector2; const Rhs: TGeoFloat): TVector2;
    class operator Subtract(const Lhs: TGeoFloat; const Rhs: TVector2): TVector2;

    class operator Multiply(const Lhs, Rhs: TVector2): TVector2;
    class operator Multiply(const Lhs: TVector2; const Rhs: TGeoFloat): TVector2;
    class operator Multiply(const Lhs: TGeoFloat; const Rhs: TVector2): TVector2;

    class operator Divide(const Lhs, Rhs: TVector2): TVector2;
    class operator Divide(const Lhs: TVector2; const Rhs: TGeoFloat): TVector2;
    class operator Divide(const Lhs: TGeoFloat; const Rhs: TVector2): TVector2;

    class operator Implicit(Value: TGeoFloat): TVector2;
    class operator Implicit(Value: TPoint): TVector2;
    class operator Implicit(Value: TPointf): TVector2;
    class operator Implicit(Value: TVec2): TVector2;

    class operator Explicit(Value: TVector2): TPointf;
    class operator Explicit(Value: TVector2): TPoint;
    class operator Explicit(Value: TVector2): TVec2;

    procedure SetLocation(const fx, fy: TGeoFloat); overload;
    function Distance(const v2: TVector2): TGeoFloat;
    function Lerp(const v2: TVector2; const t: TGeoFloat): TVector2;
    function LerpDistance(const v2: TVector2; const d: TGeoFloat): TVector2;
    function Norm: TGeoFloat;
    function length: TGeoFloat;
    function Normalize: TVector2;
  end;

function Vector4(x, y, z, w: TGeoFloat): TVector4; overload;
function Vector4(x, y, z: TGeoFloat): TVector4; overload;
function Vector4(v: TVec3): TVector4; overload;
function Vector4(v: TVec4): TVector4; overload;

function Vector3(x, y, z: TGeoFloat): TVector3; overload;
function Vector3(v: TVec3): TVector3; overload;
function Vector3(v: TVec4): TVector3; overload;

function Vec3(const x, y, z: TGeoFloat): TVec3; overload;
function Vec3(const v: TVec4): TVec3; overload;
function Vec3(const v: TVector3): TVec3; overload;
function Vec3(const v: TVector2): TVec3; overload;
function Vec3(const v: TVector2; z: TGeoFloat): TVec3; overload;

function Vec4(const x, y, z: TGeoFloat): TVec4; overload;
function Vec4(const x, y, z, w: TGeoFloat): TVec4; overload;
function Vec4(const v: TVec3): TVec4; overload;
function Vec4(const v: TVec3; const z: TGeoFloat): TVec4; overload;
function Vec4(const v: TVector3): TVec4; overload;

function vec2(const v: TVec3): TVector2; overload;
function vec2(const v: TVec4): TVector2; overload;
function vec2(const v: TVector3): TVector2; overload;
function vec2(const v: TVector4): TVector2; overload;

function VecToStr(const v: TVec2): SystemString; overload;
function VecToStr(const v: TVector2): SystemString; overload;
function VecToStr(const v: TArrayVec2): TPascalString; overload;
function VecToStr(const v: TVec3): SystemString; overload;
function VecToStr(const v: TVec4): SystemString; overload;
function VecToStr(const v: TVector3): SystemString; overload;
function VecToStr(const v: TVector4): SystemString; overload;
function RectToStr(const v: TRectV2): SystemString; overload;
function RectToStr(const v: TRect): SystemString; overload;

function StrToVec2(const s: SystemString): TVec2;
function StrToVector2(const s: SystemString): TVector2;
function StrToArrayVec2(const s: SystemString): TArrayVec2;
function StrToVec3(const s: SystemString): TVec3;
function StrToVec4(const s: SystemString): TVec4;
function StrToVector3(const s: SystemString): TVector3;
function StrToVector4(const s: SystemString): TVector4;
function StrToRect(const s: SystemString): TRect;
function StrToRectV2(const s: SystemString): TRectV2;

function GetMin(const arry: array of TGeoFloat): TGeoFloat; overload;
function GetMin(const arry: array of Integer): Integer; overload;
function GetMax(const arry: array of TGeoFloat): TGeoFloat; overload;
function GetMax(const arry: array of Integer): Integer; overload;

function FinalAngle4FMX(const a: TGeoFloat): TGeoFloat;
function CalcAngle(const v1, v2: TVec2): TGeoFloat;
function AngleDistance(const sour, dest: TGeoFloat): TGeoFloat;
function SmoothAngle(const sour, dest, Delta: TGeoFloat): TGeoFloat;
function AngleEqual(const a1, a2: TGeoFloat): Boolean;

function Distance(const v1, v2: TVec2): TGeoFloat; overload;
function Distance(const v1, v2: TRectV2): TGeoFloat; overload;

function MovementLerp(const s, d, Lerp: TGeoFloat): TGeoFloat; overload;
function MovementLerp(const s, d: TVec2; Lerp: TGeoFloat): TVec2; overload;
function MovementLerp(const s, d: TRectV2; Lerp: TGeoFloat): TRectV2; overload;

function MovementDistance(const s, d: TVec2; dt: TGeoFloat): TVec2; overload;
function MovementDistance(const s, d: TRectV2; dt: TGeoFloat): TRectV2; overload;
function MovementDistance(const sour, dest: TVector4; Distance: TGeoFloat): TVector4; overload;
function MovementDistance(const sour, dest: TVector3; Distance: TGeoFloat): TVector3; overload;

function MovementDistanceDeltaTime(const s, d: TVec2; ASpeed: TGeoFloat): Double; overload;
function MovementDistanceDeltaTime(const s, d: TRectV2; ASpeed: TGeoFloat): Double; overload;
function AngleRollDistanceDeltaTime(const s, d: TGeoFloat; ARollSpeed: TGeoFloat): Double; overload;

function BounceVector(const Current: TVector4; DeltaDistance: TGeoFloat; const BeginVector, EndVector: TVector4; var EndFlag: Boolean): TVector4; overload;
function BounceVector(const Current: TVector3; DeltaDistance: TGeoFloat; const BeginVector, EndVector: TVector3; var EndFlag: Boolean): TVector3; overload;
function BounceVector(const Current: TVector2; DeltaDistance: TGeoFloat; const BeginVector, EndVector: TVector2; var EndFlag: Boolean): TVector2; overload;
function BounceFloat(const CurrentVal, DeltaVal, StartVal, OverVal: TGeoFloat; var EndFlag: Boolean): TGeoFloat; overload;

implementation

function Vector4(x, y, z, w: TGeoFloat): TVector4;
begin
  Result.Buff[0] := x;
  Result.Buff[1] := y;
  Result.Buff[2] := z;
  Result.Buff[3] := w;
end;

function Vector4(x, y, z: TGeoFloat): TVector4;
begin
  Result.Buff[0] := x;
  Result.Buff[1] := y;
  Result.Buff[2] := z;
  Result.Buff[3] := 0;
end;

function Vector4(v: TVec3): TVector4;
begin
  Result.Buff[0] := v[0];
  Result.Buff[1] := v[1];
  Result.Buff[2] := v[2];
  Result.Buff[3] := 0;
end;

function Vector4(v: TVec4): TVector4;
begin
  Result.Buff := v;
end;

function Vector3(x, y, z: TGeoFloat): TVector3;
begin
  Result.Buff[0] := x;
  Result.Buff[1] := y;
  Result.Buff[2] := z;
end;

function Vector3(v: TVec3): TVector3;
begin
  Result.Buff := v;
end;

function Vector3(v: TVec4): TVector3;
begin
  Result.Buff[0] := v[0];
  Result.Buff[1] := v[1];
  Result.Buff[2] := v[2];
end;

function Vec3(const x, y, z: TGeoFloat): TVec3;
begin
  Result := AffineVectorMake(x, y, z);
end;

function Vec3(const v: TVec4): TVec3;
begin
  Result[0] := v[0];
  Result[1] := v[1];
  Result[2] := v[2];
end;

function Vec3(const v: TVector3): TVec3;
begin
  Result := v.Buff;
end;

function Vec3(const v: TVector2): TVec3;
begin
  Result[0] := v[0];
  Result[1] := v[1];
  Result[2] := 0;
end;

function Vec3(const v: TVector2; z: TGeoFloat): TVec3;
begin
  Result[0] := v[0];
  Result[1] := v[1];
  Result[2] := z;
end;

function Vec4(const x, y, z: TGeoFloat): TVec4;
begin
  Result := VectorMake(x, y, z, 0);
end;

function Vec4(const x, y, z, w: TGeoFloat): TVec4;
begin
  Result := VectorMake(x, y, z, w);
end;

function Vec4(const v: TVec3): TVec4;
begin
  Result := VectorMake(v);
end;

function Vec4(const v: TVec3; const z: TGeoFloat): TVec4;
begin
  Result := VectorMake(v, z);
end;

function Vec4(const v: TVector3): TVec4;
begin
  Result := VectorMake(v.Buff);
end;

function vec2(const v: TVec3): TVector2;
begin
  Result := vec2(v[0], v[1]);
end;

function vec2(const v: TVec4): TVector2;
begin
  Result := vec2(v[0], v[1]);
end;

function vec2(const v: TVector3): TVector2;
begin
  Result[0] := v.Buff[0];
  Result[1] := v.Buff[1];
end;

function vec2(const v: TVector4): TVector2;
begin
  Result[0] := v.Buff[0];
  Result[1] := v.Buff[1];
end;

function VecToStr(const v: TVec2): SystemString;
begin
  Result := PFormat('%g,%g', [v[0], v[1]]);
end;

function VecToStr(const v: TVector2): SystemString;
begin
  Result := PFormat('%g,%g', [v[0], v[1]]);
end;

function VecToStr(const v: TArrayVec2): TPascalString;
var
  i: Integer;
begin
  Result := '';
  for i := low(v) to high(v) do
    begin
      if i <> Low(v) then
          Result.Append(',');
      Result.Append('%g,%g', [v[i, 0], v[i, 1]]);
    end;
end;

function VecToStr(const v: TVec3): SystemString;
begin
  Result := PFormat('%g,%g,%g', [v[0], v[1], v[2]]);
end;

function VecToStr(const v: TVec4): SystemString;
begin
  Result := PFormat('%g,%g,%g,%g', [v[0], v[1], v[2], v[3]]);
end;

function VecToStr(const v: TVector3): SystemString;
begin
  Result := VecToStr(v.Buff);
end;

function VecToStr(const v: TVector4): SystemString;
begin
  Result := VecToStr(v.Buff);
end;

function RectToStr(const v: TRectV2): SystemString;
begin
  Result := PFormat('%g,%g,%g,%g', [v[0][0], v[0][1], v[1][0], v[1][1]]);
end;

function RectToStr(const v: TRect): SystemString;
begin
  Result := PFormat('%d,%d,%d,%d', [v.Left, v.Top, v.Right, v.Bottom]);
end;

function StrToVec2(const s: SystemString): TVec2;
var
  v, v1, v2: U_String;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');

  Result[0] := umlStrToFloat(v1, 0);
  Result[1] := umlStrToFloat(v2, 0);
end;

function StrToVector2(const s: SystemString): TVector2;
var
  v, v1, v2: U_String;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');

  Result[0] := umlStrToFloat(v1, 0);
  Result[1] := umlStrToFloat(v2, 0);
end;

function StrToArrayVec2(const s: SystemString): TArrayVec2;
var
  n, v1, v2: U_String;
  L: TVec2List;
begin
  L := TVec2List.Create;
  n := umlTrimSpace(s);
  while n.L > 0 do
    begin
      v1 := umlGetFirstStr(n, ',: ');
      n := umlDeleteFirstStr(n, ',: ');
      v2 := umlGetFirstStr(n, ',: ');
      n := umlDeleteFirstStr(n, ',: ');
      L.Add(umlStrToFloat(v1, 0), umlStrToFloat(v2, 0));
    end;
  Result := L.BuildArray();
  DisposeObject(L);
end;

function StrToVec3(const s: SystemString): TVec3;
var
  v, v1, v2, v3: U_String;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');

  Result[0] := umlStrToFloat(v1, 0);
  Result[1] := umlStrToFloat(v2, 0);
  Result[2] := umlStrToFloat(v3, 0);
end;

function StrToVec4(const s: SystemString): TVec4;
var
  v, v1, v2, v3, v4: U_String;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v4 := umlGetFirstStr(v, ',: ');

  Result[0] := umlStrToFloat(v1, 0);
  Result[1] := umlStrToFloat(v2, 0);
  Result[2] := umlStrToFloat(v3, 0);
  Result[3] := umlStrToFloat(v4, 0);
end;

function StrToVector3(const s: SystemString): TVector3;
var
  v, v1, v2, v3: U_String;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');

  Result.Buff[0] := umlStrToFloat(v1, 0);
  Result.Buff[1] := umlStrToFloat(v2, 0);
  Result.Buff[2] := umlStrToFloat(v3, 0);
end;

function StrToVector4(const s: SystemString): TVector4;
var
  v, v1, v2, v3, v4: U_String;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v4 := umlGetFirstStr(v, ',: ');

  Result.Buff[0] := umlStrToFloat(v1, 0);
  Result.Buff[1] := umlStrToFloat(v2, 0);
  Result.Buff[2] := umlStrToFloat(v3, 0);
  Result.Buff[3] := umlStrToFloat(v4, 0);
end;

function StrToRect(const s: SystemString): TRect;
begin
  Result := Rect2Rect(StrToRectV2(s));
end;

function StrToRectV2(const s: SystemString): TRectV2;
var
  v, v1, v2, v3, v4: U_String;
begin
  v := umlTrimSpace(s);
  v1 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v2 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v3 := umlGetFirstStr(v, ',: ');
  v := umlDeleteFirstStr(v, ',: ');
  v4 := umlGetFirstStr(v, ',: ');

  Result[0][0] := umlStrToFloat(v1, 0);
  Result[0][1] := umlStrToFloat(v2, 0);
  Result[1][0] := umlStrToFloat(v3, 0);
  Result[1][1] := umlStrToFloat(v4, 0);
end;

function GetMin(const arry: array of TGeoFloat): TGeoFloat;
var
  i: Integer;
begin
  Result := arry[low(arry)];
  for i := low(arry) + 1 to high(arry) do
    if Result > arry[i] then
        Result := arry[i];
end;

function GetMin(const arry: array of Integer): Integer;
var
  i: Integer;
begin
  Result := arry[low(arry)];
  for i := low(arry) + 1 to high(arry) do
    if Result > arry[i] then
        Result := arry[i];
end;

function GetMax(const arry: array of TGeoFloat): TGeoFloat;
var
  i: Integer;
begin
  Result := arry[low(arry)];
  for i := low(arry) + 1 to high(arry) do
    if Result < arry[i] then
        Result := arry[i];
end;

function GetMax(const arry: array of Integer): Integer;
var
  i: Integer;
begin
  Result := arry[low(arry)];
  for i := low(arry) + 1 to high(arry) do
    if Result < arry[i] then
        Result := arry[i];
end;

function FinalAngle4FMX(const a: TGeoFloat): TGeoFloat;
begin
  Result := NormalizeDegAngle((-a - 90) + 180);
end;

function CalcAngle(const v1, v2: TVec2): TGeoFloat;
begin
  if IsEqual(v1, v2) then
      Result := 0
  else
      Result := RadToDeg(ArcTan2(v1[0] - v2[0], v1[1] - v2[1]));
end;

function AngleDistance(const sour, dest: TGeoFloat): TGeoFloat;
begin
  Result := Abs(sour - dest);
  if Result > 180.0 then
      Result := 360.0 - Result;
end;

function SmoothAngle(const sour, dest, Delta: TGeoFloat): TGeoFloat;
var
  a1, a2: TGeoFloat;
begin
  if sour <> dest then
    begin
      if sour >= 0 then
        begin
          a1 := sour + Delta;
          a2 := sour - Delta;
        end
      else
        begin
          a1 := sour + -Delta;
          a2 := sour + Delta;
        end;

      if AngleDistance(dest, a1) >= AngleDistance(dest, a2) then
        begin
          if AngleDistance(dest, a2) > Delta then
              Result := a2
          else
              Result := dest;
        end
      else if AngleDistance(dest, a1) > Delta then
          Result := a1
      else
          Result := dest;
    end
  else
      Result := dest;
end;

function AngleEqual(const a1, a2: TGeoFloat): Boolean;
begin
  Result := AngleDistance(a1, a2) < 0.01;
end;

function Distance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := PointDistance(v1, v2);
end;

function Distance(const v1, v2: TRectV2): TGeoFloat;
var
  d1, d2: TGeoFloat;
begin
  d1 := PointDistance(v1[0], v2[0]);
  d2 := PointDistance(v1[1], v2[1]);
  if d1 >= d2 then
      Result := d1
  else
      Result := d2;
end;

function MovementLerp(const s, d, Lerp: TGeoFloat): TGeoFloat;
begin
  if Lerp < 1.0 then
      Result := s + Lerp * (d - s)
  else
      Result := d;
end;

function MovementLerp(const s, d: TVec2; Lerp: TGeoFloat): TVec2;
begin
  if Lerp < 1.0 then
    begin
      Result[0] := s[0] + Lerp * (d[0] - s[0]);
      Result[1] := s[1] + Lerp * (d[1] - s[1]);
    end
  else
      Result := d;
end;

function MovementLerp(const s, d: TRectV2; Lerp: TGeoFloat): TRectV2;
begin
  if Lerp < 1.0 then
    begin
      Result[0] := MovementLerp(s[0], d[0], Lerp);
      Result[1] := MovementLerp(s[1], d[1], Lerp);
    end
  else
      Result := d;
end;

function MovementDistance(const s, d: TVec2; dt: TGeoFloat): TVec2;
var
  k: Double;
begin
  k := dt / Sqrt((d[0] - s[0]) * (d[0] - s[0]) + (d[1] - s[1]) * (d[1] - s[1]));
  Result[0] := s[0] + k * (d[0] - s[0]);
  Result[1] := s[1] + k * (d[1] - s[1]);
end;

function MovementDistance(const s, d: TRectV2; dt: TGeoFloat): TRectV2;
begin
  if Distance(s[0], d[0]) > dt then
      Result[0] := MovementDistance(s[0], d[0], dt)
  else
      Result[0] := d[0];

  if Distance(s[1], d[1]) > dt then
      Result[1] := MovementDistance(s[1], d[1], dt)
  else
      Result[1] := d[1];
end;

function MovementDistance(const sour, dest: TVector4; Distance: TGeoFloat): TVector4;
var
  k: TGeoFloat;
begin
  // calc distance
  k := Distance / Sqrt((dest[0] - sour[0]) * (dest[0] - sour[0]) + (dest[1] - sour[1]) * (dest[1] - sour[1]) + (dest[2] - sour[2]) * (dest[2] - sour[2]) + (dest[3] - sour[3]) *
    (dest[3] - sour[3]));
  // done
  Result[0] := sour[0] + k * (dest[0] - sour[0]);
  Result[1] := sour[1] + k * (dest[1] - sour[1]);
  Result[2] := sour[2] + k * (dest[2] - sour[2]);
  Result[3] := sour[3] + k * (dest[3] - sour[3]);
end;

function MovementDistance(const sour, dest: TVector3; Distance: TGeoFloat): TVector3;
var
  k: TGeoFloat;
begin
  // calc distance
  k := Distance / Sqrt((dest[0] - sour[0]) * (dest[0] - sour[0]) + (dest[1] - sour[1]) * (dest[1] - sour[1]) + (dest[2] - sour[2]) * (dest[2] - sour[2]));
  // done
  Result[0] := sour[0] + k * (dest[0] - sour[0]);
  Result[1] := sour[1] + k * (dest[1] - sour[1]);
  Result[2] := sour[2] + k * (dest[2] - sour[2]);
end;

function MovementDistanceDeltaTime(const s, d: TVec2; ASpeed: TGeoFloat): Double;
begin
  Result := Distance(s, d) / ASpeed;
end;

function MovementDistanceDeltaTime(const s, d: TRectV2; ASpeed: TGeoFloat): Double;
var
  d1, d2: Double;
begin
  d1 := MovementDistanceDeltaTime(s[0], d[0], ASpeed);
  d2 := MovementDistanceDeltaTime(s[1], d[1], ASpeed);
  if d1 > d2 then
      Result := d1
  else
      Result := d2;
end;

function AngleRollDistanceDeltaTime(const s, d: TGeoFloat; ARollSpeed: TGeoFloat): Double;
begin
  Result := AngleDistance(s, d) / ARollSpeed;
end;

function BounceVector(const Current: TVector4; DeltaDistance: TGeoFloat; const BeginVector, EndVector: TVector4; var EndFlag: Boolean): TVector4;
  function ToVector: TVector4;
  begin
    if EndFlag then
        Result := EndVector
    else
        Result := BeginVector;
  end;

var
  k: TGeoFloat;
begin
  k := Current.Distance4D(ToVector);
  if k >= DeltaDistance then
      Result := MovementDistance(Current, ToVector, DeltaDistance)
  else
    begin
      Result := ToVector;
      EndFlag := not EndFlag;
      Result := MovementDistance(Result, ToVector, DeltaDistance - k);
    end;
end;

function BounceVector(const Current: TVector3; DeltaDistance: TGeoFloat; const BeginVector, EndVector: TVector3; var EndFlag: Boolean): TVector3;
  function ToVector: TVector3;
  begin
    if EndFlag then
        Result := EndVector
    else
        Result := BeginVector;
  end;

var
  k: TGeoFloat;
begin
  k := Current.Distance3D(ToVector);
  if k >= DeltaDistance then
      Result := MovementDistance(Current, ToVector, DeltaDistance)
  else
    begin
      Result := ToVector;
      EndFlag := not EndFlag;
      Result := MovementDistance(Result, ToVector, DeltaDistance - k);
    end;
end;

function BounceVector(const Current: TVector2; DeltaDistance: TGeoFloat; const BeginVector, EndVector: TVector2; var EndFlag: Boolean): TVector2;
  function ToVector: TVector2;
  begin
    if EndFlag then
        Result := EndVector
    else
        Result := BeginVector;
  end;

var
  k: TGeoFloat;
begin
  k := Vec2Distance(Current.Buff, ToVector.Buff);
  if k >= DeltaDistance then
      Result := Vec2LerpTo(Current.Buff, ToVector.Buff, DeltaDistance)
  else
    begin
      Result := ToVector;
      EndFlag := not EndFlag;
      Result := Vec2LerpTo(Result.Buff, ToVector.Buff, DeltaDistance - k);
    end;
end;

function BounceFloat(const CurrentVal, DeltaVal, StartVal, OverVal: TGeoFloat; var EndFlag: Boolean): TGeoFloat;
  function IfOut(Cur, Delta, dest: TGeoFloat): Boolean;
  begin
    if Cur > dest then
        Result := Cur - Delta < dest
    else
        Result := Cur + Delta > dest;
  end;

  function GetOutValue(Cur, Delta, dest: TGeoFloat): TGeoFloat;
  begin
    if IfOut(Cur, Delta, dest) then
      begin
        if Cur > dest then
            Result := dest - (Cur - Delta)
        else
            Result := Cur + Delta - dest;
      end
    else
        Result := 0;
  end;

  function GetDeltaValue(Cur, Delta, dest: TGeoFloat): TGeoFloat;
  begin
    if Cur > dest then
        Result := Cur - Delta
    else
        Result := Cur + Delta;
  end;

begin
  if (DeltaVal > 0) and (StartVal <> OverVal) then
    begin
      if EndFlag then
        begin
          if IfOut(CurrentVal, DeltaVal, OverVal) then
            begin
              EndFlag := False;
              Result := umlProcessCycleValue(OverVal, GetOutValue(CurrentVal, DeltaVal, OverVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, OverVal);
        end
      else
        begin
          if IfOut(CurrentVal, DeltaVal, StartVal) then
            begin
              EndFlag := True;
              Result := umlProcessCycleValue(StartVal, GetOutValue(CurrentVal, DeltaVal, StartVal), StartVal, OverVal, EndFlag);
            end
          else
              Result := GetDeltaValue(CurrentVal, DeltaVal, StartVal);
        end
    end
  else
      Result := CurrentVal;
end;

class operator TMatrix4.Equal(const Lhs, Rhs: TMatrix4): Boolean;
begin
  Result := VectorEquals(Lhs.Buff[0], Rhs.Buff[0]) and VectorEquals(Lhs.Buff[1], Rhs.Buff[1]) and VectorEquals(Lhs.Buff[2], Rhs.Buff[2]) and VectorEquals(Lhs.Buff[3], Rhs.Buff[3]);
end;

class operator TMatrix4.NotEqual(const Lhs, Rhs: TMatrix4): Boolean;
begin
  Result := not(Lhs = Rhs);
end;

class operator TMatrix4.Multiply(const Lhs, Rhs: TMatrix4): TMatrix4;
begin
  Result.Buff := GeometryLib.MatrixMultiply(Lhs.Buff, Rhs.Buff);
end;

class operator TMatrix4.Implicit(Value: TGeoFloat): TMatrix4;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
        Result.Buff[i, j] := Value;
end;

class operator TMatrix4.Implicit(Value: TMat4): TMatrix4;
begin
  Result.Buff := Value;
end;

function TMatrix4.Swap: TMatrix4;
var
  i, j: Integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
        Result.Buff[j, i] := Buff[i, j];
end;

function TMatrix4.Lerp(M: TMatrix4; Delta: TGeoFloat): TMatrix4;
var
  i, j: Integer;
begin
  for j := 0 to 3 do
    for i := 0 to 3 do
        Result.Buff[i][j] := Buff[i][j] + (M.Buff[i][j] - Buff[i][j]) * Delta;
end;

function TMatrix4.AffineMatrix: TAffineMatrix;
begin
  Result[0, 0] := Buff[0, 0];
  Result[0, 1] := Buff[0, 1];
  Result[0, 2] := Buff[0, 2];
  Result[1, 0] := Buff[1, 0];
  Result[1, 1] := Buff[1, 1];
  Result[1, 2] := Buff[1, 2];
  Result[2, 0] := Buff[2, 0];
  Result[2, 1] := Buff[2, 1];
  Result[2, 2] := Buff[2, 2];
end;

function TMatrix4.Invert: TMatrix4;
var
  det: TGeoFloat;
begin
  Result.Buff := Buff;
  det := GeometryLib.MatrixDeterminant(Result.Buff);
  if Abs(det) < Epsilon then
      Result.Buff := GeometryLib.IdentityHmgMatrix
  else
    begin
      GeometryLib.AdjointMatrix(Result.Buff);
      GeometryLib.ScaleMatrix(Result.Buff, 1 / det);
    end;
end;

function TMatrix4.Translate(v: TVec3): TMatrix4;
begin
  Result.Buff := Buff;
  GeometryLib.TranslateMatrix(Result.Buff, v);
end;

function TMatrix4.Normalize: TMatrix4;
begin
  Result.Buff := Buff;
  GeometryLib.NormalizeMatrix(Result.Buff);
end;

function TMatrix4.Transpose: TMatrix4;
begin
  Result.Buff := Buff;
  GeometryLib.TransposeMatrix(Result.Buff);
end;

function TMatrix4.AnglePreservingInvert: TMatrix4;
begin
  Result.Buff := GeometryLib.AnglePreservingMatrixInvert(Buff);
end;

function TMatrix4.Determinant: TGeoFloat;
begin
  Result := GeometryLib.MatrixDeterminant(Buff);
end;

function TMatrix4.Adjoint: TMatrix4;
begin
  Result.Buff := Buff;
  GeometryLib.AdjointMatrix(Result.Buff);
end;

function TMatrix4.Pitch(angle: TGeoFloat): TMatrix4;
begin
  Result.Buff := GeometryLib.Pitch(Buff, angle);
end;

function TMatrix4.Roll(angle: TGeoFloat): TMatrix4;
begin
  Result.Buff := GeometryLib.Roll(Buff, angle);
end;

function TMatrix4.Turn(angle: TGeoFloat): TMatrix4;
begin
  Result.Buff := GeometryLib.Turn(Buff, angle);
end;

function TVector4.GetVec3: TVec3;
begin
  Result := AffineVectorMake(Buff);
end;

procedure TVector4.SetVec3(const Value: TVec3);
begin
  Buff := VectorMake(Value);
end;

function TVector4.GetVec2: TVec2;
begin
  Result[0] := Buff[0];
  Result[1] := Buff[1];
end;

procedure TVector4.SetVec2(const Value: TVec2);
begin
  Buff[0] := Value[0];
  Buff[1] := Value[1];
end;

function TVector4.GetLinkValue(index: Integer): TGeoFloat;
begin
  Result := Buff[index];
end;

procedure TVector4.SetLinkValue(index: Integer; const Value: TGeoFloat);
begin
  Buff[index] := Value;
end;

class operator TVector4.Equal(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Buff[0] = Rhs.Buff[0]) and (Lhs.Buff[1] = Rhs.Buff[1]) and (Lhs.Buff[2] = Rhs.Buff[2]) and (Lhs.Buff[3] = Rhs.Buff[3]);
end;

class operator TVector4.NotEqual(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Buff[0] <> Rhs.Buff[0]) or (Lhs.Buff[1] <> Rhs.Buff[1]) or (Lhs.Buff[2] <> Rhs.Buff[2]) or (Lhs.Buff[3] <> Rhs.Buff[3]);
end;

class operator TVector4.GreaterThan(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Buff[0] > Rhs.Buff[0]) and (Lhs.Buff[1] > Rhs.Buff[1]) and (Lhs.Buff[2] > Rhs.Buff[2]) and (Lhs.Buff[3] > Rhs.Buff[3]);
end;

class operator TVector4.GreaterThanOrEqual(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Buff[0] >= Rhs.Buff[0]) and (Lhs.Buff[1] >= Rhs.Buff[1]) and (Lhs.Buff[2] >= Rhs.Buff[2]) and (Lhs.Buff[3] >= Rhs.Buff[3]);
end;

class operator TVector4.LessThan(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Buff[0] < Rhs.Buff[0]) and (Lhs.Buff[1] < Rhs.Buff[1]) and (Lhs.Buff[2] < Rhs.Buff[2]) and (Lhs.Buff[3] < Rhs.Buff[3]);
end;

class operator TVector4.LessThanOrEqual(const Lhs, Rhs: TVector4): Boolean;
begin
  Result := (Lhs.Buff[0] <= Rhs.Buff[0]) and (Lhs.Buff[1] <= Rhs.Buff[1]) and (Lhs.Buff[2] <= Rhs.Buff[2]) and (Lhs.Buff[3] <= Rhs.Buff[3]);
end;

class operator TVector4.Add(const Lhs, Rhs: TVector4): TVector4;
begin
  Result.Buff[0] := Lhs.Buff[0] + Rhs.Buff[0];
  Result.Buff[1] := Lhs.Buff[1] + Rhs.Buff[1];
  Result.Buff[2] := Lhs.Buff[2] + Rhs.Buff[2];
  Result.Buff[3] := Lhs.Buff[3] + Rhs.Buff[3];
end;

class operator TVector4.Add(const Lhs: TVector4; const Rhs: TGeoFloat): TVector4;
begin
  Result.Buff[0] := Lhs.Buff[0] + Rhs;
  Result.Buff[1] := Lhs.Buff[1] + Rhs;
  Result.Buff[2] := Lhs.Buff[2] + Rhs;
  Result.Buff[3] := Lhs.Buff[3] + Rhs;
end;

class operator TVector4.Add(const Lhs: TGeoFloat; const Rhs: TVector4): TVector4;
begin
  Result.Buff[0] := Lhs + Rhs.Buff[0];
  Result.Buff[1] := Lhs + Rhs.Buff[1];
  Result.Buff[2] := Lhs + Rhs.Buff[2];
  Result.Buff[3] := Lhs + Rhs.Buff[3];
end;

class operator TVector4.Subtract(const Lhs, Rhs: TVector4): TVector4;
begin
  Result.Buff[0] := Lhs.Buff[0] - Rhs.Buff[0];
  Result.Buff[1] := Lhs.Buff[1] - Rhs.Buff[1];
  Result.Buff[2] := Lhs.Buff[2] - Rhs.Buff[2];
  Result.Buff[3] := Lhs.Buff[3] - Rhs.Buff[3];
end;

class operator TVector4.Subtract(const Lhs: TVector4; const Rhs: TGeoFloat): TVector4;
begin
  Result.Buff[0] := Lhs.Buff[0] - Rhs;
  Result.Buff[1] := Lhs.Buff[1] - Rhs;
  Result.Buff[2] := Lhs.Buff[2] - Rhs;
  Result.Buff[3] := Lhs.Buff[3] - Rhs;
end;

class operator TVector4.Subtract(const Lhs: TGeoFloat; const Rhs: TVector4): TVector4;
begin
  Result.Buff[0] := Lhs - Rhs.Buff[0];
  Result.Buff[1] := Lhs - Rhs.Buff[1];
  Result.Buff[2] := Lhs - Rhs.Buff[2];
  Result.Buff[3] := Lhs - Rhs.Buff[3];
end;

class operator TVector4.Multiply(const Lhs, Rhs: TVector4): TVector4;
begin
  Result.Buff[0] := Lhs.Buff[0] * Rhs.Buff[0];
  Result.Buff[1] := Lhs.Buff[1] * Rhs.Buff[1];
  Result.Buff[2] := Lhs.Buff[2] * Rhs.Buff[2];
  Result.Buff[3] := Lhs.Buff[2] * Rhs.Buff[3];
end;

class operator TVector4.Multiply(const Lhs: TVector4; const Rhs: TGeoFloat): TVector4;
begin
  Result.Buff[0] := Lhs.Buff[0] * Rhs;
  Result.Buff[1] := Lhs.Buff[1] * Rhs;
  Result.Buff[2] := Lhs.Buff[2] * Rhs;
  Result.Buff[3] := Lhs.Buff[3] * Rhs;
end;

class operator TVector4.Multiply(const Lhs: TGeoFloat; const Rhs: TVector4): TVector4;
begin
  Result.Buff[0] := Lhs * Rhs.Buff[0];
  Result.Buff[1] := Lhs * Rhs.Buff[1];
  Result.Buff[2] := Lhs * Rhs.Buff[2];
  Result.Buff[3] := Lhs * Rhs.Buff[3];
end;

class operator TVector4.Multiply(const Lhs: TVector4; const Rhs: TMatrix4): TVector4;
begin
  Result.Buff := GeometryLib.VectorTransform(Lhs.Buff, Rhs.Buff);
end;

class operator TVector4.Multiply(const Lhs: TMatrix4; const Rhs: TVector4): TVector4;
begin
  Result.Buff := VectorTransform(Rhs.Buff, Lhs.Buff);
end;

class operator TVector4.Multiply(const Lhs: TVector4; const Rhs: TMat4): TVector4;
begin
  Result.Buff := GeometryLib.VectorTransform(Lhs.Buff, Rhs);
end;

class operator TVector4.Multiply(const Lhs: TMat4; const Rhs: TVector4): TVector4;
begin
  Result.Buff := GeometryLib.VectorTransform(Rhs.Buff, Lhs);
end;

class operator TVector4.Multiply(const Lhs: TVector4; const Rhs: TAffineMatrix): TVector4;
begin
  Result.Buff := GeometryLib.VectorTransform(Lhs.Buff, Rhs);
end;

class operator TVector4.Multiply(const Lhs: TAffineMatrix; const Rhs: TVector4): TVector4;
begin
  Result.Buff := GeometryLib.VectorTransform(Rhs.Buff, Lhs);
end;

class operator TVector4.Divide(const Lhs, Rhs: TVector4): TVector4;
begin
  Result.Buff[0] := Lhs.Buff[0] / Rhs.Buff[0];
  Result.Buff[1] := Lhs.Buff[1] / Rhs.Buff[1];
  Result.Buff[2] := Lhs.Buff[2] / Rhs.Buff[2];
  Result.Buff[3] := Lhs.Buff[3] / Rhs.Buff[3];
end;

class operator TVector4.Divide(const Lhs: TVector4; const Rhs: TGeoFloat): TVector4;
begin
  Result.Buff[0] := Lhs.Buff[0] / Rhs;
  Result.Buff[1] := Lhs.Buff[1] / Rhs;
  Result.Buff[2] := Lhs.Buff[2] / Rhs;
  Result.Buff[3] := Lhs.Buff[3] / Rhs;
end;

class operator TVector4.Divide(const Lhs: TGeoFloat; const Rhs: TVector4): TVector4;
begin
  Result.Buff[0] := Lhs / Rhs.Buff[0];
  Result.Buff[1] := Lhs / Rhs.Buff[1];
  Result.Buff[2] := Lhs / Rhs.Buff[2];
  Result.Buff[3] := Lhs / Rhs.Buff[3];
end;

class operator TVector4.Implicit(Value: TGeoFloat): TVector4;
begin
  Result.Buff[0] := Value;
  Result.Buff[1] := Value;
  Result.Buff[2] := Value;
  Result.Buff[3] := Value;
end;

class operator TVector4.Implicit(Value: TVec4): TVector4;
begin
  Result.Buff := Value;
end;

class operator TVector4.Implicit(Value: TVec3): TVector4;
begin
  Result.Buff := VectorMake(Value);
end;

class operator TVector4.Implicit(Value: TVec2): TVector4;
begin
  Result.Buff := VectorMake(Value[0], Value[1], 0, 0);
end;

class operator TVector4.Explicit(Value: TVector4): TVec4;
begin
  Result := Value.Buff;
end;

class operator TVector4.Explicit(Value: TVector4): TVec3;
begin
  Result := AffineVectorMake(Value.Buff);
end;

class operator TVector4.Explicit(Value: TVector4): TVec2;
begin
  Result[0] := Value.Buff[0];
  Result[1] := Value.Buff[1];
end;

procedure TVector4.SetRGBA(const r, g, b, a: TGeoFloat);
begin
  Buff[0] := r;
  Buff[1] := g;
  Buff[2] := b;
  Buff[3] := a;
end;

procedure TVector4.SetLocation(const fx, fy, fz, fw: TGeoFloat);
begin
  Buff[0] := fx;
  Buff[1] := fy;
  Buff[2] := fz;
  Buff[3] := fw;
end;

procedure TVector4.SetLocation(const fx, fy, fz: TGeoFloat);
begin
  Buff[0] := fx;
  Buff[1] := fy;
  Buff[2] := fz;
end;

function TVector4.Distance4D(const v2: TVector4): TGeoFloat;
begin
  Result := Sqrt(Sqr(v2.Buff[0] - Buff[0]) + Sqr(v2.Buff[1] - Buff[1]) + Sqr(v2.Buff[2] - Buff[2]) + Sqr(v2.Buff[3] - Buff[3]));
end;

function TVector4.Distance3D(const v2: TVector4): TGeoFloat;
begin
  Result := Sqrt(Sqr(v2.Buff[0] - Buff[0]) + Sqr(v2.Buff[1] - Buff[1]) + Sqr(v2.Buff[2] - Buff[2]));
end;

function TVector4.Distance2D(const v2: TVector4): TGeoFloat;
begin
  Result := Sqrt(Sqr(v2.Buff[0] - Buff[0]) + Sqr(v2.Buff[1] - Buff[1]));
end;

function TVector4.Lerp(const v2: TVector4; const t: TGeoFloat): TVector4;
begin
  Result.Buff[0] := Buff[0] + (v2.Buff[0] - Buff[0]) * t;
  Result.Buff[1] := Buff[1] + (v2.Buff[1] - Buff[1]) * t;
  Result.Buff[2] := Buff[2] + (v2.Buff[2] - Buff[2]) * t;
  Result.Buff[3] := Buff[3] + (v2.Buff[3] - Buff[3]) * t;
end;

function TVector4.LerpDistance(const v2: TVector4; const d: TGeoFloat): TVector4;
var
  k: Double;
begin
  k := d / Sqrt((v2.Buff[0] - Buff[0]) * (v2.Buff[0] - Buff[0]) + (v2.Buff[1] - Buff[1]) * (v2.Buff[1] - Buff[1]) + (v2.Buff[2] - Buff[2]) * (v2.Buff[2] - Buff[2]) +
    (v2.Buff[3] - Buff[3]) * (v2.Buff[3] - Buff[3]));
  Result.Buff[0] := Buff[0] + k * (v2.Buff[0] - Buff[0]);
  Result.Buff[1] := Buff[1] + k * (v2.Buff[1] - Buff[1]);
  Result.Buff[2] := Buff[2] + k * (v2.Buff[2] - Buff[2]);
  Result.Buff[3] := Buff[3] + k * (v2.Buff[3] - Buff[3]);
end;

function TVector4.Norm: TGeoFloat;
begin
  Result := Buff[0] * Buff[0] + Buff[1] * Buff[1] + Buff[2] * Buff[2] + Buff[3] * Buff[3];
end;

function TVector4.length: TGeoFloat;
begin
  Result := Sqrt(Norm);
end;

function TVector4.Normalize: TVector4;
var
  InvLen: TGeoFloat;
  vn: TGeoFloat;
begin
  vn := Norm;
  if vn = 0 then
      Result := Self
  else
    begin
      InvLen := RSqrt(vn);
      Result.Buff[0] := Buff[0] * InvLen;
      Result.Buff[1] := Buff[1] * InvLen;
      Result.Buff[2] := Buff[2] * InvLen;
      Result.Buff[3] := Buff[3] * InvLen;
    end;
end;

function TVector4.Cross(const v2: TVector4): TVector4;
begin
  Result.Buff[0] := Buff[1] * v2.Buff[2] - Buff[2] * v2.Buff[1];
  Result.Buff[1] := Buff[2] * v2.Buff[0] - Buff[0] * v2.Buff[2];
  Result.Buff[2] := Buff[0] * v2.Buff[1] - Buff[1] * v2.Buff[0];
  Result.Buff[3] := 0;
end;

function TVector4.Cross(const v2: TVec3): TVector4;
begin
  Result.Buff[0] := Buff[1] * v2[2] - Buff[2] * v2[1];
  Result.Buff[1] := Buff[2] * v2[0] - Buff[0] * v2[2];
  Result.Buff[2] := Buff[0] * v2[1] - Buff[1] * v2[0];
  Result.Buff[3] := 0;
end;

function TVector4.Cross(const v2: TVec4): TVector4;
begin
  Result.Buff[0] := Buff[1] * v2[2] - Buff[2] * v2[1];
  Result.Buff[1] := Buff[2] * v2[0] - Buff[0] * v2[2];
  Result.Buff[2] := Buff[0] * v2[1] - Buff[1] * v2[0];
  Result.Buff[3] := 0;
end;

function TVector3.GetVec2: TVec2;
begin
  Result[0] := Buff[0];
  Result[1] := Buff[1];
end;

procedure TVector3.SetVec2(const Value: TVec2);
begin
  Buff[0] := Value[0];
  Buff[1] := Value[1];
end;

function TVector3.GetLinkValue(index: Integer): TGeoFloat;
begin
  Result := Buff[index];
end;

procedure TVector3.SetLinkValue(index: Integer; const Value: TGeoFloat);
begin
  Buff[index] := Value;
end;

class operator TVector3.Equal(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Buff[0] = Rhs.Buff[0]) and (Lhs.Buff[1] = Rhs.Buff[1]) and (Lhs.Buff[2] = Rhs.Buff[2]);
end;

class operator TVector3.NotEqual(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Buff[0] <> Rhs.Buff[0]) or (Lhs.Buff[1] <> Rhs.Buff[1]) or (Lhs.Buff[2] <> Rhs.Buff[2]);
end;

class operator TVector3.GreaterThan(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Buff[0] > Rhs.Buff[0]) and (Lhs.Buff[1] > Rhs.Buff[1]) and (Lhs.Buff[2] > Rhs.Buff[2]);
end;

class operator TVector3.GreaterThanOrEqual(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Buff[0] >= Rhs.Buff[0]) and (Lhs.Buff[1] >= Rhs.Buff[1]) and (Lhs.Buff[2] >= Rhs.Buff[2]);
end;

class operator TVector3.LessThan(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Buff[0] < Rhs.Buff[0]) and (Lhs.Buff[1] < Rhs.Buff[1]) and (Lhs.Buff[2] < Rhs.Buff[2]);
end;

class operator TVector3.LessThanOrEqual(const Lhs, Rhs: TVector3): Boolean;
begin
  Result := (Lhs.Buff[0] <= Rhs.Buff[0]) and (Lhs.Buff[1] <= Rhs.Buff[1]) and (Lhs.Buff[2] <= Rhs.Buff[2]);
end;

class operator TVector3.Add(const Lhs, Rhs: TVector3): TVector3;
begin
  Result.Buff[0] := Lhs.Buff[0] + Rhs.Buff[0];
  Result.Buff[1] := Lhs.Buff[1] + Rhs.Buff[1];
  Result.Buff[2] := Lhs.Buff[2] + Rhs.Buff[2];
end;

class operator TVector3.Add(const Lhs: TVector3; const Rhs: TGeoFloat): TVector3;
begin
  Result.Buff[0] := Lhs.Buff[0] + Rhs;
  Result.Buff[1] := Lhs.Buff[1] + Rhs;
  Result.Buff[2] := Lhs.Buff[2] + Rhs;
end;

class operator TVector3.Add(const Lhs: TGeoFloat; const Rhs: TVector3): TVector3;
begin
  Result.Buff[0] := Lhs + Rhs.Buff[0];
  Result.Buff[1] := Lhs + Rhs.Buff[1];
  Result.Buff[2] := Lhs + Rhs.Buff[2];
end;

class operator TVector3.Subtract(const Lhs, Rhs: TVector3): TVector3;
begin
  Result.Buff[0] := Lhs.Buff[0] - Rhs.Buff[0];
  Result.Buff[1] := Lhs.Buff[1] - Rhs.Buff[1];
  Result.Buff[2] := Lhs.Buff[2] - Rhs.Buff[2];
end;

class operator TVector3.Subtract(const Lhs: TVector3; const Rhs: TGeoFloat): TVector3;
begin
  Result.Buff[0] := Lhs.Buff[0] - Rhs;
  Result.Buff[1] := Lhs.Buff[1] - Rhs;
  Result.Buff[2] := Lhs.Buff[2] - Rhs;
end;

class operator TVector3.Subtract(const Lhs: TGeoFloat; const Rhs: TVector3): TVector3;
begin
  Result.Buff[0] := Lhs - Rhs.Buff[0];
  Result.Buff[1] := Lhs - Rhs.Buff[1];
  Result.Buff[2] := Lhs - Rhs.Buff[2];
end;

class operator TVector3.Multiply(const Lhs, Rhs: TVector3): TVector3;
begin
  Result.Buff[0] := Lhs.Buff[0] * Rhs.Buff[0];
  Result.Buff[1] := Lhs.Buff[1] * Rhs.Buff[1];
  Result.Buff[2] := Lhs.Buff[2] * Rhs.Buff[2];
end;

class operator TVector3.Multiply(const Lhs: TVector3; const Rhs: TGeoFloat): TVector3;
begin
  Result.Buff[0] := Lhs.Buff[0] * Rhs;
  Result.Buff[1] := Lhs.Buff[1] * Rhs;
  Result.Buff[2] := Lhs.Buff[2] * Rhs;
end;

class operator TVector3.Multiply(const Lhs: TGeoFloat; const Rhs: TVector3): TVector3;
begin
  Result.Buff[0] := Lhs * Rhs.Buff[0];
  Result.Buff[1] := Lhs * Rhs.Buff[1];
  Result.Buff[2] := Lhs * Rhs.Buff[2];
end;

class operator TVector3.Multiply(const Lhs: TVector3; const Rhs: TMatrix4): TVector3;
begin
  Result.Buff := GeometryLib.VectorTransform(Lhs.Buff, Rhs.Buff);
end;

class operator TVector3.Multiply(const Lhs: TMatrix4; const Rhs: TVector3): TVector3;
begin
  Result.Buff := GeometryLib.VectorTransform(Rhs.Buff, Lhs.Buff);
end;

class operator TVector3.Multiply(const Lhs: TVector3; const Rhs: TMat4): TVector3;
begin
  Result.Buff := GeometryLib.VectorTransform(Lhs.Buff, Rhs);
end;

class operator TVector3.Multiply(const Lhs: TMat4; const Rhs: TVector3): TVector3;
begin
  Result.Buff := GeometryLib.VectorTransform(Rhs.Buff, Lhs);
end;

class operator TVector3.Multiply(const Lhs: TVector3; const Rhs: TAffineMatrix): TVector3;
begin
  Result.Buff := GeometryLib.VectorTransform(Lhs.Buff, Rhs);
end;

class operator TVector3.Multiply(const Lhs: TAffineMatrix; const Rhs: TVector3): TVector3;
begin
  Result.Buff := GeometryLib.VectorTransform(Rhs.Buff, Lhs);
end;

class operator TVector3.Divide(const Lhs, Rhs: TVector3): TVector3;
begin
  Result.Buff[0] := Lhs.Buff[0] / Rhs.Buff[0];
  Result.Buff[1] := Lhs.Buff[1] / Rhs.Buff[1];
  Result.Buff[2] := Lhs.Buff[2] / Rhs.Buff[2];
end;

class operator TVector3.Divide(const Lhs: TVector3; const Rhs: TGeoFloat): TVector3;
begin
  Result.Buff[0] := Lhs.Buff[0] / Rhs;
  Result.Buff[1] := Lhs.Buff[1] / Rhs;
  Result.Buff[2] := Lhs.Buff[2] / Rhs;
end;

class operator TVector3.Divide(const Lhs: TGeoFloat; const Rhs: TVector3): TVector3;
begin
  Result.Buff[0] := Lhs / Rhs.Buff[0];
  Result.Buff[1] := Lhs / Rhs.Buff[1];
  Result.Buff[2] := Lhs / Rhs.Buff[2];
end;

class operator TVector3.Implicit(Value: TGeoFloat): TVector3;
begin
  Result.Buff[0] := Value;
  Result.Buff[1] := Value;
  Result.Buff[2] := Value;
end;

class operator TVector3.Implicit(Value: TVec4): TVector3;
begin
  Result.Buff := AffineVectorMake(Value);
end;

class operator TVector3.Implicit(Value: TVec3): TVector3;
begin
  Result.Buff := Value;
end;

class operator TVector3.Implicit(Value: TVec2): TVector3;
begin
  Result.Buff := AffineVectorMake(Value[0], Value[1], 0);
end;

class operator TVector3.Explicit(Value: TVector3): TVec4;
begin
  Result := VectorMake(Value.Buff);
end;

class operator TVector3.Explicit(Value: TVector3): TVec3;
begin
  Result := Value.Buff;
end;

class operator TVector3.Explicit(Value: TVector3): TVec2;
begin
  Result[0] := Value.Buff[0];
  Result[1] := Value.Buff[1];
end;

procedure TVector3.SetLocation(const fx, fy, fz: TGeoFloat);
begin
  Buff[0] := fx;
  Buff[1] := fy;
  Buff[2] := fz;
end;

function TVector3.Distance3D(const v2: TVector3): TGeoFloat;
begin
  Result := Sqrt(Sqr(v2.Buff[0] - Buff[0]) + Sqr(v2.Buff[1] - Buff[1]) + Sqr(v2.Buff[2] - Buff[2]));
end;

function TVector3.Distance2D(const v2: TVector3): TGeoFloat;
begin
  Result := Sqrt(Sqr(v2.Buff[0] - Buff[0]) + Sqr(v2.Buff[1] - Buff[1]));
end;

function TVector3.Lerp(const v2: TVector3; const t: TGeoFloat): TVector3;
begin
  Result.Buff[0] := Buff[0] + (v2.Buff[0] - Buff[0]) * t;
  Result.Buff[1] := Buff[1] + (v2.Buff[1] - Buff[1]) * t;
  Result.Buff[2] := Buff[2] + (v2.Buff[2] - Buff[2]) * t;
end;

function TVector3.LerpDistance(const v2: TVector3; const d: TGeoFloat): TVector3;
var
  k: Double;
begin
  k := d / Sqrt((v2.Buff[0] - Buff[0]) * (v2.Buff[0] - Buff[0]) + (v2.Buff[1] - Buff[1]) * (v2.Buff[1] - Buff[1]) + (v2.Buff[2] - Buff[2]) * (v2.Buff[2] - Buff[2]));
  Result.Buff[0] := Buff[0] + k * (v2.Buff[0] - Buff[0]);
  Result.Buff[1] := Buff[1] + k * (v2.Buff[1] - Buff[1]);
  Result.Buff[2] := Buff[2] + k * (v2.Buff[2] - Buff[2]);
end;

function TVector3.Norm: TGeoFloat;
begin
  Result := Buff[0] * Buff[0] + Buff[1] * Buff[1] + Buff[2] * Buff[2];
end;

function TVector3.length: TGeoFloat;
begin
  Result := Sqrt(Norm);
end;

function TVector3.Normalize: TVector3;
var
  InvLen: TGeoFloat;
  vn: TGeoFloat;
begin
  vn := Norm;
  if vn = 0 then
      Result := Self
  else
    begin
      InvLen := RSqrt(vn);
      Result.Buff[0] := Buff[0] * InvLen;
      Result.Buff[1] := Buff[1] * InvLen;
      Result.Buff[2] := Buff[2] * InvLen;
    end;
end;

function TVector3.Cross(const v2: TVector3): TVector3;
begin
  Result.Buff[0] := Buff[1] * v2.Buff[2] - Buff[2] * v2.Buff[1];
  Result.Buff[1] := Buff[2] * v2.Buff[0] - Buff[0] * v2.Buff[2];
  Result.Buff[2] := Buff[0] * v2.Buff[1] - Buff[1] * v2.Buff[0];
end;

function TVector3.Vec4(fw: TGeoFloat): TVector4;
begin
  Result.SetLocation(Buff[0], Buff[1], Buff[2], fw);
end;

function TVector3.Vec4: TVector4;
begin
  Result.SetLocation(Buff[0], Buff[1], Buff[2], 0);
end;

{ TAABB }

procedure TAABB.Include(const p: TVector3);
begin
  if p.Buff[0] < Min[0] then
      Min[0] := p.Buff[0];
  if p.Buff[0] > Max[0] then
      Max[0] := p.Buff[0];

  if p.Buff[1] < Min[1] then
      Min[1] := p.Buff[1];
  if p.Buff[1] > Max[1] then
      Max[1] := p.Buff[1];

  if p.Buff[2] < Min[2] then
      Min[2] := p.Buff[2];
  if p.Buff[2] > Max[2] then
      Max[2] := p.Buff[2];
end;

procedure TAABB.FromSweep(const Start, dest: TVector3; const radius: TGeoFloat);
begin
  if Start.Buff[0] < dest.Buff[0] then
    begin
      Min[0] := Start.Buff[0] - radius;
      Max[0] := dest.Buff[0] + radius;
    end
  else
    begin
      Min[0] := dest.Buff[0] - radius;
      Max[0] := Start.Buff[0] + radius;
    end;

  if Start.Buff[1] < dest.Buff[1] then
    begin
      Min[1] := Start.Buff[1] - radius;
      Max[1] := dest.Buff[1] + radius;
    end
  else
    begin
      Min[1] := dest.Buff[1] - radius;
      Max[1] := Start.Buff[1] + radius;
    end;

  if Start.Buff[2] < dest.Buff[2] then
    begin
      Min[2] := Start.Buff[2] - radius;
      Max[2] := dest.Buff[2] + radius;
    end
  else
    begin
      Min[2] := dest.Buff[2] - radius;
      Max[2] := Start.Buff[2] + radius;
    end;
end;

function TAABB.Intersection(const aabb2: TAABB): TAABB;
var
  i: Integer;
begin
  for i := 0 to 2 do
    begin
      Result.Min[i] := MaxFloat(Min[i], aabb2.Min[i]);
      Result.Max[i] := MinFloat(Max[i], aabb2.Max[i]);
    end;
end;

procedure TAABB.Offset(const Delta: TVector3);
begin
  AddVector(Min, Delta.Buff);
  AddVector(Max, Delta.Buff);
end;

function TAABB.PointIn(const p: TVector3): Boolean;
begin
  Result := (p.Buff[0] <= Max[0]) and (p.Buff[0] >= Min[0])
    and (p.Buff[1] <= Max[1]) and (p.Buff[1] >= Min[1])
    and (p.Buff[2] <= Max[2]) and (p.Buff[2] >= Min[2]);
end;

function TVector2.GetLinkValue(index: Integer): TGeoFloat;
begin
  Result := Buff[index];
end;

procedure TVector2.SetLinkValue(index: Integer; const Value: TGeoFloat);
begin
  Buff[index] := Value;
end;

class operator TVector2.Equal(const Lhs, Rhs: TVector2): Boolean;
begin
  Result := IsEqual(Lhs.Buff, Rhs.Buff);
end;

class operator TVector2.NotEqual(const Lhs, Rhs: TVector2): Boolean;
begin
  Result := NotEqual(Lhs.Buff, Rhs.Buff);
end;

class operator TVector2.GreaterThan(const Lhs, Rhs: TVector2): Boolean;
begin
  Result := (Lhs.Buff[0] > Rhs.Buff[0]) and (Lhs.Buff[1] > Rhs.Buff[1]);
end;

class operator TVector2.GreaterThanOrEqual(const Lhs, Rhs: TVector2): Boolean;
begin
  Result := (Lhs.Buff[0] >= Rhs.Buff[0]) and (Lhs.Buff[1] >= Rhs.Buff[1]);
end;

class operator TVector2.LessThan(const Lhs, Rhs: TVector2): Boolean;
begin
  Result := (Lhs.Buff[0] < Rhs.Buff[0]) and (Lhs.Buff[1] < Rhs.Buff[1]);
end;

class operator TVector2.LessThanOrEqual(const Lhs, Rhs: TVector2): Boolean;
begin
  Result := (Lhs.Buff[0] <= Rhs.Buff[0]) and (Lhs.Buff[1] <= Rhs.Buff[1]);
end;

class operator TVector2.Add(const Lhs, Rhs: TVector2): TVector2;
begin
  Result.Buff := Vec2Add(Lhs.Buff, Rhs.Buff);
end;

class operator TVector2.Add(const Lhs: TVector2; const Rhs: TGeoFloat): TVector2;
begin
  Result.Buff := Vec2Add(Lhs.Buff, Rhs);
end;

class operator TVector2.Add(const Lhs: TGeoFloat; const Rhs: TVector2): TVector2;
begin
  Result.Buff := Vec2Add(Lhs, Rhs.Buff);
end;

class operator TVector2.Subtract(const Lhs, Rhs: TVector2): TVector2;
begin
  Result.Buff := Vec2Sub(Lhs.Buff, Rhs.Buff);
end;

class operator TVector2.Subtract(const Lhs: TVector2; const Rhs: TGeoFloat): TVector2;
begin
  Result.Buff := Vec2Sub(Lhs.Buff, Rhs);
end;

class operator TVector2.Subtract(const Lhs: TGeoFloat; const Rhs: TVector2): TVector2;
begin
  Result.Buff := Vec2Sub(Lhs, Rhs.Buff);
end;

class operator TVector2.Multiply(const Lhs, Rhs: TVector2): TVector2;
begin
  Result.Buff := Vec2Mul(Lhs.Buff, Rhs.Buff);
end;

class operator TVector2.Multiply(const Lhs: TVector2; const Rhs: TGeoFloat): TVector2;
begin
  Result.Buff := Vec2Mul(Lhs.Buff, Rhs);
end;

class operator TVector2.Multiply(const Lhs: TGeoFloat; const Rhs: TVector2): TVector2;
begin
  Result.Buff := Vec2Mul(Lhs, Rhs.Buff);
end;

class operator TVector2.Divide(const Lhs, Rhs: TVector2): TVector2;
begin
  Result.Buff := Vec2Div(Lhs.Buff, Rhs.Buff);
end;

class operator TVector2.Divide(const Lhs: TVector2; const Rhs: TGeoFloat): TVector2;
begin
  Result.Buff := Vec2Div(Lhs.Buff, Rhs);
end;

class operator TVector2.Divide(const Lhs: TGeoFloat; const Rhs: TVector2): TVector2;
begin
  Result.Buff := Vec2Div(Lhs, Rhs.Buff);
end;

class operator TVector2.Implicit(Value: TGeoFloat): TVector2;
begin
  Result.Buff := vec2(Value);
end;

class operator TVector2.Implicit(Value: TPoint): TVector2;
begin
  Result.Buff := vec2(Value);
end;

class operator TVector2.Implicit(Value: TPointf): TVector2;
begin
  Result.Buff := vec2(Value);
end;

class operator TVector2.Implicit(Value: TVec2): TVector2;
begin
  Result.Buff := Value;
end;

class operator TVector2.Explicit(Value: TVector2): TPointf;
begin
  Result := MakePointf(Value.Buff);
end;

class operator TVector2.Explicit(Value: TVector2): TPoint;
begin
  Result := MakePoint(Value.Buff);
end;

class operator TVector2.Explicit(Value: TVector2): TVec2;
begin
  Result := Value.Buff;
end;

procedure TVector2.SetLocation(const fx, fy: TGeoFloat);
begin
  Buff := vec2(fx, fy);
end;

function TVector2.Distance(const v2: TVector2): TGeoFloat;
begin
  Result := Vec2Distance(Buff, v2.Buff);
end;

function TVector2.Lerp(const v2: TVector2; const t: TGeoFloat): TVector2;
begin
  Result.Buff := Vec2Lerp(Buff, v2.Buff, t);
end;

function TVector2.LerpDistance(const v2: TVector2; const d: TGeoFloat): TVector2;
begin
  Result.Buff := Vec2LerpTo(Buff, v2.Buff, d);
end;

function TVector2.Norm: TGeoFloat;
begin
  Result := Vec2Norm(Buff);
end;

function TVector2.length: TGeoFloat;
begin
  Result := Vec2Length(Buff);
end;

function TVector2.Normalize: TVector2;
begin
  Result.Buff := Vec2Normalize(Buff);
end;

end.
