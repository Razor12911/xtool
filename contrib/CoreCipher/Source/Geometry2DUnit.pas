{ ****************************************************************************** }
{ * geometry 2D library writen by QQ 600585@qq.com                             * }
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

unit Geometry2DUnit;

{$INCLUDE zDefine.inc}

interface

uses
{$IFDEF FPC}
  FPCGenericStructlist,
{$ENDIF FPC}
  CoreClasses, Types, Math, MemoryStream64, PascalStrings, UnicodeMixedLib;

type
  TGeoFloat = Single;
  TGeoInt = Integer;
  TVec2 = array [0 .. 1] of TGeoFloat;
  PVec2 = ^TVec2;
  T2DPoint = TVec2;
  P2DPoint = PVec2;
  TPoint2 = T2DPoint;

  TArrayVec2 = array of TVec2;
  PArrayVec2 = ^TArrayVec2;
  TVec2Array = TArrayVec2;

  TArray2DPoint = TArrayVec2;
  PArray2DPoint = PArrayVec2;
  T2DPointArray = TArray2DPoint;

  TArrayPVec2 = array of PVec2;
  PArrayPVec2 = ^TArrayPVec2;
  TPVec2Array = TArrayPVec2;

  TRectV2 = array [0 .. 1] of TVec2;
  PRectV2 = ^TRectV2;
  TRect2 = TRectV2;
  TRect2D = TRectV2;
  TArrayRectV2 = array of TRectV2;
  TRectV2Array = TArrayRectV2;

  TRectV2List = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<TRectV2>;

  TLineV2 = array [0 .. 1] of TVec2;
  PLineV2 = ^TLineV2;
  TLine2 = TLineV2;
  TLine2D = TLineV2;

  TArrayLineV2 = array of TLineV2;
  PArrayLineV2 = ^TArrayLineV2;

  TLineV2_P = array [0 .. 1] of PVec2;
  PLineV2_P = ^TLineV2_P;

  TTriangle = array [0 .. 2] of TVec2;
  PTriangle = ^TTriangle;

  TTriangleArray = array of TTriangle;
  PTriangleArray = ^TTriangleArray;

  TGeoFloatArray = array of TGeoFloat;
  PGeoFloatArray = ^TGeoFloatArray;

  TArrayPoint = array of TPoint;
{$IFDEF FPC}

  TPointf = record
    X: TGeoFloat;
    Y: TGeoFloat;
  end;

  PPointf = ^TPointf;

  TRectf = record
    case TGeoInt of
      0: (Left, Top, Right, Bottom: TGeoFloat);
      1: (TopLeft, BottomRight: TPointf);
  end;

  PRectf = ^TRectf;
  TArrayPointf = array of TPointf;

function Pointf(X, Y: TGeoFloat): TPointf;
function Rectf(Left, Top, Right, Bottom: TGeoFloat): TRectf;
{$ELSE FPC}
  TArrayPointf = array of TPointf;
{$ENDIF}

function FAbs(const v: Single): Single; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FAbs(const v: Double): Double; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Clamp(const AValue, aMin, aMax: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MaxF(const v1, v2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MinF(const v1, v2: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareFloat(const f1, f2, Epsilon_: TGeoFloat): ShortInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CompareFloat(const f1, f2: TGeoFloat): ShortInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MakeVec2(const X, Y: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeVec2(const X, Y: TGeoInt): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakePoint(const X, Y: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakePoint(const X, Y: TGeoInt): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakePoint(const pt: TVec2): TPoint; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Point2Point(const pt: TVec2): TPoint; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Point2Pointf(const pt: TVec2): TPointf; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointMake(const X, Y: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointMake(const pt: TPoint): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointMake(const pt: TPointf): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Make2DPoint(const X, Y: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Make2DPoint(const X, Y: TGeoInt): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Make2DPoint(const pt: TPoint): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Make2DPoint(const pt: TPointf): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function vec2(const p: PVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function vec2(const f: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function vec2(const X, Y: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function vec2(const X, Y: TGeoInt): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function vec2(const X, Y: Int64): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function vec2(const pt: TPoint): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function vec2(const pt: TPointf): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function LineV2(const x1, y1, x2, y2: TGeoFloat): TLineV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LineV2(const lb, le: TVec2): TLineV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LineV2(const lb, le: TPoint): TLineV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LineV2(const l: TLineV2_P): TLineV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LineV2(const l: PLineV2_P): TLineV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LineV2(const l: PLineV2): TLineV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RoundVec2(const v: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MakePointf(const pt: TVec2): TPointf; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function IsZero(const v: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsZero(const pt: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsZero(const r: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function IsNan(const pt: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsNan(const X, Y: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function HypotX(const X, Y: Extended): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function PointNorm(const v: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointNegate(const v: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Vec2Norm(const v: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Negate(const v: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function vec2Inv(const v: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SetVec2(var v: TVec2; const vSrc: TVec2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Vec2Add(const v1, v2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Add(const v1: TVec2; v2: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Add(const v1: TVec2; X, Y: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Add(const v1: TGeoFloat; v2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Add(const v1: TArrayVec2; v2: TVec2): TArrayVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Add(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Vec2Sub(const v1, v2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Sub(const v1: TVec2; v2: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Sub(const v1: TGeoFloat; v2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Sub(const v1: TArrayVec2; v2: TVec2): TArrayVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Sub(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Vec2Mul(const v1, v2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1, v2: TVec2; const v3: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1, v2: TVec2; const v3, v4: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1, v2, v3: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1, v2, v3, v4: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1: TVec2; const v2: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1: TVec2; const v2, v3: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1: TVec2; const v2, v3, v4: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1: TGeoFloat; const v2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1: TArrayVec2; v2: TVec2): TArrayVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Mul(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Vec2Div(const v1: TVec2; const v2: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Div(const v1, v2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Div(const v1: TGeoFloat; const v2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function PointNormalize(const v: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Normalize(const v: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function PointLength(const v: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Length(const v: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure PointScale(var v: TVec2; factor: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointDotProduct(const v1, v2: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Distance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Distance(const x1, y1, z1, x2, y2, z2: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Distance(const l: TLineV2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Distance(const f1, f2: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FloatDistance(const f1, f2: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointDistance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointDistance(const v1, v2: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Distance(const v1, v2: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function LineDistance(const l: TLineV2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointLayDistance(const v1, v2: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SqrDistance(const v1, v2: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function PointLerp(const v1, v2: TVec2; t: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointLerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Lerp(const v1, v2: TVec2; t: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2LerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SwapPoint(var v1, v2: TVec2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure SwapVec2(var v1, v2: TVec2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Pow(v: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Pow(const v, n: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MiddleVec2(const pt1, pt2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Middle(const pt1, pt2: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function IsEqual(const Val1, Val2, Epsilon_: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsEqual(const Val1, Val2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsEqual(const Val1, Val2: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsEqual(const Val1, Val2: TVec2; Epsilon_: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function IsEqual(const Val1, Val2: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function NotEqual(const Val1, Val2, Epsilon_: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function NotEqual(const Val1, Val2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function NotEqual(const Val1, Val2: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function LessThanOrEqual(const Val1, Val2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function GreaterThanOrEqual(const Val1, Val2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function GetEquilateralTriangleCen(pt1, pt2: TVec2): TVec2; overload;

procedure Rotate(RotAng: TGeoFloat; const X, Y: TGeoFloat; out Nx, Ny: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Rotate(const RotAng: TGeoFloat; const Point: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function NormalizeDegAngle(const Angle: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// axis to pt angle
function PointAngle(const axis, pt: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Angle(const axis, pt: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
// null point to pt angle
function PointAngle(const pt: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Angle(const pt: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function AngleDistance(const s, a: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointRotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointRotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Rotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Rotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Rotation(const sour_r: TRectV2; const Angle: TGeoFloat; const pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Rotation(const sour_r: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Rotation(const sour_r: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const r: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function CircleInCircle(const cp1, cp2: TVec2; const r1, r2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CircleInRect(const cp: TVec2; const radius: TGeoFloat; r: TRectV2): Boolean;
function PointInRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointInRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointInRect(const X, Y: TGeoInt; const r: TRect): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointInRect(const pt: TPoint; const r: TRect): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointInRect(const pt: TVec2; const r: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointInRect(const Px, Py: TGeoFloat; const r: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2InRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2InRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2InRect(const pt: TVec2; const r: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2InRect(const Px, Py: TGeoFloat; const r: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectToRectIntersect(const r1, r2: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectToRectIntersect(const r1, r2: TRect): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectToRectIntersect(const r1, r2: TRectf): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectWithInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectWithInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectWithInRect(const r1, r2: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectWithInRect(const r1, r2: TRect): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectInRect(const r1, r2: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectInRect(const r1, r2: TRect): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MakeRectV2(const centre: TVec2; const width, height: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRectV2(const X, Y, radius: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRectV2(const p1, p2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRectV2(const X, Y: TGeoFloat; const p2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRectV2(const r: TRect): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRectV2(const r: TRectf): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectV2(): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const centre: TVec2; const width, height: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const X, Y, radius: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const p1, p2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const p1, p2: TPointf): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const X, Y: TGeoFloat; const p2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const r: TRect): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const r: TRectf): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectV2(const r: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MakeRect(const centre: TVec2; const width, height: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRect(const X, Y, radius: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRect(const x1, y1, x2, y2: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRect(const p1, p2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRect(const r: TRect): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRect(const r: TRectf): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RoundRect(const r: TRectV2): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Rect2Rect(const r: TRectV2): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Rect2Rect(const r: TRect): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectMake(const X, Y, radius: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectMake(const x1, y1, x2, y2: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectMake(const p1, p2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectMake(const r: TRect): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectMake(const r: TRectf): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectAdd(const r: TRectV2; v2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectAdd(const r1, r2: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectSub(const r1, r2: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectSub(const r: TRectV2; pt: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectMul(const r1, r2: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectMul(const r1: TRectV2; v2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectMul(const r1: TRectV2; f2: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectDiv(const r1, r2: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectDiv(const r1: TRectV2; f2: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectDiv(const r1: TRectV2; v2: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectOffset(const r: TRectV2; Offset: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectSizeLerp(const r: TRectV2; const rSizeLerp: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectCenScale(const r: TRectV2; const rSizeScale: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectEdge(const r: TRectV2; const Edge: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectEdge(const r: TRectV2; const Edge: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectCentre(const r: TRectV2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectCentre(const r: TRect): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectCentre(const r: TRectf): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Tri(const v1, v2, v3: TVec2): TTriangle; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function TriAdd(const t: TTriangle; v: TVec2): TTriangle; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function TriSub(const t: TTriangle; v: TVec2): TTriangle; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function TriMul(const t: TTriangle; v: TVec2): TTriangle; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function TriDiv(const t: TTriangle; v: TVec2): TTriangle; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function TriCentre(const t: TTriangle): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function TriExpand(const t: TTriangle; Dist: TGeoFloat): TTriangle;
function TriRound(const t: TTriangle): TTriangle; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Vec2Transform(const sour, dest: TRectV2; sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Vec2Transform(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat; const sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectTransform(const sour, dest, sour_rect: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectTransform(const sour, dest: TRectV2; const sour_rect: TRect): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectTransform(const sour, dest: TRectV2; const sour_rect: TRectf): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectScaleSpace(const r: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectScaleSpace(const r: TRect; const SS_width, SS_height: TGeoInt): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CalibrationRectInRect(const r, Area: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CalibrationRectInRect(const r, Area: TRect): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure FixRect(var Left, Top, Right, Bottom: TGeoInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FixRect(var Left, Top, Right, Bottom: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FixRect(r: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FixRect(r: TRect): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FixedRect(var Left, Top, Right, Bottom: TGeoInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FixedRect(var Left, Top, Right, Bottom: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FixedRect(r: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FixedRect(r: TRect): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ForwardRect(var Left, Top, Right, Bottom: TGeoInt); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ForwardRect(var Left, Top, Right, Bottom: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ForwardRect(r: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ForwardRect(r: TRect): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MakeRect(const r: TRectV2): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MakeRectf(const r: TRectV2): TRectf; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectWidth(const r: TRectV2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectHeight(const r: TRectV2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectWidth(const r: TRect): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectHeight(const r: TRect): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectWidth(const r: TRectf): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectHeight(const r: TRectf): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RoundWidth(const r: TRectV2): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RoundHeight(const r: TRectV2): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RoundWidth(const r: TRect): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RoundHeight(const r: TRect): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RoundWidth(const r: TRectf): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RoundHeight(const r: TRectf): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectArea(const r: TRectV2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectSize(const r: TRectV2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectSizeR(const r: TRectV2): TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectFit(const sour, dest: TRectV2; const Bound: Boolean): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectFit(const sour, dest: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectFit(const width, height: TGeoFloat; const bk: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FitRect(const sour, dest: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function FitRect(const width, height: TGeoFloat; const bk: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const buff: TArrayPoint): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const p1, p2, p3: TPoint): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const p1, p2, p3, p4: TPoint): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const r1, r2: TRect): TRect; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const buff: TArrayVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const p1, p2, p3: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const p1, p2, p3, p4: TVec2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BoundRect(const r1, r2: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BuffCentroid(const buff: TArrayVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BuffCentroid(const p1, p2, p3, p4: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function BuffCentroid(const p1, p2, p3: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointInPolygon(pt: TVec2; const PolygonBuff: TArrayVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function FastRamerDouglasPeucker(var Points: TArrayVec2; Epsilon_: TGeoFloat): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure FastVertexReduction(Points: TArrayVec2; Epsilon_: TGeoFloat; var output: TArrayVec2);

function Clip(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out Cx1, Cy1, Cx2, Cy2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Clip(const sour_, background_: TRectV2; var output: TRectV2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Clip(const sour, background: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Orientation(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Orientation(const x1, y1, z1, x2, y2, z2, x3, y3, z3, Px, Py, Pz: TGeoFloat): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Coplanar(const x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SimpleIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SimpleIntersect(const Point1, Point2, Point3, Point4: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function SimpleIntersect(const l1, l2: TLineV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out ix, iy: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Intersect(const pt1, pt2, pt3, pt4: TVec2; out pt: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Intersect(const l1, l2: TLineV2; out pt: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Intersect(const pt1, pt2, pt3, pt4: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function PointInCircle(const pt, cp: TVec2; radius: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function PointInTriangle(const Px, Py, x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure BuildSinCosCache(const oSin, oCos: PGeoFloatArray; const b, E: TGeoFloat);

procedure ClosestPointOnSegmentFromPoint(const x1, y1, x2, y2, Px, Py: TGeoFloat; out Nx, Ny: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ClosestPointOnSegmentFromPoint(const lb, le, pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ClosestPointOnSegmentFromLine(const l: TLineV2; const pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function ClosestPointOnSegmentFromLine(const pt: TVec2; const l: TLineV2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function MinimumDistanceFromPointToLine(const Px, Py, x1, y1, x2, y2: TGeoFloat): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MinimumDistanceFromPointToLine(const pt: TVec2; const l: TLineV2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MinimumDistanceFromPointToLine(const l: TLineV2; const pt: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function MinimumDistanceFromPointToLine(const lb, le, pt: TVec2): TGeoFloat; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// projection
function RectProjection(const sour, dest: TRectV2; const sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectProjection(const sour, dest: TRectV2; const sour_rect: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectProjectionRotationDest(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectProjectionRotationDest(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectProjectionRotationSource(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectProjectionRotationSource(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectProjectionRotationDest(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectProjectionRotationDest(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectProjectionRotationSource(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectProjectionRotationSource(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAxis, destAxis: TVec2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAxis, destAxis: TVec2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_rect: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_pt: TVec2): TVec2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_rect: TRectV2): TRectV2; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Quadrant(const Angle: TGeoFloat): TGeoInt; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ProjectionPoint(const Srcx, Srcy, Dstx, Dsty, Dist: TGeoFloat; out Nx, Ny: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ProjectionPoint(const Srcx, Srcy, Srcz, Dstx, Dsty, Dstz, Dist: TGeoFloat; out Nx, Ny, Nz: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure ProjectionPoint(const Px, Py, Angle, Distance: TGeoFloat; out Nx, Ny: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function GetCicleRadiusInPolyEdge(r: TGeoFloat; PolySlices: TGeoInt): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure Circle2LineIntersectionPoint(const lb, le, cp: TVec2; const radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: TGeoInt; out pt1, pt2: TVec2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
procedure Circle2LineIntersectionPoint(const l: TLineV2; const cp: TVec2; radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: TGeoInt; out pt1, pt2: TVec2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

procedure Circle2CircleIntersectionPoint(const cp1, cp2: TVec2; const r1, r2: TGeoFloat; out Point1, Point2: TVec2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

// circle collision Detector
function Detect_Circle2Circle(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function CircleCollision(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function Detect_Circle2CirclePoint(const p1, p2: TVec2; const r1, r2: TGeoFloat; out op1, op2: TVec2): Boolean;

// circle 2 line collision
function Detect_Circle2Line(const cp: TVec2; const r: TGeoFloat; const lb, le: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Detect_Circle2Line(const cp: TVec2; const r: TGeoFloat; const l: TLineV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}

function SameLinePtr(const lb1, le1, lb2, le2: PVec2): Boolean;

function ComputeCurvePartPrecision(const pt1, pt2, pt3, pt4: TVec2): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Interpolation_OutSide(const T_: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
function Interpolation_InSide(const t: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}


type
  TVec2List = class;
  TDeflectionPolygon = class;
  TDeflectionPolygonLines = class;

  TVec2List = class(TCoreClassObject)
  private
    FList: TCoreClassList;
    FUserData: Pointer;
    FUserObject: TCoreClassObject;

    function GetPoints(index: TGeoInt): PVec2;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddRandom(); overload;
    procedure AddRandom(rnd: TMT19937Random); overload;
    procedure Add(const X, Y: TGeoFloat); overload;
    procedure Add(const pt: TVec2); overload;
    procedure Add(pt: TPoint); overload;
    procedure Add(pt: TPointf); overload;
    procedure Add(v2l: TVec2List); overload;
    procedure Add(r: TRectV2); overload;
    procedure Add(r: TRect); overload;
    procedure Add(r: TRectf); overload;
    procedure AddSubdivision(nbCount: TGeoInt; pt: TVec2); overload;
    procedure AddSubdivisionWithDistance(avgDist: TGeoFloat; pt: TVec2); overload;
    procedure AddCirclePoint(count_: Cardinal; axis: TVec2; dist_: TGeoFloat);
    procedure AddRectangle(r: TRectV2);
    procedure Insert(idx: TGeoInt; X, Y: TGeoFloat); overload;
    procedure Insert(idx: TGeoInt; pt: TVec2); overload;
    procedure Delete(idx: TGeoInt); overload;
    function Remove(p: PVec2): TGeoInt;
    procedure Clear; overload;
    function Count: TGeoInt; overload;
    procedure RemoveSame;
    procedure SwapData(dest: TVec2List);
    procedure MoveDataTo(dest: TVec2List);

    procedure Assign(Source: TCoreClassObject);
    procedure AssignFromArrayV2(arry: TArrayVec2);

    function BuildArray: TArrayVec2;
    function BuildSplineSmoothInSideClosedArray: TArrayVec2;
    function BuildSplineSmoothOutSideClosedArray: TArrayVec2;
    function BuildSplineSmoothOpenedArray: TArrayVec2;

    function BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TArrayVec2; overload;
    function BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat): TArrayVec2; overload;
    function BuildProjectionArray(const sour, dest: TRectV2): TArrayVec2; overload;
    function BuildProjectionArray(const dest: TRectV2): TArrayVec2; overload;

    procedure ProjectionTo(const sour, dest: TRectV2; const output: TDeflectionPolygon); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: TDeflectionPolygon); overload;
    procedure ProjectionTo(const sour, dest: TRectV2; const output: TVec2List); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: TVec2List); overload;

    procedure SaveToStream(stream: TMemoryStream64); overload;
    procedure LoadFromStream(stream: TMemoryStream64); overload;

    function BoundBox: TRectV2; overload;
    function BoundCentre: TVec2;
    function CircleRadius(ACentroid: TVec2): TGeoFloat; overload;
    function Centroid: TVec2; overload;

    function InHere(pt: TVec2): Boolean; overload;
    function InRect(r: TRectV2): Boolean;
    function Rect2Intersect(r: TRectV2): Boolean;

    procedure RotateAngle(axis: TVec2; Angle: TGeoFloat); overload;
    procedure Scale(Scale_: TGeoFloat); overload;

    procedure ConvexHull(output: TVec2List); overload;
    procedure ConvexHull; overload;

    procedure SplineSmoothInSideClosed(output: TVec2List); overload;
    procedure SplineSmoothInSideClosed; overload;
    procedure SplineSmoothOutSideClosed(output: TVec2List); overload;
    procedure SplineSmoothOutSideClosed; overload;
    procedure SplineSmoothOpened(output: TVec2List); overload;
    procedure SplineSmoothOpened; overload;

    procedure ExtractToBuff(var output: TArrayVec2); overload;
    procedure GiveListDataFromBuff(output: TArrayVec2); overload;

    function SumDistance: TGeoFloat;
    procedure InterpolationTo(count_: TGeoInt; output_: TVec2List);

    procedure VertexReduction(Epsilon_: TGeoFloat); overload;
    procedure Reduction(Epsilon_: TGeoFloat); overload;

    function Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean): Boolean; overload;
    function Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean; output: TVec2List): Boolean; overload;
    function Line2NearIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean; overload;

    procedure SortOfNear(const lb, le: TVec2); overload;
    procedure SortOfNear(const pt: TVec2); overload;

    procedure Reverse; overload;

    function GetNearLine(const pt: TVec2; const ClosedMode: Boolean; out lb, le: TGeoInt): TVec2; overload;
    function GetNearLine(const pt: TVec2; const ClosedMode: Boolean): TVec2; overload;
    function GetNearLine(const pt: TVec2; const ExpandDist: TGeoFloat): TVec2; overload;

    procedure CutLineBeginPtToIdx(const pt: TVec2; const toidx: TGeoInt);

    procedure Transform(X, Y: TGeoFloat); overload;
    procedure Transform(v: TVec2); overload;
    procedure Mul(X, Y: TGeoFloat); overload;
    procedure Mul(v: TVec2); overload;
    procedure Mul(v: TGeoFloat); overload;
    procedure FDiv(X, Y: TGeoFloat); overload;
    procedure FDiv(v: TVec2); overload;
    procedure FDiv(v: TGeoFloat); overload;

    property Points[index: TGeoInt]: PVec2 read GetPoints; default;
    function First: PVec2;
    function Last: PVec2;

    procedure ExpandDistanceAsList(ExpandDist: TGeoFloat; output: TVec2List);
    procedure ExpandDistance(ExpandDist: TGeoFloat);
    procedure ExpandConvexHullAsList(ExpandDist: TGeoFloat; output: TVec2List);

    function GetExpands(idx: TGeoInt; ExpandDist: TGeoFloat): TVec2;
    property Expands[idx: TGeoInt; ExpandDist: TGeoFloat]: TVec2 read GetExpands;

    property UserData: Pointer read FUserData write FUserData;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
  end;

  TLines = class(TVec2List)
  end;

  TLinesArray = array of TLines;

  TLinesList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TLines>;

  TLinesList = class(TLinesList_Decl)
  public
    AutoFree: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Remove(obj: TLines);
    procedure Delete(index: TGeoInt);
    procedure Clear;
  end;

  T2DPointList = TVec2List;
  T2DPolygonGraph = class;

  T2DPolygon = class(TLines)
  public
    Owner: T2DPolygonGraph;
    constructor Create;
    destructor Destroy; override;
  end;

  T2DPolygonList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<T2DPolygon>;
  T2DPolygonArray = array of T2DPolygon;
  TCollapses = T2DPolygonArray;

  T2DPolygonGraph = class(TCoreClassObject)
  public
    Surround: T2DPolygon;
    Collapses: TCollapses;

    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TCoreClassObject);

    function NewCollapse(): T2DPolygon;
    procedure AddCollapse(polygon: T2DPolygon);
    procedure Clear;
    function CollapsesCount(): TGeoInt;
    function GetBands(const index: TGeoInt): T2DPolygon;
    property Bands[const index: TGeoInt]: T2DPolygon read GetBands;
    procedure Remove(p: PVec2); overload;
    procedure FreeAndRemove(polygon: T2DPolygon); overload;
    procedure RemoveNullPolygon();
    function Total: TGeoInt;
    function BuildArray: TArray2DPoint;
    function BuildPArray: TArrayPVec2;
    function ExistsPVec(p: PVec2): Boolean;
    procedure RotateAngle(axis: TVec2; Angle: TGeoFloat);
    procedure Scale(Scale_: TGeoFloat);
    procedure ProjectionTo(const sour, dest: TRectV2; const output: T2DPolygonGraph); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: T2DPolygonGraph); overload;
    function InHere(pt: TVec2): Boolean;
    function InSurround(pt: TVec2): Boolean;
    function InCollapse(pt: TVec2): Boolean;
    function Pick(pt: TVec2): T2DPolygon;
    function BoundBox: TRectV2;
    function CollapseBounds: TRectV2Array;
    function Line2Intersect(const lb, le: TVec2; output: T2DPolygon): Boolean;
    function GetNearLine(const pt: TVec2; out output: T2DPolygon; out lb, le: TGeoInt): TVec2;
    procedure Transform(X, Y: TGeoFloat); overload;
    procedure Transform(v: TVec2); overload;
    procedure Mul(X, Y: TGeoFloat); overload;
    procedure Mul(v: TVec2); overload;
    procedure Mul(v: TGeoFloat); overload;
    procedure FDiv(X, Y: TGeoFloat); overload;
    procedure FDiv(v: TVec2); overload;
    procedure VertexReduction(Epsilon_: TGeoFloat); overload;
    procedure Reduction(Epsilon_: TGeoFloat); overload;

    procedure SaveToStream(stream: TMemoryStream64);
    procedure LoadFromStream(stream: TMemoryStream64);
  end;

  T2DPolygonGraphList = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<T2DPolygonGraph>;

  TDeflectionPolygonVec = record
    Owner: TDeflectionPolygon;
    Angle: TGeoFloat;
    Dist: TGeoFloat;
  end;

  PDeflectionPolygonVec = ^TDeflectionPolygonVec;

  TExpandMode = (emConvex, emConcave);

  TDeflectionPolygon = class(TCoreClassObject)
  private
    FList: TCoreClassList;
    FName: TPascalString;
    FClassifier: TPascalString;
    FScale: TGeoFloat;
    FAngle: TGeoFloat;
    FMaxRadius: TGeoFloat;
    FPosition: TVec2;
    FExpandMode: TExpandMode;

    FUserDataObject: TCoreClassObject;
    FUserData: Pointer;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset; overload;

    procedure Assign(Source: TCoreClassObject);

    function BuildArray: TArrayVec2;
    function BuildSplineSmoothInSideClosedArray: TArrayVec2;
    function BuildSplineSmoothOutSideClosedArray: TArrayVec2;
    function BuildSplineSmoothOpenedArray: TArrayVec2;
    function BuildProjectionSplineSmoothInSideClosedArray(const sour, dest: TRectV2): TArrayVec2;
    function BuildProjectionSplineSmoothOutSideClosedArray(const sour, dest: TRectV2): TArrayVec2;

    function BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TArrayVec2; overload;
    function BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat): TArrayVec2; overload;
    function BuildProjectionArray(const sour, dest: TRectV2): TArrayVec2; overload;
    function BuildProjectionArray(const dest: TRectV2): TArrayVec2; overload;

    procedure ProjectionTo(const sour, dest: TRectV2; const output: TDeflectionPolygon); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: TDeflectionPolygon); overload;
    procedure ProjectionTo(const sour, dest: TRectV2; const output: TVec2List); overload;
    procedure ProjectionTo(const dest: TRectV2; const output: TVec2List); overload;

    procedure AddPoint(pt: TVec2); overload;
    procedure AddPoint(X, Y: TGeoFloat); overload;
    procedure AddRectangle(r: TRectV2);
    procedure AddCirclePoint(count_: Cardinal; axis: TVec2; dist_: TGeoFloat);
    procedure Add(angle_, dist_: TGeoFloat); overload;
    procedure Insert(idx: TGeoInt; angle_, dist_: TGeoFloat); overload;
    procedure InsertPoint(idx: TGeoInt; pt: TVec2); overload;
    procedure Delete(idx: TGeoInt); overload;
    procedure Clear; overload;
    function Count: TGeoInt; overload;
    procedure CopyPoly(pl: TDeflectionPolygon; AReversed: Boolean);
    procedure CopyExpandPoly(pl: TDeflectionPolygon; AReversed: Boolean; Dist: TGeoFloat);
    procedure Reverse;
    function ScaleBeforeDistance: TGeoFloat;
    function ScaleAfterDistance: TGeoFloat;

    procedure RemoveSame;

    { * auto build opt from convex hull point * }
    procedure ConvexHullFrom(From_: TVec2List); overload;

    { rebuild }
    procedure Rebuild(pl: TVec2List; Scale_: TGeoFloat; angle_: TGeoFloat; ExpandMode_: TExpandMode; Position_: TVec2); overload;
    procedure Rebuild(pl: TVec2List; reset_: Boolean); overload;
    procedure Rebuild; overload;
    procedure Rebuild(Scale_: TGeoFloat; angle_: TGeoFloat; ExpandMode_: TExpandMode; Position_: TVec2); overload;

    function BoundBox: TRectV2; overload;
    function Centroid: TVec2; overload;

    function InHere(pt: TVec2): Boolean; overload;
    function InHere(ExpandDistance_: TGeoFloat; pt: TVec2): Boolean; overload;

    { * line intersect * }
    function LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean; overload;
    function LineIntersect(ExpandDistance_: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean; overload;
    function LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean;
      out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean; overload;
    function LineIntersect(ExpandDistance_: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean;
      out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean; overload;

    { * sample line intersect * }
    function SimpleLineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean; overload;

    { * get minimum point from Polygon * }
    function GetNearLine(const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: TGeoInt): TVec2; overload;
    function GetNearLine(ExpandDistance_: TGeoFloat; const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: TGeoInt): TVec2; overload;

    function Collision2Circle(cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean): Boolean; overload;
    function Collision2Circle(cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: TDeflectionPolygonLines): Boolean; overload;
    function Collision2Circle(ExpandDistance_: TGeoFloat; cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: TDeflectionPolygonLines): Boolean; overload;

    function PolygonIntersect(Poly_: TDeflectionPolygon): Boolean; overload;
    function PolygonIntersect(vl_: TVec2List): Boolean; overload;

    function LerpToEdge(pt: TVec2; ProjDistance_, ExpandDistance_: TGeoFloat; FromIdx, toidx: TGeoInt): TVec2;

    property Scale: TGeoFloat read FScale write FScale;
    property Angle: TGeoFloat read FAngle write FAngle;
    property Position: TVec2 read FPosition write FPosition;
    function GetDeflectionPolygon(index: TGeoInt): PDeflectionPolygonVec;
    property DeflectionPolygon[index: TGeoInt]: PDeflectionPolygonVec read GetDeflectionPolygon;
    property MaxRadius: TGeoFloat read FMaxRadius;
    property ExpandMode: TExpandMode read FExpandMode write FExpandMode;
    property Name: TPascalString read FName write FName;
    property Classifier: TPascalString read FClassifier write FClassifier;

    function GetPoint(idx: TGeoInt): TVec2;
    procedure SetPoint(idx: TGeoInt; Value: TVec2);
    property Points[idx: TGeoInt]: TVec2 read GetPoint write SetPoint; default;
    function FirstPoint: TVec2;
    function LastPoint: TVec2;

    function GetExpands(idx: TGeoInt; ExpandDist: TGeoFloat): TVec2;
    property Expands[idx: TGeoInt; ExpandDist: TGeoFloat]: TVec2 read GetExpands;

    procedure SaveToStream(stream: TMemoryStream64); overload;
    procedure LoadFromStream(stream: TMemoryStream64); overload;

    property UserDataObject: TCoreClassObject read FUserDataObject write FUserDataObject;
    property UserData: Pointer read FUserData write FUserData;
  end;

  TDeflectionPolygonList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC} TGenericsList<TDeflectionPolygon>;

  TDeflectionPolygonList = class(TDeflectionPolygonList_Decl)
  public
    AutoFree: Boolean;
    BackgroundBox: TRectV2;

    constructor Create;
    destructor Destroy; override;

    procedure Remove(obj: TDeflectionPolygon);
    procedure Delete(index: TGeoInt);
    procedure Clear;

    function BoundBox: TRectV2;

    function FindPolygon(Name: TPascalString): TDeflectionPolygon;
    function MakePolygonName(Name: TPascalString): TPascalString;

    procedure SaveToStream(stream: TCoreClassStream);
    procedure LoadFromStream(stream: TCoreClassStream);

    procedure LoadFromBase64(const buff: TPascalString);
  end;

  TPoly = TDeflectionPolygon;

  TDeflectionPolygonLine = record
    buff: array [0 .. 1] of TVec2;
    OwnerDeflectionPolygon: TDeflectionPolygon;
    OwnerDeflectionPolygonIndex: array [0 .. 1] of TGeoInt;
    index: TGeoInt;
  public
    procedure SetLocation(const lb, le: TVec2);
    function ExpandPoly(ExpandDist: TGeoFloat): TDeflectionPolygonLine;
    function length: TGeoFloat;
    function MinimumDistance(const pt: TVec2): TGeoFloat; overload;
    function MinimumDistance(ExpandDist: TGeoFloat; const pt: TVec2): TGeoFloat; overload;
    function ClosestPointFromLine(const pt: TVec2): TVec2; overload;
    function ClosestPointFromLine(ExpandDist: TGeoFloat; const pt: TVec2): TVec2; overload;
    function MiddlePoint: TVec2;
  end;

  PDeflectionPolygonLine = ^TDeflectionPolygonLine;

  TDeflectionPolygonLines = class(TCoreClassPersistent)
  private
    FList: TCoreClassList;
    FUserData: Pointer;
    FUserObject: TCoreClassObject;
    function GetItems(index: TGeoInt): PDeflectionPolygonLine;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TCoreClassPersistent); override;

    property Items[index: TGeoInt]: PDeflectionPolygonLine read GetItems; default;
    function Add(v: TDeflectionPolygonLine): TGeoInt; overload;
    function Add(lb, le: TVec2): TGeoInt; overload;
    function Add(lb, le: TVec2; idx1, idx2: TGeoInt; polygon: TDeflectionPolygon): TGeoInt; overload;
    function Count: TGeoInt;
    procedure Clear;
    procedure Delete(index: TGeoInt);

    function NearLine(const ExpandDist: TGeoFloat; const pt: TVec2): PDeflectionPolygonLine;
    function FarLine(const ExpandDist: TGeoFloat; const pt: TVec2): PDeflectionPolygonLine;

    procedure SortOfNear(const pt: TVec2); overload;
    procedure SortOfFar(const pt: TVec2); overload;

    property UserData: Pointer read FUserData write FUserData;
    property UserObject: TCoreClassObject read FUserObject write FUserObject;
  end;

  TV2Rect4 = record
  public
    LeftTop, RightTop, RightBottom, LeftBottom: TVec2;

    function IsZero: Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Rotation(Angle: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Rotation(axis: TVec2; Angle: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ScaleToRect(Box: TRectV2; Edge: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ScaleToRect(Box: TRectV2; Angle, Edge: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function ScaleToRect(Box: TRectV2; axis: TVec2; Angle, Edge: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Add(v: TVec2): TV2Rect4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Sub(v: TVec2): TV2Rect4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Mul(v: TVec2): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Mul(v: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Mul(X, Y: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Div_(v: TVec2): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Div_(v: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function MoveTo(Position: TVec2): TV2Rect4; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function BoundRect: TRectV2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function BoundRectf: TRectf; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Centroid: TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Transform(v2: TVec2): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Transform(X, Y: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Expands(Dist: TGeoFloat): TV2Rect4;
    function InHere(pt: TVec2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function InHere(r: TRectV2): Boolean; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetArrayVec2: TArrayVec2;
    function GetNear(pt: TVec2): TVec2;
    function Projection(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Projection(const sour, dest: TRectV2; sourAngle, destAngle: TGeoFloat): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Projection(const sour, dest: TRectV2): TV2Rect4; overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(r: TRectV2): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(r: TRectV2; axis: TVec2; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(r: TRectV2; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(r: TRectf; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(r: TRect; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(width, height, Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(width, height: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Init(): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(r: TRectV2): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(r: TRectV2; axis: TVec2; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(r: TRectV2; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(r: TRectf; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(r: TRect; Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(width, height, Ang: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(width, height: TGeoFloat): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    class function Create(): TV2Rect4; overload; static; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  end;

  TV2R4 = TV2Rect4;

  PV2Rect4 = ^TV2Rect4;
  PV2R4 = PV2Rect4;
  TV2Rect4List = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<PV2R4>;
  TV2R4List = TV2Rect4List;

  TTriangleList_Decl = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<PTriangle>;

  TTriangleList = class(TTriangleList_Decl)
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddTri(T_: TTriangle);
    procedure Remove(p: PTriangle);
    procedure Delete(index: TGeoInt);
    procedure Clear;

    procedure BuildTriangle(polygon: TVec2List); overload;
    procedure BuildTriangle(polygon: TVec2List; MinAngle, MinSegmentLength, MaxElementSize: TGeoFloat); overload;
    procedure BuildTriangle(polygon: T2DPolygonGraph); overload;
    procedure BuildTriangle(polygon: T2DPolygonGraph; MinAngle, MinSegmentLength, MaxElementSize: TGeoFloat); overload;
  end;

  TRectPackData = record
    Rect: TRectV2;
    error: Boolean;
    Data1: Pointer;
    Data2: TCoreClassObject;
  end;

  PRectPackData = ^TRectPackData;

  TRectPacking = class(TCoreClassPersistent)
  private
    FList: TCoreClassList;
    function Pack(width, height: TGeoFloat; var X, Y: TGeoFloat): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function GetItems(const index: TGeoInt): PRectPackData;
  public
    MaxWidth, MaxHeight: TGeoFloat;
    Margins: TGeoFloat;

    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const X, Y, width, height: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(Data1: Pointer; Data2: TCoreClassObject; X, Y, width, height: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(Data1: Pointer; Data2: TCoreClassObject; r: TRectV2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure Add(Data1: Pointer; Data2: TCoreClassObject; width, height: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Data1Exists(const Data1: Pointer): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Data2Exists(const Data2: TCoreClassObject): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Count: TGeoInt;
    property Items[const index: TGeoInt]: PRectPackData read GetItems; default;

    procedure Build(SpaceWidth, SpaceHeight: TGeoFloat); overload;
    procedure Build; overload;
  end;

  THausdorf = class
  private type
    PNode = ^TNode;

    TNode = record
      Prev, Next: PNode;
      Data: TVec2;
    end;

    { An implementation of a Linked List data structure. Fields that are used in it:
      'head', 'tail' - pointers referring to the first and las elements in the list
      'Num' - number of elements stored in the list
      'looped' - indicates, if the element following by the tail is the head of the list }
    PLinkedList = ^TLinkedList;

    TLinkedList = record
      Head, Tail: PNode;
      Num: TGeoInt;
      Looped: Boolean;
    end;

    TNodeList = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<PNode>;
    TLinkList = {$IFDEF FPC}specialize {$ENDIF FPC}TGenericsList<PLinkedList>;
  private
    FPolygon1, FPolygon2, FOutput: PLinkedList;
    FRoundKOEF: TGeoFloat; { A threshold. This value is used for imprecise comparisons. }

    { temp list }
    NodeList: TNodeList;
    LinkList: TLinkList;
  private
    procedure NewNode(var p: PNode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    procedure NewLink(var p: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure creates a TVec2 using the passed coordinates and wraps it into passed wrapper.
      'wrapper' - the wrapper to wrap the TVec2 in.
      'x', 'y' - the coordinates of the TVec2.
    }
    procedure wrapVector(var wrapper: PNode; const X, Y: TGeoFloat); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.initialises the list: allocates the memory for it,
      sets the 'head' and 'tail' fields to 'nil', sets zero as a starting value for the 'num' field.
      'target' - the field to be initialised.
    }
    procedure initList(var target: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.initialises the list, reads the polygon from the specified input stream, and writes them to the specified list.
      The data format is the following. The single number on the first line specifies the number of points in the polygon.
      Each of the following lines contains two numbers with an x- and y-coordinates of the point. The list is looped after data are read.
      'target' - the list to write the data to.
      'source' - the input stream to read from.
    }
    procedure initAndReadPolygon(var target: PLinkedList; const Source: TVec2List); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The function provides an access to the list's elements by their index. If the list is looped, the index could exceed the list's num.
      The index starts from 0.
      'target' - the list to get element from.
      'n' - the index of an element.
    }
    function get(var target: PLinkedList; const n_: TGeoInt): PNode; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.gets the longest vectors form the source list and puts them to the target list.
      Note: the comparison is performed impreciesly, using the FRoundKOEF threshold.
    }
    procedure getMax(var target, Source: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.gets the shortest vectors form the source list and puts them to the target list.
      Note: the comparison is performed impreciesly, using the FRoundKOEF threshold.
    }
    procedure getMin(var target, Source: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.adds wrapped items to the specified list. The list should be initialised prior to calling the procedure.
      'target' - the list to add item to.
      'item' - the item to be added.
    }
    procedure addNodeTo(var target: PLinkedList; const item: PNode); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.adds a TVec2 to the specified list. The list should be initialised prior to calling the procedure.
      'target' - the list to add item to.
      'p' - the TVec2 to be added to the list.
    }
    procedure addTo(var target: PLinkedList; p: TVec2); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.adds a TVec2 specified by its coordinates to the specified list. The list should be initialised prior to calling the procedure.
      'target' - the list to add item to.
      'x', 'y' - the coordinates of the TVec2 to be added.
    }
    procedure addTo(var target: PLinkedList; X, Y: TGeoFloat); overload; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { The procedure THausdorf.adds the TVec2 to the specified list considering its order defined by the 'compare()' function. }
    procedure addToQ(const target: PLinkedList; const p: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { The function THausdorf.defining the order of the vectors (based on the angle to the OX-axis). }
    function Compare(const p1, p2: TVec2): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The functions tests whether the point belongs to the internal area of the polygon.
      Note: the method used in this procedures returns the valid answer if the polygon is convex. Otherwise it is not applicable.
      'pol' - the polygon.
      'p' - the point.
    }
    function contains(const pol: PLinkedList; const p: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { The procedure THausdorf.copies the source list to the target list removing the duplicates of the items in the source list. }
    procedure deleteCopies(var target, Source: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { The procedure THausdorf.calculates the Hausdorff distance from the first polygon to from second one. The data are stored in the specified list. }
    procedure hausdorfDistanceVectors(var target, Polygon1_, Polygon2_: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The function THausdorf.returns true if the specified TVec2 is already present in the specified list.
      'target' - the list to be tested.
      'p' - the item to look for.
    }
    function isInList(const target: PLinkedList; const p: TVec2): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { The function THausdorf.defines whether the current position of two convex polygons is optimal or not. }
    function isOptimal(var distVecs: PLinkedList): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.loops the list. It means, that it makes the head item be the next item after the tail.
      'target' - the list to be looped.
    }
    procedure loopTheList(var target: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.
      calculates the distance vectors from the point to the edges of the polygon.
      The results are stored in the specified list.
    }
    procedure pointPolygonDistanceVectors(var target, pol: PLinkedList; const p: TVec2); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The function THausdorf.calculates the distance TVec2 between a point and a section.
      'a', 'b' - the ends of the section.
      'p' - the point.
    }
    function pointSectionDistanceVector(const a, b, p: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.
      calculates the shortest distance vectors from each vertex of the first polygon to the second polygon.
      The data are stored to the specified list.
    }
    procedure polygonPolygonDistanceVectors(var target, Polygon1_, Polygon2_: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The function THausdorf.calculates the pseudo scalar product of two vectors.
      Note: pseudo scalar product is defined as a product of vectors' lengths multiplied by the sinus of the angle between the vectors.
      'a', 'b' - the vectors to be multiplied.
    }
    function pseudoScalarProduct(const a, b: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { The function THausdorf.
      determines the quadrant which the point belongs to. Zero is considered as point of the first quadrant.
      Each half-axis belongs to the quadrant to its left.
    }
    function Quadrant(const p: TVec2): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The procedure THausdorf.
      puts the vectors from the source list to the target list in the way, that the angle,
      closed to the OX-axis is non-decreasing.
    }
    procedure sortByAngle(var target, Source: PLinkedList); {$IFDEF INLINE_ASM} inline; {$ENDIF}
    { The function THausdorf.returns the normalised TVec2. }
    function normalise(const vec: TVec2): TVec2; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    {
      The function THausdorf.calculates the scalar product of the vectors.
      'a', 'b' - the vectors to be multiplied.
    }
    function scalarProduct(const a, b: TVec2): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  public
    class function Compute(const poly1_, poly2_: TVec2List; const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat): TGeoFloat; overload;
    class function Compute(
      const poly1_: TVec2List; const poly1_b, poly1_e: Integer;
      const poly2_: TVec2List; const poly2_b, poly2_e: Integer;
      const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat): TGeoFloat; overload;

    constructor Create(const poly1_, poly2_: TVec2List; const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat);
    destructor Destroy; override;

    function HausdorffReached(): TArrayVec2;
    function HausdorffDistance(): TGeoFloat;
    function polygonsIsOptimal(): Boolean;

    class procedure TestAndPrint(const poly1_, poly2_: TVec2List);
    class procedure Test1();
    class procedure Test2();
  end;

function ArrayVec2(const r: TRectV2): TArrayVec2; overload;
function ArrayVec2(const r: TV2Rect4): TArrayVec2; overload;
function ArrayVec2(const l: TLineV2): TArrayVec2; overload;
function ArrayVec2(const t: TTriangle): TArrayVec2; overload;

const
  MaxGeoFloat = MaxSingle;
  XPoint: T2DPoint = (1, 0);
  YPoint: T2DPoint = (0, 1);
  NULLPoint: T2DPoint = (0, 0);
  NULLVec2: T2DPoint = (0, 0);
  ZeroPoint: T2DPoint = (0, 0);
  ZeroVec2: T2DPoint = (0, 0);
  NULLRect: TRectV2 = ((0, 0), (0, 0));
  ZeroRect: TRectV2 = ((0, 0), (0, 0));
  NULLRectV2: TRectV2 = ((0, 0), (0, 0));
  ZeroRectV2: TRectV2 = ((0, 0), (0, 0));
  ZeroTriangle: TTriangle = ((0, 0), (0, 0), (0, 0));
  RightHandSide = -1;
  LeftHandSide = +1;
  CollinearOrientation = 0;
  AboveOrientation = +1;
  BelowOrientation = -1;
  CoplanarOrientation = 0;

implementation

uses Geometry3DUnit, DataFrameEngine, DoStatusIO;

const
  // Epsilon
  C_Epsilon = 1.0E-12;

  Zero = 0.0;
  PIDiv180 = 0.017453292519943295769236907684886;

{$INCLUDE GeometrySplitHeader.inc}
{$INCLUDE GeometrySplit.inc}
{$IFDEF FPC}


function Pointf(X, Y: TGeoFloat): TPointf;
begin
  Result.X := X;
  Result.Y := Y;
end;

function Rectf(Left, Top, Right, Bottom: TGeoFloat): TRectf;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;

{$ENDIF}


function FAbs(const v: Single): Single;
begin
  if v < 0 then
      Result := -v
  else
      Result := v;
end;

function FAbs(const v: Double): Double;
begin
  if v < 0 then
      Result := -v
  else
      Result := v;
end;

function Clamp(const AValue, aMin, aMax: TGeoFloat): TGeoFloat;
begin
  if aMin > aMax then
      Result := Clamp(AValue, aMax, aMin)
  else if AValue > aMax then
      Result := aMax
  else if AValue < aMin then
      Result := aMin
  else
      Result := AValue;
end;

function MaxF(const v1, v2: TGeoFloat): TGeoFloat;
begin
  if v1 > v2 then
      Result := v1
  else
      Result := v2;
end;

function MinF(const v1, v2: TGeoFloat): TGeoFloat;
begin
  if v1 < v2 then
      Result := v1
  else
      Result := v2;
end;

function CompareFloat(const f1, f2, Epsilon_: TGeoFloat): ShortInt;
begin
  if IsEqual(f1, f2, Epsilon_) then
      Result := 0
  else if f1 < f2 then
      Result := -1
  else
      Result := 1;
end;

function CompareFloat(const f1, f2: TGeoFloat): ShortInt;
begin
  if IsEqual(f1, f2, C_Epsilon) then
      Result := 0
  else if f1 < f2 then
      Result := -1
  else
      Result := 1;
end;

function MakeVec2(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakeVec2(const X, Y: TGeoInt): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakePoint(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakePoint(const X, Y: TGeoInt): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function MakePoint(const pt: TVec2): TPoint;
begin
  Result.X := Round(pt[0]);
  Result.Y := Round(pt[1]);
end;

function Point2Point(const pt: TVec2): TPoint;
begin
  Result.X := Round(pt[0]);
  Result.Y := Round(pt[1]);
end;

function Point2Pointf(const pt: TVec2): TPointf;
begin
  Result.X := pt[0];
  Result.Y := pt[1];
end;

function PointMake(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function PointMake(const pt: TPoint): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function PointMake(const pt: TPointf): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function Make2DPoint(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function Make2DPoint(const X, Y: TGeoInt): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function Make2DPoint(const pt: TPoint): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function Make2DPoint(const pt: TPointf): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function vec2(const p: PVec2): TVec2;
begin
  Result := p^;
end;

function vec2(const f: TGeoFloat): TVec2;
begin
  Result[0] := f;
  Result[1] := f;
end;

function vec2(const X, Y: TGeoFloat): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function vec2(const X, Y: TGeoInt): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function vec2(const X, Y: Int64): TVec2;
begin
  Result[0] := X;
  Result[1] := Y;
end;

function vec2(const pt: TPoint): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function vec2(const pt: TPointf): TVec2;
begin
  Result[0] := pt.X;
  Result[1] := pt.Y;
end;

function LineV2(const x1, y1, x2, y2: TGeoFloat): TLineV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function LineV2(const lb, le: TVec2): TLineV2;
begin
  Result[0] := lb;
  Result[1] := le;
end;

function LineV2(const lb, le: TPoint): TLineV2;
begin
  Result[0] := vec2(lb);
  Result[1] := vec2(le);
end;

function LineV2(const l: TLineV2_P): TLineV2;
begin
  Result[0] := l[0]^;
  Result[1] := l[1]^;
end;

function LineV2(const l: PLineV2_P): TLineV2;
begin
  Result[0] := l^[0]^;
  Result[1] := l^[1]^;
end;

function LineV2(const l: PLineV2): TLineV2;
begin
  Result := l^;
end;

function RoundVec2(const v: TVec2): TVec2;
begin
  Result[0] := Round(v[0]);
  Result[1] := Round(v[1]);
end;

function MakePointf(const pt: TVec2): TPointf;
begin
  Result.X := pt[0];
  Result.Y := pt[1];
end;

function IsZero(const v: TGeoFloat): Boolean;
begin
  Result := IsEqual(v, 0, C_Epsilon);
end;

function IsZero(const pt: TVec2): Boolean;
begin
  Result := IsEqual(pt[0], 0, C_Epsilon) and IsEqual(pt[1], 0, C_Epsilon);
end;

function IsZero(const r: TRectV2): Boolean;
begin
  Result := IsZero(r[0]) and IsZero(r[1]);
end;

function IsNan(const pt: TVec2): Boolean;
begin
  Result := Math.IsNan(pt[0]) or Math.IsNan(pt[1]);
end;

function IsNan(const X, Y: TGeoFloat): Boolean;
begin
  Result := Math.IsNan(X) or Math.IsNan(Y);
end;

function HypotX(const X, Y: Extended): TGeoFloat;
{
  formula: Sqrt(X*X + Y*Y)
  implemented as: |Y|*Sqrt(1+Sqr(X/Y)), |X| < |Y| for greater precision
}
var
  Temp, TempX, TempY: Extended;
begin
  TempX := FAbs(X);
  TempY := FAbs(Y);
  if TempX > TempY then
    begin
      Temp := TempX;
      TempX := TempY;
      TempY := Temp;
    end;
  if TempX = 0 then
      Result := TempY
  else // TempY > TempX, TempX <> 0, so TempY > 0
      Result := TempY * Sqrt(1 + Sqr(TempX / TempY));
end;

function PointNorm(const v: TVec2): TGeoFloat;
begin
  Result := v[0] * v[0] + v[1] * v[1];
end;

function PointNegate(const v: TVec2): TVec2;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
end;

function Vec2Norm(const v: TVec2): TGeoFloat;
begin
  Result := v[0] * v[0] + v[1] * v[1];
end;

function Vec2Negate(const v: TVec2): TVec2;
begin
  Result[0] := -v[0];
  Result[1] := -v[1];
end;

function vec2Inv(const v: TVec2): TVec2;
begin
  Result[0] := v[1];
  Result[1] := v[0];
end;

procedure SetVec2(var v: TVec2; const vSrc: TVec2);
begin
  v[0] := vSrc[0];
  v[1] := vSrc[1];
end;

function Vec2Add(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] + v2[0];
  Result[1] := v1[1] + v2[1];
end;

function Vec2Add(const v1: TVec2; v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + v2;
  Result[1] := v1[1] + v2;
end;

function Vec2Add(const v1: TVec2; X, Y: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + X;
  Result[1] := v1[1] + Y;
end;

function Vec2Add(const v1: TGeoFloat; v2: TVec2): TVec2;
begin
  Result[0] := v1 + v2[0];
  Result[1] := v1 + v2[1];
end;

function Vec2Add(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Add(v1[i], v2);
end;

function Vec2Add(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Add(v1[i], v2);
end;

function Vec2Sub(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] - v2[0];
  Result[1] := v1[1] - v2[1];
end;

function Vec2Sub(const v1: TVec2; v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] - v2;
  Result[1] := v1[1] - v2;
end;

function Vec2Sub(const v1: TGeoFloat; v2: TVec2): TVec2;
begin
  Result[0] := v1 - v2[0];
  Result[1] := v1 - v2[1];
end;

function Vec2Sub(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Sub(v1[i], v2);
end;

function Vec2Sub(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Sub(v1[i], v2);
end;

function Vec2Mul(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0];
  Result[1] := v1[1] * v2[1];
end;

function Vec2Mul(const v1, v2: TVec2; const v3: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3;
  Result[1] := v1[1] * v2[1] * v3;
end;

function Vec2Mul(const v1, v2: TVec2; const v3, v4: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3 * v4;
  Result[1] := v1[1] * v2[1] * v3 * v4;
end;

function Vec2Mul(const v1, v2, v3: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3[0];
  Result[1] := v1[1] * v2[1] * v3[1];
end;

function Vec2Mul(const v1, v2, v3, v4: TVec2): TVec2;
begin
  Result[0] := v1[0] * v2[0] * v3[0] * v4[0];
  Result[1] := v1[1] * v2[1] * v3[1] * v4[1];
end;

function Vec2Mul(const v1: TVec2; const v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2;
  Result[1] := v1[1] * v2;
end;

function Vec2Mul(const v1: TVec2; const v2, v3: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2 * v3;
  Result[1] := v1[1] * v2 * v3;
end;

function Vec2Mul(const v1: TVec2; const v2, v3, v4: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] * v2 * v3 * v4;
  Result[1] := v1[1] * v2 * v3 * v4;
end;

function Vec2Mul(const v1: TGeoFloat; const v2: TVec2): TVec2;
begin
  Result[0] := v1 * v2[0];
  Result[1] := v1 * v2[1];
end;

function Vec2Mul(const v1: TArrayVec2; v2: TVec2): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Mul(v1[i], v2);
end;

function Vec2Mul(const v1: TArrayVec2; v2: TGeoFloat): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, length(v1));
  for i := Low(v1) to high(v1) do
      Result[i] := Vec2Mul(v1[i], v2);
end;

function Vec2Div(const v1: TVec2; const v2: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] / v2;
  Result[1] := v1[1] / v2;
end;

function Vec2Div(const v1, v2: TVec2): TVec2;
begin
  Result[0] := v1[0] / v2[0];
  Result[1] := v1[1] / v2[1];
end;

function Vec2Div(const v1: TGeoFloat; const v2: TVec2): TVec2;
begin
  Result[0] := v1 / v2[0];
  Result[1] := v1 / v2[1];
end;

function PointNormalize(const v: TVec2): TVec2;
var
  InvLen: TGeoFloat;
  vn: TGeoFloat;
begin
  vn := PointNorm(v);
  if vn = 0 then
      SetVec2(Result, v)
  else
    begin
      InvLen := 1 / Sqrt(vn);
      Result[0] := v[0] * InvLen;
      Result[1] := v[1] * InvLen;
    end;
end;

function Vec2Normalize(const v: TVec2): TVec2;
var
  InvLen: TGeoFloat;
  vn: TGeoFloat;
begin
  vn := PointNorm(v);
  if vn = 0 then
      SetVec2(Result, v)
  else
    begin
      InvLen := 1 / Sqrt(vn);
      Result[0] := v[0] * InvLen;
      Result[1] := v[1] * InvLen;
    end;
end;

function PointLength(const v: TVec2): TGeoFloat;
begin
  Result := Sqrt(PointNorm(v));
end;

function Vec2Length(const v: TVec2): TGeoFloat;
begin
  Result := Sqrt(Vec2Norm(v));
end;

procedure PointScale(var v: TVec2; factor: TGeoFloat);
begin
  v[0] := v[0] * factor;
  v[1] := v[1] * factor;
end;

function PointDotProduct(const v1, v2: TVec2): TGeoFloat;
begin
  Result := v1[0] * v2[0] + v1[1] * v2[1];
end;

function Distance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat;
begin
  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
end;

function Distance(const x1, y1, z1, x2, y2, z2: TGeoFloat): TGeoFloat;
begin
  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1) + (z2 - z1) * (z2 - z1));
end;

function Distance(const l: TLineV2): TGeoFloat;
begin
  Result := Distance(l[0, 0], l[0, 1], l[1, 0], l[1, 1]);
end;

function Distance(const f1, f2: TGeoFloat): TGeoFloat;
begin
  Result := abs(f2 - f1);
end;

function FloatDistance(const f1, f2: TGeoFloat): TGeoFloat;
begin
  Result := abs(f2 - f1);
end;

function PointDistance(const x1, y1, x2, y2: TGeoFloat): TGeoFloat;
begin
  Result := Sqrt((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1));
end;

function PointDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqrt((v2[0] - v1[0]) * (v2[0] - v1[0]) + (v2[1] - v1[1]) * (v2[1] - v1[1]));
end;

function Vec2Distance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqrt((v2[0] - v1[0]) * (v2[0] - v1[0]) + (v2[1] - v1[1]) * (v2[1] - v1[1]));
end;

function LineDistance(const l: TLineV2): TGeoFloat;
begin
  Result := Distance(l[0, 0], l[0, 1], l[1, 0], l[1, 1]);
end;

function PointLayDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Pow(v2[0] - v1[0]) + Pow(v2[1] - v1[1]);
end;

function SqrDistance(const v1, v2: TVec2): TGeoFloat;
begin
  Result := Sqr(v2[0] - v1[0]) + Sqr(v2[1] - v1[1]);
end;

function PointLerp(const v1, v2: TVec2; t: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + (v2[0] - v1[0]) * t;
  Result[1] := v1[1] + (v2[1] - v1[1]) * t;
end;

function PointLerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2;
var
  dx: TGeoFloat;
  dy: TGeoFloat;
  k: Double;
begin
  dx := dest[0] - sour[0];
  dy := dest[1] - sour[1];
  if ((dx <> 0) or (dy <> 0)) and (d <> 0) then
    begin
      k := d / Sqrt(dx * dx + dy * dy);
      Result[0] := sour[0] + k * dx;
      Result[1] := sour[1] + k * dy;
    end
  else
    begin
      Result := sour;
    end;
end;

function Vec2Lerp(const v1, v2: TVec2; t: TGeoFloat): TVec2;
begin
  Result[0] := v1[0] + (v2[0] - v1[0]) * t;
  Result[1] := v1[1] + (v2[1] - v1[1]) * t;
end;

function Vec2LerpTo(const sour, dest: TVec2; const d: TGeoFloat): TVec2;
var
  dx: TGeoFloat;
  dy: TGeoFloat;
  k: Double;
begin
  if d = 0 then
    begin
      Result := sour;
      exit;
    end;

  dx := dest[0] - sour[0];
  dy := dest[1] - sour[1];
  k := d / Sqrt(dx * dx + dy * dy);
  Result[0] := sour[0] + k * dx;
  Result[1] := sour[1] + k * dy;
end;

procedure SwapPoint(var v1, v2: TVec2);
var
  v: TVec2;
begin
  v := v1;
  v1 := v2;
  v2 := v;
end;

procedure SwapVec2(var v1, v2: TVec2);
var
  v: TVec2;
begin
  v := v1;
  v1 := v2;
  v2 := v;
end;

function Pow(v: TGeoFloat): TGeoFloat;
begin
  Result := v * v;
end;

function Pow(const v, n: TGeoFloat): TGeoFloat;
begin
  Result := Math.Power(v, n);
end;

function MiddleVec2(const pt1, pt2: TVec2): TVec2;
begin
  Result[0] := (pt1[0] + pt2[0]) * 0.5;
  Result[1] := (pt1[1] + pt2[1]) * 0.5;
end;

function Vec2Middle(const pt1, pt2: TVec2): TVec2;
begin
  Result[0] := (pt1[0] + pt2[0]) * 0.5;
  Result[1] := (pt1[1] + pt2[1]) * 0.5;
end;

function IsEqual(const Val1, Val2, Epsilon_: TGeoFloat): Boolean;
var
  Diff: TGeoFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon_ <= Diff) and (Diff <= Epsilon_)) = (FAbs(Diff) <= Epsilon_), 'Error - Illogical error in equality Detect. (IsEqual)');
  Result := ((-Epsilon_ <= Diff) and (Diff <= Epsilon_));
end;

function IsEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := IsEqual(Val1, Val2, C_Epsilon);
end;

function IsEqual(const Val1, Val2: TVec2): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0]) and IsEqual(Val1[1], Val2[1]);
end;

function IsEqual(const Val1, Val2: TVec2; Epsilon_: TGeoFloat): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0], Epsilon_) and IsEqual(Val1[1], Val2[1], Epsilon_);
end;

function IsEqual(const Val1, Val2: TRectV2): Boolean;
begin
  Result := IsEqual(Val1[0], Val2[0]) and IsEqual(Val1[1], Val2[1]);
end;

function NotEqual(const Val1, Val2, Epsilon_: TGeoFloat): Boolean;
var
  Diff: TGeoFloat;
begin
  Diff := Val1 - Val2;
  Assert(((-Epsilon_ > Diff) or (Diff > Epsilon_)) = (FAbs(Val1 - Val2) > Epsilon_), 'Error - Illogical error in equality Detect. (NotEqual)');
  Result := ((-Epsilon_ > Diff) or (Diff > Epsilon_));
end;

function NotEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := NotEqual(Val1, Val2, C_Epsilon);
end;

function NotEqual(const Val1, Val2: TVec2): Boolean;
begin
  Result := NotEqual(Val1[0], Val2[0]) or NotEqual(Val1[1], Val2[1]);
end;

function LessThanOrEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := (Val1 < Val2) or IsEqual(Val1, Val2);
end;

function GreaterThanOrEqual(const Val1, Val2: TGeoFloat): Boolean;
begin
  Result := (Val1 > Val2) or IsEqual(Val1, Val2);
end;

function GetEquilateralTriangleCen(pt1, pt2: TVec2): TVec2;
const
  Sin60: TGeoFloat = 0.86602540378443864676372317075294;
  Cos60: TGeoFloat = 0.50000000000000000000000000000000;
var
  b, E, pt: TVec2;
begin
  b := pt1;
  E := pt2;
  E[0] := E[0] - b[0];
  E[1] := E[1] - b[1];
  pt[0] := ((E[0] * Cos60) - (E[1] * Sin60)) + b[0];
  pt[1] := ((E[1] * Cos60) + (E[0] * Sin60)) + b[1];
  Assert(Intersect(pt1, MiddleVec2(pt2, pt), pt2, MiddleVec2(pt1, pt), Result));
end;

procedure Rotate(RotAng: TGeoFloat; const X, Y: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  SinVal: TGeoFloat;
  CosVal: TGeoFloat;
begin
  RotAng := RotAng * PIDiv180;
  SinVal := Sin(RotAng);
  CosVal := Cos(RotAng);
  Nx := (X * CosVal) - (Y * SinVal);
  Ny := (Y * CosVal) + (X * SinVal);
end;

function Rotate(const RotAng: TGeoFloat; const Point: TVec2): TVec2;
begin
  Rotate(RotAng, Point[0], Point[1], Result[0], Result[1]);
end;

function NormalizeDegAngle(const Angle: TGeoFloat): TGeoFloat;
begin
  Result := Angle - Int(Angle * (1 / 360)) * 360;
  if Result > 180 then
      Result := Result - 360
  else if Result < -180 then
      Result := Result + 360;
end;

function PointAngle(const axis, pt: TVec2): TGeoFloat;
begin
  Result := NormalizeDegAngle(RadToDeg(ArcTan2(axis[1] - pt[1], axis[0] - pt[0])));
end;

function Vec2Angle(const axis, pt: TVec2): TGeoFloat;
begin
  Result := NormalizeDegAngle(RadToDeg(ArcTan2(axis[1] - pt[1], axis[0] - pt[0])));
end;

function PointAngle(const pt: TVec2): TGeoFloat;
begin
  Result := PointAngle(NULLPoint, pt);
end;

function Vec2Angle(const pt: TVec2): TGeoFloat;
begin
  Result := Vec2Angle(NULLPoint, pt);
end;

function AngleDistance(const s, a: TGeoFloat): TGeoFloat;
begin
  Result := FAbs(s - a);
  if Result > 180 then
      Result := 360 - Result;
end;

function PointRotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2;
begin
  Result[0] := axis[0] - (Cos(DegToRad(Angle)) * Dist);
  Result[1] := axis[1] - (Sin(DegToRad(Angle)) * Dist);
end;

function PointRotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2;
begin
  Result := PointRotation(axis, PointDistance(axis, pt), Angle);
end;

function Vec2Rotation(const axis: TVec2; const Dist, Angle: TGeoFloat): TVec2;
begin
  Result[0] := axis[0] - (Cos(DegToRad(Angle)) * Dist);
  Result[1] := axis[1] - (Sin(DegToRad(Angle)) * Dist);
end;

function Vec2Rotation(const axis, pt: TVec2; const Angle: TGeoFloat): TVec2;
begin
  Result := Vec2Rotation(axis, Vec2Distance(axis, pt), Angle);
end;

function Vec2Rotation(const sour_r: TRectV2; const Angle: TGeoFloat; const pt: TVec2): TVec2;
begin
  Result := Vec2Rotation(sour_r, RectCentre(sour_r), Angle, pt);
end;

function Vec2Rotation(const sour_r: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const pt: TVec2): TVec2;
begin
  Result := Vec2Rotation(axis, pt, NormalizeDegAngle(Vec2Angle(axis, pt) - Angle));
end;

function Vec2Rotation(const sour_r: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const r: TRectV2): TRectV2;
begin
  Result[0] := Vec2Rotation(sour_r, axis, Angle, r[0]);
  Result[1] := Vec2Rotation(sour_r, axis, Angle, r[1]);
end;

function CircleInCircle(const cp1, cp2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  Result := (r2 - (PointDistance(cp1, cp2) + r1) >= Zero);
end;

function CircleInRect(const cp: TVec2; const radius: TGeoFloat; r: TRectV2): Boolean;
begin
  FixRect(r[0, 0], r[0, 1], r[1, 0], r[1, 1]);
  Result := PointInRect(cp, MakeRect(Vec2Sub(r[0], radius), Vec2Add(r[1], radius)));
end;

function PointInRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function PointInRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function PointInRect(const X, Y: TGeoInt; const r: TRect): Boolean;
begin
  Result := PointInRect(X, Y, r.Left, r.Top, r.Right, r.Bottom);
end;

function PointInRect(const pt: TPoint; const r: TRect): Boolean;
begin
  Result := PointInRect(pt.X, pt.Y, r.Left, r.Top, r.Right, r.Bottom);
end;

function PointInRect(const pt: TVec2; const r: TRectV2): Boolean;
begin
  Result := PointInRect(pt[0], pt[1], r[0, 0], r[0, 1], r[1, 0], r[1, 1]);
end;

function PointInRect(const Px, Py: TGeoFloat; const r: TRectV2): Boolean;
begin
  Result := PointInRect(Px, Py, r[0, 0], r[0, 1], r[1, 0], r[1, 1]);
end;

function Vec2InRect(const Px, Py: TGeoFloat; const x1, y1, x2, y2: TGeoFloat): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function Vec2InRect(const Px, Py: TGeoInt; const x1, y1, x2, y2: TGeoInt): Boolean;
begin
  Result := ((x1 <= Px) and (Px <= x2) and (y1 <= Py) and (Py <= y2)) or ((x2 <= Px) and (Px <= x1) and (y2 <= Py) and (Py <= y1));
end;

function Vec2InRect(const pt: TVec2; const r: TRectV2): Boolean;
begin
  Result := Vec2InRect(pt[0], pt[1], r[0, 0], r[0, 1], r[1, 0], r[1, 1]);
end;

function Vec2InRect(const Px, Py: TGeoFloat; const r: TRectV2): Boolean;
begin
  Result := Vec2InRect(Px, Py, r[0, 0], r[0, 1], r[1, 0], r[1, 1]);
end;

function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := (x1 <= x4) and (x2 >= x3) and (y1 <= y4) and (y2 >= y3);
end;

function RectToRectIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean;
begin
  Result := (x1 <= x4) and (x2 >= x3) and (y1 <= y4) and (y2 >= y3);
end;

function RectToRectIntersect(const r1, r2: TRectV2): Boolean;
begin
  Result := RectToRectIntersect(r1[0, 0], r1[0, 1], r1[1, 0], r1[1, 1], r2[0, 0], r2[0, 1], r2[1, 0], r2[1, 1]);
end;

function RectToRectIntersect(const r1, r2: TRect): Boolean;
begin
  Result := RectToRectIntersect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function RectToRectIntersect(const r1, r2: TRectf): Boolean;
begin
  Result := RectToRectIntersect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function RectWithInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectWithInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectWithInRect(const r1, r2: TRectV2): Boolean;
begin
  Result := RectWithInRect(r1[0, 0], r1[0, 1], r1[1, 0], r1[1, 1], r2[0, 0], r2[0, 1], r2[1, 0], r2[1, 1]);
end;

function RectWithInRect(const r1, r2: TRect): Boolean;
begin
  Result := RectWithInRect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function RectInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectInRect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoInt): Boolean;
begin
  Result := PointInRect(x1, y1, x3, y3, x4, y4) and PointInRect(x2, y2, x3, y3, x4, y4);
end;

function RectInRect(const r1, r2: TRectV2): Boolean;
begin
  Result := RectInRect(r1[0, 0], r1[0, 1], r1[1, 0], r1[1, 1], r2[0, 0], r2[0, 1], r2[1, 0], r2[1, 1]);
end;

function RectInRect(const r1, r2: TRect): Boolean;
begin
  Result := RectInRect(r1.Left, r1.Top, r1.Right, r1.Bottom, r2.Left, r2.Top, r2.Right, r2.Bottom);
end;

function MakeRectV2(const centre: TVec2; const width, height: TGeoFloat): TRectV2;
begin
  Result[0, 0] := centre[0] - width * 0.5;
  Result[0, 1] := centre[1] - height * 0.5;
  Result[1, 0] := centre[0] + width * 0.5;
  Result[1, 1] := centre[1] + height * 0.5;
end;

function MakeRectV2(const X, Y, radius: TGeoFloat): TRectV2;
begin
  Result[0, 0] := X - radius;
  Result[0, 1] := Y - radius;
  Result[1, 0] := X + radius;
  Result[1, 1] := Y + radius;
end;

function MakeRectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function MakeRectV2(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function MakeRectV2(const X, Y: TGeoFloat; const p2: TVec2): TRectV2;
begin
  Result[0] := PointMake(X, Y);
  Result[1] := p2;
end;

function MakeRectV2(const r: TRect): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function MakeRectV2(const r: TRectf): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function RectV2(): TRectV2;
begin
  Result := ZeroRect;
end;

function RectV2(const centre: TVec2; const width, height: TGeoFloat): TRectV2;
begin
  Result[0, 0] := centre[0] - width * 0.5;
  Result[0, 1] := centre[1] - height * 0.5;
  Result[1, 0] := centre[0] + width * 0.5;
  Result[1, 1] := centre[1] + height * 0.5;
end;

function RectV2(const X, Y, radius: TGeoFloat): TRectV2;
begin
  Result[0, 0] := X - radius;
  Result[0, 1] := Y - radius;
  Result[1, 0] := X + radius;
  Result[1, 1] := Y + radius;
end;

function RectV2(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function RectV2(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function RectV2(const p1, p2: TPointf): TRectV2;
begin
  Result[0] := vec2(p1);
  Result[1] := vec2(p2);
end;

function RectV2(const X, Y: TGeoFloat; const p2: TVec2): TRectV2;
begin
  Result[0] := PointMake(X, Y);
  Result[1] := p2;
end;

function RectV2(const r: TRect): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function RectV2(const r: TRectf): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function RectV2(const r: TRectV2): TRectV2;
begin
  Result := FixedRect(r);
end;

function MakeRect(const centre: TVec2; const width, height: TGeoFloat): TRectV2;
begin
  Result[0, 0] := centre[0] - width * 0.5;
  Result[0, 1] := centre[1] - height * 0.5;
  Result[1, 0] := centre[0] + width * 0.5;
  Result[1, 1] := centre[1] + height * 0.5;
end;

function MakeRect(const X, Y, radius: TGeoFloat): TRectV2;
begin
  Result[0, 0] := X - radius;
  Result[0, 1] := Y - radius;
  Result[1, 0] := X + radius;
  Result[1, 1] := Y + radius;
end;

function MakeRect(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function MakeRect(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function MakeRect(const r: TRect): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function MakeRect(const r: TRectf): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function RoundRect(const r: TRectV2): TRect;
begin
  Result.Left := Round(r[0, 0]);
  Result.Top := Round(r[0, 1]);
  Result.Right := Round(r[1, 0]);
  Result.Bottom := Round(r[1, 1]);
end;

function Rect2Rect(const r: TRectV2): TRect;
begin
  Result.Left := Round(r[0, 0]);
  Result.Top := Round(r[0, 1]);
  Result.Right := Round(r[1, 0]);
  Result.Bottom := Round(r[1, 1]);
end;

function Rect2Rect(const r: TRect): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function RectMake(const X, Y, radius: TGeoFloat): TRectV2;
begin
  Result[0, 0] := X - radius;
  Result[0, 1] := Y - radius;
  Result[1, 0] := X + radius;
  Result[1, 1] := Y + radius;
end;

function RectMake(const x1, y1, x2, y2: TGeoFloat): TRectV2;
begin
  Result[0, 0] := x1;
  Result[0, 1] := y1;
  Result[1, 0] := x2;
  Result[1, 1] := y2;
end;

function RectMake(const p1, p2: TVec2): TRectV2;
begin
  Result[0] := p1;
  Result[1] := p2;
end;

function RectMake(const r: TRect): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function RectMake(const r: TRectf): TRectV2;
begin
  Result[0, 0] := r.Left;
  Result[0, 1] := r.Top;
  Result[1, 0] := r.Right;
  Result[1, 1] := r.Bottom;
end;

function RectAdd(const r: TRectV2; v2: TVec2): TRectV2;
begin
  Result[0] := Vec2Add(r[0], v2);
  Result[1] := Vec2Add(r[1], v2);
end;

function RectAdd(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Add(r1[0], r2[0]);
  Result[1] := Vec2Add(r1[1], r2[1]);
end;

function RectSub(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Sub(r1[0], r2[0]);
  Result[1] := Vec2Sub(r1[1], r2[1]);
end;

function RectSub(const r: TRectV2; pt: TVec2): TRectV2;
begin
  Result[0] := Vec2Sub(r[0], pt);
  Result[1] := Vec2Sub(r[1], pt);
end;

function RectMul(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], r2[0]);
  Result[1] := Vec2Mul(r1[1], r2[1]);
end;

function RectMul(const r1: TRectV2; v2: TVec2): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], v2[0]);
  Result[1] := Vec2Mul(r1[1], v2[1]);
end;

function RectMul(const r1: TRectV2; f2: TGeoFloat): TRectV2;
begin
  Result[0] := Vec2Mul(r1[0], f2);
  Result[1] := Vec2Mul(r1[1], f2);
end;

function RectDiv(const r1, r2: TRectV2): TRectV2;
begin
  Result[0] := Vec2Div(r1[0], r2[0]);
  Result[1] := Vec2Div(r1[1], r2[1]);
end;

function RectDiv(const r1: TRectV2; f2: TGeoFloat): TRectV2;
begin
  Result[0] := Vec2Div(r1[0], f2);
  Result[1] := Vec2Div(r1[1], f2);
end;

function RectDiv(const r1: TRectV2; v2: TVec2): TRectV2;
begin
  Result[0] := Vec2Div(r1[0], v2);
  Result[1] := Vec2Div(r1[1], v2);
end;

function RectOffset(const r: TRectV2; Offset: TVec2): TRectV2;
begin
  Result[0] := Vec2Add(r[0], Offset);
  Result[1] := Vec2Add(r[1], Offset);
end;

function RectSizeLerp(const r: TRectV2; const rSizeLerp: TGeoFloat): TRectV2;
begin
  Result[0] := r[0];
  Result[1] := PointLerp(r[0], r[1], rSizeLerp);
end;

function RectCenScale(const r: TRectV2; const rSizeScale: TGeoFloat): TRectV2;
var
  cen, siz: TVec2;
begin
  cen := PointLerp(r[0], r[1], 0.5);
  siz := Vec2Mul(RectSize(r), rSizeScale);
  Result[0] := Vec2Sub(cen, Vec2Mul(siz, 0.5));
  Result[1] := Vec2Add(cen, Vec2Mul(siz, 0.5));
end;

function RectEdge(const r: TRectV2; const Edge: TGeoFloat): TRectV2;
begin
  Result[0, 0] := r[0, 0] - Edge;
  Result[0, 1] := r[0, 1] - Edge;
  Result[1, 0] := r[1, 0] + Edge;
  Result[1, 1] := r[1, 1] + Edge;
end;

function RectEdge(const r: TRectV2; const Edge: TVec2): TRectV2;
begin
  Result[0, 0] := r[0, 0] - Edge[0];
  Result[0, 1] := r[0, 1] - Edge[1];
  Result[1, 0] := r[1, 0] + Edge[0];
  Result[1, 1] := r[1, 1] + Edge[1];
end;

function RectCentre(const r: TRectV2): TVec2;
begin
  Result := PointLerp(r[0], r[1], 0.5);
end;

function RectCentre(const r: TRect): TVec2;
begin
  Result := RectCentre(RectV2(r));
end;

function RectCentre(const r: TRectf): TVec2;
begin
  Result := RectCentre(RectV2(r));
end;

function Tri(const v1, v2, v3: TVec2): TTriangle;
begin
  Result[0] := v1;
  Result[1] := v2;
  Result[2] := v3;
end;

function TriAdd(const t: TTriangle; v: TVec2): TTriangle;
begin
  Result[0] := Vec2Add(t[0], v);
  Result[1] := Vec2Add(t[1], v);
  Result[2] := Vec2Add(t[2], v);
end;

function TriSub(const t: TTriangle; v: TVec2): TTriangle;
begin
  Result[0] := Vec2Sub(t[0], v);
  Result[1] := Vec2Sub(t[1], v);
  Result[2] := Vec2Sub(t[2], v);
end;

function TriMul(const t: TTriangle; v: TVec2): TTriangle;
begin
  Result[0] := Vec2Mul(t[0], v);
  Result[1] := Vec2Mul(t[1], v);
  Result[2] := Vec2Mul(t[2], v);
end;

function TriDiv(const t: TTriangle; v: TVec2): TTriangle;
begin
  Result[0] := Vec2Div(t[0], v);
  Result[1] := Vec2Div(t[1], v);
  Result[2] := Vec2Div(t[2], v);
end;

function TriCentre(const t: TTriangle): TVec2;
const
  TriCentre_OneThird = 1.0 / 3.0;
begin
  Result[0] := (t[0, 0] + t[1, 0] + t[2, 0]) * TriCentre_OneThird;
  Result[1] := (t[0, 1] + t[1, 1] + t[2, 1]) * TriCentre_OneThird;
end;

function TriExpand(const t: TTriangle; Dist: TGeoFloat): TTriangle;

  function getTriPt(idx: TGeoInt): TVec2;
  var
    lpt, pt, rpt: TVec2;
    ln, rn: TVec2;
    dx, dy, f, r: TGeoFloat;
    Cx, Cy: TGeoFloat;
  begin
    if idx > 0 then
        lpt := t[idx - 1]
    else
        lpt := t[3 - 1];
    if idx + 1 < 3 then
        rpt := t[idx + 1]
    else
        rpt := t[0];
    pt := t[idx];

    // normal : left to
    dx := (pt[0] - lpt[0]);
    dy := (pt[1] - lpt[1]);
    f := 1.0 / HypotX(dx, dy);
    ln[0] := (dy * f);
    ln[1] := -(dx * f);

    // normal : right to
    dx := (rpt[0] - pt[0]);
    dy := (rpt[1] - pt[1]);
    f := 1.0 / HypotX(dx, dy);
    rn[0] := (dy * f);
    rn[1] := -(dx * f);

    // compute the expand edge
    dx := (ln[0] + rn[0]);
    dy := (ln[1] + rn[1]);
    r := (ln[0] * dx) + (ln[1] * dy);
    if r = 0 then
        r := 1;
    Cx := (dx * Dist / r);
    Cy := (dy * Dist / r);

    Result[0] := pt[0] + Cx;
    Result[1] := pt[1] + Cy;
  end;

begin
  Result[0] := getTriPt(0);
  Result[1] := getTriPt(1);
  Result[2] := getTriPt(2);
end;

function TriRound(const t: TTriangle): TTriangle;
begin
  Result[0] := RoundVec2(t[0]);
  Result[1] := RoundVec2(t[1]);
  Result[2] := RoundVec2(t[2]);
end;

function Vec2Transform(const sour, dest: TRectV2; sour_pt: TVec2): TVec2;
begin
  Result := RectProjection(sour, dest, sour_pt);
end;

function Vec2Transform(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat; const sour_pt: TVec2): TVec2;
begin
  Result := RectRotationProjection(sour, dest, sourAngle, destAngle, sour_pt);
end;

function RectTransform(const sour, dest, sour_rect: TRectV2): TRectV2;
begin
  Result := RectProjection(sour, dest, sour_rect);
end;

function RectTransform(const sour, dest: TRectV2; const sour_rect: TRect): TRectV2;
begin
  Result := RectProjection(sour, dest, RectV2(sour_rect));
end;

function RectTransform(const sour, dest: TRectV2; const sour_rect: TRectf): TRectV2;
begin
  Result := RectProjection(sour, dest, RectV2(sour_rect));
end;

function RectScaleSpace(const r: TRectV2; const SS_width, SS_height: TGeoFloat): TRectV2;
var
  k: TGeoFloat;
  w, h, nw, nh: TGeoFloat;
  d: TVec2;
begin
  k := SS_width / SS_height;
  Result := ForwardRect(r);
  w := RectWidth(Result);
  h := RectHeight(Result);

  if w < h then
    begin
      nw := h * k;
      nh := h;
    end
  else
    begin
      nw := w;
      nh := w * k;
    end;

  d[0] := (nw - w) * 0.5;
  d[1] := (nh - h) * 0.5;
  Result[0] := Vec2Sub(Result[0], d);
  Result[1] := Vec2Add(Result[1], d);

  Result := FixRect(Result);
end;

function RectScaleSpace(const r: TRect; const SS_width, SS_height: TGeoInt): TRect;
begin
  Result := MakeRect(RectScaleSpace(RectV2(r), SS_width, SS_height));
end;

function CalibrationRectInRect(const r, Area: TRectV2): TRectV2;
var
  nr: TRectV2;
begin
  nr := ForwardRect(r);

  if nr[0, 0] < Area[0, 0] then
      nr := RectOffset(nr, vec2(Area[0, 0] - nr[0, 0], 0));
  if nr[0, 1] < Area[0, 1] then
      nr := RectOffset(nr, vec2(0, Area[0, 1] - nr[0, 1]));
  if nr[1, 0] > Area[1, 0] then
      nr := RectOffset(nr, vec2(Area[1, 0] - nr[1, 0], 0));
  if nr[1, 1] > Area[1, 1] then
      nr := RectOffset(nr, vec2(0, Area[1, 1] - nr[1, 1]));

  Result := Clip(nr, Area);
end;

function CalibrationRectInRect(const r, Area: TRect): TRect;
begin
  Result := MakeRect(CalibrationRectInRect(RectV2(r), RectV2(Area)));
end;

procedure FixRect(var Left, Top, Right, Bottom: TGeoInt);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

procedure FixRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

function FixRect(r: TRectV2): TRectV2;
begin
  Result := r;
  FixRect(Result[0, 0], Result[0, 1], Result[1, 0], Result[1, 1]);
end;

function FixRect(r: TRect): TRect;
begin
  Result := r;
  FixRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

procedure FixedRect(var Left, Top, Right, Bottom: TGeoInt);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

procedure FixedRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

function FixedRect(r: TRectV2): TRectV2;
begin
  Result := r;
  FixedRect(Result[0, 0], Result[0, 1], Result[1, 0], Result[1, 1]);
end;

function FixedRect(r: TRect): TRect;
begin
  Result := r;
  FixedRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

procedure ForwardRect(var Left, Top, Right, Bottom: TGeoInt);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

procedure ForwardRect(var Left, Top, Right, Bottom: TGeoFloat);
begin
  if Bottom < Top then
      Swap(Bottom, Top);
  if Right < Left then
      Swap(Right, Left);
end;

function ForwardRect(r: TRectV2): TRectV2;
begin
  Result := r;
  ForwardRect(Result[0, 0], Result[0, 1], Result[1, 0], Result[1, 1]);
end;

function ForwardRect(r: TRect): TRect;
begin
  Result := r;
  ForwardRect(Result.Left, Result.Top, Result.Right, Result.Bottom);
end;

function MakeRect(const r: TRectV2): TRect;
begin
  Result.Left := Round(r[0, 0]);
  Result.Top := Round(r[0, 1]);
  Result.Right := Round(r[1, 0]);
  Result.Bottom := Round(r[1, 1]);
end;

function MakeRectf(const r: TRectV2): TRectf;
begin
  Result.Left := r[0, 0];
  Result.Top := r[0, 1];
  Result.Right := r[1, 0];
  Result.Bottom := r[1, 1];
end;

function RectWidth(const r: TRectV2): TGeoFloat;
begin
  if r[1, 0] > r[0, 0] then
      Result := r[1, 0] - r[0, 0]
  else
      Result := r[0, 0] - r[1, 0];
end;

function RectHeight(const r: TRectV2): TGeoFloat;
begin
  if r[1, 1] > r[0, 1] then
      Result := r[1, 1] - r[0, 1]
  else
      Result := r[0, 1] - r[1, 1];
end;

function RectWidth(const r: TRect): TGeoInt;
begin
  if r.Right > r.Left then
      Result := r.Right - r.Left
  else
      Result := r.Left - r.Right;
end;

function RectHeight(const r: TRect): TGeoInt;
begin
  if r.Bottom > r.Top then
      Result := r.Bottom - r.Top
  else
      Result := r.Top - r.Bottom;
end;

function RectWidth(const r: TRectf): TGeoFloat;
begin
  if r.Right > r.Left then
      Result := r.Right - r.Left
  else
      Result := r.Left - r.Right;
end;

function RectHeight(const r: TRectf): TGeoFloat;
begin
  if r.Bottom > r.Top then
      Result := r.Bottom - r.Top
  else
      Result := r.Top - r.Bottom;
end;

function RoundWidth(const r: TRectV2): TGeoInt;
begin
  if r[1, 0] > r[0, 0] then
      Result := Round(r[1, 0] - r[0, 0])
  else
      Result := Round(r[0, 0] - r[1, 0]);
end;

function RoundHeight(const r: TRectV2): TGeoInt;
begin
  if r[1, 1] > r[0, 1] then
      Result := Round(r[1, 1] - r[0, 1])
  else
      Result := Round(r[0, 1] - r[1, 1]);
end;

function RoundWidth(const r: TRect): TGeoInt;
begin
  if r.Right > r.Left then
      Result := r.Right - r.Left
  else
      Result := r.Left - r.Right;
end;

function RoundHeight(const r: TRect): TGeoInt;
begin
  if r.Bottom > r.Top then
      Result := r.Bottom - r.Top
  else
      Result := r.Top - r.Bottom;
end;

function RoundWidth(const r: TRectf): TGeoInt;
begin
  if r.Right > r.Left then
      Result := Round(r.Right - r.Left)
  else
      Result := Round(r.Left - r.Right);
end;

function RoundHeight(const r: TRectf): TGeoInt;
begin
  if r.Bottom > r.Top then
      Result := Round(r.Bottom - r.Top)
  else
      Result := Round(r.Top - r.Bottom);
end;

function RectArea(const r: TRectV2): TGeoFloat;
begin
  Result := RectWidth(r) * RectHeight(r);
end;

function RectSize(const r: TRectV2): TVec2;
var
  n: TRectV2;
begin
  n := FixRect(r);
  Result := Vec2Sub(n[1], n[0]);
end;

function RectSizeR(const r: TRectV2): TRectV2;
begin
  Result[0] := ZeroVec2;
  Result[1] := RectSize(r)
end;

function RectFit(const sour, dest: TRectV2; const Bound: Boolean): TRectV2;
var
  k, kw, kh: TGeoFloat;
  rs, bs, siz, pt: TVec2;
begin
  rs := RectSize(sour);
  bs := RectSize(dest);

  kw := rs[0] / bs[0];
  kh := rs[1] / bs[1];

  if Bound then
      k := min(kw, kh)
  else
      k := max(kw, kh);

  siz := Vec2Div(rs, k);
  pt := Vec2Mul(Vec2Sub(bs, siz), 0.5);
  Result[0] := Vec2Add(dest[0], pt);
  Result[1] := Vec2Add(Result[0], siz);
end;

function RectFit(const sour, dest: TRectV2): TRectV2;
begin
  Result := RectFit(sour, dest, False);
end;

function RectFit(const width, height: TGeoFloat; const bk: TRectV2): TRectV2;
begin
  Result := RectFit(MakeRectV2(0, 0, width, height), bk);
end;

function FitRect(const sour, dest: TRectV2): TRectV2;
begin
  Result := RectFit(sour, dest);
end;

function FitRect(const width, height: TGeoFloat; const bk: TRectV2): TRectV2;
begin
  Result := RectFit(MakeRectV2(0, 0, width, height), bk);
end;

function BoundRect(const buff: TArrayPoint): TRect;
var
  t: TPoint;
  MaxX: TGeoInt;
  MaxY: TGeoInt;
  MinX: TGeoInt;
  MinY: TGeoInt;
  i: TGeoInt;
begin
  Result.Left := 0;
  Result.Top := 0;
  Result.Right := 0;
  Result.Bottom := 0;
  if length(buff) < 2 then
      exit;
  t := buff[0];
  MinX := t.X;
  MaxX := t.X;
  MinY := t.Y;
  MaxY := t.Y;

  for i := 1 to length(buff) - 1 do
    begin
      t := buff[i];
      if t.X < MinX then
          MinX := t.X
      else if t.X > MaxX then
          MaxX := t.X;
      if t.Y < MinY then
          MinY := t.Y
      else if t.Y > MaxY then
          MaxY := t.Y;
    end;
  Result.Left := MinX;
  Result.Top := MinY;
  Result.Right := MaxX;
  Result.Bottom := MaxY;
end;

function BoundRect(const p1, p2, p3: TPoint): TRect;
var
  buff: TArrayPoint;
begin
  SetLength(buff, 3);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  Result := BoundRect(buff);
end;

function BoundRect(const p1, p2, p3, p4: TPoint): TRect;
var
  buff: TArrayPoint;
begin
  SetLength(buff, 4);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  buff[3] := p4;
  Result := BoundRect(buff);
end;

function BoundRect(const r1, r2: TRect): TRect;
begin
  Result := BoundRect(r1.TopLeft, r1.BottomRight, r2.TopLeft, r2.BottomRight);
end;

function BoundRect(const buff: TArrayVec2): TRectV2;
var
  t: TVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: TGeoInt;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if length(buff) < 2 then
      exit;
  t := buff[0];
  MinX := t[0];
  MaxX := t[0];
  MinY := t[1];
  MaxY := t[1];

  for i := 1 to length(buff) - 1 do
    begin
      t := buff[i];
      if t[0] < MinX then
          MinX := t[0]
      else if t[0] > MaxX then
          MaxX := t[0];
      if t[1] < MinY then
          MinY := t[1]
      else if t[1] > MaxY then
          MaxY := t[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function BoundRect(const p1, p2, p3: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 3);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  Result := BoundRect(buff);
end;

function BoundRect(const p1, p2, p3, p4: TVec2): TRectV2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 4);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  buff[3] := p4;
  Result := BoundRect(buff);
end;

function BoundRect(const r1, r2: TRectV2): TRectV2;
begin
  Result := BoundRect(r1[0], r1[1], r2[0], r2[1]);
end;

function BuffCentroid(const buff: TArrayVec2): TVec2;
var
  i, Count: TGeoInt;
  asum: TGeoFloat;
  term: TGeoFloat;

  t1, t2: TVec2;
begin
  Result := NULLPoint;
  Count := length(buff);

  if Count < 3 then
      exit;

  asum := Zero;
  t2 := buff[Count - 1];

  for i := 0 to Count - 1 do
    begin
      t1 := buff[i];

      term := ((t2[0] * t1[1]) - (t2[1] * t1[0]));
      asum := asum + term;
      Result[0] := Result[0] + (t2[0] + t1[0]) * term;
      Result[1] := Result[1] + (t2[1] + t1[1]) * term;
      t2 := t1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function BuffCentroid(const p1, p2, p3, p4: TVec2): TVec2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 4);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  buff[3] := p4;
  Result := BuffCentroid(buff);
end;

function BuffCentroid(const p1, p2, p3: TVec2): TVec2;
var
  buff: TArrayVec2;
begin
  SetLength(buff, 3);
  buff[0] := p1;
  buff[1] := p2;
  buff[2] := p3;
  Result := BuffCentroid(buff);
end;

function PointInPolygon(pt: TVec2; const PolygonBuff: TArrayVec2): Boolean;
var
  l, i: TGeoInt;
  pi, pj: TVec2;
begin
  Result := False;
  l := length(PolygonBuff);
  if l < 3 then
      exit;
  pj := PolygonBuff[l - 1];
  for i := 0 to l - 1 do
    begin
      pi := PolygonBuff[i];
      (* upward crossing and downward crossing *)
      if ((pi[1] <= pt[1]) and (pt[1] < pj[1])) or ((pj[1] <= pt[1]) and (pt[1] < pi[1])) then
        (* compute the edge-ray intersect @ the x-coordinate *)
        if (pt[0] - pi[0] < ((pj[0] - pi[0]) * (pt[1] - pi[1]) / (pj[1] - pi[1]))) then
            Result := not Result;
      pj := pi;
    end;
end;

function FastRamerDouglasPeucker(var Points: TArrayVec2; Epsilon_: TGeoFloat): TGeoInt;
var
  i: TGeoInt;
  Range: array of TGeoInt;
  FirstIndex: TGeoInt;
  LastIndex: TGeoInt;
  LastPoint: TVec2;
  FirstLastDelta: TVec2;
  DeltaMaxIndex: TGeoInt;
  Delta: TGeoFloat;
  DeltaMax: TGeoFloat;
begin
  Result := length(Points);
  if Result < 3 then
      exit;
  FirstIndex := 0;
  LastIndex := Result - 1;
  SetLength(Range, Result);
  Range[0] := LastIndex;
  Range[LastIndex] := -1;
  Result := 0;

  repeat
    if LastIndex - FirstIndex > 1 then
      begin
        // find the point with the maximum distance
        DeltaMax := 0;
        DeltaMaxIndex := 0;
        LastPoint := Points[LastIndex];
        FirstLastDelta := Vec2Sub(Points[FirstIndex], LastPoint);
        for i := FirstIndex + 1 to LastIndex - 1 do
          begin
            Delta := FAbs((Points[i, 0] - LastPoint[0]) * FirstLastDelta[1] - (Points[i, 1] - LastPoint[1]) * FirstLastDelta[0]);
            if Delta > DeltaMax then
              begin
                DeltaMaxIndex := i;
                DeltaMax := Delta;
              end;
          end;

        // if max distance is greater than Epsilon_, split ranges
        if DeltaMax >= Epsilon_ * HypotX(FirstLastDelta[0], FirstLastDelta[1]) then
          begin
            Range[FirstIndex] := DeltaMaxIndex;
            Range[DeltaMaxIndex] := LastIndex;
            LastIndex := DeltaMaxIndex;
            Continue;
          end;
      end;

    // Include First and Last points only
    if Result <> FirstIndex then
        Points[Result] := Points[FirstIndex];
    inc(Result);
    if Result <> LastIndex then
        Points[Result] := Points[LastIndex];

    // Next range
    FirstIndex := Range[FirstIndex];
    LastIndex := Range[FirstIndex];

  until LastIndex < 0;
  inc(Result);
end;

procedure FastVertexReduction(Points: TArrayVec2; Epsilon_: TGeoFloat; var output: TArrayVec2);

  procedure FilterPoints;
  var
    index: TGeoInt;
    Count: TGeoInt;
    SqrEpsilon: TGeoFloat;
  begin
    SqrEpsilon := Sqr(Epsilon_);
    output := Points;
    Count := 1;
    for index := 1 to high(output) do
      begin
        if SqrDistance(output[Count - 1], Points[index]) > SqrEpsilon then
          begin
            if Count <> index then
                output[Count] := Points[index];
            inc(Count);
          end;
      end;
    SetLength(output, Count);
  end;

var
  Count: TGeoInt;
begin
  FilterPoints;
  Count := FastRamerDouglasPeucker(output, Epsilon_);
  SetLength(output, Count);
end;

function Clip(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out Cx1, Cy1, Cx2, Cy2: TGeoFloat): Boolean;
begin
  if RectToRectIntersect(x1, y1, x2, y2, x3, y3, x4, y4) then
    begin
      Result := True;
      if x1 < x3 then
          Cx1 := x3
      else
          Cx1 := x1;

      if x2 > x4 then
          Cx2 := x4
      else
          Cx2 := x2;

      if y1 < y3 then
          Cy1 := y3
      else
          Cy1 := y1;

      if y2 > y4 then
          Cy2 := y4
      else
          Cy2 := y2;
    end
  else
      Result := False;
end;

procedure Clip(const sour_, background_: TRectV2; var output: TRectV2);
var
  sour, background: TRectV2;
begin
  sour := ForwardRect(sour_);
  background := ForwardRect(background_);

  if RectInRect(sour, background) then
      output := sour
  else if not Clip(sour[0, 0], sour[0, 1], sour[1, 0], sour[1, 1],
    background[0, 0], background[0, 1], background[1, 0], background[1, 1],
    output[0, 0], output[0, 1], output[1, 0], output[1, 1]) then
      output := background;
end;

function Clip(const sour, background: TRectV2): TRectV2;
begin
  Clip(sour, background, Result);
end;

function Orientation(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoInt;
var
  Orin: TGeoFloat;
begin
  (* Determinant of the 3 points *)
  Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);

  if Orin > Zero then
      Result := LeftHandSide (* Orientaion is to the left-hand side *)
  else if Orin < Zero then
      Result := RightHandSide (* Orientaion is to the right-hand side *)
  else
      Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
end;

function Orientation(const x1, y1, z1, x2, y2, z2, x3, y3, z3, Px, Py, Pz: TGeoFloat): TGeoInt;
var
  Px1: TGeoFloat;
  Px2: TGeoFloat;
  Px3: TGeoFloat;
  Py1: TGeoFloat;
  Py2: TGeoFloat;
  Py3: TGeoFloat;
  Pz1: TGeoFloat;
  Pz2: TGeoFloat;
  Pz3: TGeoFloat;
  Orin: TGeoFloat;
begin
  Px1 := x1 - Px;
  Px2 := x2 - Px;
  Px3 := x3 - Px;

  Py1 := y1 - Py;
  Py2 := y2 - Py;
  Py3 := y3 - Py;

  Pz1 := z1 - Pz;
  Pz2 := z2 - Pz;
  Pz3 := z3 - Pz;

  Orin := Px1 * (Py2 * Pz3 - Pz2 * Py3) +
    Px2 * (Py3 * Pz1 - Pz3 * Py1) +
    Px3 * (Py1 * Pz2 - Pz1 * Py2);

  if Orin < Zero then
      Result := BelowOrientation (* Orientaion is below plane *)
  else if Orin > Zero then
      Result := AboveOrientation (* Orientaion is above plane *)
  else
      Result := CoplanarOrientation; (* Orientaion is coplanar to plane if Result is 0 *)
end;

function Coplanar(const x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4: TGeoFloat): Boolean;
begin
  Result := (Orientation(x1, y1, z1, x2, y2, z2, x3, y3, z3, x4, y4, z4) = CoplanarOrientation);
end;

function SimpleIntersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
begin
  Result := (
    ((Orientation(x1, y1, x2, y2, x3, y3) * Orientation(x1, y1, x2, y2, x4, y4)) <= 0) and
    ((Orientation(x3, y3, x4, y4, x1, y1) * Orientation(x3, y3, x4, y4, x2, y2)) <= 0)
    );
end;

function SimpleIntersect(const Point1, Point2, Point3, Point4: TVec2): Boolean;
begin
  Result := SimpleIntersect(Point1[0], Point1[1], Point2[0], Point2[1], Point3[0], Point3[1], Point4[0], Point4[1]);
end;

function SimpleIntersect(const l1, l2: TLineV2): Boolean;
begin
  Result := SimpleIntersect(
    l1[0, 0], l1[0, 1], l1[1, 0], l1[1, 1],
    l2[0, 0], l2[0, 1], l2[1, 0], l2[1, 1]);
end;

function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat): Boolean;
var
  UpperX: TGeoFloat;
  UpperY: TGeoFloat;
  LowerX: TGeoFloat;
  LowerY: TGeoFloat;
  Ax: TGeoFloat;
  Bx: TGeoFloat;
  Cx: TGeoFloat;
  Ay: TGeoFloat;
  By: TGeoFloat;
  Cy: TGeoFloat;
  d: TGeoFloat;
  f: TGeoFloat;
  E: TGeoFloat;
begin
  Result := False;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
    begin
      LowerX := x2;
      UpperX := x1;
    end
  else
    begin
      UpperX := x2;
      LowerX := x1;
    end;

  if Bx > Zero then
    begin
      if (UpperX < x4) or (x3 < LowerX) then
          exit;
    end
  else if (UpperX < x3) or (x4 < LowerX) then
      exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
    begin
      LowerY := y2;
      UpperY := y1;
    end
  else
    begin
      UpperY := y2;
      LowerY := y1;
    end;

  if By > Zero then
    begin
      if (UpperY < y4) or (y3 < LowerY) then
          exit;
    end
  else if (UpperY < y3) or (y4 < LowerY) then
      exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d := (By * Cx) - (Bx * Cy);
  f := (Ay * Bx) - (Ax * By);

  if f > Zero then
    begin
      if (d < Zero) or (d > f) then
          exit;
    end
  else if (d > Zero) or (d < f) then
      exit;

  E := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
    begin
      if (E < Zero) or (E > f) then
          exit;
    end
  else if (E > Zero) or (E < f) then
      exit;

  Result := True;
end;

function Intersect(const x1, y1, x2, y2, x3, y3, x4, y4: TGeoFloat; out ix, iy: TGeoFloat): Boolean;
var
  UpperX: TGeoFloat;
  UpperY: TGeoFloat;
  LowerX: TGeoFloat;
  LowerY: TGeoFloat;
  Ax: TGeoFloat;
  Bx: TGeoFloat;
  Cx: TGeoFloat;
  Ay: TGeoFloat;
  By: TGeoFloat;
  Cy: TGeoFloat;
  d: TGeoFloat;
  f: TGeoFloat;
  E: TGeoFloat;
  Ratio: TGeoFloat;
begin
  Result := False;

  Ax := x2 - x1;
  Bx := x3 - x4;

  if Ax < Zero then
    begin
      LowerX := x2;
      UpperX := x1;
    end
  else
    begin
      UpperX := x2;
      LowerX := x1;
    end;

  if Bx > Zero then
    begin
      if (UpperX < x4) or (x3 < LowerX) then
          exit;
    end
  else if (UpperX < x3) or (x4 < LowerX) then
      exit;

  Ay := y2 - y1;
  By := y3 - y4;

  if Ay < Zero then
    begin
      LowerY := y2;
      UpperY := y1;
    end
  else
    begin
      UpperY := y2;
      LowerY := y1;
    end;

  if By > Zero then
    begin
      if (UpperY < y4) or (y3 < LowerY) then
          exit;
    end
  else if (UpperY < y3) or (y4 < LowerY) then
      exit;

  Cx := x1 - x3;
  Cy := y1 - y3;
  d := (By * Cx) - (Bx * Cy);
  f := (Ay * Bx) - (Ax * By);

  if f > Zero then
    begin
      if (d < Zero) or (d > f) then
          exit;
    end
  else if (d > Zero) or (d < f) then
      exit;

  E := (Ax * Cy) - (Ay * Cx);

  if f > Zero then
    begin
      if (E < Zero) or (E > f) then
          exit;
    end
  else if (E > Zero) or (E < f) then
      exit;

  Result := True;

  (*
    From IntersectionPoint Routine

    dx1 := x2 - x1; ->  Ax
    dx2 := x4 - x3; -> -Bx
    dx3 := x1 - x3; ->  Cx

    dy1 := y2 - y1; ->  Ay
    dy2 := y1 - y3; ->  Cy
    dy3 := y4 - y3; -> -By
  *)

  Ratio := (Ax * -By) - (Ay * -Bx);

  if NotEqual(Ratio, Zero) then
    begin
      Ratio := ((Cy * -Bx) - (Cx * -By)) / Ratio;
      ix := x1 + (Ratio * Ax);
      iy := y1 + (Ratio * Ay);
    end
  else
    begin
      if IsEqual((Ax * -Cy), (-Cx * Ay)) then
        begin
          ix := x3;
          iy := y3;
        end
      else
        begin
          ix := x4;
          iy := y4;
        end;
    end;
end;

function Intersect(const pt1, pt2, pt3, pt4: TVec2; out pt: TVec2): Boolean;
begin
  Result := Intersect(pt1[0], pt1[1], pt2[0], pt2[1], pt3[0], pt3[1], pt4[0], pt4[1], pt[0], pt[1]);
end;

function Intersect(const l1, l2: TLineV2; out pt: TVec2): Boolean;
begin
  Result := Intersect(
    l1[0, 0], l1[0, 1], l1[1, 0], l1[1, 1],
    l2[0, 0], l2[0, 1], l2[1, 0], l2[1, 1],
    pt[0], pt[1]);
end;

function Intersect(const pt1, pt2, pt3, pt4: TVec2): Boolean;
begin
  Result := Intersect(pt1[0], pt1[1], pt2[0], pt2[1], pt3[0], pt3[1], pt4[0], pt4[1]);
end;

function PointInCircle(const pt, cp: TVec2; radius: TGeoFloat): Boolean;
begin
  Result := (PointLayDistance(pt, cp) <= (radius * radius));
end;

function PointInTriangle(const Px, Py, x1, y1, x2, y2, x3, y3: TGeoFloat): Boolean;
var
  Or1, Or2, Or3: TGeoInt;
begin
  Or1 := Orientation(x1, y1, x2, y2, Px, Py);
  Or2 := Orientation(x2, y2, x3, y3, Px, Py);

  if (Or1 * Or2) = -1 then
      Result := False
  else
    begin
      Or3 := Orientation(x3, y3, x1, y1, Px, Py);
      if (Or1 = Or3) or (Or3 = 0) then
          Result := True
      else if Or1 = 0 then
          Result := (Or2 * Or3) >= 0
      else if Or2 = 0 then
          Result := (Or1 * Or3) >= 0
      else
          Result := False;
    end;
end;

procedure BuildSinCosCache(const oSin, oCos: PGeoFloatArray; const b, E: TGeoFloat);
var
  i: TGeoInt;
  startAngle, stopAngle, d, alpha, beta: TGeoFloat;
begin
  startAngle := b;
  stopAngle := E + 1E-5;
  if high(oSin^) > low(oSin^) then
      d := PIDiv180 * (stopAngle - startAngle) / (high(oSin^) - low(oSin^))
  else
      d := 0;

  if high(oSin^) - low(oSin^) < 1000 then
    begin
      // Fast computation (approx 5.5x)
      alpha := 2 * Sqr(Sin(d * 0.5));
      beta := Sin(d);
      SinCos(startAngle * PIDiv180, oSin^[low(oSin^)], oCos^[low(oSin^)]);
      for i := low(oSin^) to high(oSin^) - 1 do
        begin
          // Make use of the incremental formulae:
          // cos (theta+delta) = cos(theta) - [alpha*cos(theta) + beta*sin(theta)]
          // sin (theta+delta) = sin(theta) - [alpha*sin(theta) - beta*cos(theta)]
          oCos^[i + 1] := oCos^[i] - alpha * oCos^[i] - beta * oSin^[i];
          oSin^[i + 1] := oSin^[i] - alpha * oSin^[i] + beta * oCos^[i];
        end;
    end
  else
    begin
      // Slower, but maintains precision when steps are small
      startAngle := startAngle * PIDiv180;
      for i := low(oSin^) to high(oSin^) do
          SinCos((i - low(oSin^)) * d + startAngle, oSin^[i], oCos^[i]);
    end;
end;

procedure ClosestPointOnSegmentFromPoint(const x1, y1, x2, y2, Px, Py: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  Vx: TGeoFloat;
  Vy: TGeoFloat;
  Wx: TGeoFloat;
  Wy: TGeoFloat;
  c1: TGeoFloat;
  c2: TGeoFloat;
  Ratio: TGeoFloat;
begin
  Vx := x2 - x1;
  Vy := y2 - y1;
  Wx := Px - x1;
  Wy := Py - y1;

  c1 := Vx * Wx + Vy * Wy;

  if c1 <= 0.0 then
    begin
      Nx := x1;
      Ny := y1;
      exit;
    end;

  c2 := Vx * Vx + Vy * Vy;

  if c2 <= c1 then
    begin
      Nx := x2;
      Ny := y2;
      exit;
    end;

  Ratio := c1 / c2;

  Nx := x1 + Ratio * Vx;
  Ny := y1 + Ratio * Vy;
end;

function ClosestPointOnSegmentFromPoint(const lb, le, pt: TVec2): TVec2;
begin
  ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], pt[0], pt[1], Result[0], Result[1]);
end;

function ClosestPointOnSegmentFromLine(const l: TLineV2; const pt: TVec2): TVec2;
begin
  ClosestPointOnSegmentFromPoint(l[0, 0], l[0, 1], l[1, 0], l[1, 1], pt[0], pt[1], Result[0], Result[1]);
end;

function ClosestPointOnSegmentFromLine(const pt: TVec2; const l: TLineV2): TVec2;
begin
  ClosestPointOnSegmentFromPoint(l[0, 0], l[0, 1], l[1, 0], l[1, 1], pt[0], pt[1], Result[0], Result[1]);
end;

function MinimumDistanceFromPointToLine(const Px, Py, x1, y1, x2, y2: TGeoFloat): TGeoFloat;
var
  Nx: TGeoFloat;
  Ny: TGeoFloat;
begin
  ClosestPointOnSegmentFromPoint(x1, y1, x2, y2, Px, Py, Nx, Ny);
  Result := Distance(Px, Py, Nx, Ny);
end;

function MinimumDistanceFromPointToLine(const pt: TVec2; const l: TLineV2): TGeoFloat;
begin
  Result := MinimumDistanceFromPointToLine(pt[0], pt[1], l[0, 0], l[0, 1], l[1, 0], l[1, 1]);
end;

function MinimumDistanceFromPointToLine(const l: TLineV2; const pt: TVec2): TGeoFloat;
begin
  Result := MinimumDistanceFromPointToLine(pt[0], pt[1], l[0, 0], l[0, 1], l[1, 0], l[1, 1]);
end;

function MinimumDistanceFromPointToLine(const lb, le, pt: TVec2): TGeoFloat;
begin
  Result := MinimumDistanceFromPointToLine(LineV2(lb, le), pt);
end;

function RectProjection(const sour, dest: TRectV2; const sour_pt: TVec2): TVec2;
var
  s, d: TRectV2;
begin
  s := ForwardRect(sour);
  d := ForwardRect(dest);
  Result := Vec2Add(Vec2Mul(Vec2Sub(sour_pt, s[0]), Vec2Div(RectSize(dest), RectSize(sour))), d[0]);
end;

function RectProjection(const sour, dest: TRectV2; const sour_rect: TRectV2): TRectV2;
var
  s, d: TRectV2;
begin
  s := ForwardRect(sour);
  d := ForwardRect(dest);
  Result := RectAdd(RectMul(RectSub(sour_rect, s[0]), Vec2Div(RectSize(dest), RectSize(sour))), d[0]);
end;

function RectProjectionRotationDest(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2;
var
  tmp: TVec2;
begin
  tmp := RectProjection(sour, dest, sour_pt);
  Result := Vec2Rotation(axis, tmp, NormalizeDegAngle(Vec2Angle(axis, tmp) + Angle));
end;

function RectProjectionRotationDest(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectProjectionRotationDest(sour, dest, axis, Angle, sour_rect[0]);
  Result[1] := RectProjectionRotationDest(sour, dest, axis, Angle, sour_rect[1]);
end;

function RectProjectionRotationSource(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2;
begin
  Result := RectProjection(sour, dest, Vec2Rotation(sour, axis, Angle, sour_pt));
end;

function RectProjectionRotationSource(const sour, dest: TRectV2; const axis: TVec2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectProjectionRotationSource(sour, dest, axis, Angle, sour_rect[0]);
  Result[1] := RectProjectionRotationSource(sour, dest, axis, Angle, sour_rect[1]);
end;

function RectProjectionRotationDest(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2;
begin
  Result := RectProjectionRotationDest(sour, dest, RectCentre(dest), Angle, sour_pt);
end;

function RectProjectionRotationDest(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectProjectionRotationDest(sour, dest, Angle, sour_rect[0]);
  Result[1] := RectProjectionRotationDest(sour, dest, Angle, sour_rect[1]);
end;

function RectProjectionRotationSource(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_pt: TVec2): TVec2;
begin
  Result := RectProjectionRotationSource(sour, dest, RectCentre(sour), Angle, sour_pt);
end;

function RectProjectionRotationSource(const sour, dest: TRectV2; const Angle: TGeoFloat; const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectProjectionRotationSource(sour, dest, Angle, sour_rect[0]);
  Result[1] := RectProjectionRotationSource(sour, dest, Angle, sour_rect[1]);
end;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAxis, destAxis: TVec2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_pt: TVec2): TVec2;
begin
  Result := RectProjectionRotationDest(sour, dest, destAxis, destAngle, Vec2Rotation(sour, sourAxis, sourAngle, sour_pt));
end;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAxis, destAxis: TVec2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, sour_rect[0]);
  Result[1] := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, sour_rect[1]);
end;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_pt: TVec2): TVec2;
begin
  Result := RectRotationProjection(sour, dest, RectCentre(sour), RectCentre(dest), sourAngle, destAngle, sour_pt);
end;

function RectRotationProjection(
  const sour, dest: TRectV2;
  const sourAngle, destAngle: TGeoFloat;
  const sour_rect: TRectV2): TRectV2;
begin
  Result[0] := RectRotationProjection(sour, dest, sourAngle, destAngle, sour_rect[0]);
  Result[1] := RectRotationProjection(sour, dest, sourAngle, destAngle, sour_rect[1]);
end;

function Quadrant(const Angle: TGeoFloat): TGeoInt;
begin
  Result := 0;
  if (Angle >= 0.0) and (Angle < 90.0) then
      Result := 1
  else if (Angle >= 90.0) and (Angle < 180.0) then
      Result := 2
  else if (Angle >= 180.0) and (Angle < 270.0) then
      Result := 3
  else if (Angle >= 270.0) and (Angle < 360.0) then
      Result := 4
  else if Angle = 360.0 then
      Result := 1;
end;

procedure ProjectionPoint(const Srcx, Srcy, Dstx, Dsty, Dist: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  DistRatio: TGeoFloat;
begin
  DistRatio := Dist / Distance(Srcx, Srcy, Dstx, Dsty);
  Nx := Srcx + DistRatio * (Dstx - Srcx);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
end;

procedure ProjectionPoint(const Srcx, Srcy, Srcz, Dstx, Dsty, Dstz, Dist: TGeoFloat; out Nx, Ny, Nz: TGeoFloat);
var
  DistRatio: TGeoFloat;
begin
  DistRatio := Dist / Distance(Srcx, Srcy, Srcz, Dstx, Dsty, Dstz);
  Nx := Srcx + DistRatio * (Dstx - Srcx);
  Ny := Srcy + DistRatio * (Dsty - Srcy);
  Nz := Srcz + DistRatio * (Dstz - Srcz);
end;
(* End of Project Point 3D *)

procedure ProjectionPoint(const Px, Py, Angle, Distance: TGeoFloat; out Nx, Ny: TGeoFloat);
var
  dx: TGeoFloat;
  dy: TGeoFloat;
begin
  dx := Zero;
  dy := Zero;
  case Quadrant(Angle) of
    1:
      begin
        dx := Cos(Angle * PIDiv180) * Distance;
        dy := Sin(Angle * PIDiv180) * Distance;
      end;
    2:
      begin
        dx := Sin((Angle - 90.0) * PIDiv180) * Distance * -1.0;
        dy := Cos((Angle - 90.0) * PIDiv180) * Distance;
      end;
    3:
      begin
        dx := Cos((Angle - 180.0) * PIDiv180) * Distance * -1.0;
        dy := Sin((Angle - 180.0) * PIDiv180) * Distance * -1.0;
      end;
    4:
      begin
        dx := Sin((Angle - 270.0) * PIDiv180) * Distance;
        dy := Cos((Angle - 270.0) * PIDiv180) * Distance * -1.0;
      end;
  end;
  Nx := Px + dx;
  Ny := Py + dy;
end;

function GetCicleRadiusInPolyEdge(r: TGeoFloat; PolySlices: TGeoInt): TGeoFloat;
begin
  Result := r / Sin((180 - 360 / PolySlices) * 0.5 / 180 * pi);
end;

procedure Circle2LineIntersectionPoint(const lb, le, cp: TVec2; const radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: TGeoInt; out pt1, pt2: TVec2);
var
  Px: TGeoFloat;
  Py: TGeoFloat;
  S1In: Boolean;
  s2In: Boolean;
  h: TGeoFloat;
  a: TGeoFloat;
begin
  ICnt := 0;

  S1In := PointInCircle(lb, cp, radius);
  s2In := PointInCircle(le, cp, radius);

  if S1In and s2In then
    begin
      ICnt := 2;
      pt1 := lb;
      pt2 := le;
      pt1in := True;
      pt2in := True;
      exit;
    end;

  if S1In or s2In then
    begin
      pt1in := True;
      pt2in := False;
      ICnt := 2;
      ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], cp[0], cp[1], Px, Py);
      if S1In then
        begin
          h := Distance(Px, Py, cp[0], cp[1]);
          a := Sqrt((radius * radius) - (h * h));
          pt1 := lb;
          ProjectionPoint(Px, Py, le[0], le[1], a, pt2[0], pt2[1]);
        end
      else if s2In then
        begin
          h := Distance(Px, Py, cp[0], cp[1]);
          a := Sqrt((radius * radius) - (h * h));
          pt1 := le;
          ProjectionPoint(Px, Py, lb[0], lb[1], a, pt2[0], pt2[1]);
        end;
      exit;
    end;

  pt1in := False;
  pt2in := False;

  ClosestPointOnSegmentFromPoint(lb[0], lb[1], le[0], le[1], cp[0], cp[1], Px, Py);

  if (IsEqual(lb[0], Px) and IsEqual(lb[1], Py)) or (IsEqual(le[0], Px) and IsEqual(le[1], Py)) then
      exit
  else
    begin
      h := Distance(Px, Py, cp[0], cp[1]);
      if h > radius then
          exit
      else if IsEqual(h, radius) then
        begin
          ICnt := 1;
          pt1[0] := Px;
          pt1[1] := Py;
          exit;
        end
      else if IsEqual(h, Zero) then
        begin
          ICnt := 2;
          ProjectionPoint(cp[0], cp[1], lb[0], lb[1], radius, pt1[0], pt1[1]);
          ProjectionPoint(cp[0], cp[1], le[0], le[1], radius, pt2[0], pt2[1]);
          exit;
        end
      else
        begin
          ICnt := 2;
          a := Sqrt((radius * radius) - (h * h));
          ProjectionPoint(Px, Py, lb[0], lb[1], a, pt1[0], pt1[1]);
          ProjectionPoint(Px, Py, le[0], le[1], a, pt2[0], pt2[1]);
          exit;
        end;
    end;
end;

procedure Circle2LineIntersectionPoint(const l: TLineV2; const cp: TVec2; radius: TGeoFloat;
  out pt1in, pt2in: Boolean; out ICnt: TGeoInt; out pt1, pt2: TVec2);
begin
  Circle2LineIntersectionPoint(l[0], l[1], cp, radius, pt1in, pt2in, ICnt, pt1, pt2);
end;

procedure Circle2CircleIntersectionPoint(const cp1, cp2: TVec2; const r1, r2: TGeoFloat; out Point1, Point2: TVec2);
var
  Dist: TGeoFloat;
  a: TGeoFloat;
  h: TGeoFloat;
  RatioA: TGeoFloat;
  RatioH: TGeoFloat;
  dx: TGeoFloat;
  dy: TGeoFloat;
  Phi: TVec2;
  r1Sqr: TGeoFloat;
  r2Sqr: TGeoFloat;
  dstSqr: TGeoFloat;
begin
  Dist := Distance(cp1[0], cp1[1], cp2[0], cp2[1]);

  dstSqr := Dist * Dist;
  r1Sqr := r1 * r1;
  r2Sqr := r2 * r2;

  a := (dstSqr - r2Sqr + r1Sqr) / (2 * Dist);
  h := Sqrt(r1Sqr - (a * a));

  RatioA := a / Dist;
  RatioH := h / Dist;

  dx := cp2[0] - cp1[0];
  dy := cp2[1] - cp1[1];

  Phi[0] := cp1[0] + (RatioA * dx);
  Phi[1] := cp1[1] + (RatioA * dy);

  dx := dx * RatioH;
  dy := dy * RatioH;

  Point1[0] := Phi[0] + dy;
  Point1[1] := Phi[1] - dx;

  Point2[0] := Phi[0] - dy;
  Point2[1] := Phi[1] + dx;
end;

function Detect_Circle2Circle(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  // return point disace < sum
  Result := PointDistance(p1, p2) <= r1 + r2;
end;

function CircleCollision(const p1, p2: TVec2; const r1, r2: TGeoFloat): Boolean;
begin
  // return point disace < sum
  Result := PointDistance(p1, p2) <= r1 + r2;
end;

function Detect_Circle2CirclePoint(const p1, p2: TVec2; const r1, r2: TGeoFloat; out op1, op2: TVec2): Boolean;
var
  Dist: TGeoFloat;
  a: TGeoFloat;
  h: TGeoFloat;
  RatioA: TGeoFloat;
  RatioH: TGeoFloat;
  dx: TGeoFloat;
  dy: TGeoFloat;
  Phi: TVec2;
  r1Sqr: TGeoFloat;
  r2Sqr: TGeoFloat;
  dstSqr: TGeoFloat;
begin
  Dist := Distance(p1[0], p1[1], p2[0], p2[1]);
  Result := Dist <= r1 + r2;
  if Result then
    begin
      dstSqr := Dist * Dist;
      r1Sqr := r1 * r1;
      r2Sqr := r2 * r2;

      a := (dstSqr - r2Sqr + r1Sqr) / (2 * Dist);
      h := Sqrt(r1Sqr - (a * a));

      RatioA := a / Dist;
      RatioH := h / Dist;

      dx := p2[0] - p1[0];
      dy := p2[1] - p1[1];

      Phi[0] := p1[0] + (RatioA * dx);
      Phi[1] := p1[1] + (RatioA * dy);

      dx := dx * RatioH;
      dy := dy * RatioH;

      op1[0] := Phi[0] + dy;
      op1[1] := Phi[1] - dx;

      op2[0] := Phi[0] - dy;
      op2[1] := Phi[1] + dx;
    end;
end;

// circle 2 line collision

function Detect_Circle2Line(const cp: TVec2; const r: TGeoFloat; const lb, le: TVec2): Boolean;
var
  lineCen, v1, v2: TVec2;
begin
  lineCen := PointLerp(lb, le, 0.5);
  if Detect_Circle2Circle(cp, lineCen, r, PointDistance(lb, le) * 0.5) then
    begin
      v1 := Vec2Sub(lb, cp);
      v2 := Vec2Sub(le, cp);
      Result := GreaterThanOrEqual(((r * r) * PointLayDistance(v1, v2) - Sqr(v1[0] * v2[1] - v1[1] * v2[0])), Zero);
    end
  else
      Result := False;
end;

function Detect_Circle2Line(const cp: TVec2; const r: TGeoFloat; const l: TLineV2): Boolean;
begin
  Result := Detect_Circle2Line(cp, r, l[0], l[1]);
end;

function SameLinePtr(const lb1, le1, lb2, le2: PVec2): Boolean;
begin
  Result := ((lb1 = lb2) and (le1 = le2)) or ((lb1 = le2) and (le1 = lb2));
end;

function ComputeCurvePartPrecision(const pt1, pt2, pt3, pt4: TVec2): TGeoInt;
const
  AcceptedDeviation = 0.1;
var
  len: TGeoFloat;
begin
  len := Sqr(pt1[0] - pt2[0]) + Sqr(pt1[1] - pt2[1]);
  len := max(len, Sqr(pt3[0] - pt2[0]) + Sqr(pt3[1] - pt2[1]));
  len := max(len, Sqr(pt3[0] - pt4[0]) + Sqr(pt3[1] - pt4[1]));
  Result := Round(Sqrt(Sqrt(len) / AcceptedDeviation) * 0.9);
  if Result <= 0 then
      Result := 1;
end;

function Interpolation_OutSide(const T_: TGeoFloat): TGeoFloat;
const
  Coeff = 0.5;
var
  t, tt, ttt: TGeoFloat;
begin
  t := abs(T_);
  tt := Sqr(t);
  ttt := tt * t;
  if t < 1 then
      Result := (2 - Coeff) * ttt - (3 - Coeff) * tt + 1
  else if t < 2 then
      Result := -Coeff * (ttt - 5 * tt + 8 * t - 4)
  else
      Result := 0;
end;

function Interpolation_InSide(const t: TGeoFloat): TGeoFloat;
  function pow3(X: TGeoFloat): TGeoFloat; inline;
  begin
    if X <= 0.0 then
        Result := 0.0
    else
        Result := X * X * X;
  end;

const
  globalfactor = 1 / 6;

begin
  if t > 2 then
      Result := 0
  else
      Result := globalfactor * (pow3(t + 2) - 4 * pow3(t + 1) + 6 * pow3(t) - 4 * pow3(t - 1));
end;

function TVec2List.GetPoints(index: TGeoInt): PVec2;
begin
  Result := FList[index];
end;

constructor TVec2List.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  FUserData := nil;
  FUserObject := nil;
end;

destructor TVec2List.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TVec2List.AddRandom;
begin
  Add(umlRandomRangeS(-10000, 10000), umlRandomRangeS(-10000, 10000));
end;

procedure TVec2List.AddRandom(rnd: TMT19937Random);
begin
  Add(umlRandomRangeS(rnd, -10000, 10000), umlRandomRangeS(rnd, -10000, 10000));
end;

procedure TVec2List.Add(const X, Y: TGeoFloat);
var
  p: PVec2;
begin
  new(p);
  p^ := PointMake(X, Y);
  FList.Add(p);
end;

procedure TVec2List.Add(const pt: TVec2);
var
  p: PVec2;
begin
  new(p);
  p^ := pt;
  FList.Add(p);
end;

procedure TVec2List.Add(pt: TPoint);
var
  p: PVec2;
begin
  new(p);
  p^ := vec2(pt);
  FList.Add(p);
end;

procedure TVec2List.Add(pt: TPointf);
var
  p: PVec2;
begin
  new(p);
  p^ := vec2(pt);
  FList.Add(p);
end;

procedure TVec2List.Add(v2l: TVec2List);
var
  i: TGeoInt;
begin
  for i := 0 to v2l.Count - 1 do
      Add(v2l[i]^);
end;

procedure TVec2List.Add(r: TRectV2);
begin
  Add(r[0, 0], r[0, 1]);
  Add(r[1, 0], r[0, 1]);
  Add(r[1, 0], r[1, 1]);
  Add(r[0, 0], r[1, 1]);
end;

procedure TVec2List.Add(r: TRect);
begin
  Add(RectV2(r));
end;

procedure TVec2List.Add(r: TRectf);
begin
  Add(RectV2(r));
end;

procedure TVec2List.AddSubdivision(nbCount: TGeoInt; pt: TVec2);
var
  lpt: PVec2;
  i: TGeoInt;
  t: Double;
begin
  if Count > 0 then
    begin
      lpt := FList.Last;
      t := 1.0 / nbCount;
      for i := 1 to nbCount do
          Add(PointLerp(lpt^, pt, t * i));
    end
  else
      Add(pt);
end;

procedure TVec2List.AddSubdivisionWithDistance(avgDist: TGeoFloat; pt: TVec2);
var
  lpt: PVec2;
  i, nbCount: TGeoInt;
  t: Double;
begin
  if (Count > 0) and (PointDistance(PVec2(FList.Last)^, pt) > avgDist) then
    begin
      lpt := FList.Last;
      nbCount := Trunc(PointDistance(PVec2(FList.Last)^, pt) / avgDist);
      t := 1.0 / nbCount;
      for i := 1 to nbCount do
          Add(PointLerp(lpt^, pt, t * i));
    end;
  Add(pt);
end;

procedure TVec2List.AddCirclePoint(count_: Cardinal; axis: TVec2; dist_: TGeoFloat);
var
  i: TGeoInt;
begin
  for i := 0 to count_ - 1 do
      Add(PointRotation(axis, dist_, 360 / count_ * i));
end;

procedure TVec2List.AddRectangle(r: TRectV2);
begin
  Add(r[0, 0], r[0, 1]);
  Add(r[1, 0], r[0, 1]);
  Add(r[1, 0], r[1, 1]);
  Add(r[0, 0], r[1, 1]);
end;

procedure TVec2List.Insert(idx: TGeoInt; X, Y: TGeoFloat);
begin
  Insert(idx, vec2(X, Y));
end;

procedure TVec2List.Insert(idx: TGeoInt; pt: TVec2);
var
  p: PVec2;
begin
  new(p);
  p^ := pt;
  FList.Insert(idx, p);
end;

procedure TVec2List.Delete(idx: TGeoInt);
begin
  Dispose(PVec2(FList[idx]));
  FList.Delete(idx);
end;

function TVec2List.Remove(p: PVec2): TGeoInt;
var
  i: TGeoInt;
begin
  Result := 0;
  i := 0;
  while i < FList.Count do
    begin
      if FList[i] = p then
        begin
          Dispose(PVec2(FList[i]));
          FList.Delete(i);
          inc(Result);
        end
      else
          inc(i);
    end;
end;

procedure TVec2List.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PVec2(FList[i]));
  FList.Clear;
end;

function TVec2List.Count: TGeoInt;
begin
  Result := FList.Count;
end;

procedure TVec2List.RemoveSame;
var
  l, p: PVec2;
  i: TGeoInt;
begin
  if Count < 2 then
      exit;

  l := PVec2(FList[0]);
  p := PVec2(FList[Count - 1]);
  while (Count >= 2) and (IsEqual(p^, l^)) do
    begin
      Delete(Count - 1);
      p := PVec2(FList[Count - 1]);
    end;

  if Count < 2 then
      exit;

  l := PVec2(FList[0]);
  i := 1;
  while i < Count do
    begin
      p := PVec2(FList[i]);
      if IsEqual(p^, l^) then
          Delete(i)
      else
        begin
          l := p;
          inc(i);
        end;
    end;
end;

procedure TVec2List.SwapData(dest: TVec2List);
var
  l: TCoreClassList;
begin
  l := FList;
  FList := dest.FList;
  dest.FList := l;
end;

procedure TVec2List.MoveDataTo(dest: TVec2List);
var
  i: TGeoInt;
begin
  for i := 0 to FList.Count - 1 do
      dest.FList.Add(FList[i]);
  FList.Clear;
end;

procedure TVec2List.Assign(Source: TCoreClassObject);
var
  i: TGeoInt;
begin
  if Source is TVec2List then
    begin
      Clear;
      for i := 0 to TVec2List(Source).Count - 1 do
          Add(TVec2List(Source)[i]^);
    end
  else if Source is TDeflectionPolygon then
    begin
      Clear;
      for i := 0 to TDeflectionPolygon(Source).Count - 1 do
          Add(TDeflectionPolygon(Source).Points[i]);
    end;
end;

procedure TVec2List.AssignFromArrayV2(arry: TArrayVec2);
var
  i: TGeoInt;
begin
  Clear;
  for i := low(arry) to high(arry) do
      Add(arry[i]);
end;

function TVec2List.BuildArray: TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Points[i]^;
end;

function TVec2List.BuildSplineSmoothInSideClosedArray: TArrayVec2;
var
  nl: TVec2List;
begin
  nl := TVec2List.Create;
  SplineSmoothInSideClosed(nl);
  Result := nl.BuildArray;
  DisposeObject(nl);
end;

function TVec2List.BuildSplineSmoothOutSideClosedArray: TArrayVec2;
var
  nl: TVec2List;
begin
  nl := TVec2List.Create;
  SplineSmoothOutSideClosed(nl);
  Result := nl.BuildArray;
  DisposeObject(nl);
end;

function TVec2List.BuildSplineSmoothOpenedArray: TArrayVec2;
var
  nl: TVec2List;
begin
  nl := TVec2List.Create;
  SplineSmoothOpened(nl);
  Result := nl.BuildArray;
  DisposeObject(nl);
end;

function TVec2List.BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, Points[i]^);
end;

function TVec2List.BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectRotationProjection(sour, dest, sourAngle, destAngle, Points[i]^);
end;

function TVec2List.BuildProjectionArray(const sour, dest: TRectV2): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectProjection(sour, dest, Points[i]^);
end;

function TVec2List.BuildProjectionArray(const dest: TRectV2): TArrayVec2;
begin
  Result := BuildProjectionArray(BoundBox, dest);
end;

procedure TVec2List.ProjectionTo(const sour, dest: TRectV2; const output: TDeflectionPolygon);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.AddPoint(RectProjection(sour, dest, Points[i]^));
end;

procedure TVec2List.ProjectionTo(const dest: TRectV2; const output: TDeflectionPolygon);
begin
  ProjectionTo(BoundBox, dest, output);
end;

procedure TVec2List.ProjectionTo(const sour, dest: TRectV2; const output: TVec2List);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.Add(RectProjection(sour, dest, Points[i]^));
end;

procedure TVec2List.ProjectionTo(const dest: TRectV2; const output: TVec2List);
begin
  ProjectionTo(BoundBox, dest, output);
end;

procedure TVec2List.SaveToStream(stream: TMemoryStream64);
var
  i: TGeoInt;
  p: PVec2;
begin
  stream.WriteInt32(Count);
  for i := 0 to Count - 1 do
    begin
      p := GetPoints(i);
      stream.WriteSingle(p^[0]);
      stream.WriteSingle(p^[1]);
    end;
end;

procedure TVec2List.LoadFromStream(stream: TMemoryStream64);
var
  c: TGeoInt;
  i: TGeoInt;
  v: TVec2;
begin
  Clear;
  c := stream.ReadInt32;
  for i := 0 to c - 1 do
    begin
      v[0] := stream.ReadSingle;
      v[1] := stream.ReadSingle;
      Add(v);
    end;
end;

function TVec2List.BoundBox: TRectV2;
var
  p: PVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: TGeoInt;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if Count < 2 then
      exit;
  p := Points[0];
  MinX := p^[0];
  MaxX := p^[0];
  MinY := p^[1];
  MaxY := p^[1];

  for i := 1 to Count - 1 do
    begin
      p := Points[i];
      if p^[0] < MinX then
          MinX := p^[0]
      else if p^[0] > MaxX then
          MaxX := p^[0];
      if p^[1] < MinY then
          MinY := p^[1]
      else if p^[1] > MaxY then
          MaxY := p^[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function TVec2List.BoundCentre: TVec2;
begin
  Result := RectCentre(BoundBox);
end;

function TVec2List.CircleRadius(ACentroid: TVec2): TGeoFloat;
var
  i: TGeoInt;
  LayLen: TGeoFloat;
  LayDist: TGeoFloat;
begin
  Result := 0;
  if Count < 3 then
      exit;
  LayLen := -1;
  for i := 0 to Count - 1 do
    begin
      LayDist := PointLayDistance(ACentroid, Points[i]^);
      if LayDist > LayLen then
          LayLen := LayDist;
    end;
  Result := Sqrt(LayLen);
end;

function TVec2List.Centroid: TVec2;
var
  i: TGeoInt;
  asum: TGeoFloat;
  term: TGeoFloat;

  p1, p2: PVec2;
begin
  Result := NULLPoint;

  if Count = 2 then
    begin
      p1 := Points[0];
      p2 := Points[1];
      Result := MiddleVec2(p1^, p2^);
      exit;
    end;

  if Count < 3 then
      exit;

  asum := Zero;
  p2 := Points[Count - 1];

  for i := 0 to Count - 1 do
    begin
      p1 := Points[i];

      term := ((p2^[0] * p1^[1]) - (p2^[1] * p1^[0]));
      asum := asum + term;
      Result[0] := Result[0] + (p2^[0] + p1^[0]) * term;
      Result[1] := Result[1] + (p2^[1] + p1^[1]) * term;
      p2 := p1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function TVec2List.InHere(pt: TVec2): Boolean;
var
  i: TGeoInt;
  pi, pj: PVec2;
begin
  Result := False;
  if Count < 3 then
      exit;
  pj := Points[Count - 1];
  for i := 0 to Count - 1 do
    begin
      pi := Points[i];
      if ((pi^[1] <= pt[1]) and (pt[1] < pj^[1])) or  // an upward crossing
        ((pj^[1] <= pt[1]) and (pt[1] < pi^[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if (pt[0] - pi^[0] < ((pj^[0] - pi^[0]) * (pt[1] - pi^[1]) / (pj^[1] - pi^[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

function TVec2List.InRect(r: TRectV2): Boolean;
var
  i: TGeoInt;
begin
  Result := False;
  for i := 0 to Count - 1 do
      Result := Result or PointInRect(Points[i]^, r);
end;

function TVec2List.Rect2Intersect(r: TRectV2): Boolean;
var
  i: TGeoInt;
  r4: TV2Rect4;
begin
  Result := False;
  for i := 0 to Count - 1 do
      Result := Result or PointInRect(Points[i]^, r);

  if not Result then
    begin
      r4 := TV2Rect4.Init(r);
      Result := Result or Line2Intersect(r4.LeftTop, r4.RightTop, True);
      Result := Result or Line2Intersect(r4.RightTop, r4.RightBottom, True);
      Result := Result or Line2Intersect(r4.RightBottom, r4.LeftBottom, True);
      Result := Result or Line2Intersect(r4.LeftBottom, r4.LeftTop, True);
    end;
end;

procedure TVec2List.RotateAngle(axis: TVec2; Angle: TGeoFloat);
var
  i: TGeoInt;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Points[i];
      p^ := PointRotation(axis, p^, PointAngle(axis, p^) + Angle);
    end;
end;

procedure TVec2List.Scale(Scale_: TGeoFloat);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      PointScale(Points[i]^, Scale_);
end;

procedure TVec2List.ConvexHull(output: TVec2List);

const
  RightHandSide = -1;
  LeftHandSide = +1;
  CounterClockwise = +1;
  CollinearOrientation = 0;

type
  T2DHullPoint = record
    X: TGeoFloat;
    Y: TGeoFloat;
    Ang: TGeoFloat;
  end;

  TCompareResult = (eGreaterThan, eLessThan, eEqual);

var
  Point: array of T2DHullPoint;
  Stack: array of T2DHullPoint;
  StackHeadPosition: TGeoInt;
  Anchor: T2DHullPoint;

  function CartesianAngle(const X, Y: TGeoFloat): TGeoFloat;
  const
    _180DivPI = 57.295779513082320876798154814105000;
  begin
    if (X > Zero) and (Y > Zero) then
        Result := (ArcTan(Y / X) * _180DivPI)
    else if (X < Zero) and (Y > Zero) then
        Result := (ArcTan(-X / Y) * _180DivPI) + 90.0
    else if (X < Zero) and (Y < Zero) then
        Result := (ArcTan(Y / X) * _180DivPI) + 180.0
    else if (X > Zero) and (Y < Zero) then
        Result := (ArcTan(-X / Y) * _180DivPI) + 270.0
    else if (X = Zero) and (Y > Zero) then
        Result := 90.0
    else if (X < Zero) and (Y = Zero) then
        Result := 180.0
    else if (X = Zero) and (Y < Zero) then
        Result := 270.0
    else
        Result := Zero;
  end;

  procedure Swap(i, j: TGeoInt; var Point: array of T2DHullPoint);
  var
    Temp: T2DHullPoint;
  begin
    Temp := Point[i];
    Point[i] := Point[j];
    Point[j] := Temp;
  end;

  function hEqual(const p1, p2: T2DHullPoint): Boolean;
  begin
    Result := IsEqual(p1.X, p2.X) and IsEqual(p1.Y, p2.Y);
  end;

  function CompareAngles(const p1, p2: T2DHullPoint): TCompareResult;
  begin
    if p1.Ang < p2.Ang then
        Result := eLessThan
    else if p1.Ang > p2.Ang then
        Result := eGreaterThan
    else if hEqual(p1, p2) then
        Result := eEqual
    else if Distance(Anchor.X, Anchor.Y, p1.X, p1.Y) < Distance(Anchor.X, Anchor.Y, p2.X, p2.Y) then
        Result := eLessThan
    else
        Result := eGreaterThan;
  end;

  procedure RQSort(Left, Right: TGeoInt; var Point: array of T2DHullPoint);
  var
    i: TGeoInt;
    j: TGeoInt;
    Middle: TGeoInt;
    Pivot: T2DHullPoint;
  begin
    repeat
      i := Left;
      j := Right;
      Middle := (Left + Right) div 2;
      (* Median of 3 Pivot Selection *)
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      if CompareAngles(Point[Right], Point[Middle]) = eLessThan then
          Swap(Right, Middle, Point);
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      Pivot := Point[Right];
      repeat
        while CompareAngles(Point[i], Pivot) = eLessThan do
            inc(i);
        while CompareAngles(Point[j], Pivot) = eGreaterThan do
            dec(j);
        if i <= j then
          begin
            Swap(i, j, Point);
            inc(i);
            dec(j);
          end;
      until i > j;
      if Left < j then
          RQSort(Left, j, Point);
      Left := i;
    until i >= Right;
  end;

  procedure Push(Pnt: T2DHullPoint);
  begin
    inc(StackHeadPosition);
    Stack[StackHeadPosition] := Pnt;
  end;

  function Pop: Boolean;
  begin
    Result := False;
    if StackHeadPosition >= 0 then
      begin
        Result := True;
        dec(StackHeadPosition);
      end;
  end;

  function Head: T2DHullPoint;
  begin
    Assert((StackHeadPosition >= 0) and (StackHeadPosition < length(Stack)), 'Invalid stack-head position.');
    Result := Stack[StackHeadPosition];
  end;

  function PreHead: T2DHullPoint;
  begin
    Assert(((StackHeadPosition - 1) >= 0) and ((StackHeadPosition - 1) < length(Stack)), 'Invalid pre stack-head position.');
    Result := Stack[StackHeadPosition - 1];
  end;

  function PreHeadExist: Boolean;
  begin
    Result := (StackHeadPosition > 0);
  end;

  function Orientation(p1, p2, p3: T2DHullPoint): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Orientation2(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    var
      Orin: TGeoFloat;
    begin
      (* Determinant of the 3 points *)
      Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);
      if Orin > Zero then
          Result := LeftHandSide (* Orientaion is to the left-hand side *)
      else if Orin < Zero then
          Result := RightHandSide (* Orientaion is to the right-hand side *)
      else
          Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
    end;

  begin
    Result := Orientation2(p1.X, p1.Y, p2.X, p2.Y, p3.X, p3.Y);
  end;

  procedure GrahamScan;
  var
    i: TGeoInt;
    Orin: TGeoInt;
  begin
    Push(Point[0]);
    Push(Point[1]);
    i := 2;
    while i < length(Point) do
      begin
        if PreHeadExist then
          begin
            Orin := Orientation(PreHead, Head, Point[i]);
            if Orin = CounterClockwise then
              begin
                Push(Point[i]);
                inc(i);
              end
            else
                Pop;
          end
        else
          begin
            Push(Point[i]);
            inc(i);
          end;
      end;
  end;

var
  i: TGeoInt;
  j: TGeoInt;
  p: PVec2;
begin
  if Count <= 3 then
    begin
      for i := 0 to Count - 1 do
          output.Add(Points[i]^);
      exit;
    end;
  StackHeadPosition := -1;

  try
    SetLength(Point, Count);
    SetLength(Stack, Count);
    j := 0;
    for i := 0 to Count - 1 do
      begin
        p := Points[i];
        Point[i].X := p^[0];
        Point[i].Y := p^[1];
        Point[i].Ang := 0.0;
        if Point[i].Y < Point[j].Y then
            j := i
        else if Point[i].Y = Point[j].Y then
          if Point[i].X < Point[j].X then
              j := i;
      end;

    Swap(0, j, Point);
    Point[0].Ang := 0;
    Anchor := Point[0];
    (* Calculate angle of the vertex ([ith point]-[anchorpoint]-[most left point]) *)
    for i := 1 to length(Point) - 1 do
        Point[i].Ang := CartesianAngle(Point[i].X - Anchor.X, Point[i].Y - Anchor.Y);
    (* Sort points in ascending order according to their angles *)
    RQSort(1, length(Point) - 1, Point);
    GrahamScan;
    (* output list *)
    for i := 0 to StackHeadPosition do
        output.Add(Stack[i].X, Stack[i].Y);
  finally
    (* Final clean-up *)
    Finalize(Stack);
    Finalize(Point);
  end;
end;

procedure TVec2List.ConvexHull;
var
  nl: TVec2List;
begin
  nl := TVec2List.Create;
  ConvexHull(nl);
  SwapData(nl);
  DisposeObject(nl);
end;

procedure TVec2List.SplineSmoothInSideClosed(output: TVec2List);
var
  i, j, idx, pre: TGeoInt;
  ptPrev, ptPrev2, ptNext, ptNext2, v: TVec2;
  t: TGeoFloat;
begin
  if Count < 3 then
    begin
      output.Assign(Self);
      exit;
    end;

  idx := 0;
  for i := 0 to Count - 1 do
    begin
      ptPrev2 := Points[(i + Count - 1) mod Count]^;
      ptPrev := Points[i]^;
      ptNext := Points[(i + 1) mod Count]^;
      ptNext2 := Points[(i + 2) mod Count]^;

      pre := ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2);

      if i = 0 then
          j := 0
      else
          j := 1;

      while j <= pre do
        begin
          t := j / pre;
          v := Vec2Mul(ptPrev2, Interpolation_InSide(t + 1));
          v := Vec2Add(v, Vec2Mul(ptPrev, Interpolation_InSide(t)));
          v := Vec2Add(v, Vec2Mul(ptNext, Interpolation_InSide(t - 1)));
          v := Vec2Add(v, Vec2Mul(ptNext2, Interpolation_InSide(t - 2)));
          if not IsNan(v) then
              output.Add(v);
          inc(idx);
          inc(j);
        end;
    end;
  RemoveSame;
end;

procedure TVec2List.SplineSmoothInSideClosed;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  SwapData(vl);
  vl.SplineSmoothInSideClosed(Self);
  DisposeObject(vl);
end;

procedure TVec2List.SplineSmoothOutSideClosed(output: TVec2List);
var
  i, j, idx, pre: TGeoInt;
  ptPrev, ptPrev2, ptNext, ptNext2, v: TVec2;
  t: TGeoFloat;
begin
  if Count < 3 then
    begin
      output.Assign(Self);
      exit;
    end;

  idx := 0;
  for i := 0 to Count - 1 do
    begin
      ptPrev2 := Points[(i + Count - 1) mod Count]^;
      ptPrev := Points[i]^;
      ptNext := Points[(i + 1) mod Count]^;
      ptNext2 := Points[(i + 2) mod Count]^;

      pre := ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2);

      if i = 0 then
          j := 0
      else
          j := 1;

      while j <= pre do
        begin
          t := j / pre;
          v := Vec2Mul(ptPrev2, Interpolation_OutSide(t + 1));
          v := Vec2Add(v, Vec2Mul(ptPrev, Interpolation_OutSide(t)));
          v := Vec2Add(v, Vec2Mul(ptNext, Interpolation_OutSide(t - 1)));
          v := Vec2Add(v, Vec2Mul(ptNext2, Interpolation_OutSide(t - 2)));
          if not IsNan(v) then
              output.Add(v);
          inc(idx);
          inc(j);
        end;
    end;
  RemoveSame;
end;

procedure TVec2List.SplineSmoothOutSideClosed;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  SwapData(vl);
  vl.SplineSmoothOutSideClosed(Self);
  DisposeObject(vl);
end;

procedure TVec2List.SplineSmoothOpened(output: TVec2List);
const
  EndCoeff = 0;
var
  i, j, idx, pre: TGeoInt;
  ptPrev, ptPrev2, ptNext, ptNext2, v: TVec2;
  t: TGeoFloat;
begin
  if Count < 3 then
    begin
      output.Assign(Self);
      exit;
    end;

  idx := 0;

  for i := 0 to Count - 2 do
    begin
      ptPrev := Points[i]^;
      ptNext := Points[i + 1]^;

      if i = 0 then
          ptPrev2 := Vec2Add(ptPrev, Vec2Mul(Vec2Mul(Vec2Add(ptNext, Points[i + 2]^), EndCoeff), (1 / (1 + 2 * EndCoeff))))
      else
          ptPrev2 := Points[i - 1]^;

      if i = Count - 2 then
          ptNext2 := Vec2Add(ptNext, Vec2Mul(Vec2Mul(Vec2Add(ptPrev, Points[i - 1]^), EndCoeff), (1 / (1 + 2 * EndCoeff))))
      else
          ptNext2 := Points[i + 2]^;

      pre := ComputeCurvePartPrecision(ptPrev2, ptPrev, ptNext, ptNext2);

      if i = 0 then
          j := 0
      else
          j := 1;

      while j <= pre do
        begin
          t := j / pre;
          v := Vec2Mul(ptPrev2, Interpolation_OutSide(t + 1));
          v := Vec2Add(v, Vec2Mul(ptPrev, Interpolation_OutSide(t)));
          v := Vec2Add(v, Vec2Mul(ptNext, Interpolation_OutSide(t - 1)));
          v := Vec2Add(v, Vec2Mul(ptNext2, Interpolation_OutSide(t - 2)));
          if not IsNan(v) then
              output.Add(v);
          inc(idx);
          inc(j);
        end;
    end;
  RemoveSame;
end;

procedure TVec2List.SplineSmoothOpened;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  SwapData(vl);
  vl.SplineSmoothOpened(Self);
  DisposeObject(vl);
end;

procedure TVec2List.ExtractToBuff(var output: TArrayVec2);
var
  i: TGeoInt;
begin
  SetLength(output, Count);
  for i := 0 to Count - 1 do
      output[i] := Points[i]^;
end;

procedure TVec2List.GiveListDataFromBuff(output: TArrayVec2);
var
  i: TGeoInt;
begin
  Clear;
  for i := low(output) to high(output) do
      Add(output[i]);
end;

function TVec2List.SumDistance: TGeoFloat;
var
  i: TGeoInt;
  p1, p2: PVec2;
begin
  Result := 0;
  if Count <= 0 then
      exit;
  p1 := First;
  for i := 1 to Count - 1 do
    begin
      p2 := Points[i];
      Result := Result + Vec2Distance(p1^, p2^);
      p1 := p2;
    end;
end;

procedure TVec2List.InterpolationTo(count_: TGeoInt; output_: TVec2List);
var
  sum_: TGeoFloat;
  avgDist: TGeoFloat;
  i: TGeoInt;
  t: TVec2;
  d, tmp: TGeoFloat;
begin
  sum_ := SumDistance();
  avgDist := sum_ / count_;
  output_.Clear;

  i := 0;
  t := First^;
  d := avgDist;
  output_.Add(t);

  while i < Count do
    begin
      tmp := Vec2Distance(t, Points[i]^);
      if tmp < d then
        begin
          d := d - tmp;
          t := Points[i]^;
          inc(i);
        end
      else
        begin
          t := Vec2LerpTo(t, Points[i]^, d);
          output_.Add(t);
          d := avgDist;
        end;
    end;
  if output_.Count < count_ then
      output_.Add(Last^);

  if Vec2Distance(output_.First^, vec2(0, 0)) > Vec2Distance(output_.Last^, vec2(0, 0)) then
      output_.Reverse;
end;

procedure TVec2List.VertexReduction(Epsilon_: TGeoFloat);
var
  buff, output: TArrayVec2;
  f, l: TVec2;
begin
  RemoveSame;
  f := First^;
  l := Last^;
  ExtractToBuff(buff);
  FastVertexReduction(buff, Epsilon_, output);
  GiveListDataFromBuff(output);
  Insert(0, f);
  Add(l);
  RemoveSame;
end;

procedure TVec2List.Reduction(Epsilon_: TGeoFloat);
begin
  VertexReduction(Epsilon_);
end;

function TVec2List.Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  p1, p2: PVec2;
begin
  Result := False;
  if FList.Count > 1 then
    begin
      p1 := FList[0];
      for i := 1 to FList.Count - 1 do
        begin
          p2 := FList[i];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
            begin
              Result := True;
              exit;
            end;
          p1 := p2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          p2 := FList[0];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
              Result := True;
        end;
    end;
end;

function TVec2List.Line2Intersect(const lb, le: TVec2; ClosedPolyMode: Boolean; output: TVec2List): Boolean;
var
  i: TGeoInt;
  p1, p2: PVec2;
  ox, oy: TGeoFloat;
begin
  Result := False;
  if FList.Count > 1 then
    begin
      p1 := FList[0];
      for i := 1 to FList.Count - 1 do
        begin
          p2 := FList[i];
          if output <> nil then
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
                begin
                  output.Add(ox, oy);
                  Result := True;
                end;
            end
          else
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
                begin
                  Result := True;
                  exit;
                end;
            end;
          p1 := p2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          p2 := FList[0];
          if output <> nil then
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
                begin
                  output.Add(ox, oy);
                  Result := True;
                end;
            end
          else
            begin
              if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1]) then
                  Result := True;
            end;
        end;
    end;
end;

function TVec2List.Line2NearIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean;
var
  i: TGeoInt;
  p1, p2: PVec2;
  ox, oy: TGeoFloat;
  d, d2: TGeoFloat;
begin
  Result := False;
  if FList.Count > 1 then
    begin
      p1 := FList[0];
      d := 0.0;
      for i := 1 to FList.Count - 1 do
        begin
          p2 := FList[i];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
            begin
              d2 := PointDistance(lb, PointMake(ox, oy));
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := PointMake(ox, oy);
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          p1 := p2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          p2 := FList[0];
          if Intersect(lb[0], lb[1], le[0], le[1], p1^[0], p1^[1], p2^[0], p2^[1], ox, oy) then
            begin
              d2 := PointDistance(lb, PointMake(ox, oy));
              if (d = 0) or (d2 < d) then
                begin
                  IntersectPt := PointMake(ox, oy);
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

procedure TVec2List.SortOfNear(const lb, le: TVec2);

  function Compare_(Left, Right: Pointer): TGeoInt;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := MinimumDistanceFromPointToLine(lb, le, PVec2(Left)^);
    d2 := MinimumDistanceFromPointToLine(lb, le, PVec2(Right)^);
    Result := CompareValue(d1, d2);
  end;

  procedure fastSort_(var arry_: TCoreClassPointerList; l, r: TGeoInt);
  var
    i, j: TGeoInt;
    p: Pointer;
  begin
    repeat
      i := l;
      j := r;
      p := arry_[(l + r) shr 1];
      repeat
        while Compare_(arry_[i], p) < 0 do
            inc(i);
        while Compare_(arry_[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Swap(arry_[i], arry_[j]);
            inc(i);
            dec(j);
          end;
      until i > j;
      if l < j then
          fastSort_(arry_, l, j);
      l := i;
    until i >= r;
  end;

begin
  if Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);
end;

procedure TVec2List.SortOfNear(const pt: TVec2);

  function Compare_(Left, Right: Pointer): TGeoInt;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := PointDistance(PVec2(Left)^, pt);
    d2 := PointDistance(PVec2(Right)^, pt);
    Result := CompareValue(d1, d2);
  end;

  procedure fastSort_(var arry_: TCoreClassPointerList; l, r: TGeoInt);
  var
    i, j: TGeoInt;
    p: Pointer;
  begin
    repeat
      i := l;
      j := r;
      p := arry_[(l + r) shr 1];
      repeat
        while Compare_(arry_[i], p) < 0 do
            inc(i);
        while Compare_(arry_[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Swap(arry_[i], arry_[j]);
            inc(i);
            dec(j);
          end;
      until i > j;
      if l < j then
          fastSort_(arry_, l, j);
      l := i;
    until i >= r;
  end;

begin
  if Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);
end;

procedure TVec2List.Reverse;
var
  NewList: TCoreClassList;
  i, c: TGeoInt;
begin
  NewList := TCoreClassList.Create;
  c := Count - 1;
  NewList.Count := c + 1;
  for i := c downto 0 do
      NewList[c - i] := FList[i];
  DisposeObject(FList);
  FList := NewList;
end;

function TVec2List.GetNearLine(const pt: TVec2; const ClosedMode: Boolean; out lb, le: TGeoInt): TVec2;
var
  i: TGeoInt;
  pt1, pt2: PVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];

          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);

          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedMode then
        begin
          pt2 := Points[0];
          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0]^;
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TVec2List.GetNearLine(const pt: TVec2; const ClosedMode: Boolean): TVec2;
var
  i: TGeoInt;
  pt1, pt2: PVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];

          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);

          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
            end;

          pt1 := pt2;
        end;
      if ClosedMode then
        begin
          pt2 := Points[0];
          opt := ClosestPointOnSegmentFromPoint(pt1^, pt2^, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0]^;
        end
      else
        begin
          Result := NULLPoint;
        end;
    end;
end;

function TVec2List.GetNearLine(const pt: TVec2; const ExpandDist: TGeoFloat): TVec2;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDist];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDist];

          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);

          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
            end;

          pt1 := pt2;
        end;

      pt2 := Expands[0, ExpandDist];
      opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
      d2 := PointDistance(pt, opt);
      if (d = 0.0) or (d2 < d) then
        begin
          Result := opt;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0]^;
        end
      else
        begin
          Result := NULLPoint;
        end;
    end;
end;

procedure TVec2List.CutLineBeginPtToIdx(const pt: TVec2; const toidx: TGeoInt);
var
  i: TGeoInt;
begin
  for i := 0 to toidx - 2 do
      Delete(0);
  Points[0]^ := pt;
end;

procedure TVec2List.Transform(X, Y: TGeoFloat);
var
  i: TGeoInt;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Points[i];
      p^[0] := p^[0] + X;
      p^[1] := p^[1] + Y;
    end;
end;

procedure TVec2List.Transform(v: TVec2);
begin
  Transform(v[0], v[1]);
end;

procedure TVec2List.Mul(X, Y: TGeoFloat);
var
  i: TGeoInt;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Points[i];
      p^[0] := p^[0] * X;
      p^[1] := p^[1] * Y;
    end;
end;

procedure TVec2List.Mul(v: TVec2);
begin
  Mul(v[0], v[1]);
end;

procedure TVec2List.Mul(v: TGeoFloat);
begin
  Mul(v, v);
end;

procedure TVec2List.FDiv(X, Y: TGeoFloat);
var
  i: TGeoInt;
  p: PVec2;
begin
  for i := 0 to Count - 1 do
    begin
      p := Points[i];
      p^[0] := p^[0] / X;
      p^[1] := p^[1] / Y;
    end;
end;

procedure TVec2List.FDiv(v: TVec2);
begin
  FDiv(v[0], v[1]);
end;

procedure TVec2List.FDiv(v: TGeoFloat);
begin
  FDiv(v, v);
end;

function TVec2List.First: PVec2;
begin
  if Count > 0 then
      Result := Points[0]
  else
      Result := nil;
end;

function TVec2List.Last: PVec2;
begin
  if Count > 0 then
      Result := Points[Count - 1]
  else
      Result := nil;
end;

procedure TVec2List.ExpandDistanceAsList(ExpandDist: TGeoFloat; output: TVec2List);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.Add(GetExpands(i, ExpandDist));
end;

procedure TVec2List.ExpandDistance(ExpandDist: TGeoFloat);
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  SwapData(vl);
  vl.ExpandDistanceAsList(ExpandDist, Self);
  DisposeObject(vl);
end;

procedure TVec2List.ExpandConvexHullAsList(ExpandDist: TGeoFloat; output: TVec2List);
var
  pl: TVec2List;
begin
  pl := TVec2List.Create;
  ConvexHull(pl);
  pl.ExpandDistanceAsList(ExpandDist, output);
  DisposeObject(pl);
end;

function TVec2List.GetExpands(idx: TGeoInt; ExpandDist: TGeoFloat): TVec2;
var
  lpt, pt, rpt: TVec2;
  ln, rn: TVec2;
  dx, dy, f, r: TGeoFloat;
  Cx, Cy: TGeoFloat;
begin
  if (ExpandDist = 0) or (Count < 2) then
    begin
      Result := Points[idx]^;
      exit;
    end;

  if idx > 0 then
      lpt := Points[idx - 1]^
  else
      lpt := Points[Count - 1]^;
  if idx + 1 < Count then
      rpt := Points[idx + 1]^
  else
      rpt := Points[0]^;
  pt := Points[idx]^;

  // normal : left to
  dx := (pt[0] - lpt[0]);
  dy := (pt[1] - lpt[1]);
  f := 1.0 / HypotX(dx, dy);
  ln[0] := (dy * f);
  ln[1] := -(dx * f);

  // normal : right to
  dx := (rpt[0] - pt[0]);
  dy := (rpt[1] - pt[1]);
  f := 1.0 / HypotX(dx, dy);
  rn[0] := (dy * f);
  rn[1] := -(dx * f);

  // compute the expand edge
  dx := (ln[0] + rn[0]);
  dy := (ln[1] + rn[1]);
  r := (ln[0] * dx) + (ln[1] * dy);
  if r = 0 then
      r := 1;
  Cx := (dx * ExpandDist / r);
  Cy := (dy * ExpandDist / r);

  Result[0] := pt[0] + Cx;
  Result[1] := pt[1] + Cy;
end;

constructor TLinesList.Create;
begin
  inherited Create;
  AutoFree := False;
end;

destructor TLinesList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLinesList.Remove(obj: TLines);
begin
  if AutoFree then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TLinesList.Delete(index: TGeoInt);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFree then
          DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TLinesList.Clear;
var
  i: TGeoInt;
begin
  if AutoFree then
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
  inherited Clear;
end;

constructor T2DPolygon.Create;
begin
  inherited Create;
  Owner := nil;
end;

destructor T2DPolygon.Destroy;
begin
  inherited Destroy;
end;

constructor T2DPolygonGraph.Create;
begin
  inherited Create;
  Surround := T2DPolygon.Create;
  Surround.Owner := Self;
  SetLength(Collapses, 0);
end;

destructor T2DPolygonGraph.Destroy;
begin
  Clear();
  DisposeObject(Surround);
  inherited Destroy;
end;

procedure T2DPolygonGraph.Assign(Source: TCoreClassObject);
var
  i: TGeoInt;
begin
  Clear;
  if Source is T2DPolygonGraph then
    begin
      Surround.Assign(T2DPolygonGraph(Source).Surround);
      SetLength(Collapses, T2DPolygonGraph(Source).CollapsesCount);
      for i := 0 to T2DPolygonGraph(Source).CollapsesCount - 1 do
        begin
          Collapses[i] := T2DPolygon.Create;
          Collapses[i].Owner := Self;
          Collapses[i].Assign(T2DPolygonGraph(Source).Collapses[i]);
        end;
    end
  else
      Surround.Assign(Source);
end;

function T2DPolygonGraph.NewCollapse: T2DPolygon;
begin
  SetLength(Collapses, CollapsesCount + 1);
  Result := T2DPolygon.Create;
  Result.Owner := Self;
  Collapses[CollapsesCount - 1] := Result;
end;

procedure T2DPolygonGraph.AddCollapse(polygon: T2DPolygon);
begin
  polygon.Owner := Self;
  SetLength(Collapses, CollapsesCount + 1);
  Collapses[CollapsesCount - 1] := polygon;
  polygon.RemoveSame;
end;

procedure T2DPolygonGraph.Clear;
var
  i: TGeoInt;
begin
  Surround.Clear;
  for i := 0 to length(Collapses) - 1 do
      DisposeObject(Collapses[i]);
  SetLength(Collapses, 0);
end;

function T2DPolygonGraph.CollapsesCount(): TGeoInt;
begin
  Result := length(Collapses);
end;

function T2DPolygonGraph.GetBands(const index: TGeoInt): T2DPolygon;
begin
  Result := Collapses[index];
end;

procedure T2DPolygonGraph.Remove(p: PVec2);
var
  i: TGeoInt;
begin
  if Surround.Remove(p) > 0 then
      exit;
  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].Remove(p) > 0 then
        exit;
end;

procedure T2DPolygonGraph.FreeAndRemove(polygon: T2DPolygon);
var
  i: TGeoInt;
  l: T2DPolygonList;
begin
  if polygon = Surround then
      Clear()
  else
    begin
      l := T2DPolygonList.Create;
      for i := Low(Collapses) to High(Collapses) do
        if Collapses[i] = polygon then
            DisposeObject(Collapses[i])
        else
            l.Add(Collapses[i]);

      SetLength(Collapses, l.Count);
      for i := 0 to l.Count - 1 do
          Collapses[i] := l[i];

      DisposeObject(l);
    end;
end;

procedure T2DPolygonGraph.RemoveNullPolygon;
var
  i: TGeoInt;
  l: T2DPolygonList;
begin
  l := T2DPolygonList.Create;
  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].Count = 0 then
        l.Add(Collapses[i]);

  for i := 0 to l.Count - 1 do
      FreeAndRemove(l[i]);
  DisposeObject(l);
end;

function T2DPolygonGraph.Total: TGeoInt;
var
  i: TGeoInt;
begin
  Result := Surround.Count;
  for i := 0 to CollapsesCount - 1 do
      inc(Result, Collapses[i].Count);
end;

function T2DPolygonGraph.BuildArray: TArray2DPoint;
var
  i, j, k: TGeoInt;
begin
  SetLength(Result, Total);
  k := 0;
  for i := 0 to Surround.Count - 1 do
    begin
      Result[k] := Surround[i]^;
      inc(k);
    end;
  for i := 0 to CollapsesCount - 1 do
    for j := 0 to Collapses[i].Count - 1 do
      begin
        Result[k] := Collapses[i][j]^;
        inc(k);
      end;
end;

function T2DPolygonGraph.BuildPArray: TArrayPVec2;
var
  i, j, k: TGeoInt;
begin
  SetLength(Result, Total);
  k := 0;
  for i := 0 to Surround.Count - 1 do
    begin
      Result[k] := Surround[i];
      inc(k);
    end;
  for i := 0 to CollapsesCount - 1 do
    for j := 0 to Collapses[i].Count - 1 do
      begin
        Result[k] := Collapses[i][j];
        inc(k);
      end;
end;

function T2DPolygonGraph.ExistsPVec(p: PVec2): Boolean;
var
  i: TGeoInt;
begin
  Result := True;
  if Surround.FList.IndexOf(p) >= 0 then
      exit;
  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].FList.IndexOf(p) >= 0 then
        exit;
  Result := False;
end;

procedure T2DPolygonGraph.RotateAngle(axis: TVec2; Angle: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.RotateAngle(axis, Angle);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].RotateAngle(axis, Angle);
end;

procedure T2DPolygonGraph.Scale(Scale_: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.Scale(Scale_);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].Scale(Scale_);
end;

procedure T2DPolygonGraph.ProjectionTo(const sour, dest: TRectV2; const output: T2DPolygonGraph);
var
  i, j: TGeoInt;
  geo: T2DPolygon;
begin
  output.Clear;

  for i := 0 to Surround.Count - 1 do
      output.Surround.Add(RectProjection(sour, dest, Surround[i]^));

  for j := 0 to CollapsesCount - 1 do
    begin
      geo := output.NewCollapse();
      for i := 0 to Collapses[j].Count - 1 do
          geo.Add(RectProjection(sour, dest, Collapses[j][i]^));
    end;
end;

procedure T2DPolygonGraph.ProjectionTo(const dest: TRectV2; const output: T2DPolygonGraph);
begin
  ProjectionTo(BoundBox, dest, output);
end;

function T2DPolygonGraph.InHere(pt: TVec2): Boolean;
begin
  Result := False;
  if not InSurround(pt) then
      exit;
  if InCollapse(pt) then
      exit;
  Result := True;
end;

function T2DPolygonGraph.InSurround(pt: TVec2): Boolean;
begin
  Result := Surround.InHere(pt);
end;

function T2DPolygonGraph.InCollapse(pt: TVec2): Boolean;
var
  i: TGeoInt;
begin
  Result := True;
  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].InHere(pt) then
        exit;
  Result := False;
end;

function T2DPolygonGraph.Pick(pt: TVec2): T2DPolygon;
var
  i: TGeoInt;
begin
  Result := nil;

  for i := 0 to CollapsesCount - 1 do
    if Collapses[i].InHere(pt) then
      begin
        Result := Collapses[i];
        exit;
      end;

  if Surround.InHere(pt) then
      Result := Surround;
end;

function T2DPolygonGraph.BoundBox: TRectV2;
begin
  Result := Surround.BoundBox;
end;

function T2DPolygonGraph.CollapseBounds: TRectV2Array;
var
  i: TGeoInt;
begin
  SetLength(Result, CollapsesCount);
  for i := 0 to CollapsesCount - 1 do
      Result[i] := Collapses[i].BoundBox;
end;

function T2DPolygonGraph.Line2Intersect(const lb, le: TVec2; output: T2DPolygon): Boolean;
var
  i: TGeoInt;
begin
  Result := Surround.Line2Intersect(lb, le, True, output);
  for i := 0 to CollapsesCount - 1 do
      Result := Result or Collapses[i].Line2Intersect(lb, le, True, output);
end;

function T2DPolygonGraph.GetNearLine(const pt: TVec2; out output: T2DPolygon; out lb, le: TGeoInt): TVec2;
type
  TNearLineData = record
    l: T2DPolygon;
    lb, le: TGeoInt;
    near_pt: TVec2;
  end;

  PNearLineData = ^TNearLineData;
  TNearLineDataArray = array of TNearLineData;
  TNearLineDataPtrArray = array of PNearLineData;

var
  buff_ori: TNearLineDataArray;
  buff: TNearLineDataPtrArray;
  procedure Fill_buff;
  var
    i: TGeoInt;
  begin
    for i := 0 to length(buff) - 1 do
        buff[i] := @buff_ori[i];
  end;

  procedure extract_NearLine();
  var
    i: TGeoInt;
  begin
    buff_ori[0].l := Surround;
    buff_ori[0].near_pt := Surround.GetNearLine(pt, True, buff_ori[0].lb, buff_ori[0].le);

    for i := 0 to CollapsesCount - 1 do
      begin
        buff_ori[i + 1].l := Collapses[i];
        buff_ori[i + 1].near_pt := Collapses[i].GetNearLine(pt, True, buff_ori[i + 1].lb, buff_ori[i + 1].le);
      end;
  end;

  procedure Fill_Result;
  var
    i: TGeoInt;
  begin
    // write result
    output := buff[0]^.l;
    lb := buff[0]^.lb;
    le := buff[0]^.le;
    Result := buff[0]^.near_pt;

    for i := 1 to length(buff) - 1 do
      begin
        if PointDistance(buff[i]^.near_pt, pt) < PointDistance(Result, pt) then
          begin
            output := buff[i]^.l;
            lb := buff[i]^.lb;
            le := buff[i]^.le;
            Result := buff[i]^.near_pt;
          end;
      end;
  end;

begin
  SetLength(buff_ori, CollapsesCount + 1);
  SetLength(buff, CollapsesCount + 1);
  Fill_buff();
  extract_NearLine();
  Fill_Result();

  // free buff
  SetLength(buff_ori, 0);
  SetLength(buff, 0);
end;

procedure T2DPolygonGraph.Transform(X, Y: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.Transform(X, Y);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].Transform(X, Y);
end;

procedure T2DPolygonGraph.Transform(v: TVec2);
begin
  Transform(v[0], v[1]);
end;

procedure T2DPolygonGraph.Mul(X, Y: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.Mul(X, Y);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].Mul(X, Y);
end;

procedure T2DPolygonGraph.Mul(v: TVec2);
begin
  Mul(v[0], v[1]);
end;

procedure T2DPolygonGraph.Mul(v: TGeoFloat);
begin
  Mul(v, v);
end;

procedure T2DPolygonGraph.FDiv(X, Y: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.FDiv(X, Y);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].FDiv(X, Y);
end;

procedure T2DPolygonGraph.FDiv(v: TVec2);
begin
  FDiv(v[0], v[1]);
end;

procedure T2DPolygonGraph.VertexReduction(Epsilon_: TGeoFloat);
var
  i: TGeoInt;
begin
  Surround.VertexReduction(Epsilon_);
  for i := 0 to CollapsesCount - 1 do
      Collapses[i].VertexReduction(Epsilon_);
end;

procedure T2DPolygonGraph.Reduction(Epsilon_: TGeoFloat);
begin
  VertexReduction(Epsilon_);
end;

procedure T2DPolygonGraph.SaveToStream(stream: TMemoryStream64);
var
  d: TDataFrameEngine;
  m64: TMemoryStream64;
  i: TGeoInt;
begin
  d := TDataFrameEngine.Create;
  d.WriteInteger(CollapsesCount);

  m64 := TMemoryStream64.CustomCreate(64 * 1024);
  Surround.SaveToStream(m64);
  d.WriteStream(m64);
  DisposeObject(m64);

  for i := 0 to CollapsesCount - 1 do
    begin
      m64 := TMemoryStream64.CustomCreate(64 * 1024);
      Collapses[i].SaveToStream(m64);
      d.WriteStream(m64);
      DisposeObject(m64);
    end;

  d.EncodeTo(stream, False);
  DisposeObject(d);
end;

procedure T2DPolygonGraph.LoadFromStream(stream: TMemoryStream64);
var
  d: TDataFrameEngine;
  m64: TMemoryStream64;
  c, i: TGeoInt;
begin
  Clear;
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, False);
  c := d.Reader.ReadInteger;

  m64 := TMemoryStream64.Create;
  d.Reader.ReadStream(m64);
  m64.Position := 0;
  Surround.LoadFromStream(m64);
  DisposeObject(m64);

  for i := 0 to c - 1 do
    begin
      m64 := TMemoryStream64.Create;
      d.Reader.ReadStream(m64);
      m64.Position := 0;
      NewCollapse.LoadFromStream(m64);
      DisposeObject(m64);
    end;

  DisposeObject(d);
end;

constructor TDeflectionPolygon.Create;
begin
  inherited Create;
  FMaxRadius := 0;
  FList := TCoreClassList.Create;
  FName := '';
  FClassifier := '';
  FPosition := PointMake(0, 0);
  FScale := 1.0;
  FAngle := 0;
  FExpandMode := emConvex;
  FUserDataObject := nil;
  FUserData := nil;
end;

destructor TDeflectionPolygon.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TDeflectionPolygon.Reset;
begin
  FPosition := PointMake(0, 0);
  FMaxRadius := 0;
  FScale := 1.0;
  FAngle := 0;
  Clear;
end;

procedure TDeflectionPolygon.Assign(Source: TCoreClassObject);
var
  i: TGeoInt;
  p, p2: PDeflectionPolygonVec;
begin
  if Source is TDeflectionPolygon then
    begin
      Clear;

      FScale := TDeflectionPolygon(Source).FScale;
      FAngle := TDeflectionPolygon(Source).FAngle;
      FMaxRadius := TDeflectionPolygon(Source).FMaxRadius;
      FPosition := TDeflectionPolygon(Source).FPosition;
      FExpandMode := TDeflectionPolygon(Source).FExpandMode;

      for i := 0 to TDeflectionPolygon(Source).FList.Count - 1 do
        begin
          new(p);
          p2 := TDeflectionPolygon(Source).DeflectionPolygon[i];
          p^.Owner := Self;
          p^.Angle := p2^.Angle;
          p^.Dist := p2^.Dist;
          FList.Add(p);
        end;
    end
  else if Source is TVec2List then
    begin
      Rebuild(TVec2List(Source), False);
    end;
end;

function TDeflectionPolygon.BuildArray: TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := Points[i];
end;

function TDeflectionPolygon.BuildSplineSmoothInSideClosedArray: TArrayVec2;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  vl.Assign(Self);
  Result := vl.BuildSplineSmoothInSideClosedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildSplineSmoothOutSideClosedArray: TArrayVec2;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  vl.Assign(Self);
  Result := vl.BuildSplineSmoothOutSideClosedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildSplineSmoothOpenedArray: TArrayVec2;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  vl.Assign(Self);
  Result := vl.BuildSplineSmoothOpenedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildProjectionSplineSmoothInSideClosedArray(const sour, dest: TRectV2): TArrayVec2;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  ProjectionTo(sour, dest, vl);
  Result := vl.BuildSplineSmoothInSideClosedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildProjectionSplineSmoothOutSideClosedArray(const sour, dest: TRectV2): TArrayVec2;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  ProjectionTo(sour, dest, vl);
  Result := vl.BuildSplineSmoothOutSideClosedArray;
  DisposeObject(vl);
end;

function TDeflectionPolygon.BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, Points[i]);
end;

function TDeflectionPolygon.BuildRotationProjectionArray(const sour, dest: TRectV2; const sourAngle, destAngle: TGeoFloat): TArrayVec2;
var
  i: Integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectRotationProjection(sour, dest, sourAngle, destAngle, Points[i]);
end;

function TDeflectionPolygon.BuildProjectionArray(const sour, dest: TRectV2): TArrayVec2;
var
  i: TGeoInt;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
      Result[i] := RectProjection(sour, dest, Points[i]);
end;

function TDeflectionPolygon.BuildProjectionArray(const dest: TRectV2): TArrayVec2;
begin
  Result := BuildProjectionArray(BoundBox, dest);
end;

procedure TDeflectionPolygon.ProjectionTo(const sour, dest: TRectV2; const output: TDeflectionPolygon);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.AddPoint(RectProjection(sour, dest, Points[i]));
end;

procedure TDeflectionPolygon.ProjectionTo(const dest: TRectV2; const output: TDeflectionPolygon);
begin
  ProjectionTo(BoundBox, dest, output);
end;

procedure TDeflectionPolygon.ProjectionTo(const sour, dest: TRectV2; const output: TVec2List);
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      output.Add(RectProjection(sour, dest, Points[i]));
end;

procedure TDeflectionPolygon.ProjectionTo(const dest: TRectV2; const output: TVec2List);
begin
  ProjectionTo(BoundBox, dest, output);
end;

procedure TDeflectionPolygon.AddPoint(pt: TVec2);
begin
  AddPoint(pt[0], pt[1]);
end;

procedure TDeflectionPolygon.AddPoint(X, Y: TGeoFloat);
var
  pt: TVec2;
begin
  pt := PointMake(X, Y);
  Add(PointAngle(FPosition, pt), PointDistance(FPosition, pt));
end;

procedure TDeflectionPolygon.AddRectangle(r: TRectV2);
begin
  AddPoint(r[0, 0], r[0, 1]);
  AddPoint(r[1, 0], r[0, 1]);
  AddPoint(r[1, 0], r[1, 1]);
  AddPoint(r[0, 0], r[1, 1]);
end;

procedure TDeflectionPolygon.AddCirclePoint(count_: Cardinal; axis: TVec2; dist_: TGeoFloat);
var
  i: TGeoInt;
begin
  for i := 0 to count_ - 1 do
      AddPoint(PointRotation(axis, dist_, 360 / count_ * i));
end;

procedure TDeflectionPolygon.Add(angle_, dist_: TGeoFloat);
var
  p: PDeflectionPolygonVec;
begin
  if dist_ > FMaxRadius then
      FMaxRadius := dist_;
  new(p);
  p^.Owner := Self;
  p^.Angle := angle_ - FAngle;
  p^.Dist := dist_ / FScale;
  FList.Add(p);
end;

procedure TDeflectionPolygon.Insert(idx: TGeoInt; angle_, dist_: TGeoFloat);
var
  p: PDeflectionPolygonVec;
begin
  if dist_ > FMaxRadius then
      FMaxRadius := dist_;
  new(p);
  p^.Owner := Self;
  p^.Angle := angle_;
  p^.Dist := dist_;
  FList.Insert(idx, p);
end;

procedure TDeflectionPolygon.InsertPoint(idx: TGeoInt; pt: TVec2);
begin
  Insert(idx, NormalizeDegAngle(PointAngle(FPosition, pt) - Angle), PointDistance(FPosition, pt) / Scale);
end;

procedure TDeflectionPolygon.Delete(idx: TGeoInt);
begin
  Dispose(PDeflectionPolygonVec(FList[idx]));
  FList.Delete(idx);
end;

procedure TDeflectionPolygon.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PDeflectionPolygonVec(FList[i]));
  FList.Clear;
end;

function TDeflectionPolygon.Count: TGeoInt;
begin
  Result := FList.Count;
end;

procedure TDeflectionPolygon.CopyPoly(pl: TDeflectionPolygon; AReversed: Boolean);
  procedure _Append(a, d: TGeoFloat);
  var
    p: PDeflectionPolygonVec;
  begin
    if d > FMaxRadius then
        FMaxRadius := d;
    new(p);
    p^.Owner := Self;
    p^.Angle := a;
    p^.Dist := d;
    FList.Add(p);
  end;

var
  i: TGeoInt;
begin
  Clear;
  FScale := pl.FScale;
  FAngle := pl.FAngle;
  FPosition := pl.FPosition;
  FMaxRadius := 0;
  if AReversed then
    begin
      for i := pl.Count - 1 downto 0 do
        with pl.DeflectionPolygon[i]^ do
            _Append(Angle, Dist);
    end
  else
    begin
      for i := 0 to pl.Count - 1 do
        with pl.DeflectionPolygon[i]^ do
            _Append(Angle, Dist);
    end;
end;

procedure TDeflectionPolygon.CopyExpandPoly(pl: TDeflectionPolygon; AReversed: Boolean; Dist: TGeoFloat);
var
  i: TGeoInt;
begin
  Clear;
  FScale := pl.FScale;
  FAngle := pl.FAngle;
  FPosition := pl.FPosition;
  FMaxRadius := 0;
  if AReversed then
    begin
      for i := pl.Count - 1 downto 0 do
          AddPoint(pl.Expands[i, Dist]);
    end
  else
    for i := 0 to pl.Count - 1 do
        AddPoint(pl.Expands[i, Dist]);
end;

procedure TDeflectionPolygon.Reverse;
var
  NewList: TCoreClassList;
  i, c: TGeoInt;
begin
  NewList := TCoreClassList.Create;
  c := Count - 1;
  NewList.Count := c + 1;
  for i := c downto 0 do
      NewList[c - i] := FList[i];
  DisposeObject(FList);
  FList := NewList;
end;

function TDeflectionPolygon.ScaleBeforeDistance: TGeoFloat;
var
  i: TGeoInt;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + PDeflectionPolygonVec(FList[i])^.Dist;
end;

function TDeflectionPolygon.ScaleAfterDistance: TGeoFloat;
var
  i: TGeoInt;
begin
  Result := 0;
  for i := 0 to Count - 1 do
      Result := Result + PDeflectionPolygonVec(FList[i])^.Dist * FScale;
end;

procedure TDeflectionPolygon.RemoveSame;
var
  l, p: PDeflectionPolygonVec;
  i: TGeoInt;
begin
  if Count < 2 then
      exit;

  l := PDeflectionPolygonVec(FList[0]);
  p := PDeflectionPolygonVec(FList[Count - 1]);
  while (Count >= 2) and (IsEqual(p^.Angle, l^.Angle)) and (IsEqual(p^.Dist, l^.Dist)) do
    begin
      Delete(Count - 1);
      p := PDeflectionPolygonVec(FList[Count - 1]);
    end;

  if Count < 2 then
      exit;

  l := PDeflectionPolygonVec(FList[0]);
  i := 1;
  while i < Count do
    begin
      p := PDeflectionPolygonVec(FList[i]);
      if (IsEqual(p^.Angle, l^.Angle)) and (IsEqual(p^.Dist, l^.Dist)) then
          Delete(i)
      else
        begin
          l := p;
          inc(i);
        end;
    end;
end;

procedure TDeflectionPolygon.ConvexHullFrom(From_: TVec2List);

const
  RightHandSide = -1;
  LeftHandSide = +1;
  CounterClockwise = +1;
  CollinearOrientation = 0;

type
  T2DHullPoint = record
    X: TGeoFloat;
    Y: TGeoFloat;
    Ang: TGeoFloat;
  end;

  TCompareResult = (eGreaterThan, eLessThan, eEqual);

var
  Point: array of T2DHullPoint;
  Stack: array of T2DHullPoint;
  StackHeadPosition: TGeoInt;
  Anchor: T2DHullPoint;

  function CartesianAngle(const X, Y: TGeoFloat): TGeoFloat; {$IFDEF INLINE_ASM} inline; {$ENDIF}
  const
    _180DivPI = 57.295779513082320876798154814105000;
  begin
    if (X > Zero) and (Y > Zero) then
        Result := (ArcTan(Y / X) * _180DivPI)
    else if (X < Zero) and (Y > Zero) then
        Result := (ArcTan(-X / Y) * _180DivPI) + 90.0
    else if (X < Zero) and (Y < Zero) then
        Result := (ArcTan(Y / X) * _180DivPI) + 180.0
    else if (X > Zero) and (Y < Zero) then
        Result := (ArcTan(-X / Y) * _180DivPI) + 270.0
    else if (X = Zero) and (Y > Zero) then
        Result := 90.0
    else if (X < Zero) and (Y = Zero) then
        Result := 180.0
    else if (X = Zero) and (Y < Zero) then
        Result := 270.0
    else
        Result := Zero;
  end;

  procedure Swap(i, j: TGeoInt; var Point: array of T2DHullPoint);
  var
    Temp: T2DHullPoint;
  begin
    Temp := Point[i];
    Point[i] := Point[j];
    Point[j] := Temp;
  end;

  function CompareAngles(const p1, p2: T2DHullPoint): TCompareResult;
    function hEqual(const p1, p2: T2DHullPoint): Boolean; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    begin
      Result := IsEqual(p1.X, p2.X) and IsEqual(p1.Y, p2.Y);
    end;

  begin
    if p1.Ang < p2.Ang then
        Result := eLessThan
    else if p1.Ang > p2.Ang then
        Result := eGreaterThan
    else if hEqual(p1, p2) then
        Result := eEqual
    else if Distance(Anchor.X, Anchor.Y, p1.X, p1.Y) < Distance(Anchor.X, Anchor.Y, p2.X, p2.Y) then
        Result := eLessThan
    else
        Result := eGreaterThan;
  end;

  procedure RQSort(Left, Right: TGeoInt; var Point: array of T2DHullPoint);
  var
    i: TGeoInt;
    j: TGeoInt;
    Middle: TGeoInt;
    Pivot: T2DHullPoint;
  begin
    repeat
      i := Left;
      j := Right;
      Middle := (Left + Right) div 2;
      (* Median of 3 Pivot Selection *)
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      if CompareAngles(Point[Right], Point[Middle]) = eLessThan then
          Swap(Right, Middle, Point);
      if CompareAngles(Point[Middle], Point[Left]) = eLessThan then
          Swap(Left, Middle, Point);
      Pivot := Point[Right];
      repeat
        while CompareAngles(Point[i], Pivot) = eLessThan do
            inc(i);
        while CompareAngles(Point[j], Pivot) = eGreaterThan do
            dec(j);
        if i <= j then
          begin
            Swap(i, j, Point);
            inc(i);
            dec(j);
          end;
      until i > j;
      if Left < j then
          RQSort(Left, j, Point);
      Left := i;
    until i >= Right;
  end;

  procedure Push(Pnt: T2DHullPoint);
  begin
    inc(StackHeadPosition);
    Stack[StackHeadPosition] := Pnt;
  end;

  function Pop: Boolean;
  begin
    Result := False;
    if StackHeadPosition >= 0 then
      begin
        Result := True;
        dec(StackHeadPosition);
      end;
  end;

  function Head: T2DHullPoint;
  begin
    Assert((StackHeadPosition >= 0) and (StackHeadPosition < length(Stack)), 'Invalid stack-head position.');
    Result := Stack[StackHeadPosition];
  end;

  function PreHead: T2DHullPoint;
  begin
    Assert(((StackHeadPosition - 1) >= 0) and ((StackHeadPosition - 1) < length(Stack)), 'Invalid pre stack-head position.');
    Result := Stack[StackHeadPosition - 1];
  end;

  function PreHeadExist: Boolean;
  begin
    Result := (StackHeadPosition > 0);
  end;

  function Orientation(p1, p2, p3: T2DHullPoint): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    function Orientation2(const x1, y1, x2, y2, Px, Py: TGeoFloat): TGeoInt; {$IFDEF INLINE_ASM} inline; {$ENDIF}
    var
      Orin: TGeoFloat;
    begin
      (* Determinant of the 3 points *)
      Orin := (x2 - x1) * (Py - y1) - (Px - x1) * (y2 - y1);
      if Orin > Zero then
          Result := LeftHandSide (* Orientaion is to the left-hand side *)
      else if Orin < Zero then
          Result := RightHandSide (* Orientaion is to the right-hand side *)
      else
          Result := CollinearOrientation; (* Orientaion is neutral aka collinear *)
    end;

  begin
    Result := Orientation2(p1.X, p1.Y, p2.X, p2.Y, p3.X, p3.Y);
  end;

  procedure GrahamScan;
  var
    i: TGeoInt;
    Orin: TGeoInt;
  begin
    Push(Point[0]);
    Push(Point[1]);
    i := 2;
    while i < length(Point) do
      begin
        if PreHeadExist then
          begin
            Orin := Orientation(PreHead, Head, Point[i]);
            if Orin = CounterClockwise then
              begin
                Push(Point[i]);
                inc(i);
              end
            else
                Pop;
          end
        else
          begin
            Push(Point[i]);
            inc(i);
          end;
      end;
  end;

  function CalcCentroid: TVec2;
  var
    i: TGeoInt;
    j: TGeoInt;
    asum: TGeoFloat;
    term: TGeoFloat;
  begin
    Result := NULLPoint;

    asum := Zero;
    j := StackHeadPosition;

    for i := 0 to StackHeadPosition do
      begin
        term := ((Stack[j].X * Stack[i].Y) - (Stack[j].Y * Stack[i].X));
        asum := asum + term;
        Result[0] := Result[0] + (Stack[j].X + Stack[i].X) * term;
        Result[1] := Result[1] + (Stack[j].Y + Stack[i].Y) * term;
        j := i;
      end;

    if NotEqual(asum, Zero) then
      begin
        Result[0] := Result[0] / (3.0 * asum);
        Result[1] := Result[1] / (3.0 * asum);
      end;
  end;

var
  i: TGeoInt;
  j: TGeoInt;
  pt: TVec2;
begin
  if From_.Count <= 3 then
      exit;

  StackHeadPosition := -1;

  try
    SetLength(Point, From_.Count);
    SetLength(Stack, From_.Count);
    j := 0;
    for i := 0 to From_.Count - 1 do
      begin
        pt := From_[i]^;
        Point[i].X := pt[0];
        Point[i].Y := pt[1];
        Point[i].Ang := 0.0;
        if Point[i].Y < Point[j].Y then
            j := i
        else if Point[i].Y = Point[j].Y then
          if Point[i].X < Point[j].X then
              j := i;
      end;

    Swap(0, j, Point);
    Point[0].Ang := 0;
    Anchor := Point[0];
    (* Calculate angle of the vertex ([ith point]-[anchorpoint]-[most left point]) *)
    for i := 1 to length(Point) - 1 do
        Point[i].Ang := CartesianAngle(Point[i].X - Anchor.X, Point[i].Y - Anchor.Y);
    (* Sort points in ascending order according to their angles *)
    RQSort(1, length(Point) - 1, Point);
    GrahamScan;

    { * make Circle * }
    FPosition := CalcCentroid;
    FMaxRadius := 0;

    { * rebuild opt * }
    FScale := 1.0;
    FAngle := 0;

    { * clear * }
    Clear;

    (* output list to self *)
    for i := 0 to StackHeadPosition do
        AddPoint(Stack[i].X, Stack[i].Y);
  finally
    (* Final clean-up *)
    Finalize(Stack);
    Finalize(Point);
  end;
  Rebuild;
end;

procedure TDeflectionPolygon.Rebuild(pl: TVec2List; Scale_: TGeoFloat; angle_: TGeoFloat; ExpandMode_: TExpandMode; Position_: TVec2);
var
  i: TGeoInt;
  Ply: TDeflectionPolygon;
begin
  { * rebuild opt * }
  FMaxRadius := 0;
  FScale := Scale_;
  FAngle := angle_;
  ExpandMode := ExpandMode_;
  FPosition := Position_;

  { * rebuild Polygon * }
  Clear;
  for i := 0 to pl.Count - 1 do
      AddPoint(pl[i]^);
end;

procedure TDeflectionPolygon.Rebuild(pl: TVec2List; reset_: Boolean);
var
  i: TGeoInt;
  Ply: TDeflectionPolygon;
begin
  { * rebuild opt * }
  FMaxRadius := 0;
  if reset_ then
    begin
      FPosition := pl.BoundCentre;
      FScale := 1.0;
      FAngle := 0;
    end;

  { * rebuild Polygon * }
  Clear;
  for i := 0 to pl.Count - 1 do
      AddPoint(pl[i]^);

  Ply := TDeflectionPolygon.Create;
  with Ply do
    begin
      CopyExpandPoly(Self, False, 1);
      if (Self.FExpandMode = emConvex) and (Self.ScaleBeforeDistance > ScaleBeforeDistance) then
          Self.Reverse
      else if (Self.FExpandMode = emConcave) and (Self.ScaleBeforeDistance < ScaleBeforeDistance) then
          Self.Reverse;
    end;
  DisposeObject(Ply);
end;

procedure TDeflectionPolygon.Rebuild;
var
  pl: TVec2List;
  i: TGeoInt;
begin
  pl := TVec2List.Create;
  for i := 0 to Count - 1 do
      pl.Add(GetPoint(i));
  Rebuild(pl, True);
  DisposeObject(pl);
end;

procedure TDeflectionPolygon.Rebuild(Scale_, angle_: TGeoFloat; ExpandMode_: TExpandMode; Position_: TVec2);
var
  pl: TVec2List;
  i: TGeoInt;
begin
  pl := TVec2List.Create;
  for i := 0 to Count - 1 do
      pl.Add(GetPoint(i));
  Scale := Scale_;
  Angle := angle_;
  ExpandMode := ExpandMode_;
  Position := Position_;
  Rebuild(pl, False);
  DisposeObject(pl);
end;

function TDeflectionPolygon.BoundBox: TRectV2;
var
  p: TVec2;
  MaxX: TGeoFloat;
  MaxY: TGeoFloat;
  MinX: TGeoFloat;
  MinY: TGeoFloat;
  i: TGeoInt;
begin
  Result := MakeRectV2(Zero, Zero, Zero, Zero);
  if Count < 2 then
      exit;
  p := Points[0];
  MinX := p[0];
  MaxX := p[0];
  MinY := p[1];
  MaxY := p[1];

  for i := 1 to Count - 1 do
    begin
      p := Points[i];
      if p[0] < MinX then
          MinX := p[0]
      else if p[0] > MaxX then
          MaxX := p[0];
      if p[1] < MinY then
          MinY := p[1]
      else if p[1] > MaxY then
          MaxY := p[1];
    end;
  Result := MakeRectV2(MinX, MinY, MaxX, MaxY);
end;

function TDeflectionPolygon.Centroid: TVec2;
var
  i: TGeoInt;
  asum: TGeoFloat;
  term: TGeoFloat;

  pt1, pt2: TVec2;
begin
  Result := NULLPoint;

  if Count < 3 then
      exit;

  asum := Zero;
  pt2 := Points[Count - 1];

  for i := 0 to Count - 1 do
    begin
      pt1 := Points[i];

      term := ((pt2[0] * pt1[1]) - (pt2[1] * pt1[0]));
      asum := asum + term;
      Result[0] := Result[0] + (pt2[0] + pt1[0]) * term;
      Result[1] := Result[1] + (pt2[1] + pt1[1]) * term;
      pt2 := pt1;
    end;

  if NotEqual(asum, Zero) then
    begin
      Result[0] := Result[0] / (3.0 * asum);
      Result[1] := Result[1] / (3.0 * asum);
    end;
end;

function TDeflectionPolygon.InHere(pt: TVec2): Boolean;
var
  i: TGeoInt;
  pi, pj: TVec2;
begin
  Result := False;
  if Count < 3 then
      exit;
  if not PointInCircle(pt, FPosition, FMaxRadius * FScale) then
      exit;
  pj := GetPoint(Count - 1);
  for i := 0 to Count - 1 do
    begin
      pi := GetPoint(i);
      if ((pi[1] <= pt[1]) and (pt[1] < pj[1])) or  // an upward crossing
        ((pj[1] <= pt[1]) and (pt[1] < pi[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if (pt[0] - pi[0] < ((pj[0] - pi[0]) * (pt[1] - pi[1]) / (pj[1] - pi[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

function TDeflectionPolygon.InHere(ExpandDistance_: TGeoFloat; pt: TVec2): Boolean;
var
  i: TGeoInt;
  pi, pj: TVec2;
begin
  Result := False;
  if Count < 3 then
      exit;
  if not PointInCircle(pt, FPosition, FMaxRadius * FScale + ExpandDistance_) then
      exit;
  pj := Expands[Count - 1, ExpandDistance_];
  for i := 0 to Count - 1 do
    begin
      pi := Expands[i, ExpandDistance_];
      if ((pi[1] <= pt[1]) and (pt[1] < pj[1])) or  // an upward crossing
        ((pj[1] <= pt[1]) and (pt[1] < pi[1])) then // a downward crossing
        begin
          (* compute the edge-ray intersect @ the x-coordinate *)
          if ((pt[0] - pi[0]) < ((pj[0] - pi[0]) * (pt[1] - pi[1]) / (pj[1] - pi[1]))) then
              Result := not Result;
        end;
      pj := pi;
    end;
end;

function TDeflectionPolygon.LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale, lb, le) then
      exit;

  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];
          if Intersect(lb, le, pt1, pt2) then
            begin
              Result := True;
              exit;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          if Intersect(lb, le, pt1, pt2) then
            begin
              Result := True;
            end;
        end;
    end;
end;

function TDeflectionPolygon.LineIntersect(ExpandDistance_: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale + ExpandDistance_, lb, le) then
      exit;

  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDistance_];
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDistance_];
          if SimpleIntersect(lb, le, pt1, pt2) then
            begin
              Result := True;
              exit;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Expands[0, ExpandDistance_];
          if SimpleIntersect(lb, le, pt1, pt2) then
              Result := True;
        end;
    end;
end;

function TDeflectionPolygon.LineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale, lb, le) then
      exit;

  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

function TDeflectionPolygon.LineIntersect(ExpandDistance_: TGeoFloat; const lb, le: TVec2; const ClosedPolyMode: Boolean; out idx1, idx2: TGeoInt; out IntersectPt: TVec2): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale + ExpandDistance_, lb, le) then
      exit;

  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDistance_];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDistance_];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  d := d2;
                  idx1 := i - 1;
                  idx2 := i;
                  Result := True;
                end;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Expands[0, ExpandDistance_];
          if Intersect(lb, le, pt1, pt2, opt) then
            begin
              d2 := PointDistance(lb, opt);
              if (d = 0.0) or (d2 < d) then
                begin
                  IntersectPt := opt;
                  // d := d2;
                  idx1 := FList.Count - 1;
                  idx2 := 0;
                  Result := True;
                end;
            end;
        end;
    end;
end;

function TDeflectionPolygon.SimpleLineIntersect(const lb, le: TVec2; const ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
begin
  Result := False;
  if not Detect_Circle2Line(FPosition, FMaxRadius * FScale, lb, le) then
      exit;

  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];
          if SimpleIntersect(lb, le, pt1, pt2) then
            begin
              Result := True;
              exit;
            end;
          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          if SimpleIntersect(lb, le, pt1, pt2) then
              Result := True;
        end;
    end;
end;

function TDeflectionPolygon.GetNearLine(const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: TGeoInt): TVec2;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Points[0];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Points[i];

          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);

          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Points[0];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0];
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TDeflectionPolygon.GetNearLine(ExpandDistance_: TGeoFloat; const pt: TVec2; const ClosedPolyMode: Boolean; out lb, le: TGeoInt): TVec2;
var
  i: TGeoInt;
  pt1, pt2: TVec2;
  opt: TVec2;
  d, d2: TGeoFloat;
begin
  if FList.Count > 1 then
    begin
      pt1 := Expands[0, ExpandDistance_];
      d := 0.0;
      for i := 1 to Count - 1 do
        begin
          pt2 := Expands[i, ExpandDistance_];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              d := d2;
              lb := i - 1;
              le := i;
            end;

          pt1 := pt2;
        end;
      if ClosedPolyMode and (Count >= 3) then
        begin
          pt2 := Expands[0, ExpandDistance_];
          opt := ClosestPointOnSegmentFromPoint(pt1, pt2, pt);
          d2 := PointDistance(pt, opt);
          if (d = 0.0) or (d2 < d) then
            begin
              Result := opt;
              lb := FList.Count - 1;
              le := 0;
            end;
        end;
    end
  else
    begin
      if Count = 1 then
        begin
          Result := Points[0];
          lb := 0;
          le := 0;
        end
      else
        begin
          Result := NULLPoint;
          lb := -1;
          le := -1;
        end;
    end;
end;

function TDeflectionPolygon.Collision2Circle(cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean): Boolean;
var
  i: TGeoInt;
  curpt, destpt: TVec2;
begin
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale, r)) and (Count > 0) then
    begin
      Result := True;
      curpt := Points[0];
      for i := 1 to Count - 1 do
        begin
          destpt := Points[i];
          if Detect_Circle2Line(cp, r, curpt, destpt) then
              exit;
          curpt := destpt;
        end;
      if ClosedPolyMode and (Count >= 3) then
        if Detect_Circle2Line(cp, r, curpt, Points[0]) then
            exit;
    end;
  Result := False;
end;

function TDeflectionPolygon.Collision2Circle(cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: TDeflectionPolygonLines): Boolean;
var
  i: TGeoInt;
  curpt, destpt: TVec2;
begin
  Result := False;
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale, r)) and (Count > 0) then
    begin
      curpt := Points[0];
      for i := 1 to Count - 1 do
        begin
          destpt := Points[i];
          if Detect_Circle2Line(cp, r, curpt, destpt) then
            begin
              OutputLine.Add(curpt, destpt, i - 1, i, Self);
              Result := True;
            end;
          curpt := destpt;
        end;
      if ClosedPolyMode and (Count >= 3) then
        if Detect_Circle2Line(cp, r, curpt, Points[0]) then
          begin
            OutputLine.Add(curpt, Points[0], Count - 1, 0, Self);
            Result := True;
          end;
    end;
end;

function TDeflectionPolygon.Collision2Circle(ExpandDistance_: TGeoFloat; cp: TVec2; r: TGeoFloat; ClosedPolyMode: Boolean; OutputLine: TDeflectionPolygonLines): Boolean;
var
  i: TGeoInt;
  curpt, destpt: TVec2;
begin
  Result := False;
  if (Detect_Circle2Circle(FPosition, cp, FMaxRadius * FScale + ExpandDistance_, r)) and (Count > 0) then
    begin
      curpt := Expands[0, ExpandDistance_];
      for i := 1 to Count - 1 do
        begin
          destpt := Expands[i, ExpandDistance_];
          if Detect_Circle2Line(cp, r, curpt, destpt) then
            begin
              OutputLine.Add(curpt, destpt, i - 1, i, Self);
              Result := True;
            end;
          curpt := destpt;
        end;
      if ClosedPolyMode and (Count >= 3) then
        if Detect_Circle2Line(cp, r, curpt, Expands[0, ExpandDistance_]) then
          begin
            OutputLine.Add(curpt, Expands[0, ExpandDistance_], Count - 1, 0, Self);
            Result := True;
          end;
    end;
end;

function TDeflectionPolygon.PolygonIntersect(Poly_: TDeflectionPolygon): Boolean;
var
  i: TGeoInt;
begin
  Result := Detect_Circle2Circle(Position, Poly_.Position, MaxRadius * FScale, Poly_.MaxRadius * Poly_.Scale);
  if not Result then
      exit;

  for i := 0 to Count - 1 do
    if Poly_.InHere(Points[i]) then
        exit;

  for i := 0 to Poly_.Count - 1 do
    if InHere(Poly_.Points[i]) then
        exit;

  // line intersect
  for i := 1 to Poly_.Count - 1 do
    if LineIntersect(Poly_.Points[i - 1], Poly_.Points[i], True) then
        exit;

  // line intersect
  if LineIntersect(Poly_.Points[Count - 1], Poly_.Points[0], True) then
      exit;

  Result := False;
end;

function TDeflectionPolygon.PolygonIntersect(vl_: TVec2List): Boolean;
var
  i: TGeoInt;
begin
  Result := True;

  for i := 0 to Count - 1 do
    if vl_.InHere(Points[i]) then
        exit;

  for i := 0 to vl_.Count - 1 do
    if InHere(vl_[i]^) then
        exit;

  // line intersect
  for i := 1 to vl_.Count - 1 do
    if LineIntersect(vl_[i - 1]^, vl_[i]^, True) then
        exit;

  // line intersect
  if LineIntersect(vl_[Count - 1]^, vl_[0]^, True) then
      exit;

  Result := False;
end;

function TDeflectionPolygon.LerpToEdge(pt: TVec2; ProjDistance_, ExpandDistance_: TGeoFloat; FromIdx, toidx: TGeoInt): TVec2;
  function NextIndexStep(CurIdx: TGeoInt; curDir: ShortInt): TGeoInt;
  begin
    if curDir < 0 then
      begin
        if CurIdx = 0 then
            Result := Count - 1
        else if CurIdx > 0 then
            Result := CurIdx - 1
        else
            Result := Count + CurIdx - 1;
      end
    else
      begin
        if CurIdx = Count - 1 then
            Result := 0
        else if CurIdx < Count - 1 then
            Result := CurIdx + 1
        else
            Result := CurIdx - Count;
      end;
    if (Result < 0) or (Result >= Count) then
        Result := -1;
  end;

var
  idxDir: ShortInt;
  ToPt: TVec2;
  d: TGeoFloat;
begin
  Result := pt;
  if Count <= 1 then
      exit;

  if (FromIdx = Count - 1) and (toidx = 0) then
      idxDir := 1
  else if (FromIdx = 0) and (toidx = Count - 1) then
      idxDir := -1
  else if toidx < FromIdx then
      idxDir := -1
  else
      idxDir := 1;

  while True do
    begin
      ToPt := Expands[toidx, ExpandDistance_];
      d := PointDistance(pt, ToPt);

      if ProjDistance_ < d then
        begin
          Result := PointLerpTo(pt, ToPt, ProjDistance_);
          exit;
        end;

      if d > 0 then
        begin
          pt := PointLerpTo(pt, ToPt, d);
          ProjDistance_ := ProjDistance_ - d;
        end;
      toidx := NextIndexStep(toidx, idxDir);
    end;
end;

function TDeflectionPolygon.GetDeflectionPolygon(index: TGeoInt): PDeflectionPolygonVec;
begin
  Result := FList[index];
end;

function TDeflectionPolygon.GetPoint(idx: TGeoInt): TVec2;
var
  p: PDeflectionPolygonVec;
begin
  p := GetDeflectionPolygon(idx);
  Result := PointRotation(FPosition, p^.Dist * FScale, p^.Angle + FAngle);
end;

procedure TDeflectionPolygon.SetPoint(idx: TGeoInt; Value: TVec2);
var
  p: PDeflectionPolygonVec;
begin
  p := GetDeflectionPolygon(idx);
  p^.Angle := PointAngle(FPosition, Value) - FAngle;
  p^.Dist := PointDistance(FPosition, Value);
  if p^.Dist > FMaxRadius then
      FMaxRadius := p^.Dist;
  p^.Dist := p^.Dist / FScale;
end;

function TDeflectionPolygon.FirstPoint: TVec2;
begin
  Result := GetPoint(0);
end;

function TDeflectionPolygon.LastPoint: TVec2;
begin
  Result := GetPoint(Count - 1);
end;

function TDeflectionPolygon.GetExpands(idx: TGeoInt; ExpandDist: TGeoFloat): TVec2;
var
  lpt, pt, rpt: TVec2;
  ln, rn: TVec2;
  dx, dy, f, r: TGeoFloat;
  Cx, Cy: TGeoFloat;
begin
  if (ExpandDist = 0) or (Count < 2) then
    begin
      Result := Points[idx];
      exit;
    end;

  if idx > 0 then
      lpt := Points[idx - 1]
  else
      lpt := Points[Count - 1];
  if idx + 1 < Count then
      rpt := Points[idx + 1]
  else
      rpt := Points[0];
  pt := Points[idx];

  // normal : left to
  dx := (pt[0] - lpt[0]);
  dy := (pt[1] - lpt[1]);
  f := 1.0 / HypotX(dx, dy);
  ln[0] := (dy * f);
  ln[1] := -(dx * f);

  // normal : right to
  dx := (rpt[0] - pt[0]);
  dy := (rpt[1] - pt[1]);
  f := 1.0 / HypotX(dx, dy);
  rn[0] := (dy * f);
  rn[1] := -(dx * f);

  // compute the expand edge
  dx := (ln[0] + rn[0]);
  dy := (ln[1] + rn[1]);
  r := (ln[0] * dx) + (ln[1] * dy);
  if r = 0 then
      r := 1;
  Cx := (dx * ExpandDist / r);
  Cy := (dy * ExpandDist / r);

  if FExpandMode = emConcave then
    begin
      Result[0] := pt[0] - Cx;
      Result[1] := pt[1] - Cy;
    end
  else
    begin
      Result[0] := pt[0] + Cx;
      Result[1] := pt[1] + Cy;
    end;
end;

procedure TDeflectionPolygon.SaveToStream(stream: TMemoryStream64);
var
  i: TGeoInt;
  p: PDeflectionPolygonVec;
begin
  stream.WriteSingle(FScale);
  stream.WriteSingle(FAngle);
  stream.WriteSingle(FPosition[0]);
  stream.WriteSingle(FPosition[1]);
  stream.WriteInt32(Count);
  for i := 0 to Count - 1 do
    begin
      p := DeflectionPolygon[i];
      stream.WriteSingle(p^.Angle);
      stream.WriteSingle(p^.Dist);
    end;
end;

procedure TDeflectionPolygon.LoadFromStream(stream: TMemoryStream64);
var
  c: TGeoInt;
  i: TGeoInt;
  p: PDeflectionPolygonVec;
begin
  Clear;
  FScale := stream.ReadSingle;
  FAngle := stream.ReadSingle;
  FPosition[0] := stream.ReadSingle;
  FPosition[1] := stream.ReadSingle;
  FMaxRadius := 0;
  c := stream.ReadInt32;
  for i := 0 to c - 1 do
    begin
      new(p);
      p^.Owner := Self;
      p^.Angle := stream.ReadSingle;
      p^.Dist := stream.ReadSingle;
      FList.Add(p);

      if p^.Dist > FMaxRadius then
          FMaxRadius := p^.Dist;
    end;
end;

constructor TDeflectionPolygonList.Create;
begin
  inherited Create;
  AutoFree := False;
  BackgroundBox := ZeroRectV2;
end;

destructor TDeflectionPolygonList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TDeflectionPolygonList.Remove(obj: TDeflectionPolygon);
begin
  if AutoFree then
      DisposeObject(obj);
  inherited Remove(obj);
end;

procedure TDeflectionPolygonList.Delete(index: TGeoInt);
begin
  if (index >= 0) and (index < Count) then
    begin
      if AutoFree then
          DisposeObject(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TDeflectionPolygonList.Clear;
var
  i: TGeoInt;
begin
  if AutoFree then
    for i := 0 to Count - 1 do
        DisposeObject(Items[i]);
  inherited Clear;
end;

function TDeflectionPolygonList.BoundBox: TRectV2;
var
  i: TGeoInt;
begin
  Result := ZeroRectV2;
  if Count > 0 then
    begin
      Result := First.BoundBox;
      for i := 1 to Count - 1 do
          Result := BoundRect(Result, Items[i].BoundBox);
    end;
end;

function TDeflectionPolygonList.FindPolygon(Name: TPascalString): TDeflectionPolygon;
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
    if Name.Same(Items[i].Name) then
      begin
        Result := Items[i];
        exit;
      end;
  Result := nil;
end;

function TDeflectionPolygonList.MakePolygonName(Name: TPascalString): TPascalString;
var
  i: TGeoInt;
begin
  Result := Name;
  i := 0;
  while FindPolygon(Result) <> nil do
    begin
      Result := PFormat('%s%d', [Name.Text, i]);
      inc(i);
    end;
end;

procedure TDeflectionPolygonList.SaveToStream(stream: TCoreClassStream);
var
  d: TDataFrameEngine;
  i: TGeoInt;
  dp: TDeflectionPolygon;
  m64: TMemoryStream64;
begin
  d := TDataFrameEngine.Create;

  d.WriteRectV2(BackgroundBox);

  d.WriteInteger(Count);

  for i := 0 to Count - 1 do
    begin
      dp := Items[i];
      d.WriteString(dp.Name);
      d.WriteString(dp.Classifier);

      m64 := TMemoryStream64.Create;
      dp.SaveToStream(m64);
      d.WriteStream(m64);
      DisposeObject(m64);
    end;

  d.EncodeAsZLib(stream, False);
  DisposeObject(d);
end;

procedure TDeflectionPolygonList.LoadFromStream(stream: TCoreClassStream);
var
  d: TDataFrameEngine;
  i, c: TGeoInt;
  dp: TDeflectionPolygon;
  m64: TMemoryStream64;
begin
  Clear;
  d := TDataFrameEngine.Create;
  d.DecodeFrom(stream, False);

  BackgroundBox := d.Reader.ReadRectV2;

  c := d.Reader.ReadInteger;

  for i := 0 to c - 1 do
    begin
      dp := TDeflectionPolygon.Create;
      dp.FName := d.Reader.ReadString;
      dp.FClassifier := d.Reader.ReadString;

      m64 := TMemoryStream64.Create;
      d.Reader.ReadStream(m64);
      m64.Position := 0;
      dp.LoadFromStream(m64);
      DisposeObject(m64);
      Add(dp);
    end;

  DisposeObject(d);
end;

procedure TDeflectionPolygonList.LoadFromBase64(const buff: TPascalString);
var
  m64: TMemoryStream64;
begin
  if not umlTestBase64(buff) then
      RaiseInfo('illegal base64 data.');

  m64 := TMemoryStream64.Create;
  umlDecodeStreamBASE64(buff, m64);
  m64.Position := 0;
  LoadFromStream(m64);
  DisposeObject(m64);
end;

procedure TDeflectionPolygonLine.SetLocation(const lb, le: TVec2);
begin
  buff[0] := lb;
  buff[1] := le;
end;

function TDeflectionPolygonLine.ExpandPoly(ExpandDist: TGeoFloat): TDeflectionPolygonLine;
begin
  Result := Self;
  if OwnerDeflectionPolygon <> nil then
    begin
      Result.buff[0] := OwnerDeflectionPolygon.Expands[OwnerDeflectionPolygonIndex[0], ExpandDist];
      Result.buff[1] := OwnerDeflectionPolygon.Expands[OwnerDeflectionPolygonIndex[1], ExpandDist];
    end;
end;

function TDeflectionPolygonLine.length: TGeoFloat;
begin
  Result := PointDistance(buff[0], buff[1]);
end;

function TDeflectionPolygonLine.MinimumDistance(const pt: TVec2): TGeoFloat;
begin
  Result := PointDistance(pt, ClosestPointFromLine(pt));
end;

function TDeflectionPolygonLine.MinimumDistance(ExpandDist: TGeoFloat; const pt: TVec2): TGeoFloat;
begin
  Result := PointDistance(pt, ClosestPointFromLine(ExpandDist, pt));
end;

function TDeflectionPolygonLine.ClosestPointFromLine(const pt: TVec2): TVec2;
begin
  Result := ClosestPointOnSegmentFromPoint(buff[0], buff[1], pt);
end;

function TDeflectionPolygonLine.ClosestPointFromLine(ExpandDist: TGeoFloat; const pt: TVec2): TVec2;
var
  E: TDeflectionPolygonLine;
begin
  E := ExpandPoly(ExpandDist);
  Result := ClosestPointOnSegmentFromPoint(E.buff[0], E.buff[1], pt);
end;

function TDeflectionPolygonLine.MiddlePoint: TVec2;
begin
  Result := MiddleVec2(buff[0], buff[1]);
end;

function TDeflectionPolygonLines.GetItems(index: TGeoInt): PDeflectionPolygonLine;
begin
  Result := FList[index];
end;

constructor TDeflectionPolygonLines.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  FUserData := nil;
  FUserObject := nil;
end;

destructor TDeflectionPolygonLines.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited Destroy;
end;

procedure TDeflectionPolygonLines.Assign(Source: TCoreClassPersistent);
var
  i: TGeoInt;
begin
  if Source is TDeflectionPolygonLines then
    begin
      Clear;
      for i := 0 to TDeflectionPolygonLines(Source).Count - 1 do
          Add(TDeflectionPolygonLines(Source)[i]^);
    end;
end;

function TDeflectionPolygonLines.Add(v: TDeflectionPolygonLine): TGeoInt;
var
  p: PDeflectionPolygonLine;
begin
  new(p);
  p^ := v;
  Result := FList.Add(p);
  p^.index := Result;
end;

function TDeflectionPolygonLines.Add(lb, le: TVec2): TGeoInt;
var
  p: PDeflectionPolygonLine;
begin
  new(p);
  p^.buff[0] := lb;
  p^.buff[1] := le;
  p^.OwnerDeflectionPolygonIndex[0] := -1;
  p^.OwnerDeflectionPolygonIndex[1] := -1;
  p^.OwnerDeflectionPolygon := nil;
  Result := FList.Add(p);
  p^.index := Result;
end;

function TDeflectionPolygonLines.Add(lb, le: TVec2; idx1, idx2: TGeoInt; polygon: TDeflectionPolygon): TGeoInt;
var
  p: PDeflectionPolygonLine;
begin
  new(p);
  p^.buff[0] := lb;
  p^.buff[1] := le;
  p^.OwnerDeflectionPolygonIndex[0] := idx1;
  p^.OwnerDeflectionPolygonIndex[1] := idx2;
  p^.OwnerDeflectionPolygon := polygon;
  Result := FList.Add(p);
  p^.index := Result;
end;

function TDeflectionPolygonLines.Count: TGeoInt;
begin
  Result := FList.Count;
end;

procedure TDeflectionPolygonLines.Delete(index: TGeoInt);
var
  p: PDeflectionPolygonLine;
  i: TGeoInt;
begin
  p := FList[index];
  Dispose(p);
  FList.Delete(index);
  for i := index to Count - 1 do
      Items[i]^.index := i;
end;

procedure TDeflectionPolygonLines.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      Dispose(PDeflectionPolygonLine(FList[i]));
  FList.Clear;
end;

function TDeflectionPolygonLines.NearLine(const ExpandDist: TGeoFloat; const pt: TVec2): PDeflectionPolygonLine;
var
  d, d2: TGeoFloat;
  l: PDeflectionPolygonLine;
  i: TGeoInt;
begin
  Result := nil;
  if Count = 1 then
    begin
      Result := Items[0];
    end
  else if Count > 1 then
    begin
      l := Items[0];
      if ExpandDist = 0 then
          d := l^.MinimumDistance(pt)
      else
          d := l^.MinimumDistance(ExpandDist, pt);
      Result := l;

      for i := 1 to Count - 1 do
        begin
          l := Items[i];

          if ExpandDist = 0 then
              d2 := l^.MinimumDistance(pt)
          else
              d2 := l^.MinimumDistance(ExpandDist, pt);

          if d2 < d then
            begin
              Result := l;
              d := d2;
            end;
        end;
    end;
end;

function TDeflectionPolygonLines.FarLine(const ExpandDist: TGeoFloat; const pt: TVec2): PDeflectionPolygonLine;
var
  d, d2: TGeoFloat;
  l: PDeflectionPolygonLine;
  i: TGeoInt;
begin
  Result := nil;
  if Count > 0 then
    begin
      l := Items[0];
      if ExpandDist = 0 then
          d := l^.MinimumDistance(pt)
      else
          d := l^.MinimumDistance(ExpandDist, pt);
      Result := l;

      for i := 1 to Count - 1 do
        begin
          l := Items[i];

          if ExpandDist = 0 then
              d2 := l^.MinimumDistance(pt)
          else
              d2 := l^.MinimumDistance(ExpandDist, pt);

          if d2 > d then
            begin
              Result := l;
              d := d2;
            end;
        end;
    end;
end;

procedure TDeflectionPolygonLines.SortOfNear(const pt: TVec2);

  function Compare_(Left, Right: Pointer): TGeoInt;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := PDeflectionPolygonLine(Left)^.MinimumDistance(pt);
    d2 := PDeflectionPolygonLine(Right)^.MinimumDistance(pt);
    Result := CompareValue(d1, d2);
  end;

  procedure fastSort_(var arry_: TCoreClassPointerList; l, r: TGeoInt);
  var
    i, j: TGeoInt;
    p: Pointer;
  begin
    repeat
      i := l;
      j := r;
      p := arry_[(l + r) shr 1];
      repeat
        while Compare_(arry_[i], p) < 0 do
            inc(i);
        while Compare_(arry_[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Swap(arry_[i], arry_[j]);
            inc(i);
            dec(j);
          end;
      until i > j;
      if l < j then
          fastSort_(arry_, l, j);
      l := i;
    until i >= r;
  end;

var
  i: TGeoInt;
begin
  if Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);
  for i := 0 to Count - 1 do
      Items[i]^.index := i;
end;

procedure TDeflectionPolygonLines.SortOfFar(const pt: TVec2);

  function Compare_(Left, Right: Pointer): TGeoInt;
  var
    d1, d2: TGeoFloat;
  begin
    d1 := PDeflectionPolygonLine(Left)^.MinimumDistance(pt);
    d2 := PDeflectionPolygonLine(Right)^.MinimumDistance(pt);
    Result := CompareValue(d2, d1);
  end;

  procedure fastSort_(var arry_: TCoreClassPointerList; l, r: TGeoInt);
  var
    i, j: TGeoInt;
    p: Pointer;
  begin
    repeat
      i := l;
      j := r;
      p := arry_[(l + r) shr 1];
      repeat
        while Compare_(arry_[i], p) < 0 do
            inc(i);
        while Compare_(arry_[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Swap(arry_[i], arry_[j]);
            inc(i);
            dec(j);
          end;
      until i > j;
      if l < j then
          fastSort_(arry_, l, j);
      l := i;
    until i >= r;
  end;

var
  i: TGeoInt;
begin
  if Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);
  for i := 0 to Count - 1 do
      Items[i]^.index := i;
end;

function TV2Rect4.IsZero: Boolean;
begin
  Result :=
    Geometry2DUnit.IsZero(LeftTop) and
    Geometry2DUnit.IsZero(RightTop) and
    Geometry2DUnit.IsZero(RightBottom) and
    Geometry2DUnit.IsZero(LeftBottom);
end;

function TV2Rect4.Rotation(Angle: TGeoFloat): TV2Rect4;
var
  axis: TVec2;
begin
  axis := Centroid;
  Result.LeftTop := PointRotation(axis, LeftTop, PointAngle(axis, LeftTop) + Angle);
  Result.RightTop := PointRotation(axis, RightTop, PointAngle(axis, RightTop) + Angle);
  Result.RightBottom := PointRotation(axis, RightBottom, PointAngle(axis, RightBottom) + Angle);
  Result.LeftBottom := PointRotation(axis, LeftBottom, PointAngle(axis, LeftBottom) + Angle);
end;

function TV2Rect4.Rotation(axis: TVec2; Angle: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := PointRotation(axis, LeftTop, PointAngle(axis, LeftTop) + Angle);
  Result.RightTop := PointRotation(axis, RightTop, PointAngle(axis, RightTop) + Angle);
  Result.RightBottom := PointRotation(axis, RightBottom, PointAngle(axis, RightBottom) + Angle);
  Result.LeftBottom := PointRotation(axis, LeftBottom, PointAngle(axis, LeftBottom) + Angle);
end;

function TV2Rect4.ScaleToRect(Box: TRectV2; Edge: TGeoFloat): TV2Rect4;
var
  boxSelf, nArea: TRectV2;
begin
  boxSelf := BoundRect();
  nArea := RectEdge(Box, -abs(Edge));
  Result.LeftTop := RectProjection(boxSelf, nArea, LeftTop);
  Result.RightTop := RectProjection(boxSelf, nArea, RightTop);
  Result.RightBottom := RectProjection(boxSelf, nArea, RightBottom);
  Result.LeftBottom := RectProjection(boxSelf, nArea, LeftBottom);
end;

function TV2Rect4.ScaleToRect(Box: TRectV2; Angle, Edge: TGeoFloat): TV2Rect4;
var
  boxSelf, nArea: TRectV2;
begin
  boxSelf := BoundRect();
  nArea := RectEdge(Box, -abs(Edge));
  Result.LeftTop := RectProjectionRotationDest(boxSelf, nArea, Angle, LeftTop);
  Result.RightTop := RectProjectionRotationDest(boxSelf, nArea, Angle, RightTop);
  Result.RightBottom := RectProjectionRotationDest(boxSelf, nArea, Angle, RightBottom);
  Result.LeftBottom := RectProjectionRotationDest(boxSelf, nArea, Angle, LeftBottom);
end;

function TV2Rect4.ScaleToRect(Box: TRectV2; axis: TVec2; Angle, Edge: TGeoFloat): TV2Rect4;
var
  boxSelf, nArea: TRectV2;
begin
  boxSelf := BoundRect();
  nArea := RectEdge(Box, -abs(Edge));
  Result.LeftTop := RectProjectionRotationDest(boxSelf, nArea, axis, Angle, LeftTop);
  Result.RightTop := RectProjectionRotationDest(boxSelf, nArea, axis, Angle, RightTop);
  Result.RightBottom := RectProjectionRotationDest(boxSelf, nArea, axis, Angle, RightBottom);
  Result.LeftBottom := RectProjectionRotationDest(boxSelf, nArea, axis, Angle, LeftBottom);
end;

function TV2Rect4.Add(v: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, v);
  Result.RightTop := Vec2Add(RightTop, v);
  Result.RightBottom := Vec2Add(RightBottom, v);
  Result.LeftBottom := Vec2Add(LeftBottom, v);
end;

function TV2Rect4.Sub(v: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Sub(LeftTop, v);
  Result.RightTop := Vec2Sub(RightTop, v);
  Result.RightBottom := Vec2Sub(RightBottom, v);
  Result.LeftBottom := Vec2Sub(LeftBottom, v);
end;

function TV2Rect4.Mul(v: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, v);
  Result.RightTop := Vec2Mul(RightTop, v);
  Result.RightBottom := Vec2Mul(RightBottom, v);
  Result.LeftBottom := Vec2Mul(LeftBottom, v);
end;

function TV2Rect4.Mul(v: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, v);
  Result.RightTop := Vec2Mul(RightTop, v);
  Result.RightBottom := Vec2Mul(RightBottom, v);
  Result.LeftBottom := Vec2Mul(LeftBottom, v);
end;

function TV2Rect4.Mul(X, Y: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Mul(LeftTop, X, Y);
  Result.RightTop := Vec2Mul(RightTop, X, Y);
  Result.RightBottom := Vec2Mul(RightBottom, X, Y);
  Result.LeftBottom := Vec2Mul(LeftBottom, X, Y);
end;

function TV2Rect4.Div_(v: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Div(LeftTop, v);
  Result.RightTop := Vec2Div(RightTop, v);
  Result.RightBottom := Vec2Div(RightBottom, v);
  Result.LeftBottom := Vec2Div(LeftBottom, v);
end;

function TV2Rect4.Div_(v: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Div(LeftTop, v);
  Result.RightTop := Vec2Div(RightTop, v);
  Result.RightBottom := Vec2Div(RightBottom, v);
  Result.LeftBottom := Vec2Div(LeftBottom, v);
end;

function TV2Rect4.MoveTo(Position: TVec2): TV2Rect4;
begin
  Result := Init(Position, PointDistance(LeftTop, RightTop), PointDistance(LeftBottom, RightBottom), 0);
end;

function TV2Rect4.BoundRect: TRectV2;
begin
  Result := Geometry2DUnit.BoundRect(LeftTop, RightTop, RightBottom, LeftBottom);
end;

function TV2Rect4.BoundRectf: TRectf;
begin
  Result := MakeRectf(BoundRect);
end;

function TV2Rect4.Centroid: TVec2;
begin
  Result := Geometry2DUnit.BuffCentroid(LeftTop, RightTop, RightBottom, LeftBottom);
end;

function TV2Rect4.Transform(v2: TVec2): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, v2);
  Result.RightTop := Vec2Add(RightTop, v2);
  Result.RightBottom := Vec2Add(RightBottom, v2);
  Result.LeftBottom := Vec2Add(LeftBottom, v2);
end;

function TV2Rect4.Transform(X, Y: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := Vec2Add(LeftTop, X, Y);
  Result.RightTop := Vec2Add(RightTop, X, Y);
  Result.RightBottom := Vec2Add(RightBottom, X, Y);
  Result.LeftBottom := Vec2Add(LeftBottom, X, Y);
end;

function TV2Rect4.Expands(Dist: TGeoFloat): TV2Rect4;
var
  vl: TVec2List;
begin
  vl := TVec2List.Create;
  vl.Add(LeftTop);
  vl.Add(RightTop);
  vl.Add(RightBottom);
  vl.Add(LeftBottom);
  Result.LeftTop := vl.Expands[0, Dist];
  Result.RightTop := vl.Expands[1, Dist];
  Result.RightBottom := vl.Expands[2, Dist];
  Result.LeftBottom := vl.Expands[3, Dist];
  DisposeObject(vl);
end;

function TV2Rect4.InHere(pt: TVec2): Boolean;
var
  buff: TArrayVec2;
begin
  buff := GetArrayVec2;
  Result := PointInPolygon(pt, buff);
  SetLength(buff, 0);
end;

function TV2Rect4.InHere(r: TRectV2): Boolean;
var
  buff: TArrayVec2;
begin
  buff := GetArrayVec2;
  Result := PointInPolygon(r[0], buff) and PointInPolygon(r[1], buff);
  SetLength(buff, 0);
end;

function TV2Rect4.GetArrayVec2: TArrayVec2;
begin
  SetLength(Result, 4);
  Result[0] := LeftTop;
  Result[1] := RightTop;
  Result[2] := RightBottom;
  Result[3] := LeftBottom;
end;

function TV2Rect4.GetNear(pt: TVec2): TVec2;
var
  tmpPt: TVec2;
  tmpDist, d: TGeoFloat;
begin
  tmpPt := ClosestPointOnSegmentFromPoint(LeftTop, RightTop, pt);
  tmpDist := Vec2Distance(tmpPt, pt);
  d := tmpDist;
  Result := tmpPt;

  tmpPt := ClosestPointOnSegmentFromPoint(RightTop, RightBottom, pt);
  tmpDist := Vec2Distance(tmpPt, pt);
  if tmpDist < d then
    begin
      d := tmpDist;
      Result := tmpPt;
    end;

  tmpPt := ClosestPointOnSegmentFromPoint(RightBottom, LeftBottom, pt);
  tmpDist := Vec2Distance(tmpPt, pt);
  if tmpDist < d then
    begin
      d := tmpDist;
      Result := tmpPt;
    end;

  tmpPt := ClosestPointOnSegmentFromPoint(LeftBottom, LeftTop, pt);
  tmpDist := Vec2Distance(tmpPt, pt);
  if tmpDist < d then
    begin
      d := tmpDist;
      Result := tmpPt;
    end;
end;

function TV2Rect4.Projection(const sour, dest: TRectV2; const sourAxis, destAxis: TVec2; const sourAngle, destAngle: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, LeftTop);
  Result.RightTop := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, RightTop);
  Result.RightBottom := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, RightBottom);
  Result.LeftBottom := RectRotationProjection(sour, dest, sourAxis, destAxis, sourAngle, destAngle, LeftBottom);
end;

function TV2Rect4.Projection(const sour, dest: TRectV2; sourAngle, destAngle: TGeoFloat): TV2Rect4;
begin
  Result.LeftTop := RectRotationProjection(sour, dest, sourAngle, destAngle, LeftTop);
  Result.RightTop := RectRotationProjection(sour, dest, sourAngle, destAngle, RightTop);
  Result.RightBottom := RectRotationProjection(sour, dest, sourAngle, destAngle, RightBottom);
  Result.LeftBottom := RectRotationProjection(sour, dest, sourAngle, destAngle, LeftBottom);
end;

function TV2Rect4.Projection(const sour, dest: TRectV2): TV2Rect4;
begin
  Result.LeftTop := RectProjection(sour, dest, LeftTop);
  Result.RightTop := RectProjection(sour, dest, RightTop);
  Result.RightBottom := RectProjection(sour, dest, RightBottom);
  Result.LeftBottom := RectProjection(sour, dest, LeftBottom);
end;

class function TV2Rect4.Init(r: TRectV2): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := PointMake(r[0, 0], r[0, 1]);
      RightTop := PointMake(r[1, 0], r[0, 1]);
      RightBottom := PointMake(r[1, 0], r[1, 1]);
      LeftBottom := PointMake(r[0, 0], r[1, 1]);
    end;
end;

class function TV2Rect4.Init(r: TRectV2; axis: TVec2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := PointMake(r[0, 0], r[0, 1]);
      RightTop := PointMake(r[1, 0], r[0, 1]);
      RightBottom := PointMake(r[1, 0], r[1, 1]);
      LeftBottom := PointMake(r[0, 0], r[1, 1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(axis, Ang);
end;

class function TV2Rect4.Init(r: TRectV2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := PointMake(r[0, 0], r[0, 1]);
      RightTop := PointMake(r[1, 0], r[0, 1]);
      RightBottom := PointMake(r[1, 0], r[1, 1]);
      LeftBottom := PointMake(r[0, 0], r[1, 1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(Ang);
end;

class function TV2Rect4.Init(r: TRectf; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(r), Ang);
end;

class function TV2Rect4.Init(r: TRect; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(r), Ang);
end;

class function TV2Rect4.Init(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4;
var
  r: TRectV2;
begin
  r[0, 0] := CenPos[0] - width * 0.5;
  r[0, 1] := CenPos[1] - height * 0.5;
  r[1, 0] := CenPos[0] + width * 0.5;
  r[1, 1] := CenPos[1] + height * 0.5;
  Result := Init(r, Ang);
end;

class function TV2Rect4.Init(width, height, Ang: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(0, 0, width, height), Ang);
end;

class function TV2Rect4.Init(width, height: TGeoFloat): TV2Rect4;
begin
  Result := Init(MakeRectV2(0, 0, width, height), 0);
end;

class function TV2Rect4.Init(): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := NULLPoint;
      RightTop := NULLPoint;
      RightBottom := NULLPoint;
      LeftBottom := NULLPoint;
    end;
end;

class function TV2Rect4.Create(r: TRectV2): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := PointMake(r[0, 0], r[0, 1]);
      RightTop := PointMake(r[1, 0], r[0, 1]);
      RightBottom := PointMake(r[1, 0], r[1, 1]);
      LeftBottom := PointMake(r[0, 0], r[1, 1]);
    end;
end;

class function TV2Rect4.Create(r: TRectV2; axis: TVec2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := PointMake(r[0, 0], r[0, 1]);
      RightTop := PointMake(r[1, 0], r[0, 1]);
      RightBottom := PointMake(r[1, 0], r[1, 1]);
      LeftBottom := PointMake(r[0, 0], r[1, 1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(axis, Ang);
end;

class function TV2Rect4.Create(r: TRectV2; Ang: TGeoFloat): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := PointMake(r[0, 0], r[0, 1]);
      RightTop := PointMake(r[1, 0], r[0, 1]);
      RightBottom := PointMake(r[1, 0], r[1, 1]);
      LeftBottom := PointMake(r[0, 0], r[1, 1]);
    end;
  if Ang <> 0 then
      Result := Result.Rotation(Ang);
end;

class function TV2Rect4.Create(r: TRectf; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Create(MakeRectV2(r), Ang);
end;

class function TV2Rect4.Create(r: TRect; Ang: TGeoFloat): TV2Rect4;
begin
  Result := Create(MakeRectV2(r), Ang);
end;

class function TV2Rect4.Create(CenPos: TVec2; width, height, Ang: TGeoFloat): TV2Rect4;
var
  r: TRectV2;
begin
  r[0, 0] := CenPos[0] - width * 0.5;
  r[0, 1] := CenPos[1] - height * 0.5;
  r[1, 0] := CenPos[0] + width * 0.5;
  r[1, 1] := CenPos[1] + height * 0.5;
  Result := Create(r, Ang);
end;

class function TV2Rect4.Create(width, height, Ang: TGeoFloat): TV2Rect4;
begin
  Result := Create(MakeRectV2(0, 0, width, height), Ang);
end;

class function TV2Rect4.Create(width, height: TGeoFloat): TV2Rect4;
begin
  Result := Create(MakeRectV2(0, 0, width, height), 0);
end;

class function TV2Rect4.Create(): TV2Rect4;
begin
  with Result do
    begin
      LeftTop := NULLPoint;
      RightTop := NULLPoint;
      RightBottom := NULLPoint;
      LeftBottom := NULLPoint;
    end;
end;

constructor TTriangleList.Create;
begin
  inherited Create;
end;

destructor TTriangleList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTriangleList.AddTri(T_: TTriangle);
var
  p: PTriangle;
begin
  new(p);
  p^ := T_;
  inherited Add(p);
end;

procedure TTriangleList.Remove(p: PTriangle);
begin
  Dispose(p);
  inherited Remove(p);
end;

procedure TTriangleList.Delete(index: TGeoInt);
begin
  if (index >= 0) and (index < Count) then
    begin
      Dispose(Items[index]);
      inherited Delete(index);
    end;
end;

procedure TTriangleList.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to Count - 1 do
      Dispose(Items[i]);
  inherited Clear;
end;

procedure TTriangleList.BuildTriangle(polygon: TVec2List);
var
  Graph: TGraph2D_;
  mesh: TDelaunayMesh2D_;
  i: TGeoInt;
  v1, v2: TVec2;
  first_vert, Vert1, vert2: TVertex2D_;
  mesh_tri: TTriangle2D_;
  T_: TTriangle;
begin
  Clear;

  if polygon.Count < 3 then
      exit;

  Graph := TGraph2D_.Create;
  mesh := TDelaunayMesh2D_.Create;

  v1 := polygon[0]^;
  Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
  first_vert := Vert1;
  Graph.Vertices.Add(Vert1);
  for i := 1 to polygon.Count - 1 do
    begin
      v2 := polygon[i]^;
      vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
      Graph.Vertices.Add(vert2);
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
      v1 := v2;
      Vert1 := vert2;
    end;
  Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));

  mesh.AddGraph(Graph);

  // If the mesh has no segments, we will add a convex hull
  if mesh.Segments.Count = 0 then
      mesh.ConvexHull;

  mesh.Triangulate(TRemovalStyle_.rsOutside);

  for i := 0 to mesh.Triangles.Count - 1 do
    begin
      mesh_tri := mesh.Triangles[i];
      with mesh_tri.Vertices[0].Point^ do
          T_[0] := vec2(X, Y);
      with mesh_tri.Vertices[1].Point^ do
          T_[1] := vec2(X, Y);
      with mesh_tri.Vertices[2].Point^ do
          T_[2] := vec2(X, Y);

      AddTri(T_);
    end;

  DisposeObject(mesh);
  DisposeObject(Graph);
end;

procedure TTriangleList.BuildTriangle(polygon: TVec2List; MinAngle, MinSegmentLength, MaxElementSize: TGeoFloat);
var
  Graph: TGraph2D_;
  mesh: TQualityMesh2D_;
  i: TGeoInt;
  v1, v2: TVec2;
  first_vert, Vert1, vert2: TVertex2D_;
  mesh_tri: TTriangle2D_;
  T_: TTriangle;
begin
  Clear;

  if polygon.Count < 3 then
      exit;

  Graph := TGraph2D_.Create;
  mesh := TQualityMesh2D_.Create;

  v1 := polygon[0]^;
  Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
  first_vert := Vert1;
  Graph.Vertices.Add(Vert1);
  for i := 1 to polygon.Count - 1 do
    begin
      v2 := polygon[i]^;
      vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
      Graph.Vertices.Add(vert2);
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
      v1 := v2;
      Vert1 := vert2;
    end;
  Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));

  mesh.AddGraph(Graph);

  // If the mesh has no segments, we will add a convex hull
  if mesh.Segments.Count = 0 then
      mesh.ConvexHull;

  mesh.MinimumAngle := MinAngle;
  mesh.MinimumSegmentLength := MinSegmentLength;
  mesh.MaximumElementSize := MaxElementSize;

  mesh.Triangulate(TRemovalStyle_.rsOutside);

  for i := 0 to mesh.Triangles.Count - 1 do
    begin
      mesh_tri := mesh.Triangles[i];
      with mesh_tri.Vertices[0].Point^ do
          T_[0] := vec2(X, Y);
      with mesh_tri.Vertices[1].Point^ do
          T_[1] := vec2(X, Y);
      with mesh_tri.Vertices[2].Point^ do
          T_[2] := vec2(X, Y);

      AddTri(T_);
    end;

  DisposeObject(mesh);
  DisposeObject(Graph);
end;

procedure TTriangleList.BuildTriangle(polygon: T2DPolygonGraph);
var
  Graph: TGraph2D_;
  mesh: TDelaunayMesh2D_;
  i, j: TGeoInt;
  poly: T2DPolygon;
  v1, v2: TVec2;
  first_vert, Vert1, vert2: TVertex2D_;
  mesh_tri: TTriangle2D_;
  T_: TTriangle;
begin
  Clear;

  if polygon.Surround.Count < 3 then
      exit;

  Graph := TGraph2D_.Create;
  mesh := TDelaunayMesh2D_.Create;

  v1 := polygon.Surround[0]^;
  Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
  first_vert := Vert1;
  Graph.Vertices.Add(Vert1);
  for i := 1 to polygon.Surround.Count - 1 do
    begin
      v2 := polygon.Surround[i]^;
      vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
      Graph.Vertices.Add(vert2);
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
      v1 := v2;
      Vert1 := vert2;
    end;
  Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));

  for j := 0 to polygon.CollapsesCount - 1 do
    begin
      poly := polygon.Collapses[j];

      v1 := poly[0]^;
      Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
      first_vert := Vert1;
      Graph.Vertices.Add(Vert1);
      for i := 1 to poly.Count - 1 do
        begin
          v2 := poly[i]^;
          vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
          Graph.Vertices.Add(vert2);
          Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
          v1 := v2;
          Vert1 := vert2;
        end;
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));
    end;

  mesh.AddGraph(Graph);

  // If the mesh has no segments, we will add a convex hull
  if mesh.Segments.Count = 0 then
      mesh.ConvexHull;

  mesh.Triangulate(TRemovalStyle_.rsNone);

  for i := 0 to mesh.Triangles.Count - 1 do
    begin
      mesh_tri := mesh.Triangles[i];
      with mesh_tri.Vertices[0].Point^ do
          T_[0] := vec2(X, Y);
      with mesh_tri.Vertices[1].Point^ do
          T_[1] := vec2(X, Y);
      with mesh_tri.Vertices[2].Point^ do
          T_[2] := vec2(X, Y);

      if polygon.InHere(TriCentre(T_)) then
          AddTri(T_);
    end;

  DisposeObject(mesh);
  DisposeObject(Graph);
end;

procedure TTriangleList.BuildTriangle(polygon: T2DPolygonGraph; MinAngle, MinSegmentLength, MaxElementSize: TGeoFloat);
var
  Graph: TGraph2D_;
  mesh: TQualityMesh2D_;
  i, j: TGeoInt;
  poly: T2DPolygon;
  v1, v2: TVec2;
  first_vert, Vert1, vert2: TVertex2D_;
  mesh_tri: TTriangle2D_;
  T_: TTriangle;
begin
  Clear;

  if polygon.Surround.Count < 3 then
      exit;

  Graph := TGraph2D_.Create;
  mesh := TQualityMesh2D_.Create;

  v1 := polygon.Surround[0]^;
  Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
  first_vert := Vert1;
  Graph.Vertices.Add(Vert1);
  for i := 1 to polygon.Surround.Count - 1 do
    begin
      v2 := polygon.Surround[i]^;
      vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
      Graph.Vertices.Add(vert2);
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
      v1 := v2;
      Vert1 := vert2;
    end;
  Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));

  for j := 0 to polygon.CollapsesCount - 1 do
    begin
      poly := polygon.Collapses[j];

      v1 := poly[0]^;
      Vert1 := TVertex2D_.CreateWithCoords(v1[0], v1[1]);
      first_vert := Vert1;
      Graph.Vertices.Add(Vert1);
      for i := 1 to poly.Count - 1 do
        begin
          v2 := poly[i]^;
          vert2 := TVertex2D_.CreateWithCoords(v2[0], v2[1]);
          Graph.Vertices.Add(vert2);
          Graph.Segments.Add(TSegment2D_.CreateWithVertices(Vert1, vert2));
          v1 := v2;
          Vert1 := vert2;
        end;
      Graph.Segments.Add(TSegment2D_.CreateWithVertices(vert2, first_vert));
    end;

  mesh.AddGraph(Graph);

  // If the mesh has no segments, we will add a convex hull
  if mesh.Segments.Count = 0 then
      mesh.ConvexHull;

  mesh.MinimumAngle := MinAngle;
  mesh.MinimumSegmentLength := MinSegmentLength;
  mesh.MaximumElementSize := MaxElementSize;

  mesh.Triangulate(TRemovalStyle_.rsNone);

  for i := 0 to mesh.Triangles.Count - 1 do
    begin
      mesh_tri := mesh.Triangles[i];
      with mesh_tri.Vertices[0].Point^ do
          T_[0] := vec2(X, Y);
      with mesh_tri.Vertices[1].Point^ do
          T_[1] := vec2(X, Y);
      with mesh_tri.Vertices[2].Point^ do
          T_[2] := vec2(X, Y);

      if polygon.InHere(TriCentre(T_)) then
          AddTri(T_);
    end;

  DisposeObject(mesh);
  DisposeObject(Graph);
end;

function TRectPacking.Pack(width, height: TGeoFloat; var X, Y: TGeoFloat): Boolean;
var
  i: TGeoInt;
  p: PRectPackData;
  r, b: TGeoFloat;
begin
  MaxWidth := max(MaxWidth, width);
  MaxHeight := max(MaxHeight, height);

  i := 0;
  while i < FList.Count do
    begin
      p := FList[i];
      if (width <= RectWidth(p^.Rect)) and (height <= RectHeight(p^.Rect)) then
        begin
          FList.Delete(i);
          X := p^.Rect[0, 0];
          Y := p^.Rect[0, 1];
          r := X + width;
          b := Y + height;
          MaxWidth := max(MaxWidth, max(width, r));
          MaxHeight := max(MaxHeight, max(height, b));
          Add(X, b, width, p^.Rect[1, 1] - b);
          Add(r, Y, p^.Rect[1, 0] - r, height);
          Add(r, b, p^.Rect[1, 0] - r, p^.Rect[1, 1] - b);
          Result := True;
          Dispose(p);
          exit;
        end;
      inc(i);
    end;
  X := 0;
  Y := 0;
  Result := False;
end;

function TRectPacking.GetItems(const index: TGeoInt): PRectPackData;
begin
  Result := PRectPackData(FList[index]);
end;

constructor TRectPacking.Create;
begin
  inherited Create;
  FList := TCoreClassList.Create;
  MaxWidth := 0;
  MaxHeight := 0;
  Margins := 2;
end;

destructor TRectPacking.Destroy;
begin
  Clear;
  DisposeObject(FList);
  inherited;
end;

procedure TRectPacking.Clear;
var
  i: TGeoInt;
begin
  for i := 0 to FList.Count - 1 do
      Dispose(PRectPackData(FList[i]));
  FList.Clear;
end;

function TRectPacking.Count: TGeoInt;
begin
  Result := FList.Count;
end;

procedure TRectPacking.Add(const X, Y, width, height: TGeoFloat);
var
  p: PRectPackData;
begin
  new(p);
  p^.Rect := FixRect(MakeRectV2(X, Y, X + width, Y + height));
  p^.error := True;
  p^.Data1 := nil;
  p^.Data2 := nil;
  FList.Add(p);
end;

procedure TRectPacking.Add(Data1: Pointer; Data2: TCoreClassObject; X, Y, width, height: TGeoFloat);
var
  p: PRectPackData;
begin
  new(p);
  p^.Rect := FixRect(MakeRectV2(0, 0, width, height));
  p^.error := True;
  p^.Data1 := Data1;
  p^.Data2 := Data2;
  FList.Add(p);
end;

procedure TRectPacking.Add(Data1: Pointer; Data2: TCoreClassObject; r: TRectV2);
begin
  Add(Data1, Data2, 0, 0, RectWidth(r), RectHeight(r));
end;

procedure TRectPacking.Add(Data1: Pointer; Data2: TCoreClassObject; width, height: TGeoFloat);
begin
  Add(Data1, Data2, 0, 0, width, height);
end;

function TRectPacking.Data1Exists(const Data1: Pointer): Boolean;
var
  i: TGeoInt;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if (PRectPackData(FList[i])^.Data1 = Data1) then
        exit;
  Result := False;
end;

function TRectPacking.Data2Exists(const Data2: TCoreClassObject): Boolean;
var
  i: TGeoInt;
begin
  Result := True;
  for i := 0 to FList.Count - 1 do
    if (PRectPackData(FList[i])^.Data2 = Data2) then
        exit;
  Result := False;
end;

procedure TRectPacking.Build(SpaceWidth, SpaceHeight: TGeoFloat);

  function Compare_(Left, Right: Pointer): TGeoInt;
  begin
    Result := CompareValue(RectArea(PRectPackData(Right)^.Rect), RectArea(PRectPackData(Left)^.Rect));
  end;

  procedure fastSort_(var arry_: TCoreClassPointerList; l, r: TGeoInt);
  var
    i, j: TGeoInt;
    p: Pointer;
  begin
    repeat
      i := l;
      j := r;
      p := arry_[(l + r) shr 1];
      repeat
        while Compare_(arry_[i], p) < 0 do
            inc(i);
        while Compare_(arry_[j], p) > 0 do
            dec(j);
        if i <= j then
          begin
            if i <> j then
                Swap(arry_[i], arry_[j]);
            inc(i);
            dec(j);
          end;
      until i > j;
      if l < j then
          fastSort_(arry_, l, j);
      l := i;
    until i >= r;
  end;

var
  newLst: TRectPacking;
  p: PRectPackData;
  i: TGeoInt;
  X, Y, w, h: TGeoFloat;
begin
  if FList.Count > 1 then
      fastSort_(FList.ListData^, 0, Count - 1);

  newLst := TRectPacking.Create;
  newLst.Add(0, 0, SpaceWidth, SpaceHeight);
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];

      X := p^.Rect[0, 0];
      Y := p^.Rect[0, 1];

      w := RectWidth(p^.Rect);
      h := RectHeight(p^.Rect);

      p^.error := not newLst.Pack(w + Margins, h + Margins, X, Y);

      if not p^.error then
          p^.Rect := MakeRectV2(X, Y, X + w, Y + h);
    end;

  MaxWidth := newLst.MaxWidth;
  MaxHeight := newLst.MaxHeight;

  DisposeObject(newLst);
end;

procedure TRectPacking.Build;
var
  i: TGeoInt;
  p: PRectPackData;
  w, h: TGeoFloat;
begin
  w := 0;
  h := 0;
  for i := 0 to FList.Count - 1 do
    begin
      p := FList[i];
      w := w + RectWidth(p^.Rect) + Margins + 10;
      h := h + RectHeight(p^.Rect) + Margins + 10;
    end;
  Build(w, h);
end;

procedure THausdorf.NewNode(var p: PNode);
begin
  new(p);
  NodeList.Add(p);
end;

procedure THausdorf.NewLink(var p: PLinkedList);
begin
  new(p);
  LinkList.Add(p);
end;

procedure THausdorf.wrapVector(var wrapper: PNode; const X, Y: TGeoFloat);
begin
  NewNode(wrapper);
  wrapper^.Prev := nil;
  wrapper^.Next := nil;
  wrapper^.Data[0] := X;
  wrapper^.Data[1] := Y;
end;

procedure THausdorf.initList(var target: PLinkedList);
begin
  NewLink(target);
  target^.Head := nil;
  target^.Tail := nil;
  target^.Num := 0;
  target^.Looped := False;
end;

procedure THausdorf.initAndReadPolygon(var target: PLinkedList; const Source: TVec2List);
var
  i: TGeoInt;
begin
  initList(target);
  for i := 0 to Source.Count - 1 do
      addTo(target, Source[i]^);
  loopTheList(target);
end;

function THausdorf.get(var target: PLinkedList; const n_: TGeoInt): PNode;
var
  curNode: PNode;
  n, i: TGeoInt;
begin
  if (n_ > target^.Num) and (not target^.Looped) then
      Result := nil
  else
    begin
      n := n_ mod target^.Num;
      curNode := target^.Head;
      for i := 0 to n - 1 do
          curNode := curNode^.Next;
      Result := curNode;
    end;
end;

procedure THausdorf.getMax(var target, Source: PLinkedList);
var
  i: TGeoInt;
  max: TGeoFloat;
  curNode: PNode;
begin
  curNode := Source^.Head;
  max := Vec2Length(curNode^.Data);
  curNode := curNode^.Next;

  for i := 2 to Source^.Num do
    begin
      if (Vec2Length(curNode^.Data) > max) then
          max := Vec2Length(curNode^.Data);
      curNode := curNode^.Next;
    end;

  curNode := Source^.Head;

  for i := 1 to Source^.Num do
    begin
      if abs(Vec2Length(curNode^.Data) - max) <= FRoundKOEF then
          addTo(target, curNode^.Data);
      curNode := curNode^.Next;
    end;
end;

procedure THausdorf.getMin(var target, Source: PLinkedList);
var
  i: TGeoInt;
  min: TGeoFloat;
  curNode: PNode;
begin
  curNode := Source^.Head;
  min := Vec2Length(curNode^.Data);
  curNode := curNode^.Next;

  for i := 2 to Source^.Num do
    begin
      if (Vec2Length(curNode^.Data) < min) then
          min := Vec2Length(curNode^.Data);
      curNode := curNode^.Next;
    end;

  curNode := Source^.Head;

  for i := 1 to Source^.Num do
    begin
      if abs(Vec2Length(curNode^.Data) - min) <= FRoundKOEF then
          addTo(target, curNode^.Data);
      curNode := curNode^.Next;
    end;
end;

procedure THausdorf.addNodeTo(var target: PLinkedList; const item: PNode);
begin
  if target^.Tail = nil then
    begin
      target^.Head := item;
      target^.Tail := item;
      target^.Num := 1;
    end
  else
    begin
      target^.Tail^.Next := item;
      item^.Prev := target^.Tail;
      target^.Tail := item;
      target^.Num := target^.Num + 1;
    end;
end;

procedure THausdorf.addTo(var target: PLinkedList; p: TVec2);
begin
  addTo(target, p[0], p[1]);
end;

procedure THausdorf.addTo(var target: PLinkedList; X, Y: TGeoFloat);
var
  curNode: PNode;
begin
  wrapVector(curNode, X, Y);
  addNodeTo(target, curNode);
end;

procedure THausdorf.addToQ(const target: PLinkedList; const p: TVec2);
var
  nd, current, pom: PNode;
begin
  { Wrap the node to the queue item container }
  NewNode(nd);
  nd^.Data := p;
  nd^.Next := nil;
  nd^.Prev := nil;

  if target^.Head = nil then
    begin
      target^.Head := nd;
      target^.Tail := nd;
      target^.Num := target^.Num + 1;
    end
  else { Walk along the queue until the place for the item is found. The item should be inserted before the 'current' item. We need to be careful in the situations when the 'current' item is the last one in the queue though. }
    begin
      current := target^.Head;
      while (Compare(nd^.Data, current^.Data) = -1) and (current^.Next <> nil) do
          current := current^.Next;

      if current = target^.Head then
        begin
          if Compare(nd^.Data, current^.Data) = 1 then
            begin
              nd^.Next := target^.Head;
              target^.Head^.Prev := nd;
              target^.Head := nd;
              target^.Num := target^.Num + 1;
            end
          else
            begin
              current^.Next := nd;
              nd^.Prev := current;
              target^.Num := target^.Num + 1;
            end;
        end
      else if Compare(nd^.Data, current^.Data) = 1 then
        begin
          nd^.Next := current;
          nd^.Prev := current^.Prev;
          current^.Prev := nd;

          pom := nd^.Prev;

          pom^.Next := nd;
          target^.Num := target^.Num + 1;
        end
      else
        begin
          current^.Next := nd;
          nd^.Prev := current;
          target^.Num := target^.Num + 1;
        end;
    end;
end;

function THausdorf.Compare(const p1, p2: TVec2): TGeoInt;
var
  nP1, nP2: TVec2;
begin
  if (Quadrant(p1) > Quadrant(p2)) then
    begin
      Result := -1;
      exit
    end
  else if (Quadrant(p1) < Quadrant(p2)) then
    begin
      Result := 1;
      exit
    end
  else
    begin
      nP1 := normalise(p1);
      nP2 := normalise(p2);

      if (Quadrant(nP1) = 1) or (Quadrant(nP1) = 2) then
        begin
          if (nP1[0] < nP2[0]) then
            begin
              Result := -1;
              exit;
            end
          else if (nP1[0] < nP2[0]) then
            begin
              Result := 1;
              exit;
            end
          else
            begin
              Result := 0;
              exit;
            end;
        end
      else
        begin
          if (nP1[0] > nP2[0]) then
            begin
              Result := -1;
              exit;
            end
          else if (nP1[0] > nP2[0]) then
            begin
              Result := 1;
              exit;
            end
          else
            begin
              Result := 0;
              exit;
            end;
        end;
    end;
end;

function THausdorf.contains(const pol: PLinkedList; const p: TVec2): Boolean;
var
  ab, bc, ap, bp: TVec2;
  pr1, pr2: TGeoFloat;
  i: TGeoInt;
  curNode: PNode;
begin
  if (pol^.Num < 2) then
    begin
      Result := False;
      exit;
    end;

  curNode := pol^.Head;
  bc := Vec2Sub(curNode^.Next^.Data, curNode^.Data);
  bp := Vec2Sub(p, curNode^.Data);
  pr2 := pseudoScalarProduct(bp, bc);
  curNode := curNode^.Next;

  for i := 0 to pol^.Num - 1 do
    begin
      ab := bc;
      bc := Vec2Sub(curNode^.Next^.Data, curNode^.Data);
      ap := bp;
      bp := Vec2Sub(p, curNode^.Data);
      pr1 := pr2;
      pr2 := pseudoScalarProduct(bp, bc);

      curNode := curNode^.Next;

      if (abs(pr1) <= FRoundKOEF) and (abs(pr2) <= FRoundKOEF) then
        begin
          Result := True;
          exit;
        end

      else if (abs(pr1) <= FRoundKOEF) or (abs(pr2) <= FRoundKOEF) then
          Continue;

      if (pr1 * pr2 < 0) then
        begin
          Result := False;
          exit;
        end;
    end;
  Result := True;
end;

procedure THausdorf.deleteCopies(var target, Source: PLinkedList);
var
  i: TGeoInt;
  curNode: PNode;
begin
  curNode := Source^.Head;
  for i := 0 to Source^.Num - 1 do
    begin
      if (not isInList(target, curNode^.Data)) then
          addTo(target, curNode^.Data);
      curNode := curNode^.Next;
    end;
end;

procedure THausdorf.hausdorfDistanceVectors(var target, Polygon1_, Polygon2_: PLinkedList);
var
  pom1, pom2: PLinkedList;
  i: TGeoInt;
  curNode: PNode;
begin
  initList(pom1);
  polygonPolygonDistanceVectors(pom1, Polygon1_, Polygon2_);
  initList(pom2);
  getMax(pom2, pom1);
  initList(pom1);
  polygonPolygonDistanceVectors(pom1, Polygon2_, Polygon1_);

  curNode := pom1^.Head;
  for i := 1 to pom1^.Num do
    begin
      curNode^.Data := Vec2Negate(curNode^.Data);
      curNode := curNode^.Next;
    end;
  getMax(pom2, pom1);
  initList(pom1);
  getMax(pom1, pom2);
  deleteCopies(target, pom1);
end;

function THausdorf.isInList(const target: PLinkedList; const p: TVec2): Boolean;
var
  i: TGeoInt;
  curNode: PNode;
begin
  if (target^.Head = nil) then
    begin
      Result := False;
      exit;
    end;

  curNode := target^.Head;
  for i := 0 to target^.Num - 1 do
    begin
      if (abs(curNode^.Data[0] - p[0]) <= FRoundKOEF) and (abs(curNode^.Data[1] - p[1]) <= FRoundKOEF) then
        begin
          Result := True;
          exit;
        end;
      curNode := curNode^.Next;
    end;

  Result := False;
end;

function THausdorf.isOptimal(var distVecs: PLinkedList): Boolean;
var
  pom: PLinkedList;
  Zero: TVec2;
begin
  if distVecs^.Num = 1 then
      Result := Vec2Length(distVecs^.Head^.Data) <= FRoundKOEF
  else if distVecs^.Num = 2 then
      Result := (pseudoScalarProduct(distVecs^.Head^.Data, distVecs^.Head^.Next^.Data) <= 0) and
      (abs(pseudoScalarProduct(normalise(distVecs^.Head^.Data), normalise(distVecs^.Head^.Next^.Data))) <= FRoundKOEF)
  else
    begin
      initList(pom);
      sortByAngle(pom, distVecs);
      Zero[0] := 0;
      Zero[1] := 0;
      Result := contains(pom, Zero);
    end;
end;

procedure THausdorf.loopTheList(var target: PLinkedList);
begin
  target^.Tail^.Next := target^.Head;
  target^.Head^.Prev := target^.Tail;
  target^.Looped := True;
end;

procedure THausdorf.pointPolygonDistanceVectors(var target, pol: PLinkedList; const p: TVec2);
var
  i: TGeoInt;
  curNode: PNode;
begin
  if (contains(pol, p)) then
      addTo(target, 0, 0)
  else
    begin
      curNode := pol^.Head;
      for i := 1 to pol^.Num do
        begin
          addTo(target, pointSectionDistanceVector(curNode^.Data, curNode^.Next^.Data, p));
          curNode := curNode^.Next;
        end;
    end;
end;

function THausdorf.pointSectionDistanceVector(const a, b, p: TVec2): TVec2;
var
  ab, ap, bp: TVec2;
  falls1, falls2: Boolean;
  t: TGeoFloat;
  tAB: TVec2;
begin
  ab := Vec2Sub(b, a);
  ap := Vec2Sub(p, a);
  bp := Vec2Sub(p, b);

  falls1 := scalarProduct(ab, ap) > 0;
  falls2 := scalarProduct(Vec2Negate(ab), bp) > 0;

  if (not falls1) then
    begin
      Result := Vec2Negate(ap);
      exit;
    end;

  if (not falls2) then
    begin
      Result := Vec2Negate(bp);
      exit;
    end;

  t := scalarProduct(ab, ap) / scalarProduct(ab, ab);
  tAB := Vec2Mul(ab, t);
  tAB := Vec2Add(a, tAB);
  Result := Vec2Sub(tAB, p);
end;

procedure THausdorf.polygonPolygonDistanceVectors(var target, Polygon1_, Polygon2_: PLinkedList);
var
  i: TGeoInt;
  curNode: PNode;
  pom1: PLinkedList;
begin
  curNode := Polygon1_^.Head;

  for i := 0 to Polygon1_^.Num - 1 do
    begin
      initList(pom1);
      pointPolygonDistanceVectors(pom1, Polygon2_, curNode^.Data);
      getMin(target, pom1);
      curNode := curNode^.Next;
    end;
end;

function THausdorf.pseudoScalarProduct(const a, b: TVec2): TGeoFloat;
begin
  pseudoScalarProduct := (a[0] * b[1]) - (a[1] * b[0]);
end;

function THausdorf.Quadrant(const p: TVec2): TGeoInt;
begin
  Quadrant := 1;
  if (p[0] > 0) and (p[1] >= 0) then
      Quadrant := 1
  else if (p[0] <= 0) and (p[1] > 0) then
      Quadrant := 2
  else if (p[0] < 0) and (p[1] <= 0) then
      Quadrant := 3
  else if (p[0] >= 0) and (p[1] < 0) then
      Quadrant := 4;
end;

procedure THausdorf.sortByAngle(var target, Source: PLinkedList);
var
  i: TGeoInt;
  curNode: PNode;
begin
  curNode := Source^.Head;

  for i := 1 to Source^.Num do
    begin
      addToQ(target, curNode^.Data);
      curNode := curNode^.Next;
    end;

  target^.Tail := get(target, target^.Num - 1);
  loopTheList(target);
end;

function THausdorf.normalise(const vec: TVec2): TVec2;
var
  len: TGeoFloat;
begin
  len := Vec2Length(vec);
  Result[0] := vec[0] / len;
  Result[1] := vec[1] / len;
end;

function THausdorf.scalarProduct(const a, b: TVec2): TGeoFloat;
begin
  scalarProduct := (a[0] * b[0]) + (a[1] * b[1]);
end;

class function THausdorf.Compute(const poly1_, poly2_: TVec2List; const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat): TGeoFloat;
begin
  with THausdorf.Create(poly1_, poly2_, detail_, ROUND_KOEF) do
    begin
      Result := HausdorffDistance();
      Free;
    end;
end;

class function THausdorf.Compute(
  const poly1_: TVec2List; const poly1_b, poly1_e: Integer;
  const poly2_: TVec2List; const poly2_b, poly2_e: Integer;
  const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat): TGeoFloat;
var
  i: Integer;
  nP1, nP2: TVec2List;
begin
  nP1 := TVec2List.Create;
  for i := poly1_b to poly1_e do
      nP1.Add(poly1_[i]^);

  nP2 := TVec2List.Create;
  for i := poly2_b to poly2_e do
      nP2.Add(poly2_[i]^);

  with THausdorf.Create(nP1, nP2, detail_, ROUND_KOEF) do
    begin
      Result := HausdorffDistance();
      Free;
    end;
  DisposeObject(nP1);
  DisposeObject(nP2);
end;

constructor THausdorf.Create(const poly1_, poly2_: TVec2List; const detail_: TGeoInt; const ROUND_KOEF: TGeoFloat);
var
  np: TVec2List;
begin
  inherited Create;
  FPolygon1 := nil;
  FPolygon2 := nil;
  FOutput := nil;
  FRoundKOEF := if_(ROUND_KOEF <= 0, 0.0001, ROUND_KOEF);
  NodeList := TNodeList.Create;
  LinkList := TLinkList.Create;

  if detail_ > 0 then
    begin
      np := TVec2List.Create;
      poly1_.InterpolationTo(detail_, np);
      initAndReadPolygon(FPolygon1, np);
      DisposeObject(np);

      np := TVec2List.Create;
      poly2_.InterpolationTo(detail_, np);
      initAndReadPolygon(FPolygon2, np);
      DisposeObject(np);
    end
  else
    begin
      initAndReadPolygon(FPolygon1, poly1_);
      initAndReadPolygon(FPolygon2, poly2_);
    end;

  initList(FOutput);
  hausdorfDistanceVectors(FOutput, FPolygon1, FPolygon2);
end;

destructor THausdorf.Destroy;
var
  i: TGeoInt;
begin
  for i := 0 to NodeList.Count - 1 do
      Dispose(NodeList[i]);
  DisposeObject(NodeList);

  for i := 0 to LinkList.Count - 1 do
      Dispose(LinkList[i]);
  DisposeObject(LinkList);

  inherited Destroy;
end;

function THausdorf.HausdorffReached: TArrayVec2;
var
  i: TGeoInt;
  curNode: PNode;
begin
  SetLength(Result, FOutput^.Num);
  curNode := FOutput^.Head;
  for i := 0 to FOutput^.Num - 1 do
    begin
      Result[i] := curNode^.Data;
      curNode := curNode^.Next;
    end;
end;

function THausdorf.HausdorffDistance: TGeoFloat;
begin
  Result := Vec2Length(FOutput^.Head^.Data);
end;

function THausdorf.polygonsIsOptimal: Boolean;
begin
  Result := isOptimal(FOutput);
end;

class procedure THausdorf.TestAndPrint(const poly1_, poly2_: TVec2List);
var
  buff: TArrayVec2;
  v2: TVec2;
begin
  with THausdorf.Create(poly1_, poly2_, 0, 0.0001) do
    begin
      DoStatus('The distance is reached on the following vectors:');
      buff := HausdorffReached();
      for v2 in buff do
          DoStatus('%f %f', [v2[0], v2[1]]);
      SetLength(buff, 0);

      DoStatus('Hausdorff distance: %f', [HausdorffDistance]);
      DoStatus('The mutual position of the polygons is optimal: %s', [umlBoolToStr(polygonsIsOptimal).Text]);
      Free;
    end;
end;

class procedure THausdorf.Test1;
var
  vl1, vl2: TVec2List;
begin
  vl1 := TVec2List.Create;
  vl1.Add(2, 1);
  vl1.Add(-2, 1);
  vl1.Add(-2, -1);
  vl1.Add(2, -1);

  vl2 := TVec2List.Create;
  vl2.Add(1, 2);
  vl2.Add(-1, 2);
  vl2.Add(-1, -2);
  vl2.Add(1, -2);

  TestAndPrint(vl1, vl2);

  DisposeObject(vl1);
  DisposeObject(vl2);
end;

class procedure THausdorf.Test2;
var
  vl1, vl2: TVec2List;
begin
  vl1 := TVec2List.Create;
  vl1.Add(0, 200);
  vl1.Add(86.6025403, 50);
  vl1.Add(173.2050807, 200);

  vl2 := TVec2List.Create;
  vl2.Add(213.3974597, 550.0);
  vl2.Add(386.6025403, 550.0);
  vl2.Add(300.0, 700.0);

  TestAndPrint(vl1, vl2);

  DisposeObject(vl1);
  DisposeObject(vl2);
end;

function ArrayVec2(const r: TRectV2): TArrayVec2;
begin
  SetLength(Result, 4);
  Result[0] := PointMake(r[0, 0], r[0, 1]);
  Result[1] := PointMake(r[1, 0], r[0, 1]);
  Result[2] := PointMake(r[1, 0], r[1, 1]);
  Result[3] := PointMake(r[0, 0], r[1, 1]);
end;

function ArrayVec2(const r: TV2Rect4): TArrayVec2;
begin
  SetLength(Result, 4);
  Result[0] := r.LeftTop;
  Result[1] := r.RightTop;
  Result[2] := r.RightBottom;
  Result[3] := r.LeftBottom;
end;

function ArrayVec2(const l: TLineV2): TArrayVec2;
begin
  SetLength(Result, 2);
  Result[0] := l[0];
  Result[1] := l[1];
end;

function ArrayVec2(const t: TTriangle): TArrayVec2;
begin
  SetLength(Result, 3);
  Result[0] := t[0];
  Result[1] := t[1];
  Result[2] := t[2];
end;

end.
