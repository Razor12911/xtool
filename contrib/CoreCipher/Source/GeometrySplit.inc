const
  cZero2D: TPoing2D_ = (X: 0; Y: 0);
  // Default guaranteed miminum angle for quality mesh generator
  cDefaultMinimumAngle: Double = 30; // limited on 41.4 degrees and lower;
  // Default minimum segment length
  cDefaultMinimumSegmentLength: Double = 0.5;
  cOneThird: Double = 1 / 3;
  // Default precision when triangulating
  cDefaultTriangulationPrecision = 1E-3;

  // raise exception message
  sAddingNonUniqueObject = 'Adding non-unique object to list is not allowed';
  sListMustBeSorted = 'List must be sorted';
  sInvalidTriangleForSegment = 'Invalid triangle for segment';
  sTriangleVertexHookupError = 'triangle-vertex hookup error';
  sNoTriangleForVertex = 'No triangle for vertex';
  sCrossSegmentIntersectionError = 'CrossSegment-Intersection Error';

function CompareCardinal(C1, C2: Cardinal): integer;
begin
  if C1 < C2 then
      Result := -1
  else
    if C1 > C2 then
      Result := 1
  else
      Result := 0;
end;

function CompareInteger(Int1, Int2: integer): integer;
begin
  if Int1 < Int2 then
      Result := -1
  else
    if Int1 > Int2 then
      Result := 1
  else
      Result := 0;
end;

function CompareInt64(const Int1, Int2: int64): integer;
begin
  if Int1 < Int2 then
      Result := -1
  else
    if Int1 > Int2 then
      Result := 1
  else
      Result := 0;
end;

function ComparePointer(Item1, Item2: pointer): integer;
begin
  if integer(Item1) < integer(Item2) then
      Result := -1
  else
    if integer(Item1) > integer(Item2) then
      Result := 1
  else
      Result := 0;
end;

function CompareBool(Bool1, Bool2: boolean): integer;
begin
  if Bool1 < Bool2 then
      Result := -1
  else
    if Bool1 > Bool2 then
      Result := 1
  else
      Result := 0;
end;

function CompareDouble(const Double1, Double2: Double): integer;
begin
  if Double1 < Double2 then
      Result := -1
  else
    if Double1 > Double2 then
      Result := 1
  else
      Result := 0;
end;

procedure TCustomObjectList.Append(AItem: TCoreClassObject);
begin
  Insert(Count, AItem);
end;

function TCustomSortedList.Add(AItem: TCoreClassObject): integer;
begin
  if Sorted then
    begin
      Find(AItem, Result);
      Insert(Result, AItem);
    end
  else
      Result := inherited Add(AItem);
end;

function TCustomSortedList.AddUnique(Item: TCoreClassObject; RaiseError: boolean): integer;
begin
  if Find(Item, Result) then
    begin
      if RaiseError then
          RaiseInfo(sAddingNonUniqueObject);
      Delete(Result);
    end;
  Insert(Result, Item);
end;

constructor TCustomSortedList.Create(AutoFreeObj_: boolean);
begin
  inherited Create(AutoFreeObj_);
  FSorted := true;
end;

function TCustomSortedList.Find(Item: TCoreClassObject; out Index: integer): boolean;
var
  AMin, AMax: integer;
begin
  Result := false;

  if Sorted then
    begin
      // Find position for insert - binary method
      Index := 0;
      AMin := 0;
      AMax := Count;
      while AMin < AMax do
        begin
          Index := (AMin + AMax) div 2;
          case DoCompare(Items[Index], Item) of
            - 1: AMin := Index + 1;
            0: begin
                Result := true;
                exit;
              end;
            1: AMax := Index;
          end;
        end;
      Index := AMin;
    end
  else
    begin
      // If not a sorted list, then find it with the IndexOf() method
      Index := IndexOf(Item);
      if Index >= 0 then
        begin
          Result := true;
          exit;
        end;
      // Not found: set it to Count
      Index := Count;
    end;
end;

procedure TCustomSortedList.FindMultiple(Item: TCoreClassObject; out AIndex, ACount: integer);
var
  IdxStart: integer;
  IdxClose: integer;
begin
  if not Sorted then
      RaiseInfo(sListMustBeSorted);

  ACount := 0;

  // Find one
  if not Find(Item, AIndex) then
      exit;

  // Check upward from item
  IdxStart := AIndex;
  while (IdxStart > 0) and (DoCompare(Items[IdxStart - 1], Item) = 0) do
      dec(IdxStart);

  // Check downward from item
  IdxClose := AIndex;
  while (IdxClose < Count - 1) and (DoCompare(Items[IdxClose + 1], Item) = 0) do
      inc(IdxClose);

  // Result
  AIndex := IdxStart;
  ACount := IdxClose - IdxStart + 1;
end;

procedure TCustomSortedList.SetSorted(AValue: boolean);
begin
  if AValue <> FSorted then
    begin
      FSorted := AValue;
      if FSorted then
          Sort;
    end;
end;

procedure TCustomSortedList.Sort;
// local
  procedure QuickSort(iLo, iHi: integer);
  var
    Lo, Hi, Mid: Integer;
  begin
    Lo := iLo;
    Hi := iHi;
    Mid := (Lo + Hi) div 2;
    repeat
      while DoCompare(Items[Lo], Items[Mid]) < 0 do
          inc(Lo);
      while DoCompare(Items[Hi], Items[Mid]) > 0 do
          dec(Hi);
      if Lo <= Hi then
        begin
          // Swap pointers;
          Exchange(Lo, Hi);
          if Mid = Lo then
              Mid := Hi
          else
            if Mid = Hi then
              Mid := Lo;
          inc(Lo);
          dec(Hi);
        end;
    until Lo > Hi;

    if Hi > iLo then
        QuickSort(iLo, Hi);

    if Lo < iHi then
        QuickSort(Lo, iHi);
  end;

// main
begin
  if Count > 1 then
    begin
      QuickSort(0, Count - 1);
    end;
  FSorted := true;
end;

function TSortedList.DoCompare(Item1, Item2: TCoreClassObject): integer;
begin
  if assigned(FOnCompare_) then
      Result := FOnCompare_(Item1, Item2, FCompareInfo)
  else if assigned(FCompareMethod) then
      Result := FCompareMethod(Item1, Item2, FCompareInfo)
  else
      Result := ComparePointer(Item1, Item2);
end;

function MakePoint2D(const AX, AY: Double): TPoing2D_;
begin
  Result.X := AX;
  Result.Y := AY;
end;

procedure AddPoint2D(const A, B: TPoing2D_; var Result: TPoing2D_);
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

function SquaredLength2D(const APoint: TPoing2D_): Double;
begin
  Result := Sqr(APoint.X) + Sqr(APoint.Y);
end;

function Length2D(const APoint: TPoing2D_): Double;
begin
  Result := Sqrt(SquaredLength2D(APoint));
end;

function PtsEqual2D(const A, B: TPoing2D_; const Eps: Double = 1E-12): boolean;
begin
  Result := (abs(A.X - B.X) + abs(A.Y - B.Y)) <= Eps;
end;

function SquaredDist2D(const A, B: TPoing2D_): Double;
begin
  Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;

function Dist2D(const A, B: TPoing2D_): Double;
begin
  Result := Sqrt(SquaredDist2D(A, B));
end;

function TaxicabDist2D(const A, B: TPoing2D_): Double;
begin
  Result := abs(A.X - B.X) + abs(A.Y - B.Y);
end;

function CrossProduct2D(const Vector1, Vector2: TPoing2D_): Double;
begin
  Result := Vector1.X * Vector2.Y - Vector1.Y * Vector2.X;
end;

function DotProduct2D(const Vector1, Vector2: TPoing2D_): Double;
begin
  Result := Vector1.X * Vector2.X + Vector1.Y * Vector2.Y;
end;

procedure NormalizeVector2D(var AVector: TPoing2D_);
var
  L: Double;
begin
  L := Length2D(AVector);

  if L > 0 then
    begin
      L := 1 / L;
      AVector.X := AVector.X * L;
      AVector.Y := AVector.Y * L;
    end
  else
    begin
      // Avoid division by zero, return unity vec along X
      AVector.X := 1;
      AVector.Y := 0;
    end;
end;

procedure SubstractPoint2D(const A, B: TPoing2D_; var Result: TPoing2D_);
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

function Delta2D(const A, B: TPoing2D_): TPoing2D_;
begin
  Result.X := B.X - A.X;
  Result.Y := B.Y - A.Y;
end;

function MidPoint2D(const A, B: TPoing2D_): TPoing2D_;
begin
  Result.X := (A.X + B.X) * 0.5;
  Result.Y := (A.Y + B.Y) * 0.5;
end;

procedure Interpolation2D(const P1, P2: TPoing2D_; const xf: Double; var Result: TPoing2D_);
var
  xf1: Double;
begin
  xf1 := 1 - xf;
  Result.X := P1.X * xf1 + P2.X * xf;
  Result.Y := P1.Y * xf1 + P2.Y * xf;
end;

function PointToLineDist2DSqr(const P, P1, P2: TPoing2D_): Double;
// Point-Line distance
var
  q: Double;
  Pq: TPoing2D_;
begin
  if PtsEqual2D(P1, P2) then
    begin
      // Point to point
      Result := SquaredDist2D(P, P1);
      exit;
    end;

  // Minimum
  q := ((P.X - P1.X) * (P2.X - P1.X) + (P.Y - P1.Y) * (P2.Y - P1.Y)) / (Sqr(P2.X - P1.X) + Sqr(P2.Y - P1.Y));

  // Limit q to 0 <= q <= 1
  if q < 0 then
      q := 0;

  if q > 1 then
      q := 1;

  // Distance
  Interpolation2D(P1, P2, q, Pq);
  Result := SquaredDist2D(P, Pq);
end;

function IntersectLines2D(const P1, P2, Q1, Q2: TPoing2D_; var R: TPoing2D_; var PosP, PosQ: Double; const Eps: Double = 1E-12): boolean;
var
  DeltaP, DeltaQ: TPoing2D_;
  Num, Py, Px, DenP, DenQ: Double;
begin
  Result := false;

  // Check numerator
  DeltaP := Delta2D(P1, P2);
  DeltaQ := Delta2D(Q1, Q2);
  Num := DeltaQ.Y * DeltaP.X - DeltaQ.X * DeltaP.Y;
  if abs(Num) < Eps then
      exit;

  // Denominators
  Result := true;
  Px := P1.X - Q1.X;
  Py := P1.Y - Q1.Y;
  DenP := DeltaQ.X * Py - DeltaQ.Y * Px;
  DenQ := DeltaP.X * Py - DeltaP.Y * Px;
  PosP := DenP / Num;
  PosQ := DenQ / Num;

  // intersection point
  Interpolation2D(P1, P2, PosP, R);
end;

procedure MirrorPointInLine(const P, Base, Dir: TPoing2D_; out R: TPoing2D_);
// Mirror point P into the line with basepoint Base and direction Dir, put
// result in R. Dir *must* be normalized.
var
  Pb, Pp: TPoing2D_;
  Frac: Double;
begin
  // P relative to base
  SubstractPoint2D(P, Base, Pb);

  // find projection on line
  Frac := DotProduct2D(Pb, Dir);
  Pp.X := Base.X + Dir.X * Frac;
  Pp.Y := Base.Y + Dir.Y * Frac;

  // Result is reflection of this point
  R.X := Pp.X * 2 - P.X;
  R.Y := Pp.Y * 2 - P.Y;
end;

function AngleBetweenVectors(A, B: TPoing2D_): Double;
// Returns the angle between vector A and B in radians
var
  C, S: Double;
begin
  // Normalize A and B
  NormalizeVector2D(A);
  NormalizeVector2D(B);

  // Dotproduct = cosine, crossproduct = sine
  C := DotProduct2D(A, B);
  S := CrossProduct2D(A, B);

  // Sine = Y, Cosine = X, use arctan2 function
  Result := ArcTan2(S, C);
end;

function CircleFrom3PointsR2(const A, B, C: TPoing2D_; var Center: TPoing2D_; var R2, Den: Double): boolean;
var
  A1, A2: Double;
begin
  // Calculate circle center and radius (squared)
  Den := ((B.Y - C.Y) * (B.X - A.X) - (B.Y - A.Y) * (B.X - C.X)) * 2;
  A1 := (A.X + B.X) * (B.X - A.X) + (B.Y - A.Y) * (A.Y + B.Y);
  A2 := (B.X + C.X) * (B.X - C.X) + (B.Y - C.Y) * (B.Y + C.Y);

  // Make sure we don't divide by zero
  if abs(Den) > 1E-20 then
    begin

      Result := true;

      // Calculated circle center of circle through points a, b, c
      Center.X := (A1 * (B.Y - C.Y) - A2 * (B.Y - A.Y)) / Den;
      Center.Y := (A2 * (B.X - A.X) - A1 * (B.X - C.X)) / Den;

      // Squared radius of this circle
      R2 := SquaredDist2D(Center, A);

    end
  else
    begin

      // Co-linear, or close to it
      Result := false;

    end;
end;

function CircleFrom3Points(const A, B, C: TPoing2D_; var Center: TPoing2D_; var Radius: Double): boolean;
var
  R2, Den: Double;
begin
  Result := CircleFrom3PointsR2(A, B, C, Center, R2, Den);
  if Result then
      Radius := Sqrt(R2);
end;

function NormalFrom2Points_(const A, B: TPoing2D_): TPoing2D_;
// Create the normal of a line from A to B, this is the vector perpendicular to
// it (rotated 90 degrees clockwise), of unit length
var
  D: TPoing2D_;
begin
  D := Delta2D(A, B);
  // Turn 90 deg clockwise
  Result.X := D.Y;
  Result.Y := -D.X;
  NormalizeVector2D(Result);
end;

// Return the distance of P above the line through Base with Normal. If Dist is
// negative, P lies below the line.
function AboveBelowDist2D_(const Base, Normal, P: TPoing2D_): Double;
// Return the distance of P above the line through Base with Normal. If Dist is
// negative, P lies below the line.
begin
  Result := DotProduct2D(Normal, Delta2D(Base, P));
end;

function BetweenPointsTest2D_(const A, B, Point: TPoing2D_): integer;
var
  AB, D: TPoing2D_;
begin
  AB := Delta2D(A, B);
  D := Delta2D(A, Point);
  if DotProduct2D(D, AB) < 0 then
    begin
      Result := -1;
      exit;
    end;
  D := Delta2D(B, Point);
  if DotProduct2D(D, AB) > 0 then
    begin
      Result := 1;
      exit;
    end;
  Result := 0;
end;

procedure TVertex2D_.Assign(Source: TCoreClassPersistent);
begin
  if Source is TVertex2D_ then
    begin
      FPoint := TVertex2D_(Source).FPoint;
    end
  else
      inherited Assign(Source);
end;

constructor TVertex2D_.Create;
begin
  inherited Create;
end;

constructor TVertex2D_.CreateWithCoords(const AX, AY: Double);
begin
  Create;
  FPoint.X := AX;
  FPoint.Y := AY;
end;

function TVertex2D_.GetPoint: PPoing2D_;
begin
  Result := @FPoint;
end;

function TVertex2D_.GetX: Double;
begin
  Result := FPoint.X;
end;

function TVertex2D_.GetY: Double;
begin
  Result := FPoint.Y;
end;

procedure TVertex2D_.SetX(const Value: Double);
begin
  FPoint.X := Value;
end;

procedure TVertex2D_.SetY(const Value: Double);
begin
  FPoint.Y := Value;
end;

procedure TTriVertex2D_.Assign(Source: TCoreClassPersistent);
begin
  if Source is TTriVertex2D_ then
      FTriangle := TTriVertex2D_(Source).FTriangle;
  inherited Assign(Source);
end;

function TTriVertex2D_.GetTriangle: TTriangle2D_;
begin
  Result := FTriangle;
end;

procedure TTriVertex2D_.SetTriangle(const Value: TTriangle2D_);
begin
  FTriangle := Value;
end;

function TVertex2DList_.GetItems(Index: integer): TVertex2D_;
begin
  Result := inherited Items[index] as TVertex2D_;
end;

procedure TVertex2DList_.SetItems(Index: integer; const Value: TVertex2D_);
begin
  inherited Items[index] := Value;
end;

procedure TSegment2D_.Assign(Source: TCoreClassPersistent);
begin
  if Source is TSegment2D_ then
    begin
      FVertex1 := TSegment2D_(Source).FVertex1;
      FVertex2 := TSegment2D_(Source).FVertex2;
    end
  else
      inherited Assign(Source);
end;

procedure TSegment2D_.CalculateMetrics;
var
  R: Double;
begin
  if not assigned(FVertex1) or not assigned(FVertex2) then
    begin
      FCenter := cZero2D;
      FNormal := cZero2D;
      FSquaredEncroachRadius := 0;
    end
  else
    begin
      FCenter := MidPoint2D(FVertex1.FPoint, FVertex2.FPoint);
      R := Dist2D(FVertex1.FPoint, FCenter);
      // Take an encroach radius that is 10% bigger than the actual radius
      FSquaredEncroachRadius := Sqr(R * 1.1);
      // Normal
      FNormal := NormalFrom2Points_(FVertex1.FPoint, FVertex2.FPoint);
    end;
  FValidMetrics := true;
end;

constructor TSegment2D_.CreateWithVertices(AVertex1, AVertex2: TVertex2D_);
begin
  Create;
  FVertex1 := AVertex1;
  FVertex2 := AVertex2;
end;

function TSegment2D_.GetCenter: TPoing2D_;
begin
  if not FValidMetrics then
      CalculateMetrics;
  Result := FCenter;
end;

function TSegment2D_.GetNormal: TPoing2D_;
begin
  if not FValidMetrics then
      CalculateMetrics;
  Result := FNormal;
end;

function TSegment2D_.GetSquaredEncroachRadius: Double;
begin
  if not FValidMetrics then
      CalculateMetrics;
  Result := FSquaredEncroachRadius;
end;

function TSegment2D_.IntersectWith(ASegment: TSegment2D_): TVertex2D_;
var
  R: TPoing2D_;
  PosP, PosQ: Double;
begin
  Result := nil;
  if IntersectLines2D(
    FVertex1.FPoint, FVertex2.FPoint,
    ASegment.Vertex1.FPoint, ASegment.Vertex2.FPoint,
    R, PosP, PosQ) then
    begin
      if (PosP > 0) and (PosP < 1) and (PosQ > 0) and (PosQ < 1) then
        begin
          // OK we found an intersection, lying within both segments
          Result := TTriVertex2D_.CreateWithCoords(R.X, R.Y);
        end;
    end;
end;

procedure TSegment2D_.Invalidate;
begin
  FValidMetrics := false;
end;

function TSegment2D_.IsVertexOnSegment(AVertex: TVertex2D_; APrecisionSqr: Double): boolean;
begin
  Result := PointToLineDist2DSqr(AVertex.FPoint,
    FVertex1.FPoint, FVertex2.FPoint) <= APrecisionSqr;
end;

function TSegment2D_.PointEncroaches(const P: TPoing2D_): boolean;
var
  C: TPoing2D_;
begin
  C := GetCenter;
  Result := SquaredDist2D(C, P) < FSquaredEncroachRadius;
end;

procedure TSegment2D_.ReplaceVertex(OldVertex, NewVertex: TVertex2D_);
begin
  if FVertex1 = OldVertex then
      SetVertex1(NewVertex);
  if FVertex2 = OldVertex then
      SetVertex2(NewVertex);
end;

procedure TSegment2D_.SetVertex1(const Value: TVertex2D_);
begin
  FVertex1 := Value;
  FValidMetrics := false;
end;

procedure TSegment2D_.SetVertex2(const Value: TVertex2D_);
begin
  FVertex2 := Value;
  FValidMetrics := false;
end;

function TSegment2DList_.GetItems(Index: integer): TSegment2D_;
begin
  Result := inherited Items[index] as TSegment2D_;
end;

function TTriangle2D_.AngleCosine(Index: integer): Double;
var
  D1, D2: TPoing2D_;
begin
  Result := 0;
  if not(assigned(FVertices[0]) and assigned(FVertices[1]) and assigned(FVertices[2])) then
      exit;
  D1 := Delta2D(Vertices[Index].FPoint, Vertices[Index + 1].FPoint);
  D2 := Delta2D(Vertices[Index].FPoint, Vertices[Index + 2].FPoint);
  NormalizeVector2D(D1);
  NormalizeVector2D(D2);
  Result := DotProduct2D(D1, D2);
end;

function TTriangle2D_.Area: Double;
var
  Pa, Pb, Pc: PPoing2D_;
begin
  if assigned(FVertices[0]) and assigned(FVertices[1]) and assigned(FVertices[2]) then
    begin
      Pa := FVertices[0].Point;
      Pb := FVertices[1].Point;
      Pc := FVertices[2].Point;
      Result := CrossProduct2D(Delta2D(Pa^, Pb^), Delta2D(Pa^, Pc^)) * 0.5;
    end
  else
      Result := 0;
end;

procedure TTriangle2D_.CalculateMetrics;
var
  Pa, Pb, Pc: PPoing2D_;
begin
  if assigned(FVertices[0]) and assigned(FVertices[1]) and assigned(FVertices[2]) then
    begin
      Pa := FVertices[0].Point;
      Pb := FVertices[1].Point;
      Pc := FVertices[2].Point;
      // Center
      FCenter.X := (Pa^.X + Pb^.X + Pc^.X) * cOneThird;
      FCenter.Y := (Pa^.Y + Pb^.Y + Pc^.Y) * cOneThird;
      // Normals
      FNormals[0] := NormalFrom2Points_(Pa^, Pb^);
      FNormals[1] := NormalFrom2Points_(Pb^, Pc^);
      FNormals[2] := NormalFrom2Points_(Pc^, Pa^);
    end;
  // Set flag
  FValidMetrics := true;
end;

constructor TTriangle2D_.Create;
begin
  inherited Create;
  FRegionIndex := -1;
end;

function TTriangle2D_.EdgeFromCenterTowardsPoint(const APoint: TPoing2D_): integer;
var
  i: integer;
  C, Delta: TPoing2D_;
  CP1, CP2: Double;
begin
  Result := -1;
  if not(assigned(FVertices[0]) and assigned(FVertices[1]) and assigned(FVertices[2])) then
      exit;
  C := GetCenter;
  Delta := Delta2D(C, APoint);
  CP2 := CrossProduct2D(Delta2D(C, Vertices[0].FPoint), Delta);
  for i := 0 to 2 do
    begin
      CP1 := CP2;
      CP2 := CrossProduct2D(Delta2D(C, Vertices[i + 1].FPoint), Delta);
      if (CP1 >= 0) and (CP2 < 0) then
        begin
          Result := i;
          exit;
        end;
    end;
end;

function TTriangle2D_.GetCenter: TPoing2D_;
begin
  if not FValidMetrics then
      CalculateMetrics;
  Result := FCenter;
end;

function TTriangle2D_.GetNeighbours(Index: integer): TTriangle2D_;
begin
  Result := FNeighbours[Index mod 3];
end;

function TTriangle2D_.GetSegments(Index: integer): TSegment2D_;
begin
  Result := nil;
end;

function TTriangle2D_.GetVertices(Index: integer): TVertex2D_;
begin
  Result := FVertices[Index mod 3];
end;

function TTriangle2D_.HitTest(const APoint: TPoing2D_): THitTestTriangle_;
var
  i, Res: integer;
  P: array [0 .. 2] of PPoing2D_;
  Tol, TolSqr, TolOut, Dist: Double;
begin
  Result := httNone;

  if not(assigned(FVertices[0]) and assigned(FVertices[1]) and assigned(FVertices[2])) then
      exit;

  if not FValidMetrics then
      CalculateMetrics;
  Tol := FMesh.FPrecision;
  TolSqr := FMesh.FPrecisionSqr;
  TolOut := Tol * 1E-3;

  // Sides check to determine insideness
  P[0] := FVertices[0].Point;
  P[1] := FVertices[1].Point;
  P[2] := FVertices[2].Point;

  // Check first side
  for i := 0 to 2 do
    begin
      Dist := AboveBelowDist2D_(P[i]^, FNormals[i], APoint);
      // More than TolOut away.. this point is outside this triangle
      if Dist > TolOut then
          exit;

      if abs(Dist) <= Tol then
        begin
          // Possibly on this line: check endpoints
          if SquaredDist2D(P[i]^, APoint) < TolSqr then
            begin
              // Yes on first vertex
              case i of
                0: Result := httVtx0;
                1: Result := httVtx1;
                2: Result := httVtx2;
              end;
              exit;
            end;
          if SquaredDist2D(P[(i + 1) mod 3]^, APoint) < TolSqr then
            begin
              // Yes on second vertex
              case i of
                0: Result := httVtx1;
                1: Result := httVtx2;
                2: Result := httVtx0;
              end;
              exit;
            end;
          // determine if between two vertices
          Res := BetweenPointsTest2D_(P[i]^, P[(i + 1) mod 3]^, APoint);
          if Res = 0 then
            begin
              // Indeed, between the vertices
              if Dist <= 0 then
                begin
                  case i of
                    0: Result := httEdge0;
                    1: Result := httEdge1;
                    2: Result := httEdge2;
                  end;
                end
              else
                begin
                  case i of
                    0: Result := httClose0;
                    1: Result := httClose1;
                    2: Result := httClose2;
                  end;
                end;
              exit;
            end;
        end;
    end;

  // Arriving here means inside
  Result := httBody;

end;

procedure TTriangle2D_.HookupNeighbours(TriangleA, TriangleB, TriangleC: TTriangle2D_);
begin
  FNeighbours[0] := TriangleA;
  FNeighbours[1] := TriangleB;
  FNeighbours[2] := TriangleC;
end;

procedure TTriangle2D_.HookupVertices(VertexA, VertexB, VertexC: TVertex2D_);
begin
  SetVertices(0, VertexA);
  SetVertices(1, VertexB);
  SetVertices(2, VertexC);
end;

procedure TTriangle2D_.Invalidate;
begin
  FValidMetrics := false;
end;

procedure TTriangle2D_.InvalidateSegments;
var
  i: integer;
  S: TSegment2D_;
begin
  for i := 0 to 2 do
    begin
      S := Segments[i];
      if assigned(S) then
          S.Invalidate;
    end;
end;

function TTriangle2D_.NeighbourIndex(ATriangle: TTriangle2D_): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to 2 do
    if FNeighbours[i] = ATriangle then
      begin
        Result := i;
        exit;
      end;
end;

procedure TTriangle2D_.ReplaceNeighbour(OldNeighbour, NewNeighbour: TTriangle2D_);
var
  Idx: integer;
begin
  Idx := NeighbourIndex(OldNeighbour);
  if Idx >= 0 then
      FNeighbours[Idx] := NewNeighbour;
end;

function TTriangle2D_.SegmentIndex(ASegment: TSegment2D_): integer;
var
  i: integer;
begin
  for i := 0 to 2 do
    if Segments[i] = ASegment then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

procedure TTriangle2D_.SetNeighbours(Index: integer; const Value: TTriangle2D_);
begin
  FNeighbours[Index mod 3] := Value;
end;

procedure TTriangle2D_.SetSegments(Index: integer; const Value: TSegment2D_);
begin
  // Default does nothing
end;

procedure TTriangle2D_.SetVertices(Index: integer; const Value: TVertex2D_);
var
  Idx: integer;
begin
  Idx := Index mod 3;
  if FVertices[Idx] <> Value then
    begin
      Value.Triangle := Self;
      FVertices[Idx] := Value;
      FValidMetrics := false;
      InvalidateSegments;
    end;
end;

function TTriangle2D_.SmallestAngleCosine: Double;
var
  i: integer;
  D: array [0 .. 2] of TPoing2D_;
  ACos: Double;
begin
  Result := 0;
  if not(assigned(FVertices[0]) and assigned(FVertices[1]) and assigned(FVertices[2])) then
      exit;
  for i := 0 to 2 do
    begin
      D[i] := Delta2D(Vertices[i].FPoint, Vertices[i + 1].FPoint);
      NormalizeVector2D(D[i]);
    end;
  for i := 0 to 2 do
    begin
      ACos := abs(DotProduct2D(D[i], D[(i + 1) mod 3]));
      if ACos > Result then
          Result := ACos;
    end;
  if Result > 1 then
      Result := 1;
end;

function TTriangle2D_.SquaredLongestEdgeLength: Double;
var
  i: integer;
  L: Double;
begin
  Result := 0;
  for i := 0 to 2 do
    begin
      L := SquaredDist2D(Vertices[i].FPoint, Vertices[i + 1].FPoint);
      if L > Result then
          Result := L;
    end;
end;

function TTriangle2D_.VertexIndex(AVertex: TVertex2D_): integer;
var
  i: integer;
begin
  for i := 0 to 2 do
    if FVertices[i] = AVertex then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

function TTriangle2DList_.GetItems(Index: integer): TTriangle2D_;
begin
  Result := inherited Items[index] as TTriangle2D_;
end;

procedure TTriangleGroup2D_.AddTriangleAndEdge(ATriangle: TTriangle2D_; AEdge: integer);
begin
  // adjust capacity
  if FCount >= FCapacity then
    begin
      FCapacity := FCount * 3 div 2 + 4;
      SetLength(FTriangles, FCapacity);
      SetLength(FEdges, FCapacity);
    end;
  FTriangles[FCount] := ATriangle;
  FEdges[FCount] := AEdge mod 3;
  inc(FCount);
end;

procedure TTriangleGroup2D_.Clear;
begin
  FCount := 0;
end;

procedure TTriangleGroup2D_.Delete(AIndex: integer);
var
  i: integer;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
      exit;
  for i := AIndex to FCount - 2 do
    begin
      FTriangles[i] := FTriangles[i + 1];
      FEdges[i] := FEdges[i + 1];
    end;
  dec(FCount);
end;

procedure TTriangleGroup2D_.Exchange(Index1, Index2: integer);
var
  T: TTriangle2D_;
  E: integer;
begin
  if (Index1 < 0) or (Index1 >= FCount) or
    (Index2 < 0) or (Index2 >= FCount) then
      exit;
  T := FTriangles[Index1];
  FTriangles[Index1] := FTriangles[Index2];
  FTriangles[Index2] := T;
  E := FEdges[Index1];
  FEdges[Index1] := FEdges[Index2];
  FEdges[Index2] := E;
end;

function TTriangleGroup2D_.GetEdges(Index: integer): integer;
begin
  Result := FEdges[Index mod FCount];
end;

function TTriangleGroup2D_.GetTriangles(Index: integer): TTriangle2D_;
begin
  Result := FTriangles[Index mod FCount];
end;

procedure TTriangleGroup2D_.InsertTriangleAndEdge(AIndex: integer;
  ATriangle: TTriangle2D_; AEdge: integer);
var
  i: integer;
begin
  // adjust capacity
  if FCount >= FCapacity then
    begin
      FCapacity := FCount * 3 div 2 + 4;
      SetLength(FTriangles, FCapacity);
      SetLength(FEdges, FCapacity);
    end;
  // Move up above index position
  for i := FCount downto AIndex + 1 do
    begin
      FTriangles[i] := FTriangles[i - 1];
      FEdges[i] := FEdges[i - 1];
    end;
  // Insert at index position
  FTriangles[AIndex] := ATriangle;
  FEdges[AIndex] := AEdge mod 3;
  inc(FCount);
end;

procedure TTriangleGroup2D_.SetEdges(Index: integer; const Value: integer);
begin
  FEdges[Index mod FCount] := Value;
end;

{ TTriangleFan2D_ }

procedure TTriangleFan2D_.BuildTriangleFan(ABase: TTriangle2D_);
var
  Triangle: TTriangle2D_;
  Idx: integer;
begin
  // Reset count
  FCount := 0;
  Triangle := ABase;
  // scan anti-clockwise around center
  repeat
    Idx := Triangle.VertexIndex(FCenter);
    if Idx < 0 then
        RaiseInfo(sTriangleVertexHookupError);
    // add at end of list.. first one to be inserted is ABase, then any others
    // in anti-clockwise direction around center
    AddTriangleAndEdge(Triangle, Idx);
    // next triangle
    Triangle := Triangle.Neighbours[Idx + 2];
  until (Triangle = ABase) or (Triangle = nil);
  if Triangle = nil then
    begin
      // in case we hit a border (no neighbours): we also scan clockwise from
      // base, and insert before rest of items. This will usually only happen
      // for meshes with holes or vertices at the borders of the mesh
      Idx := ABase.VertexIndex(FCenter);
      Triangle := ABase.Neighbours[Idx];
      while Triangle <> nil do
        begin
          Idx := Triangle.VertexIndex(FCenter);
          // insert at first position in list
          InsertTriangleAndEdge(0, Triangle, Idx);
          Triangle := Triangle.Neighbours[Idx];
        end;
    end;
end;

procedure TTriangleFan2D_.Clear;
begin
  inherited Clear;
  FCenter := nil;
end;

function TTriangleFan2D_.GetVertices(Index: integer): TVertex2D_;
var
  Idx: integer;
begin
  Idx := Index mod Count;
  Result := FTriangles[Idx].Vertices[FEdges[Idx] + 1];
end;

procedure TTriangleFan2D_.MoveToVertexAt(AIndex: integer);
var
  Vertex: TVertex2D_;
begin
  Vertex := Vertices[AIndex];
  FCenter := Vertex;
  BuildTriangleFan(FTriangles[AIndex]);
end;

procedure TTriangleFan2D_.SetCenter(const Value: TVertex2D_);
var
  Base: TTriangle2D_;
begin
  FCenter := Value;
  if not assigned(Value) then
      exit;
  // to do: build triangle list
  Base := Value.Triangle;
  if not assigned(Base) then
      RaiseInfo(sNoTriangleForVertex);
  BuildTriangleFan(Base);
end;

function TTriangleFan2D_.TriangleIdxInDirection(const APoint: TPoing2D_): integer;
var
  CP1, CP2: Double;
  DeltaVP: TPoing2D_;
begin
  Result := -1;
  if FCount = 0 then
      exit;
  Result := 0;
  DeltaVP := Delta2D(FCenter.FPoint, APoint);
  CP2 := CrossProduct2D(
    // Edge at bottom side
    Delta2D(FCenter.FPoint, FTriangles[0].Vertices[FEdges[0] + 1].FPoint),
    // From center to vertex point
    DeltaVP);
  repeat
    CP1 := CP2;
    CP2 := CrossProduct2D(
      // Edge at top side
      Delta2D(FCenter.FPoint, FTriangles[Result].Vertices[FEdges[Result] + 2].FPoint),
      // From center to vertex point
      DeltaVP);

    // For CP1 we use "greater than or equal" and for CP2 "smaller than", this way
    // at least one of them should return the favour, even within machine precision
    if (CP1 >= 0) and (CP2 < 0) then
      // point lies above or on bottom edge, and below top edge, so this is the
      // triangle we are looking for
        exit;
    inc(Result);
  until Result = FCount;
  // arriving here means we didn't find it.. this is possible for borders
  Result := -1;
end;

function TTriangleFan2D_.TriangleInDirection(const APoint: TPoing2D_): TTriangle2D_;
var
  Idx: integer;
begin
  Idx := TriangleIdxInDirection(APoint);
  if Idx < 0 then
      Result := nil
  else
      Result := FTriangles[Idx];
end;

function TTriangleFan2D_.VertexIndex(AVertex: TVertex2D_): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if GetVertices(i) = AVertex then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

function TTriangleChain2D_.BuildChain(AVertex1, AVertex2: TVertex2D_; var ASearchFan: TTriangleFan2D_): boolean;
var
  Idx, Edge: integer;
  Fan: TTriangleFan2D_;
  Triangle, Previous: TTriangle2D_;
  Vertex: TVertex2D_;
  Delta12: TPoing2D_;
begin
  Result := false;
  FVertex1 := AVertex1;
  FVertex2 := AVertex2;
  Clear;
  if not assigned(FVertex1) or not assigned(FVertex2) then
      exit;
  if FVertex1 = FVertex2 then
    begin
      Result := true;
      exit;
    end;

  // Searchfan to use
  if assigned(ASearchFan) then
      Fan := ASearchFan
  else
      Fan := TTriangleFan2D_.Create;
  try

    Fan.Center := FVertex1;
    Idx := Fan.VertexIndex(FVertex2);
    if Idx >= 0 then
      begin
        // Goody goody, we can stop because we directly found *one* triangle connecting
        // the two vertices
        AddTriangleAndEdge(Fan.Triangles[Idx], Fan.OutwardEdges[Idx] + 1);
        Result := true;
        exit;
      end;

    // No direct one, so we locate the triangle in the direction of Vertex 2
    Idx := Fan.TriangleIdxInDirection(FVertex2.FPoint);

    // If there's none, we're doomed.. we cannot build the chain
    if Idx < 0 then
        exit;

    Delta12 := Delta2D(FVertex1.FPoint, FVertex2.FPoint);

    // First triangle and edge
    Triangle := Fan.Triangles[Idx];
    Edge := (Fan.OutwardEdges[Idx] + 1) mod 3;
    AddTriangleAndEdge(Triangle, Edge);

    // Now we repeat, and keep adding triangle/edge combi's until we found Vertex 2
    repeat
      // Move up one triangle, taking the neighbour on offending edge
      Previous := Triangle;
      Triangle := Previous.Neighbours[Edge];

      // No triangle neighbour? We're doomed..
      if not assigned(Triangle) then
          exit;

      // The edge on the new triangle that is offending
      Edge := Triangle.NeighbourIndex(Previous);

      // The vertex opposite of this one might be our end vertex
      Vertex := Triangle.Vertices[Edge + 2];
      if Vertex = FVertex2 then
        begin
          // Yep! We found the end of the chain
          AddTriangleAndEdge(Triangle, Edge + 2);
          Result := true;
          exit;
        end;

      // On which side of the line is this opposite vertex? This way we determine
      // the offending edge
      if CrossProduct2D(Delta12, Delta2D(FVertex1.FPoint, Vertex.FPoint)) < 0 then
        begin
          // On right side, so the next offending edge is two vertices away
          Edge := Edge + 2;
        end
      else
        begin
          // On left side, so next offending edge is one vertex away
          Edge := Edge + 1;
        end;

      // Now we add the triangle, and the edge that is offending
      AddTriangleAndEdge(Triangle, Edge);

    until false;

  finally
    // Free searchfan if temporary
    if Fan <> ASearchFan then
        DisposeObject(Fan);
  end;
end;

{ TTriMesh2D_ }

function TTriMesh2D_.AbsoluteArea: Double;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Triangles.Count - 1 do
      Result := Result + abs(Triangles[i].Area);
end;

function TTriMesh2D_.BoundingBox(var AMin, AMax: TPoing2D_): boolean;
var
  i: integer;
  P: PPoing2D_;
begin
  if FVertices.Count > 0 then
    begin
      AMin := FVertices[0].Point^;
      AMax := FVertices[0].Point^;
      for i := 1 to FVertices.Count - 1 do
        begin
          P := FVertices[i].Point;
          if P^.X < AMin.X then
              AMin.X := P^.X;
          if P^.X > AMax.X then
              AMax.X := P^.X;
          if P^.Y < AMin.Y then
              AMin.Y := P^.Y;
          if P^.Y > AMax.Y then
              AMax.Y := P^.Y;
        end;
      Result := true;
    end
  else
    begin
      AMin := cZero2D;
      AMax := cZero2D;
      Result := false;
    end;
end;

procedure TTriMesh2D_.Clear;
begin
  FVertices.Clear;
  FTriangles.Clear;
  FSegments.Clear;
  InitializeInfo;
end;

procedure TTriMesh2D_.ConvexHull;
var
  ch: TConvexHull_;
begin
  ch := TConvexHull_.Create;
  ch.MakeConvexHull(Self);
  DisposeObject(ch);
end;

constructor TTriMesh2D_.Create;
begin
  inherited Create;
  FVertices := TVertex2DList_.Create(true);
  FTriangles := TTriangle2DList_.Create(true);
  FSegments := TSegment2DList_.Create(true);
  SetPrecision(cDefaultTriangulationPrecision);
end;

destructor TTriMesh2D_.Destroy;
begin
  DisposeObject(FVertices);
  FVertices := nil;

  DisposeObject(FTriangles);
  FTriangles := nil;

  DisposeObject(FSegments);
  FSegments := nil;
  inherited Destroy;
end;

class function TTriMesh2D_.GetSegmentClass: TSegment2DClass_;
begin
  Result := TSegment2D_;
end;

class function TTriMesh2D_.GetTriangleClass: TTriangle2DClass_;
begin
  Result := TTriangle2D_;
end;

class function TTriMesh2D_.GetVertexClass: TVertex2DClass_;
begin
  Result := TTriVertex2D_;
end;

procedure TTriMesh2D_.InitializeInfo;
begin
  FSearchSteps := 0;
end;

function TTriMesh2D_.LocateClosestVertex(const APoint: TPoing2D_; AFan: TTriangleFan2D_): TVertex2D_;
var
  i, BestIndex: integer;
  Fan: TTriangleFan2D_;
  IsClosest: boolean;
  CenterDist, FanDist, BestDist: Double;
begin
  Result := nil;
  BestDist := 0;
  BestIndex := 0;
  if FTriangles.Count = 0 then
      exit;

  // Initialize triangle fan
  if assigned(AFan) then
      Fan := AFan
  else
      Fan := TTriangleFan2D_.Create;
  if Fan.Center = nil then
      Fan.Center := FTriangles[0].Vertices[0];

  // Do search.. we use the taxicab distance here, that's faster than squared
  // distance, and more stable numerically
  repeat
    IsClosest := true;
    inc(FSearchSteps);
    CenterDist := TaxicabDist2D(Fan.Center.Point^, APoint);
    for i := 0 to Fan.Count - 1 do
      begin
        FanDist := TaxicabDist2D(Fan.Vertices[i].Point^, APoint);
        if FanDist < CenterDist then
            IsClosest := false;
        if (i = 0) or (FanDist < BestDist) then
          begin
            BestDist := FanDist;
            BestIndex := i;
          end;
      end;
    if not IsClosest then
        Fan.MoveToVertexAt(BestIndex);
  until IsClosest;

  // Result
  Result := Fan.Center;

  // Finalize triangle fan
  if Fan <> AFan then
      DisposeObject(Fan);
end;

function TTriMesh2D_.NewSegment: TSegment2D_;
begin
  Result := GetSegmentClass.Create;
end;

function TTriMesh2D_.NewTriangle: TTriangle2D_;
begin
  Result := GetTriangleClass.Create;
  Result.FMesh := Self;
end;

function TTriMesh2D_.NewVertex: TVertex2D_;
begin
  Result := GetVertexClass.Create;
end;

procedure TTriMesh2D_.OptimizeForFEM(AVertices: TVertex2DList_);
var
  i, Idx, IdxSeed, IdxLowest: integer;
  SL: TSortedList;
  Seed, Connected: TTriangle2D_;
  ConnectedIdx: array [0 .. 2] of integer;
  V: TVertex2D_;
  // local
  procedure MoveForward;
  var
    i: integer;
  begin
    // Move triangle Seed to Idx
    IdxSeed := FTriangles.IndexOf(Seed);
    FTriangles.Exchange(Idx, IdxSeed);
    inc(Idx);
    // Any vertices used and not yet in array are added
    for i := 0 to 2 do
      begin
        V := Seed.Vertices[i];
        if assigned(V) and (AVertices.IndexOf(V) < 0) then
            AVertices.Add(V);
      end;
  end;

// main
begin
  AVertices.Clear;
  SL := TSortedList.Create(false);
  try
    // Sort the triangles by their left position
    SL.OnCompare := {$IFDEF FPC}@{$ENDIF FPC}TriangleCompareLeft;
    // Add all triangles to the sortlist
    for i := 0 to FTriangles.Count - 1 do
        SL.Add(FTriangles[i]);
    // Current swap index
    Idx := 0;
    Seed := nil;
    while SL.Count > 0 { Idx < FTriangles.Count - 2 } do
      begin
        // Seed triangle (the one we're working from)
        if not assigned(Seed) then
          begin
            Seed := TTriangle2D_(SL[0]);
            SL.Delete(0);
            MoveForward;
          end;

        // Find lowest connected triangle index in SL
        IdxLowest := FTriangles.Count;
        for i := 0 to 2 do
            ConnectedIdx[i] := SL.IndexOf(Seed.Neighbours[i]);
        for i := 0 to 2 do
          if (ConnectedIdx[i] >= 0) and (ConnectedIdx[i] < IdxLowest) then
              IdxLowest := ConnectedIdx[i];

        // Did we find a connected triangle still existing in our sl?
        if IdxLowest < FTriangles.Count then
            Connected := TTriangle2D_(SL[IdxLowest])
        else
            Connected := nil;

        // Do we have a connected triangle?
        if assigned(Connected) then
          begin
            // We have a connection.. do the exchange
            Seed := Connected;
            SL.Delete(IdxLowest);
            MoveForward;
          end
        else
          begin
            // No connection.. re-initialize seed
            Seed := nil;
          end;
      end;
  finally
      DisposeObject(SL);
  end;
end;

procedure TTriMesh2D_.RemoveNonSegments;
var
  i: integer;
  S: TSegment2D_;
begin
  for i := FSegments.Count - 1 downto 0 do
    begin
      S := FSegments[i];
      if not assigned(S.Vertex1) or not assigned(S.Vertex2) or (S.Vertex1 = S.Vertex2) then
          FSegments.Delete(i);
    end;
end;

procedure TTriMesh2D_.SetPrecision(const Value: Double);
begin
  if FPrecision <> Value then
    begin
      FPrecision := Value;
      FPrecisionSqr := Sqr(FPrecision);
    end;
end;

function TTriMesh2D_.SignedArea: Double;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Triangles.Count - 1 do
      Result := Result + Triangles[i].Area;
end;

function TTriMesh2D_.TriangleCompareLeft(Item1, Item2: TCoreClassObject; Info: pointer): integer;
// compare two triangles and decide which one is most on the left
var
  T1, T2: TTriangle2D_;
begin
  T1 := TTriangle2D_(Item1);
  T2 := TTriangle2D_(Item2);
  Result := CompareDouble(T1.Center.X, T2.Center.X);
end;

type
  TMeshAccess = class(TTriMesh2D_)
  end;

procedure TConvexHull_.AddSegment(Idx1, Idx2: integer);
var
  S: TSegment2D_;
begin
  S := TMeshAccess(FMesh).NewSegment;
  S.Vertex1 := FMesh.Vertices[Idx1];
  S.Vertex2 := FMesh.Vertices[Idx2];
  FMesh.Segments.Add(S);
end;

procedure TConvexHull_.AddVertexToHull(AVertex: TVertex2D_);
var
  i, Idx, Count: integer;
  S, S1, S2: TSegment2D_;
  IdxFirst, IdxLast: integer;

  procedure DeleteSegmentRange(AStart, ACount: integer);
  begin
    // Deleting a segment range is a bit tricky, since the range can wrap around in the list
    while ACount > 0 do begin
        // We delete segments at AStart until that's at the end of the list, else
        // we delete from the 0 position of the list
        if AStart < FMesh.Segments.Count then
            FMesh.Segments.Delete(AStart)
        else
            FMesh.Segments.Delete(0);
        dec(ACount);
      end;
  end;

// main
begin
  // init
  Count := FMesh.Segments.Count;
  IdxFirst := -1;
  IdxLast := -1;

  // Loop through all segments, and find first and last that are violated
  for i := 0 to Count - 1 do
    begin
      S := FMesh.Segments[i];
      if SegmentViolated(S, AVertex) then
        begin
          // OK, this segment isn't abided..
          IdxFirst := i;
          // Find first one
          if i = 0 then
            begin
              Idx := Count - 1;
              S := FMesh.Segments[Idx];
              while SegmentViolated(S, AVertex) do
                begin
                  dec(Idx);
                  S := FMesh.Segments[Idx];
                end;
              IdxFirst := (Idx + 1) mod Count;
            end;
          // Find last one
          Idx := i + 1;
          S := FMesh.Segments[Idx mod Count];
          while SegmentViolated(S, AVertex) do
            begin
              inc(Idx);
              S := FMesh.Segments[Idx mod Count];
            end;
          IdxLast := Idx - 1;
          // Make sure to have a positive delta idx
          if IdxLast < IdxFirst then
              inc(IdxLast, Count);
          break;
        end;
    end;

  // The vertex fell within all segments, so it's already in the hull -> we can stop
  if IdxFirst = -1 then
      exit;

  if IdxFirst = IdxLast then
    begin
      // If first and last indices are equal, we must split up the segment
      S1 := FMesh.Segments[IdxFirst];
      S2 := TMeshAccess(FMesh).NewSegment;
      S2.Vertex1 := AVertex;
      S2.Vertex2 := S1.Vertex2;
      S1.Vertex2 := AVertex;
      FMesh.Segments.Insert(IdxFirst + 1, S2);
    end
  else
    begin
      // Otherwise we move first segment's endpoint and last segment's startpoint
      // to the vertex, and remove intermediate segments
      S1 := FMesh.Segments[IdxFirst];
      S2 := FMesh.Segments[IdxLast mod Count];
      S1.Vertex2 := AVertex;
      S2.Vertex1 := AVertex;
      Count := IdxLast - IdxFirst - 1;
      if Count > 0 then
          DeleteSegmentRange(IdxFirst + 1, Count);
    end;
end;

function TConvexHull_.IsLeftOfLine(const V1, V2, AVertex: TVertex2D_): boolean;
begin
  Result := CrossProduct2D(Delta2D(V1.Point^, AVertex.Point^), Delta2D(V1.Point^, V2.Point^)) > 0;
end;

procedure TConvexHull_.MakeConvexHull(AMesh: TTriMesh2D_);
var
  i: integer;
begin
  if not assigned(AMesh) then
      exit;
  FMesh := AMesh;

  // Start by clearing the segments
  FMesh.Segments.Clear;

  // We need at least 3 vertices
  if FMesh.Vertices.Count < 3 then
      exit;

  // Build initial 3 segments
  if IsLeftOfLine(FMesh.Vertices[0], FMesh.Vertices[1], FMesh.Vertices[2]) then
    begin
      // vertex 0, 1, 2 counterclockwise
      AddSegment(0, 1);
      AddSegment(1, 2);
      AddSegment(2, 0);
    end
  else
    begin
      // vertex 0, 1, 2 clockwise, so add 0-2, 2-1 and 1-0
      AddSegment(0, 2);
      AddSegment(2, 1);
      AddSegment(1, 0);
    end;

  // Now add each of the other vertices in turn to the hull
  for i := 3 to FMesh.Vertices.Count - 1 do
      AddVertexToHull(FMesh.Vertices[i]);
end;

function TConvexHull_.SegmentViolated(ASegment: TSegment2D_; AVertex: TVertex2D_): boolean;
begin
  Result := not IsLeftOfLine(ASegment.Vertex1, ASegment.Vertex2, AVertex);
end;

procedure TGraph2D_.Clear;
begin
  FVertices.Clear;
  FSegments.Clear;
end;

constructor TGraph2D_.Create;
begin
  inherited Create;
  FVertices := TVertex2DList_.Create;
  FSegments := TSegment2DList_.Create;
end;

destructor TGraph2D_.Destroy;
begin
  DisposeObject(FSegments);
  FSegments := nil;
  DisposeObject(FVertices);
  FVertices := nil;
  inherited Destroy;
end;

procedure TGraph2D_.ReplaceVertex(OldVertex, NewVertex: TVertex2D_);
var
  i: integer;
begin
  for i := 0 to Segments.Count - 1 do
      Segments[i].ReplaceVertex(OldVertex, NewVertex);
end;

function TSegmentTriangle2D_.GetSegments(Index: integer): TSegment2D_;
begin
  Result := FSegments[Index mod 3];
end;

procedure TSegmentTriangle2D_.SetSegments(Index: integer; const Value: TSegment2D_);
var
  Idx: integer;
begin
  Idx := Index mod 3;
  if FSegments[Idx] <> Value then
    begin
      FSegments[Idx] := Value;
      // make sure to recalculate, because e.g. delaunay props change
      Invalidate;
    end;
end;

function TMeshRegionList_.GetItems(Index: integer): TMeshRegion_;
begin
  Result := inherited Items[index] as TMeshRegion_;
end;

procedure TTriangulationMesh2D_.AddGraph(AGraph: TGraph2D_);
var
  i, Idx, FirstVtx: integer;
  V: TVertex2D_;
  S: TSegment2D_;
begin
  // Store first element indices
  FirstVtx := Vertices.Count;

  // Add vertices
  for i := 0 to AGraph.Vertices.Count - 1 do
    begin
      V := NewVertex;
      V.Assign(AGraph.Vertices[i]);
      Vertices.Add(V);
    end;

  // Add segments
  for i := 0 to AGraph.Segments.Count - 1 do
    begin
      S := NewSegment;
      S.Assign(AGraph.Segments[i]);
      // Now figure out which vertices this segment connects
      Idx := AGraph.Vertices.IndexOf(S.Vertex1);
      S.Vertex1 := Vertices[FirstVtx + Idx];
      Idx := AGraph.Vertices.IndexOf(S.Vertex2);
      S.Vertex2 := Vertices[FirstVtx + Idx];
      Segments.Add(S);
    end;
end;

function TTriangulationMesh2D_.AddSegmentToTriangulation(ASegment: TSegment2D_): boolean;
var
  i, j, E1, E2: integer;
  Triangle, T1, T2: TTriangle2D_;
  CrossSegment: TSegment2D_;
  Vertex, V1, V2: TVertex2D_;
  // local
  procedure SplitAndAddSegment(AVertex: TVertex2D_);
  var
    NewS: TSegment2D_;
  begin
    NewS := TSegment2D_.Create;
    NewS.Vertex1 := AVertex;
    NewS.Vertex2 := ASegment.Vertex2;
    ASegment.Vertex2 := AVertex;
    Segments.Add(NewS);
  end;

// main
begin
  // Build a triangle chain from Vertex1 to Vertex2
  Result := FSegmentChain.BuildChain(ASegment.Vertex1, ASegment.Vertex2, FSearchFan);

  // Do any of the offending edges have a vertex *on* the segment to-be-added?
  for i := 0 to FSegmentChain.Count - 2 do
    begin
      Triangle := FSegmentChain.Triangles[i];
      V1 := Triangle.Vertices[FSegmentChain.Edges[i]];
      V2 := Triangle.Vertices[FSegmentChain.Edges[i] + 1];
      if ASegment.IsVertexOnSegment(V1, FPrecisionSqr) then
        begin
          // Yep, V1 lies on our segment
          SplitAndAddSegment(V1);
          Result := false;
          exit;
        end;
      if ASegment.IsVertexOnSegment(V2, FPrecisionSqr) then
        begin
          // Yep, V2 lies on our segment
          SplitAndAddSegment(V2);
          Result := false;
          exit;
        end;
    end;

  // Do any triangles in this chain contain a segment crossing our segment to-be-added?
  for i := 0 to FSegmentChain.Count - 2 do
    begin
      Triangle := FSegmentChain.Triangles[i];
      CrossSegment := Triangle.Segments[FSegmentChain.Edges[i]];
      if assigned(CrossSegment) then
        begin
          // Indeed, we must split this one: find intersection
          Vertex := ASegment.IntersectWith(CrossSegment);
          if not assigned(Vertex) then
            // We do have a cross segment but no intersection.. this should not happen
              RaiseInfo(sCrossSegmentIntersectionError);
          // Add this vertex to our list
          Vertices.Add(Vertex);
          // Insert the vertex into the triangulation, this will force the other
          // segment to split as well
          AddVertexToTriangulation(Vertex, nil);
          // Now split our segment.
          SplitAndAddSegment(Vertex);
          Result := false;
          exit;
        end;
    end;

  // If we have more than one triangle in the chain, we must reduce it by swapping
  // triangle pairs. The triangles removed from the chain are stored in FRemovals
  if FSegmentChain.Count > 1 then
      ReduceSegmentChain(FSegmentChain, FRemovals);

  // After reduction we hopefully have only one segment.. so now we can add
  // this segment
  if FSegmentChain.Count = 1 then
    begin
      T1 := FSegmentChain.Triangles[0];
      E1 := FSegmentChain.Edges[0] + 2;
      T2 := T1.Neighbours[E1];
      E2 := T2.NeighbourIndex(T1);
      T1.Segments[E1] := ASegment;
      T2.Segments[E2] := ASegment;
      DoExecutionStep('Add Segment');
    end;

  // Now we will re-check the list of removed triangles
  for i := 0 to FRemovals.Count - 1 do
    for j := 0 to 2 do
        CheckTriangleWithEdge(FRemovals[i], j, nil);
  FRemovals.Clear;
end;

function TTriangulationMesh2D_.AddVertexToTriangulation(AVertex: TVertex2D_; Updates: TTriangle2DList_): boolean;
var
  Triangle: TTriangle2D_;
  OldVertex: TVertex2D_;
  Status: THitTestTriangle_;
begin
  Triangle := nil;
  Result := true;
  Status := HitTestTriangles(AVertex.Point^, Triangle, true);
  // If on the body of the triangle, we will split the triangle into 3 sub-
  // triangles.
  // If on one of the edges, split the triangle into 2, on the edge, as well
  // as the triangle neighbouring it.
  // If on one of the vertices (httVtx0..2), this means that an earlier triangle
  // was formed with one of the vertices very close to this one-to-be-added.
  // We will skip this vertex, return the hit vertex, and we will inform about that.
  case Status of
    httNone:
      begin
        // Deary, we didn't find any triangle! This situation should normally not
        // arise, unless the initial mesh is not large enough, or the mesh is at
        // its limit of numerical precision. We will simply skip the vertex.
        Result := false;
        exit;
      end;
    httBody: SplitTriangleBody(Triangle, AVertex, Updates);
    httVtx0 .. httVtx2:
      begin
        DoExecutionStep(PFormat('Vertex %d skipped (too close to another one)', [Vertices.IndexOf(AVertex)]));
        inc(FVertexSkipCount);
        case Status of
          httVtx0: OldVertex := Triangle.Vertices[0];
          httVtx1: OldVertex := Triangle.Vertices[1];
          httVtx2: OldVertex := Triangle.Vertices[2];
          else
            OldVertex := nil;
        end;
        // In case a vertex lies too close to another one, we will not add this
        // vertex, but we must make sure any segments to-be-added do not use this
        // vertex but the new one
        ReplaceVertexInSegments(AVertex, OldVertex);
        Result := false;
      end;
    httEdge0: SplitTriangleEdge(Triangle, 0, AVertex, Updates);
    httEdge1: SplitTriangleEdge(Triangle, 1, AVertex, Updates);
    httEdge2: SplitTriangleEdge(Triangle, 2, AVertex, Updates);
  end;
end;

function TTriangulationMesh2D_.BruteForceHitTestTriangles(
  const APoint: TPoing2D_; var ATriangle: TTriangle2D_): THitTestTriangle_;
var
  i: integer;
begin
  Result := httNone;
  ATriangle := nil;
  for i := 0 to Triangles.Count - 1 do
    begin
      Result := Triangles[i].HitTest(APoint);
      if not(Result in [httNone, httClose0, httClose1, httClose2]) then
        begin
          ATriangle := Triangles[i];
          exit;
        end;
    end;
end;

function TTriangulationMesh2D_.BuildTriangleFan(AList: TTriangle2DList_;
  AVertex: TVertex2D_): boolean;
// local
  procedure FindRecursive(ABase: TTriangle2D_);
  var
    i: integer;
    N: TTriangle2D_;
  begin
    // loop through neighbours
    for i := 0 to 2 do
      begin
        N := ABase.Neighbours[i];
        if not assigned(N) then
            continue;
        // does neighbour contain AVertex?
        if N.VertexIndex(AVertex) = -1 then
            continue;
        // Obviously.. check if we do not have it already
        if AList.IndexOf(N) >= 0 then
            continue;
        // Ok, new one.. add it
        AList.Add(N);
        FindRecursive(N);
      end;
  end;

// main
var
  i: integer;
  TriBase: TTriangle2D_;
begin
  AList.Clear;
  Result := false;
  if not assigned(AVertex) then
      exit;
  TriBase := AVertex.Triangle;
  if not assigned(TriBase) or (TriBase.VertexIndex(AVertex) = -1) then
    begin
      TriBase := nil;
      // Hecky-decky: not a correct pointer.. try the brute force method to find
      // one
      for i := 0 to Triangles.Count - 1 do
        if Triangles[i].VertexIndex(AVertex) >= 0 then
          begin
            TriBase := Triangles[i];
            break;
          end;
      if not assigned(TriBase) then
          exit;
    end;
  Result := true;
  AList.Add(TriBase);
  FindRecursive(TriBase);
end;

procedure TTriangulationMesh2D_.CheckTriangleWithEdge(ATriangle: TTriangle2D_; AEdge: integer; Updates: TTriangle2DList_);
begin
  // default does nothing
end;

procedure TTriangulationMesh2D_.Clear;
begin
  inherited Clear;
  FCornerPoints.Clear;
  FRemovals.Clear;
  FSearchFan.Clear;
  FSegmentChain.Clear;
end;

constructor TTriangulationMesh2D_.Create;
begin
  inherited Create;
  FCornerPoints := TVertex2DList_.Create(true);
  FRegions := TMeshRegionList_.Create(true);
  FRemovals := TTriangle2DList_.Create(false);
  FSearchFan := TTriangleFan2D_.Create;
  FSegmentChain := TTriangleChain2D_.Create;
end;

destructor TTriangulationMesh2D_.Destroy;
begin
  DisposeObject(FCornerPoints);
  FCornerPoints := nil;
  DisposeObject(FRegions);
  FRegions := nil;
  DisposeObject(FRemovals);
  FRemovals := nil;
  DisposeObject(FSearchFan);
  FSearchFan := nil;
  DisposeObject(FSegmentChain);
  FSegmentChain := nil;
  inherited Destroy;
end;

procedure TTriangulationMesh2D_.DetectRegions;
var
  i, RIdx, Idx: integer;
  T, N: TTriangle2D_;
  S: TSegment2D_;
  R: TMeshRegion_;
  Borders: TTriangle2DList_;
  // recursive, local
  procedure FloodRegion(ATriangle: TTriangle2D_; AIndex: integer);
  var
    i: integer;
    N: TTriangle2D_;
    S: TSegment2D_;
  begin
    Borders.Remove(ATriangle);
    ATriangle.RegionIndex := AIndex;
    // Direct neighbours?
    for i := 0 to 2 do
      begin
        N := ATriangle.Neighbours[i];
        S := ATriangle.Segments[i];
        if assigned(N) then
          begin
            if N.RegionIndex >= 0 then
                continue;
            if assigned(S) then
              begin
                // There's a segment inbetween, we add this one to the border list
                if Borders.IndexOf(N) < 0 then
                    Borders.Add(N);
              end
            else
              begin
                // A neighbour in the same region
                FloodRegion(N, AIndex);
              end;
          end;
      end;
  end;

// main
begin
  // Clear all regions and indices
  Regions.Clear;
  for i := 0 to Triangles.Count - 1 do
      Triangles[i].RegionIndex := -1;

  // Initial region with winding number 0
  R := TMeshRegion_.Create;
  Regions.Add(R);
  RIdx := 0;

  // Find a seed triangle, this is any triangle which doesn't have all neighbours set
  T := nil;
  for i := 0 to Triangles.Count - 1 do
    begin
      T := Triangles[i];
      Idx := T.NeighbourIndex(nil);
      if Idx >= 0 then
        begin
          S := T.Segments[Idx];
          if assigned(S) then
            begin
              if S.Vertex1 = T.Vertices[i] then
                  R.WindingNumber := 1
              else
                  R.WindingNumber := -1;
            end
          else
            begin
              R.WindingNumber := 0;
              R.IsOuterRegion := true;
            end;
          break;
        end;
    end;
  if not assigned(T) then
      exit;

  // Temporary border list
  Borders := TTriangle2DList_.Create(false);
  try
    // Keep on flooding regions until there are no more bordering triangles
    repeat
      // Flood the mesh region from the triangle with RIdx region index
      FloodRegion(T, RIdx);

      R := nil;
      // Do we have any borders?
      if Borders.Count > 0 then
        begin
          T := Borders[0];
          for i := 0 to 2 do
            begin
              N := T.Neighbours[i];
              S := T.Segments[i];
              if assigned(N) and assigned(S) then
                begin
                  if N.RegionIndex >= 0 then
                    begin
                      // OK, found a neighbour with region initialized.. we base our
                      // new region on the relation here
                      R := TMeshRegion_.Create;
                      if S.Vertex1 = T.Vertices[i] then
                          R.WindingNumber := Regions[N.RegionIndex].WindingNumber + 1
                      else
                          R.WindingNumber := Regions[N.RegionIndex].WindingNumber - 1;
                      Regions.Add(R);
                      inc(RIdx);
                      break;
                    end;
                end;
            end;
        end;

    until (Borders.Count = 0) or (R = nil);

  finally
      DisposeObject(Borders);
  end;
end;

procedure TTriangulationMesh2D_.DoExecutionStep(const AMessage: SystemString);
begin
  if assigned(FOnExecutionStep) then
      FOnExecutionStep(Self, AMessage);
end;

procedure TTriangulationMesh2D_.DoPhaseComplete(const AMessage: SystemString);
begin
  if assigned(FOnPhaseComplete) then
      FOnPhaseComplete(Self, AMessage);
end;

procedure TTriangulationMesh2D_.DoStatus(const AMessage: SystemString);
begin
  if assigned(FOnStatus) then
      FOnStatus(Self, AMessage);
end;

procedure TTriangulationMesh2D_.FinalizeInfo;
begin
  // Calculation time
  FCalculationTime := (GetTimeTick - FTick) / 1000;
end;

class function TTriangulationMesh2D_.GetTriangleClass: TTriangle2DClass_;
begin
  Result := TSegmentTriangle2D_;
end;

function TTriangulationMesh2D_.HitTestTriangles(const APoint: TPoing2D_;
  var ATriangle: TTriangle2D_; UseQuick: boolean): THitTestTriangle_;
var
  Neighbour: TTriangle2D_;
  Closest: TVertex2D_;
  Edge: integer;
begin
  Result := httNone;
  if UseQuick then
    begin
      // Use a quick-search to find a likely triangle as a basis
      Closest := LocateClosestVertex(APoint, FSearchFan);
      // FSearchFan is a vertex jumper
      ATriangle := FSearchFan.TriangleInDirection(APoint);
      if not assigned(ATriangle) then
          ATriangle := Closest.Triangle;
      if not assigned(ATriangle) and (Triangles.Count > 0) then
          ATriangle := Triangles[0];
    end
  else
    begin
      // We skip the quicksearch
      if not assigned(ATriangle) then
          RaiseInfo('triangle must be assigned without quicksearch');
    end;

  // no triangles?
  if not assigned(ATriangle) then
      exit;

  repeat
    // Hit-test the triangle
    Result := ATriangle.HitTest(APoint);
    inc(FHitTests);

    // Deal with close hits
    if Result in [httClose0 .. httClose2] then
      begin
        // Try neighbour on this side
        Neighbour := nil;
        case Result of
          httClose0: Neighbour := ATriangle.Neighbours[0];
          httClose1: Neighbour := ATriangle.Neighbours[1];
          httClose2: Neighbour := ATriangle.Neighbours[2];
        end;
        if assigned(Neighbour) then
          begin
            ATriangle := Neighbour;
            Result := ATriangle.HitTest(APoint);
            inc(FHitTests);
          end;
        case Result of
          httClose0: Result := httEdge0;
          httClose1: Result := httEdge1;
          httClose2: Result := httEdge2;
        end;
      end;
    if Result <> httNone then
        break;

    // Find neighbouring triangle
    Edge := ATriangle.EdgeFromCenterTowardsPoint(APoint);
    if Edge = -1 then
        RaiseInfo('Unable to find direction');
    ATriangle := ATriangle.Neighbours[Edge];

    // No neighbour: we have ended up in "da middle of nawheere"
    if not assigned(ATriangle) then
        break;

  until Result <> httNone;
end;

procedure TTriangulationMesh2D_.InitializeInfo;
begin
  inherited InitializeInfo;
  FVertexSkipCount := 0;
  FSplitEdgeCount := 0;
  FSplitBodyCount := 0;
  FHitTests := 0;
  FCalculationTime := 0;
  FTick := GetTimeTick;
end;

procedure TTriangulationMesh2D_.PostProcessMesh;
begin
  // default does nothing
end;

procedure TTriangulationMesh2D_.PrepareMeshConstruction;
const
  cGrowFactor = 0.2;
var
  Delta: TPoing2D_;
  Tri1, Tri2: TTriangle2D_;
begin
  // Calculate bounding box
  if not BoundingBox(FBBMin, FBBMax) then
      exit;

  // MeshMin / MeshMax
  Delta := Delta2D(FBBMin, FBBMax);
  FMeshMin.X := FBBMin.X - Delta.X * cGrowFactor;
  FMeshMin.Y := FBBMin.Y - Delta.Y * cGrowFactor;
  FMeshMax.X := FBBMax.X + Delta.X * cGrowFactor;
  FMeshMax.Y := FBBMax.Y + Delta.Y * cGrowFactor;

  // Add 4 vertices and 2 triangles bounding the mesh area
  FCornerPoints.Clear;
  FCornerPoints.Add(GetVertexClass.CreateWithCoords(FMeshMin.X, FMeshMin.Y));
  FCornerPoints.Add(GetVertexClass.CreateWithCoords(FMeshMax.X, FMeshMin.Y));
  FCornerPoints.Add(GetVertexClass.CreateWithCoords(FMeshMax.X, FMeshMax.Y));
  FCornerPoints.Add(GetVertexClass.CreateWithCoords(FMeshMin.X, FMeshMax.Y));
  Tri1 := NewTriangle;
  Tri2 := NewTriangle;
  Tri1.HookupVertices(FCornerPoints[2], FCornerPoints[0], FCornerPoints[1]);
  Tri1.Neighbours[0] := Tri2;
  Tri2.HookupVertices(FCornerPoints[0], FCornerPoints[2], FCornerPoints[3]);
  Tri2.Neighbours[0] := Tri1;
  Triangles.Add(Tri1);
  Triangles.Add(Tri2);
  DoExecutionStep('prepare mesh');
  FAreaInitial := SignedArea;
end;

procedure TTriangulationMesh2D_.ReduceSegmentChain(AChain: TTriangleChain2D_; ARemovals: TTriangle2DList_);
begin
  // default does nothing
end;

procedure TTriangulationMesh2D_.RemoveMeshConstruction(ARemovalStyle: TRemovalStyle_);
var
  i: integer;
  T: TTriangle2D_;
  R: TMeshRegion_;
  MustRemove: boolean;
begin
  DetectRegions;
  // If we are not going to delete anything.. then leave now
  if ARemovalStyle = rsNone then
      exit;

  for i := Triangles.Count - 1 downto 0 do
    begin
      T := Triangles[i];
      if T.RegionIndex < 0 then
          continue;
      R := Regions[T.RegionIndex];
      case ARemovalStyle of
        rsOutside: MustRemove := R.IsOuterRegion;
        rsEvenOdd: MustRemove := not odd(R.WindingNumber);
        rsNonZero: MustRemove := R.WindingNumber = 0;
        rsNegative: MustRemove := R.WindingNumber < 0;
        else
          MustRemove := false;
      end; // case
      if MustRemove then
          RemoveTriangleFromMesh(T);
    end;

  // Remove the 4 corner points with triangle fans
  FCornerPoints.Clear;
  FSearchFan.Clear;
end;

procedure TTriangulationMesh2D_.RemoveTriangleFromMesh(ATriangle: TTriangle2D_);
var
  i, Idx: integer;
  N: TTriangle2D_;
  V: TVertex2D_;
begin
  // Remove ATriangle from neighbour pointers
  for i := 0 to 2 do
    begin
      N := ATriangle.Neighbours[i];
      if not assigned(N) then
          continue;
      Idx := N.NeighbourIndex(ATriangle);
      if Idx = -1 then
          continue;
      N.Neighbours[Idx] := nil;
    end;

  // Any vertex pointing at it should have it's pointer reset
  for i := 0 to 2 do
    begin
      V := ATriangle.Vertices[i];
      if assigned(V) and (V.Triangle = ATriangle) then
        begin
          // Point the vertex to one of the neighbours that also shares this triangle
          if assigned(ATriangle.Neighbours[i]) then
            begin
              V.Triangle := ATriangle.Neighbours[i];
              continue;
            end;
          if assigned(ATriangle.Neighbours[i + 2]) then
            begin
              V.Triangle := ATriangle.Neighbours[i + 2];
              continue;
            end;
          // If there are no neighbours, just nil it.. the vertex is orphaned
          V.Triangle := nil;
        end;
    end;
  // Now remove the triangle from the principal list
  Triangles.Remove(ATriangle);
end;

procedure TTriangulationMesh2D_.ReplaceVertexInSegments(Old_, New_: TVertex2D_);
var
  i, Idx: integer;
begin
  // all segments containing OldVertex should point to NewVertex
  for i := 0 to Segments.Count - 1 do
      Segments[i].ReplaceVertex(Old_, New_);
  // we also remove OldVertex from our vertices list, by setting its index to nil
  Idx := Vertices.IndexOf(Old_);
  if Idx >= 0 then
      Vertices[Idx] := nil;
end;

procedure TTriangulationMesh2D_.SplitTriangleBody(ATriangle: TTriangle2D_;
  AVertex: TVertex2D_; Updates: TTriangle2DList_);
var
  Tri0, Tri1, Tri2, N1, N2: TTriangle2D_;
begin
  // We already found that APoint lies within ATriangle, now we split ATriangle
  // into 3 subtriangles
  inc(FSplitBodyCount);

  // The old triangle will be new triangle 0
  Tri0 := ATriangle;

  // New triangle 1 & 2
  Tri1 := NewTriangle;
  Tri2 := NewTriangle;

  // Set neighbour's pointers back
  N1 := Tri0.Neighbours[1];
  N2 := Tri0.Neighbours[2];
  if assigned(N1) then
      N1.ReplaceNeighbour(Tri0, Tri1);
  if assigned(N2) then
      N2.ReplaceNeighbour(Tri0, Tri2);

  // Setup neighbours
  Tri1.HookupNeighbours(N1, Tri2, Tri0);
  Tri2.HookupNeighbours(N2, Tri0, Tri1);
  Tri0.Neighbours[1] := Tri1;
  Tri0.Neighbours[2] := Tri2;

  // Setup vertices
  Tri1.HookupVertices(Tri0.Vertices[1], Tri0.Vertices[2], AVertex);
  Tri2.HookupVertices(Tri0.Vertices[2], Tri0.Vertices[0], AVertex);
  // must come after
  Tri0.Vertices[2] := AVertex;

  Triangles.Add(Tri1);
  Triangles.Add(Tri2);
  DoExecutionStep('split body');

  // Check segments
  Tri1.Segments[0] := Tri0.Segments[1];
  Tri2.Segments[0] := Tri0.Segments[2];
  Tri0.Segments[1] := nil;
  Tri0.Segments[2] := nil;

  // Add to updates
  if assigned(Updates) then
    begin
      Updates.Add(Tri0);
      Updates.Add(Tri1);
      Updates.Add(Tri2);
    end;

  // Check these triangles.. in default triangulator this does nothing, but
  // descendants can override
  CheckTriangleWithEdge(Tri0, 0, Updates);
  CheckTriangleWithEdge(Tri1, 0, Updates);
  CheckTriangleWithEdge(Tri2, 0, Updates);
end;

procedure TTriangulationMesh2D_.SplitTriangleEdge(ATriangle: TTriangle2D_; AEdge: integer; AVertex: TVertex2D_; Updates: TTriangle2DList_);
var
  Tri11, Tri12, Tri21, Tri22, N1, N2: TTriangle2D_;
  E1, E2: integer;
  Pv, Po, Pl, Pr: PPoing2D_;
  NegTest: boolean;
  S, NewS: TSegment2D_;
begin
  // We found that AVertex lies *on* ATriangle's edge with index AEdge. Hence we
  // split ATriangle, and it's neighbour (if any).
  Tri11 := ATriangle;
  E1 := AEdge;
  E2 := -1;
  Tri21 := Tri11.Neighbours[E1];
  if assigned(Tri21) then
    begin

      // Check edge consistency
      E2 := Tri21.NeighbourIndex(Tri11);
      if E2 = -1 then
        // this should not happen.. the integrity is breached
          RaiseInfo('edges do not match');

      // Since the vertex to insert lays on ATriangle, it doesn't lay on the opposite
      // one. Therefore, we must check if the opposite triangles won't be negative
      // after creation
      Pv := AVertex.Point;
      Po := Tri21.Vertices[E2 + 2].Point;
      Pl := Tri11.Vertices[E1].Point;
      Pr := Tri11.Vertices[E1 + 1].Point;
      NegTest := false;
      if CrossProduct2D(Delta2D(Po^, Pv^), Delta2D(Po^, Pl^)) <= 0 then
          NegTest := true;
      if CrossProduct2D(Delta2D(Po^, Pr^), Delta2D(Po^, Pv^)) <= 0 then
          NegTest := true;
      if NegTest then
        begin
          // Oops! Indeed.. do a triangle body split instead
          SplitTriangleBody(ATriangle, AVertex, Updates);
          exit;
        end;

      inc(FSplitEdgeCount);

      // Split Tri11 and Tri21
      Tri12 := NewTriangle;
      Tri22 := NewTriangle;

      // Set neighbour's pointers back
      N1 := Tri11.Neighbours[E1 + 1];
      if assigned(N1) then
          N1.ReplaceNeighbour(Tri11, Tri12);
      N2 := Tri21.Neighbours[E2 + 1];
      if assigned(N2) then
          N2.ReplaceNeighbour(Tri21, Tri22);

      // Setup neighbours
      Tri11.Neighbours[E1] := Tri22;
      Tri11.Neighbours[E1 + 1] := Tri12;
      Tri12.HookupNeighbours(Tri11, Tri21, N1);
      Tri21.Neighbours[E2] := Tri12;
      Tri21.Neighbours[E2 + 1] := Tri22;
      Tri22.HookupNeighbours(Tri21, Tri11, N2);

      // Setup vertices
      Tri12.HookupVertices(Tri11.Vertices[E1 + 2], AVertex, Tri11.Vertices[E1 + 1]);
      Tri11.Vertices[E1 + 1] := AVertex;
      Tri22.HookupVertices(Tri21.Vertices[E2 + 2], AVertex, Tri21.Vertices[E2 + 1]);
      Tri21.Vertices[E2 + 1] := AVertex;

      Triangles.Add(Tri12);
      Triangles.Add(Tri22);

      // Add to updates list
      if assigned(Updates) then
        begin
          Updates.Add(Tri11);
          Updates.Add(Tri12);
          Updates.Add(Tri21);
          Updates.Add(Tri22);
        end;

    end
  else
    begin

      // Split just Tri11
      Tri12 := NewTriangle;
      Tri22 := nil;

      // Set neighbour's pointers back
      N1 := Tri11.Neighbours[E1 + 1];
      if assigned(N1) then
          N1.ReplaceNeighbour(Tri11, Tri12);

      // Setup neighbours
      Tri11.Neighbours[E1 + 1] := Tri12;
      Tri12.HookupNeighbours(Tri11, nil, N1);

      // Setup vertices
      Tri12.HookupVertices(Tri11.Vertices[E1 + 2], AVertex, Tri11.Vertices[E1 + 1]);
      Tri11.Vertices[E1 + 1] := AVertex;

      Triangles.Add(Tri12);

      // Add to updates list
      if assigned(Updates) then
        begin
          Updates.Add(Tri11);
          Updates.Add(Tri12);
        end;

    end;

  // Correct segments: first the segment to split up (if any)
  S := Tri11.Segments[E1];
  if assigned(S) then
    begin
      // Yeppers, split segment and add the new one. We also directly assign
      // the new segment to the triangles, so this new segment doesnt need to
      // be added explicitly to the mesh with AddSegment.
      NewS := NewSegment;
      if S.Vertex1 = Tri11.Vertices[E1] then
        begin
          // Segment same direction as Tri11 edge:
          NewS.Vertex1 := AVertex;
          NewS.Vertex2 := Tri12.Vertices[2];
          S.Vertex2 := AVertex;
        end
      else
        begin
          // Segment opposite direction as Tri11 edge:
          NewS.Vertex1 := Tri12.Vertices[2];
          NewS.Vertex2 := AVertex;
          S.Vertex1 := AVertex;
        end;
      Tri12.Segments[1] := NewS;
      if assigned(Tri21) then
        begin
          Tri21.Segments[E2] := NewS;
          Tri22.Segments[1] := S;
        end;
      // Add the new segment to our list
      Segments.Add(NewS);
    end;

  // Other segments: Tri12 takes over from Tri11 on one side
  S := Tri11.Segments[E1 + 1];
  Tri11.Segments[E1 + 1] := nil;
  Tri12.Segments[2] := S;
  if assigned(Tri21) then
    begin
      // Tri22 takes over from Tri21 on one side
      S := Tri21.Segments[E2 + 1];
      Tri21.Segments[E2 + 1] := nil;
      Tri22.Segments[2] := S;
    end;

  DoExecutionStep('split edge');

  // Check triangles
  CheckTriangleWithEdge(Tri11, E1 + 2, Updates);
  CheckTriangleWithEdge(Tri12, 2, Updates);
  if assigned(Tri21) then
    begin
      CheckTriangleWithEdge(Tri21, E2 + 2, Updates);
      CheckTriangleWithEdge(Tri22, 2, Updates);
    end;
end;

procedure TTriangulationMesh2D_.Triangulate(ARemovalStyle: TRemovalStyle_);
var
  i, j: integer;
  S: TSegment2D_;
  T: TTriangle2D_;
begin
  // Reset info
  InitializeInfo;

  // Prepare mesh area
  PrepareMeshConstruction;
  DoPhaseComplete('Mesh Construction');
  FSearchFan.Clear;

  // Add all vertices to the triangulation. Some vertices might get skipped if
  // they fall on top of another one, in that case the accompanying segment will
  // be updated trough ReplaceVertexInSegments
  for i := 0 to Vertices.Count - 1 do
      AddVertexToTriangulation(Vertices[i], nil);
  DoPhaseComplete('Vertex addition');

  // Since segments might have been updated and not be functional any longer..
  RemoveNonSegments;

  // Add all segments to the triangulation, creating a constrained triangulation.
  // We use a "while" loop because segments might be split and segments can be
  // inserted on the fly
  i := 0;
  while i < Segments.Count do
    begin
      AddSegmentToTriangulation(Segments[i]);
      inc(i);
    end;
  DoPhaseComplete('Segment addition');

  // Remove the elements we added for construction
  if ARemovalStyle = rsNone then
    begin
      // If construction is left on, we must add segments to all nil neighbours
      // (because only segmented outside edges subdivide well for postprocessing)
      for i := 0 to Triangles.Count - 1 do
        begin
          T := Triangles[i];
          for j := 0 to 2 do
            begin
              if T.Neighbours[j] = nil then
                begin
                  S := NewSegment;
                  S.Vertex1 := T.Vertices[j];
                  S.Vertex2 := T.Vertices[j + 1];
                  Segments.Add(S);
                  T.Segments[j] := S;
                end;
            end;
        end;
    end
  else
    begin
      RemoveMeshConstruction(rsOutside);
    end;
  DoPhaseComplete('Perform removal');

  // Do post processing (virtual)
  PostProcessMesh;

  if not(ARemovalStyle in [rsNone, rsOutside]) then
    begin
      RemoveMeshConstruction(ARemovalStyle);
      DoPhaseComplete('Remove fill-rule');
    end;

  // finalize info
  FinalizeInfo;
end;

procedure TDelaunayTriangle2D_.CalculateMetrics;
var
  Pa, Pb, Pc: PPoing2D_;
  Den, A1, A2, R: Double;

begin
  inherited CalculateMetrics;
  // Calculate circle center and radius (squared)
  Pa := Vertices[0].Point;
  Pb := Vertices[1].Point;
  Pc := Vertices[2].Point;
  Den := ((Pb^.Y - Pc^.Y) * (Pb^.X - Pa^.X) - (Pb^.Y - Pa^.Y) * (Pb^.X - Pc^.X)) * 2;
  A1 := (Pa^.X + Pb^.X) * (Pb^.X - Pa^.X) + (Pb^.Y - Pa^.Y) * (Pa^.Y + Pb^.Y);
  A2 := (Pb^.X + Pc^.X) * (Pb^.X - Pc^.X) + (Pb^.Y - Pc^.Y) * (Pb^.Y + Pc^.Y);

  // Make sure we don't divide by zero
  if abs(Den) > 1E-20 then
    begin
      // Calculated circle center of circle through points a, b, c
      FCircleCenter.X := (A1 * (Pb^.Y - Pc^.Y) - A2 * (Pb^.Y - Pa^.Y)) / Den;
      FCircleCenter.Y := (A2 * (Pb^.X - Pa^.X) - A1 * (Pb^.X - Pc^.X)) / Den;
      // Squared radius of this circle
      // We use a radius that is a fraction smaller than the real radius (by
      // DelaunayPrecision) to allow miniscule infringement of the delaunay property.
      // This will avoid indecisiveness and endless swapping
      R := Dist2D(FCircleCenter, Pa^) - TDelaunayMesh2D_(FMesh).FDelaunayPrecision;
      if R < 0 then
          R := 0;
      FSquaredRadius := Sqr(R);
    end
  else
    begin
      FCircleCenter := Center;
      FSquaredRadius := 0;
    end;
  inc(TDelaunayMesh2D_(FMesh).FCircleCalcCount);
end;

function TDelaunayTriangle2D_.GetCircleCenter: TPoing2D_;
begin
  if not FValidMetrics then
      CalculateMetrics;
  Result := FCircleCenter;
end;

function TDelaunayTriangle2D_.GetSquaredRadius: Double;
begin
  if not FValidMetrics then
      CalculateMetrics;
  Result := FSquaredRadius;
end;

function TDelaunayTriangle2D_.IsDelaunay: boolean;
var
  i, j: integer;
  N: TTriangle2D_;
  V: TVertex2D_;
  C: TPoing2D_;
  RSqr: Double;
begin
  Result := false;
  // The center of the circle
  C := GetCircleCenter;
  // The square of the radius
  RSqr := FSquaredRadius;

  // Loop through neighbours
  for i := 0 to 2 do
    begin
      N := Neighbours[i];
      // No neighbour, or a segment on this edge: skip
      if not assigned(N) or assigned(Segments[i]) then
          continue;
      for j := 0 to 2 do
        begin
          V := N.Vertices[j];
          // Not one of the shared vertices?
          if (V = Vertices[i]) or (V = Vertices[i + 1]) then
              continue;
          // Determine the distance, and compare
          if SquaredDist2D(V.Point^, C) < RSqr then
            // Indeed, one of the opposite points is in, so we return "false"
              exit;
        end;
    end;
  // Ending up here means this triangle abides Delaunay
  Result := true;
end;

function TDelaunayTriangle2D_.VertexInCircle(AVertex: TVertex2D_): boolean;
var
  C: TPoing2D_;
begin
  C := GetCircleCenter;
  Result := SquaredDist2D(C, AVertex.Point^) <= FSquaredRadius;
end;

{ TQualityTriangle2D_ }

procedure TQualityTriangle2D_.CalculateMetrics;
begin
  inherited CalculateMetrics;
  FQuality := SmallestAngleCosine;
end;

function TQualityTriangle2D_.EncroachedSegmentFromPoint(const APoint: TPoing2D_): TSegment2D_;
var
  i: integer;
  S: TSegment2D_;
  SqrR: Double;
begin
  Result := nil;
  SqrR := 0;
  for i := 0 to 2 do
    begin
      S := Segments[i];
      if assigned(S) then
        begin
          if S.PointEncroaches(APoint) then
            begin
              if S.SquaredEncroachRadius > SqrR then
                begin
                  Result := S;
                  SqrR := S.SquaredEncroachRadius;
                end;
            end;
        end;
    end;
end;

function TQualityTriangle2D_.GetOffCenter: TPoing2D_;
var
  SquaredBeta, L0Sqr, L1Sqr, L2Sqr, LMinSqr, HSqr, A: Double;
  EMin: integer;
  P, q, Delta, B: TPoing2D_;
begin
  if not FValidMetrics then
      CalculateMetrics;
  // Squared edge lengths
  L0Sqr := SquaredDist2D(Vertices[0].Point^, Vertices[1].Point^);
  L1Sqr := SquaredDist2D(Vertices[1].Point^, Vertices[2].Point^);
  L2Sqr := SquaredDist2D(Vertices[2].Point^, Vertices[0].Point^);
  // Minimum squared edge length
  LMinSqr := L0Sqr;
  EMin := 0;
  if L1Sqr < LMinSqr then
    begin
      LMinSqr := L1Sqr;
      EMin := 1;
    end;
  if L2Sqr < LMinSqr then
    begin
      LMinSqr := L2Sqr;
      EMin := 2;
    end;
  // Squared beta factor
  SquaredBeta := FSquaredRadius / LMinSqr;
  // Offcenter: when the beta factor is higher than the required one,
  // we calculate the position of the offcenter such that the quality is exactly ok
  if SquaredBeta > TQualityMesh2D_(FMesh).FSquaredBeta then
    begin
      // Point B between PQ
      P := Vertices[EMin].Point^;
      q := Vertices[EMin + 1].Point^;
      HSqr := SquaredDist2D(P, q) * 0.25; // H = half of the distance between PQ
      B := MidPoint2D(P, q);
      A := Sqrt(FSquaredRadius - HSqr);
      Delta := Delta2D(B, FCircleCenter);
      NormalizeVector2D(Delta);
      // Off-center lies on point from B along carrier vector Delta, over distance a
      Result.X := B.X + A * Delta.X;
      Result.Y := B.Y + A * Delta.Y;
    end
  else
    // Otherwise, we use the circle center for the off-center
      Result := FCircleCenter;
end;

function TQualityTriangle2D_.GetQuality: Double;
begin
  if not FValidMetrics then
      CalculateMetrics;
  Result := FQuality;
end;

function TQualityTriangle2D_.HasEncroachedSegment: boolean;
var
  i: integer;
  S: TSegment2D_;
begin
  Result := false;
  for i := 0 to 2 do
    begin
      S := Segments[i];
      if assigned(S) then
        begin
          Result := S.PointEncroaches(Vertices[i + 2].Point^);
          if Result then
              exit;
        end;
    end;
end;

function TSortedTriangle2DList_.DoCompare(Item1, Item2: TCoreClassObject): integer;
var
  T1, T2: TQualityTriangle2D_;
begin
  T1 := TQualityTriangle2D_(Item1);
  T2 := TQualityTriangle2D_(Item2);
  // We compare quality and want the highest "quality" first (smallest angles),
  // so we invert
  Result := -CompareDouble(T1.Quality, T2.Quality);
end;

function TSortedTriangle2DList_.GetItems(Index: integer): TQualityTriangle2D_;
begin
  Result := inherited Items[index] as TQualityTriangle2D_;
end;

procedure TEncroachItemList_.AddItem(AEncroacher, ATriangle: TTriangle2D_; ASegment: TSegment2D_);
var
  i: integer;
  Item: TEncroachItem_;
begin
  // Make sure we're unique
  for i := 0 to Count - 1 do
    begin
      Item := Items[i];
      if (Item.Encroacher = AEncroacher) and (Item.Triangle = ATriangle) and
        (Item.Segment = ASegment) then
          exit;
    end;
  // If it doesn't exist yet, we create an item
  Item := TEncroachItem_.Create;
  Item.Encroacher := AEncroacher;
  Item.Triangle := ATriangle;
  Item.Segment := ASegment;
  Add(Item);
end;

function TEncroachItemList_.DoCompare(Item1, Item2: TCoreClassObject): integer;
var
  E1, E2: TEncroachItem_;
begin
  E1 := TEncroachItem_(Item1);
  E2 := TEncroachItem_(Item2);
  // We want the longest segment first, so we sort by squared radius, and invert
  Result := -CompareDouble(E1.Segment.SquaredEncroachRadius, E2.Segment.SquaredEncroachRadius);
end;

function TEncroachItemList_.GetItems(Index: integer): TEncroachItem_;
begin
  Result := inherited Items[index] as TEncroachItem_;
end;

function TEncroachItemList_.IndexByTriangle(ATriangle: TTriangle2D_): integer;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].Triangle = ATriangle then
      begin
        Result := i;
        exit;
      end;
  Result := -1;
end;

procedure TEncroachItemList_.RemoveAllItemsWithSegment(ASegment: TSegment2D_);
var
  i: integer;
  Item: TEncroachItem_;
begin
  for i := Count - 1 downto 0 do
    begin
      Item := Items[i];
      if Item.Segment = ASegment then
          Delete(i);
    end;
end;

procedure TEncroachItemList_.RemoveAllItemsWithTriangle(ATriangle: TTriangle2D_);
var
  i: integer;
  Item: TEncroachItem_;
begin
  for i := Count - 1 downto 0 do
    begin
      Item := Items[i];
      if (Item.Encroacher = ATriangle) or (Item.Triangle = ATriangle) then
          Delete(i);
    end;
end;

{ TDelaunayMesh2D_ }

function TDelaunayMesh2D_.AllowSwapTriangles(T1, T2: TTriangle2D_; E1, E2: integer): boolean;
var
  P10, P12, P20, P22: PPoing2D_;
begin
  Result := false;

  // We do not allow a swap if there's a segment on the edge between the triangles
  if assigned(T1.Segments[E1]) then
      exit;

  // The corner vertices
  P10 := T1.Vertices[E1].Point;
  P12 := T1.Vertices[E1 + 2].Point;
  P20 := T2.Vertices[E2].Point;
  P22 := T2.Vertices[E2 + 2].Point;

  // Point P20 inside or on border?
  if CrossProduct2D(Delta2D(P22^, P20^), Delta2D(P22^, P12^)) <= 0 then
      exit;
  // Point P10 inside or on border?
  if CrossProduct2D(Delta2D(P12^, P10^), Delta2D(P12^, P22^)) <= 0 then
      exit;

  // Avoid creating triangles with no width
  if PointToLineDist2DSqr(P20^, P22^, P12^) < FPrecisionSqr then
      exit;
  if PointToLineDist2DSqr(P10^, P12^, P22^) < FPrecisionSqr then
      exit;

  // Arriving here means "all ok"
  Result := true;
end;

procedure TDelaunayMesh2D_.CheckTriangleWithEdge(ATriangle: TTriangle2D_;
  AEdge: integer; Updates: TTriangle2DList_);
// local
  procedure CheckRecursive(ATriangle: TTriangle2D_; AEdge: integer);
  var
    T1, T2: TTriangle2D_;
    E1, E2: integer;
  begin
    // Two triangles
    T1 := ATriangle;
    T2 := ATriangle.Neighbours[AEdge];
    if not assigned(T2) then
        exit;
    // Two edge indices
    E1 := AEdge;
    E2 := T2.NeighbourIndex(T1);
    if E2 = -1 then
      // this should not happen.. the integrity is breached
        RaiseInfo('edges do not match');

    // Check if we need to swap these triangles
    if TDelaunayTriangle2D_(T1).VertexInCircle(T2.Vertices[E2 + 2]) or
      TDelaunayTriangle2D_(T2).VertexInCircle(T1.Vertices[E1 + 2]) then
      begin
        if not AllowSwapTriangles(T1, T2, E1, E2) then
            exit;
        // Yes we must swap
        SwapTriangles(T1, T2, E1, E2, Updates);
        // Recursive call
        CheckRecursive(T1, E1);
        CheckRecursive(T2, E2);
        CheckRecursive(T1, (E1 + 2) mod 3);
        CheckRecursive(T2, (E2 + 2) mod 3);
      end;
  end;

// main
begin
  CheckRecursive(ATriangle, AEdge);
end;

function TDelaunayMesh2D_.ForceDelaunay: integer;
var
  i, j, NewCount: integer;
  T: TDelaunayTriangle2D_;
begin
  Result := NonDelaunayTriangleCount;
  if Result = 0 then
      exit;
  repeat
    for i := 0 to Triangles.Count - 1 do
      begin
        T := TDelaunayTriangle2D_(Triangles[i]);
        if not T.IsDelaunay then
          begin
            // try in all directions
            for j := 0 to 2 do
                CheckTriangleWithEdge(T, j, nil);
          end;
      end;
    NewCount := NonDelaunayTriangleCount;
    DoExecutionStep('force delaunay cycle');
    if (NewCount >= Result) or (NewCount = 0) then
        break;
    Result := NewCount;
  until false;
  Result := NewCount;
end;

class function TDelaunayMesh2D_.GetTriangleClass: TTriangle2DClass_;
begin
  // This is the class we use
  Result := TDelaunayTriangle2D_;
end;

procedure TDelaunayMesh2D_.InitializeInfo;
begin
  inherited InitializeInfo;
  FSwapCount := 0;
  FCircleCalcCount := 0;
end;

function TDelaunayMesh2D_.IsDelaunay: boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to Triangles.Count - 1 do
    begin
      if not TDelaunayTriangle2D_(Triangles[i]).IsDelaunay then
          exit;
    end;
  Result := true;
end;

function TDelaunayMesh2D_.NonDelaunayTriangleCount: integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Triangles.Count - 1 do
    begin
      if not TDelaunayTriangle2D_(Triangles[i]).IsDelaunay then
          inc(Result);
    end;
end;

procedure TDelaunayMesh2D_.ReduceSegmentChain(AChain: TTriangleChain2D_; ARemovals: TTriangle2DList_);
var
  i, Idx, BackupIdx, StartIdx, S1, S2: integer;
  T1, T2: TTriangle2D_;
  E1, E2: integer;
  V1, V2, P1, P2: TVertex2D_;
  Delta: TPoing2D_;
  MustExchange: boolean;
  // local
  procedure GetTrianglesAndEdges(AIndex: integer);
  begin
    // Triangles and edges for the pair
    T1 := AChain.Triangles[AIndex];
    T2 := AChain.Triangles[AIndex + 1];
    E1 := T1.NeighbourIndex(T2);
    E2 := T2.NeighbourIndex(T1);
    if (E1 < 0) or (E2 < 0) then
        RaiseInfo('Edges do not match');
    // P1 and P2 vertex (left/right) for the pair
    P1 := T1.Vertices[E1 + 2];
    P2 := T2.Vertices[E2 + 2];
    // P1 below, on, or above line?
    if P1 = V1 then
        S1 := 0
    else
        S1 := Sign(CrossProduct2D(Delta, Delta2D(V1.Point^, P1.Point^)));
    // P2 below, on, or above line?
    if P2 = V2 then
        S2 := 0
    else
        S2 := Sign(CrossProduct2D(Delta, Delta2D(V1.Point^, P2.Point^)));
  end;

// main
begin
  if AChain.Count = 0 then
      exit;
  ARemovals.Clear;
  // Start and end vertex
  V1 := AChain.Triangles[0].Vertices[AChain.Edges[0] + 2];
  Idx := AChain.Count - 1;
  V2 := AChain.Triangles[Idx].Vertices[AChain.Edges[Idx]];
  // Delta
  Delta := Delta2D(V1.Point^, V2.Point^);

  StartIdx := 0;
  while AChain.Count > 1 do
    begin
      // Search for a swappable pair
      Idx := -1;
      BackupIdx := -1;
      for i := StartIdx to AChain.Count - 2 do
        begin
          // Triangles and edges for the pair
          GetTrianglesAndEdges(i);

          // Does it make sense to swap?
          if (S1 * S2 < 0) and AllowSwapTriangles(T1, T2, E1, E2) then
            begin
              // No, one point is above one is below, so the swap will not help us.
              // But in some cases, we *must* do this swap, if there are no others
              if BackupIdx < 0 then
                  BackupIdx := i;
              continue;
            end;

          if AllowSwapTriangles(T1, T2, E1, E2) then
            begin
              // OK, this pair may be swapped
              Idx := i;
              break;
            end;
        end;

      StartIdx := 0;

      // Swap a pair that cannot be deleted, but might keep the algo going?
      if (Idx < 0) and (BackupIdx >= 0) then
        begin
          Idx := BackupIdx;
          GetTrianglesAndEdges(Idx);
        end;

      // If Idx isn't found, there are no more triangles to swap.. bad news
      if Idx < 0 then
          RaiseInfo('Cannot reduce triangle chain');

      // We can swap this pair
      SwapTriangles(T1, T2, E1, E2, nil);

      // No deletion if below/above.. instead exchange them if sequence changed
      if S1 * S2 < 0 then
        begin
          if (Idx > 0) then
              MustExchange := T1.NeighbourIndex(AChain.Triangles[Idx - 1]) < 0
          else
              MustExchange := T2.NeighbourIndex(AChain.Triangles[Idx + 2]) < 0;
          if MustExchange then
              AChain.Exchange(Idx, Idx + 1)
          else
              StartIdx := Idx + 1;
          continue;
        end;

      // Determine which one to take out of the list
      if (S1 > 0) or (S2 > 0) then
        begin
          // triangle 2 must go
          AChain.Delete(Idx + 1);
          ARemovals.Add(T2);
        end
      else
        begin
          // triangle 1 must go (also in case S1 = 0 and S2 = 0, aka the last 2)
          AChain.Delete(Idx);
          ARemovals.Add(T1);
        end;
    end;

  if AChain.Count = 1 then
      AChain.Edges[0] := AChain.Triangles[0].VertexIndex(V2);

end;

procedure TDelaunayMesh2D_.SetPrecision(const Value: Double);
begin
  inherited SetPrecision(Value);
  // We set the delaunay precision as 1% of the precision
  FDelaunayPrecision := Value * 0.01;
end;

procedure TDelaunayMesh2D_.SwapTriangles(T1, T2: TTriangle2D_; E1, E2: integer; Updates: TTriangle2DList_);
var
  N: TTriangle2D_;
begin
  inc(FSwapCount);
  if assigned(Updates) then
    begin
      Updates.Add(T1);
      Updates.Add(T2);
    end;

  // Vertex triangle pointes
  T1.Vertices[E1 + 1].Triangle := T2;
  T2.Vertices[E2 + 1].Triangle := T1;

  // Vertex swap
  T1.Vertices[E1 + 1] := T2.Vertices[E2 + 2];
  T2.Vertices[E2 + 1] := T1.Vertices[E1 + 2];

  // Update neighbours' pointers back
  N := T1.Neighbours[E1 + 1];
  if assigned(N) then
      N.ReplaceNeighbour(T1, T2);
  N := T2.Neighbours[E2 + 1];
  if assigned(N) then
      N.ReplaceNeighbour(T2, T1);

  // Update our neighbour pointers
  T1.Neighbours[E1] := T2.Neighbours[E2 + 1];
  T2.Neighbours[E2] := T1.Neighbours[E1 + 1];
  T1.Neighbours[E1 + 1] := T2;
  T2.Neighbours[E2 + 1] := T1;

  // Update segments
  T1.Segments[E1] := T2.Segments[E2 + 1];
  T2.Segments[E2] := T1.Segments[E1 + 1];
  T1.Segments[E1 + 1] := nil;
  T2.Segments[E2 + 1] := nil;

  // Show result to user
  DoExecutionStep('swap triangles');
end;

{ TQualityMesh2D_ }

procedure TQualityMesh2D_.BuildBadTriangleList;
var
  i: integer;
  T: TQualityTriangle2D_;
begin
  DoStatus('Building bad triangle list');
  // Build the lists completely (first time)
  FBadTriangles.Clear;
  for i := 0 to Triangles.Count - 1 do
    begin
      T := TQualityTriangle2D_(Triangles[i]);
      if IsBadTriangle(T) then
          FBadTriangles.Add(T);
    end;
end;

procedure TQualityMesh2D_.Clear;
begin
  inherited Clear;
  FBadTriangles.Clear;
  FEncroached.Clear;
  FUpdates.Clear;
  FSteinerPoints.Clear;
end;

constructor TQualityMesh2D_.Create;
begin
  inherited Create;
  FBadTriangles := TSortedTriangle2DList_.Create(false);
  FEncroached := TEncroachItemList_.Create(true);
  FUpdates := TTriangle2DList_.Create(false);
  FSteinerPoints := TVertex2DList_.Create;
  SetMinimumAngle(cDefaultMinimumAngle);
  SetMinimumSegmentLength(cDefaultMinimumSegmentLength);
end;

function TQualityMesh2D_.DegenerateTriangleCount: integer;
var
  i, j: integer;
  S: TSegment2D_;
  T: TQualityTriangle2D_;
begin
  Result := 0;
  for i := 0 to Triangles.Count - 1 do
    begin
      T := TQualityTriangle2D_(Triangles[i]);
      for j := 0 to 2 do
        begin
          S := T.Segments[j];
          if not assigned(S) then
              continue;
          if IsDegenerate(S) then
            begin
              inc(Result);
              break;
            end;
        end;
    end;
end;

destructor TQualityMesh2D_.Destroy;
begin
  DisposeObject(FBadTriangles);
  FBadTriangles := nil;
  DisposeObject(FEncroached);
  FEncroached := nil;
  DisposeObject(FUpdates);
  FUpdates := nil;
  DisposeObject(FSteinerPoints);
  FSteinerPoints := nil;
  inherited Destroy;
end;

class function TQualityMesh2D_.GetTriangleClass: TTriangle2DClass_;
begin
  Result := TQualityTriangle2D_;
end;

function TQualityMesh2D_.IsBadTriangle(ATriangle: TQualityTriangle2D_): boolean;
begin
  // Minimum angle?
  Result := ATriangle.Quality > FMinimumAngleCos;
  if Result then
      exit;
  // Maximum element size?
  if FMaximumElementSize > 0 then
      Result := ATriangle.Area > FMaximumElementSize;
end;

function TQualityMesh2D_.IsDegenerate(ASegment: TSegment2D_): boolean;
begin
  // Check: do not split triangles on a segment shorter than our precision
  Result := ASegment.SquaredEncroachRadius < FMinSegLengthSqr;
end;

procedure TQualityMesh2D_.LocalRefine(const X, Y, AMaximumElementSize: Double);
var
  P: TPoing2D_;
  T: TTriangle2D_;
  // local
  function MustRefine(const P: TPoing2D_): boolean;
  var
    Res: THitTestTriangle_;
  begin
    Result := false;
    // Find the triangle under XY
    Res := HitTestTriangles(P, T, true);
    if Res = httNone then
        exit;
    Result := T.Area > AMaximumElementSize;
  end;

// main
begin
  P.X := X;
  P.Y := Y;
  T := nil;
  // repeat as long as there's work to do
  while MustRefine(P) do
    begin
      // Add the triangle to the bad list, so it gets split
      FBadTriangles.Add(T);
      // Now process the bad list
      ProcessBadTriangleList;
    end;
end;

function TQualityMesh2D_.MinimumAngleInMesh: Double;
var
  i: integer;
  ACos: Double;
  T: TQualityTriangle2D_;
begin
  Result := 0;
  for i := 0 to Triangles.Count - 1 do
    begin
      T := TQualityTriangle2D_(Triangles[i]);

      // Smallest angle cosine in triangle
      ACos := T.SmallestAngleCosine;
      if ACos > Result then
          Result := ACos;
    end;
  // Convert cosine to degrees
  Result := ArcCos(Result) * 180 / pi;
end;

procedure TQualityMesh2D_.PostProcessMesh;
begin
  // The algorithm is as follows. We first build a list of all bad triangles.
  // We then check this list to see if these bad triangles encroach on any
  // segments.
  BuildBadTriangleList;
  ProcessBadTriangleList;
  DoStatus(PFormat('Current min. angle: %5.2f', [MinimumAngleInMesh]));
  DoPhaseComplete('Quality generation');
end;

procedure TQualityMesh2D_.ProcessBadTriangleList;
var
  i: integer;
  T: TQualityTriangle2D_;
begin
  DoStatus('Processing bad triangle list');

  // Test all bad triangles for encroachment
  for i := FBadTriangles.Count - 1 downto 0 do
    begin
      T := TQualityTriangle2D_(FBadTriangles[i]);
      // We call with TestOnly, so no triangles get actually split
      SplitBadTriangle(T, true);
    end;

  repeat

    // Split all encroached segments, this has priority.
    while FEncroached.Count > 0 do
      begin
        // Split encroached segment
        SplitEncroachedSegment(FEncroached[0]);
        // Deal with possible updates
        UpdateLists;
      end;

    // Next, any bad triangle get split (only the worst one, then recheck encroachment)
    if FBadTriangles.Count > 0 then
      begin
        T := TQualityTriangle2D_(FBadTriangles[0]);
        DoStatus(PFormat('Current min. angle %5.2f in bad triangles (%d)',
          [ArcCos(T.Quality) * 180 / pi, FBadTriangles.Count]));
        SplitBadTriangle(T, false);
        // Deal with possible updates
        UpdateLists;
      end;

  until (FEncroached.Count = 0) and (FBadTriangles.Count = 0);
end;

procedure TQualityMesh2D_.SetBeta(const Value: Double);
begin
  FBeta := Value;
  FSquaredBeta := Sqr(FBeta);
end;

procedure TQualityMesh2D_.SetMinimumAngle(const Value: Double);
var
  MinAngleRad: Double;
begin
  if Value > 41.4 then
      RaiseInfo('Minimum value too high');
  FMinimumAngleDeg := Value;
  MinAngleRad := Value * pi / 180;
  FMinimumAngleCos := cos(MinAngleRad);
  // Calculate related beta factor. We adjust it downwards *just* a bit to
  // avoid detecting inserted quality triangles as having angles too small.
  SetBeta(1 / (2 * sin(0.5 * MinAngleRad)) - 1E-5);
end;

procedure TQualityMesh2D_.SetMinimumSegmentLength(const Value: Double);
begin
  FMinimumSegmentLength := Value;
  FMinSegLengthSqr := Sqr(FMinimumSegmentLength);
end;

procedure TQualityMesh2D_.SplitBadTriangle(ATriangle: TQualityTriangle2D_; TestOnly: boolean);
var
  i: integer;
  TriFound, N: TTriangle2D_;
  P: TPoing2D_;
  S: TSegment2D_;
  V: TVertex2D_;
  Res: boolean;
begin
  // Is the triangle worth splitting?
  if ATriangle.SquaredLongestEdgeLength < FMinSegLengthSqr then
    begin
      FBadTriangles.Remove(ATriangle);
      exit;
    end;

  // Get the off-center of this triangle
  P := ATriangle.OffCenter;

  repeat
    // Find the triangle at this point
    TriFound := ATriangle;
    HitTestTriangles(P, TriFound, false);
    if not assigned(TriFound) then
      begin
        // No triangle found: this means the offcenter is outside the triangulated area.
        // We cannot simply neglect this fact: we will try another point halfway between
        // the center and offcenter
        P := MidPoint2D(P, ATriangle.Center);
      end
    else
        break;
  until false;

  // We found a triangle. Do we encroach upon it?
  S := TQualityTriangle2D_(TriFound).EncroachedSegmentFromPoint(P);
  if not assigned(S) then
    begin
      // Also check neighbour triangles
      for i := 0 to 2 do
        begin
          N := TriFound.Neighbours[i];
          if assigned(N) then
            begin
              S := TQualityTriangle2D_(N).EncroachedSegmentFromPoint(P);
            end;
          if assigned(S) then
            begin
              TriFound := N;
              break;
            end;
        end;
    end;

  // We encroached on segment S if it exists
  if assigned(S) then
    begin
      if IsDegenerate(S) then
        begin
          // We are only going to get better by splitting a degenerate segment which
          // we won't do.. so just remove this badboy
          FBadTriangles.Remove(ATriangle);
          exit;
        end
      else
        begin
          // Deary.. it does encroach on a non-degenerate triangle with segment. Our
          // triangle still stays bad, but we add the encroached segment to be split.
          FEncroached.AddItem(ATriangle, TriFound, S);
          exit;
        end;
    end;

  // As long as there are encroached segments, we don't add bad triangles
  if TestOnly or (FEncroached.Count > 0) then
      exit;

  // Arriving here means the triangle doesn't encroach on somebody, we can safely
  // split it (by *adding* the vertex, this will correctly split triangles on edges).
  V := GetVertexClass.CreateWithCoords(P.X, P.Y);
  FSteinerPoints.Add(V);
  Res := AddVertexToTriangulation(V, FUpdates);
  if not Res then
    begin
      FSteinerPoints.Delete(FSteinerPoints.Count - 1);
      FBadTriangles.Remove(ATriangle);
    end;
end;

procedure TQualityMesh2D_.SplitEncroachedSegment(AItem: TEncroachItem_);
var
  T: TQualityTriangle2D_;
  S: TSegment2D_;
  V: TVertex2D_;
  C: TPoing2D_;
begin
  // get the first encroached segment (usually there's only one)
  T := TQualityTriangle2D_(AItem.Triangle);
  S := AItem.Segment;

  // Check: do not split triangles that are degenerate
  if IsDegenerate(S) then
    begin
      FEncroached.RemoveAllItemsWithSegment(S);
      exit;
    end;

  // Split this segment: we must make a new vertex and add it to our steiner points
  C := S.Center;
  V := NewVertex;
  V.Point^ := C;
  FSteinerPoints.Add(V);

  // Now split the triangle
  SplitTriangleEdge(T, T.SegmentIndex(S), V, FUpdates);
  // And remove this segment from the list of encroached segments
  FEncroached.RemoveAllItemsWithSegment(S);
end;

procedure TQualityMesh2D_.UpdateLists;
var
  i: integer;
  T: TQualityTriangle2D_;
begin
  for i := 0 to FUpdates.Count - 1 do
    begin
      T := TQualityTriangle2D_(FUpdates[i]);
      if not assigned(T) then
          continue;
      FEncroached.RemoveAllItemsWithTriangle(T);
      if IsBadTriangle(T) then
        begin
          FBadTriangles.Extract(T);
          FBadTriangles.Add(T);
          // Also re-test the bad triangle
          SplitBadTriangle(T, true);
        end
      else
          FBadTriangles.Remove(T);
    end;
  FUpdates.Clear;
end;
