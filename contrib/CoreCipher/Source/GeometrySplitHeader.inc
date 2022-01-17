type
  TItemCompareEvent = function(Item1, Item2: TCoreClassObject; Info: pointer): integer of object;
  TItemCompareMethod = function(Item1, Item2: TCoreClassObject; Info: pointer): integer;

  TCustomObjectList = class(TCoreClassObjectList)
  public
    procedure Append(AItem: TCoreClassObject);
  end;

  // TCustomSortedList is a TObjectList descendant providing easy sorting
  // capabilities, while keeping simplicity. Override the DoCompare method
  // to compare two items.
  TCustomSortedList = class(TCustomObjectList)
  private
    FSorted: boolean;
    procedure SetSorted(AValue: boolean);
  protected
    // Override this method to implement the object comparison between two
    // items. The default just compares the item pointers
    function DoCompare(Item1, Item2: TCoreClassObject): integer; virtual; abstract;
  public
    constructor Create(AutoFreeObj_: boolean);
    function Add(AItem: TCoreClassObject): integer;
    // AddUnique behaves just like Add but checks if the item to add is unique
    // by checking the result of the Find function. If the item is found it is
    // replaced by the new item (old item removed), unless RaiseError = True, in
    // that case an exception is raised.
    function AddUnique(Item: TCoreClassObject; RaiseError: boolean = false): integer; virtual;
    function Find(Item: TCoreClassObject; out Index: integer): boolean; virtual;
    // Find (multiple) items equal to Item, and return Index of first equal
    // item and the number of multiples in Count
    procedure FindMultiple(Item: TCoreClassObject; out AIndex, ACount: integer); virtual;
    procedure Sort; virtual;
    property Sorted: boolean read FSorted write SetSorted default true;
  end;

  // TSortedList is an object list that provides an events or method template
  // to compare items. Assign either OnCompare (for an event) or CompareMethod
  // (for a method template) to do the comparison of items. Additional information
  // required for the compare method can be passed with the CompareInfo pointer.
  TSortedList = class(TCustomSortedList)
  private
    FCompareInfo: pointer;
    FOnCompare_: TItemCompareEvent;
    FCompareMethod: TItemCompareMethod;
  protected
    function DoCompare(Item1, Item2: TCoreClassObject): integer; override;
  public
    property CompareInfo: pointer read FCompareInfo write FCompareInfo;
    // Use CompareMethod if you want to specify a compare method as stand-alone method
    property CompareMethod: TItemCompareMethod read FCompareMethod write FCompareMethod;
    // Use OnCompare if you want to specify a compare method as a method of a class
    property OnCompare: TItemCompareEvent read FOnCompare_ write FOnCompare_;
  end;

  TPoing2D_ = record
    case byte of
      0: (X, Y: double);
      1: (Elem: array [0 .. 1] of double);
  end;

  PPoing2D_ = ^TPoing2D_;

  TTriangle2D_ = class;
  TTriMesh2D_ = class;

  // Basic 2D vertex class, containing an FPoint (TPoing2D_) field with X and Y coordinate.
  TVertex2D_ = class(TCoreClassPersistent)
  private
    FPoint: TPoing2D_;
    function GetX: double;
    function GetY: double;
    procedure SetX(const Value: double);
    procedure SetY(const Value: double);
    function GetPoint: PPoing2D_;
  protected
    function GetTriangle: TTriangle2D_; virtual; abstract;
    procedure SetTriangle(const Value: TTriangle2D_); virtual; abstract;
  public
    constructor Create; virtual;
    constructor CreateWithCoords(const AX, AY: double);
    procedure Assign(Source: TCoreClassPersistent); override;
    property X: double read GetX write SetX;
    property Y: double read GetY write SetY;
    property Point: PPoing2D_ read GetPoint;
    // Reference back to the triangle this vertex belongs to. In fact, there can
    // be many triangles this vertex belongs to, but this is one of them (not
    // specified which one). If Triangle = nil, there is no reference yet.
    property Triangle: TTriangle2D_ read GetTriangle write SetTriangle;
  end;

  TVertex2DClass_ = class of TVertex2D_;

  TVertex2DList_ = class(TCoreClassObjectList)
  private
    function GetItems(Index: integer): TVertex2D_;
    procedure SetItems(Index: integer; const Value: TVertex2D_);
  public
    property Items[Index: integer]: TVertex2D_ read GetItems write SetItems; default;
  end;

  // 2D vertex class with additional Triangle pointer
  TTriVertex2D_ = class(TVertex2D_)
  private
    FTriangle: TTriangle2D_;
  protected
    function GetTriangle: TTriangle2D_; override;
    procedure SetTriangle(const Value: TTriangle2D_); override;
  public
    procedure Assign(Source: TCoreClassPersistent); override;
  end;

  // A segment is a boundary that connects two vertices. When a segment is present
  // the triangles bordering it cannot be swapped, thus constraining the triangulation
  TSegment2D_ = class(TCoreClassPersistent)
  private
    FValidMetrics: boolean;
    FVertex1: TVertex2D_;
    FVertex2: TVertex2D_;
    FCenter: TPoing2D_;
    FNormal: TPoing2D_;
    FSquaredEncroachRadius: double;
    procedure SetVertex1(const Value: TVertex2D_);
    procedure SetVertex2(const Value: TVertex2D_);
    function GetCenter: TPoing2D_;
    function GetSquaredEncroachRadius: double;
    function GetNormal: TPoing2D_;
  protected
    procedure CalculateMetrics; virtual;
  public
    constructor CreateWithVertices(AVertex1, AVertex2: TVertex2D_);
    procedure Assign(Source: TCoreClassPersistent); override;
    procedure Invalidate;
    // Replaces references in the segment to OldVertex by a reference to NewVertex.
    procedure ReplaceVertex(OldVertex, NewVertex: TVertex2D_);
    // Find the intersection point of us with ASegment, and create and return a vertex
    // if it does, or nil if no intersection.
    function IntersectWith(ASegment: TSegment2D_): TVertex2D_;
    // Is AVertex lying on this segment? Use the passed precision as tolerance
    // (pass the square of required precision)
    function IsVertexOnSegment(AVertex: TVertex2D_; APrecisionSqr: double): boolean;
    // Does point P encroach on this segment?
    function PointEncroaches(const P: TPoing2D_): boolean;
    // Reference to start vertex of this segment
    property Vertex1: TVertex2D_ read FVertex1 write SetVertex1;
    // Reference to end vertex of this segment
    property Vertex2: TVertex2D_ read FVertex2 write SetVertex2;
    // Center (midpoint) between the two vertices
    property Center: TPoing2D_ read GetCenter;
    // The normal of this segment. It points outwards from the graph, perpendicular
    // to the segment, and is unit length
    property Normal: TPoing2D_ read GetNormal;
    // The encroach radius is slightly bigger (10%) than the actual radius of
    // the segment's circle. This way, points encroach on it slightly quicker
    // esp near segment endpoints.
    property SquaredEncroachRadius: double read GetSquaredEncroachRadius;
  end;

  TSegment2DClass_ = class of TSegment2D_;

  TSegment2DList_ = class(TCoreClassObjectList)
  private
    function GetItems(Index: integer): TSegment2D_;
  public
    property Items[Index: integer]: TSegment2D_ read GetItems; default;
  end;

  // hit-test result for hit-testing triangles
  THitTestTriangle_ = (
    httNone,   // Not on the triangle
    httBody,   // On the body of the triangle
    httVtx0,   // On or close to triangle's vertex 0
    httVtx1,   // On or close to triangle's vertex 1
    httVtx2,   // On or close to triangle's vertex 2
    httEdge0,  // On the body, and on edge 0
    httEdge1,  // On the body, and on edge 1
    httEdge2,  // On the body, and on edge 2
    httClose0, // Not on the body but close to edge 0
    httClose1, // Not on the body but close to edge 1
    httClose2  // Not on the body but close to edge 2
    );

  // Basic class for triangles that are present in a triangular 2D mesh
  TTriangle2D_ = class(TCoreClassPersistent)
  private
    FVertices: array [0 .. 2] of TVertex2D_;
    FNormals: array [0 .. 2] of TPoing2D_;
    FNeighbours: array [0 .. 2] of TTriangle2D_;
    FCenter: TPoing2D_;
    FRegionIndex: integer;
    function GetVertices(Index: integer): TVertex2D_;
    procedure SetVertices(Index: integer; const Value: TVertex2D_);
    function GetNeighbours(Index: integer): TTriangle2D_;
    procedure SetNeighbours(Index: integer; const Value: TTriangle2D_);
    function GetCenter: TPoing2D_;
  protected
    FValidMetrics: boolean;
    FMesh: TTriMesh2D_; // pointer back to mesh
    function GetSegments(Index: integer): TSegment2D_; virtual;
    procedure SetSegments(Index: integer; const Value: TSegment2D_); virtual;
    // Calcuate metrics for this triangle, may be overridden in descendants to
    // calculate more metrics
    procedure CalculateMetrics; virtual;
    procedure InvalidateSegments;
  public
    constructor Create; virtual;
    procedure Invalidate;
    // Set the vertices a, b, c all at the same time
    procedure HookupVertices(VertexA, VertexB, VertexC: TVertex2D_);
    // Set the neighbours a, b, c all at the same time
    procedure HookupNeighbours(TriangleA, TriangleB, TriangleC: TTriangle2D_);
    // Replace the neighbour OldNeighbour (if we have it) by NewNeighbour
    procedure ReplaceNeighbour(OldNeighbour, NewNeighbour: TTriangle2D_);
    // Returns index 0, 1, or 2 if ATriangle is one of it's neighbours, or -1
    // if ATriangle isn't
    function NeighbourIndex(ATriangle: TTriangle2D_): integer;
    // Returns index 0, 1, or 2 if AVertex is one of the vertices of ATriangle,
    // or -1 if not
    function VertexIndex(AVertex: TVertex2D_): integer;
    // Returns index 0, 1, or 2 if ASegment is one of the segments of ATriangle,
    // or -1 if not
    function SegmentIndex(ASegment: TSegment2D_): integer;
    // Hit-test the triangle with APoint, and return one of the hittest
    // values.
    function HitTest(const APoint: TPoing2D_): THitTestTriangle_;
    // Returns the edge index of the edge that crosses the line when going from
    // the center of this triangle to point APoint (and beyond).
    function EdgeFromCenterTowardsPoint(const APoint: TPoing2D_): integer;
    // Returns the signed area of this triangle (result is positive when triangle
    // is defined counter-clockwise, and negative if clockwise).
    function Area: double;
    // Returns the cosine of the angle at vertex Index
    function AngleCosine(Index: integer): double;
    // Returns the cosine of the smallest angle in the triangle
    function SmallestAngleCosine: double;
    // Returns the square of the length of the longest edge
    function SquaredLongestEdgeLength: double;
    // References to the vertices of which this triangle consists. The vertices
    // are numbered 0, 1, 2 (also referred to as a, b, c).
    // The triangle must always be described in counterclockwise direction.
    property Vertices[Index: integer]: TVertex2D_ read GetVertices write SetVertices;
    // References to the neighbouring triangles, or nil if there is none at this location
    // Neighbour 0 corresponds to the neighbour along edge ab, neighbour 1 to edge bc
    // and neighbour 2 to edge ca.
    property Neighbours[Index: integer]: TTriangle2D_ read GetNeighbours write SetNeighbours;
    // Returns reference to the segment at edge Index. Segments are only actually
    // added in a descendant class, so in the base class TTriangle2D_ nil is returned
    property Segments[Index: integer]: TSegment2D_ read GetSegments write SetSegments;
    // Returns center of triangle (3 points averaged)
    property Center: TPoing2D_ read GetCenter;
    // Index of the region this triangle belongs to, or -1 if none
    property RegionIndex: integer read FRegionIndex write FRegionIndex;
  end;

  TTriangle2DClass_ = class of TTriangle2D_;

  // List of triangles.
  TTriangle2DList_ = class(TCoreClassObjectList)
  private
    function GetItems(Index: integer): TTriangle2D_;
  public
    property Items[Index: integer]: TTriangle2D_ read GetItems; default;
  end;

  // The object represents general triangle-edge group.
  // Capacity will be increased when needed, but will never be reduced, to avoid memory fragmentation.
  TTriangleGroup2D_ = class(TCoreClassPersistent)
  private
    FTriangles: array of TTriangle2D_;
    FEdges: array of integer;
    FCount: integer;
    FCapacity: integer;
    function GetTriangles(Index: integer): TTriangle2D_;
  protected
    function GetEdges(Index: integer): integer;
    procedure SetEdges(Index: integer; const Value: integer);
  public
    procedure Clear; virtual;
    // Add a triangle reference and edge index to the end of the list
    procedure AddTriangleAndEdge(ATriangle: TTriangle2D_; AEdge: integer);
    // Insert a triangle reference and edge index in the list at AIndex
    procedure InsertTriangleAndEdge(AIndex: integer; ATriangle: TTriangle2D_; AEdge: integer);
    // Delete triangle and edge at AIndex
    procedure Delete(AIndex: integer);
    // Exchange triangle/edge pairs at Index1 and Index2
    procedure Exchange(Index1, Index2: integer);
    // List of triangles in this triangle group
    property Triangles[Index: integer]: TTriangle2D_ read GetTriangles;
    // Number of triangles in the triangle group
    property Count: integer read FCount;
  end;

  // Represents a fan of triangles around the Vertex. This class is used in linear searches.
  TTriangleFan2D_ = class(TTriangleGroup2D_)
  private
    FCenter: TVertex2D_;
    procedure SetCenter(const Value: TVertex2D_);
    function GetVertices(Index: integer): TVertex2D_;
  protected
    procedure BuildTriangleFan(ABase: TTriangle2D_); virtual;
  public
    procedure Clear; override;
    // Move the triangle fan to another center vertex that lies on the other end
    // of the outgoing edge of triangle at AIndex
    procedure MoveToVertexAt(AIndex: integer);
    // Return the index of the triangle that might cover the point APoint
    function TriangleIdxInDirection(const APoint: TPoing2D_): integer;
    // Return the triangle that might cover the vertex AVertex
    function TriangleInDirection(const APoint: TPoing2D_): TTriangle2D_;
    // Runs through the Vertices array, and if a vertex matches, it's index is
    // returned. If none matches, -1 is returned.
    function VertexIndex(AVertex: TVertex2D_): integer;
    // The center vertex of the triangle fan. Set Center to a vertex in the mesh
    // and the triangle fan around it will be rebuilt. Center must have a pointer
    // back to a triangle (it cannot be nil)
    property Center: TVertex2D_ read FCenter write SetCenter;
    // List of outward pointing edge indices in this triangle fan
    property OutwardEdges[Index: integer]: integer read GetEdges;
    // Vertices at the other end of the outward pointing edge at Index in the
    // triangle fan
    property Vertices[Index: integer]: TVertex2D_ read GetVertices;
  end;

  // A triangle chain between vertex1 and vertex2
  TTriangleChain2D_ = class(TTriangleGroup2D_)
  private
    FVertex1, FVertex2: TVertex2D_;
  public
    // Build a triangle chain from Vertex1 to Vertex2. For searching, use ASearchFan
    // if assigned, or use temporary search fan if nil. If a chain was found,
    // the function result is true
    function BuildChain(AVertex1, AVertex2: TVertex2D_; var ASearchFan: TTriangleFan2D_): boolean;
    // List of edge indices in this triangle chain.. the edge index points to the
    // edge crossing the line from Vertex1 to Vertex2, except for the last one,
    // where it indicates the index of Vertex2.
    property Edges[Index: integer]: integer read GetEdges write SetEdges;
  end;

  // A mesh consisting of triangles and vertices, where each triangle contains reference to 3 vertices.
  TTriMesh2D_ = class(TCoreClassPersistent)
  private
    FPrecision: double;
    FVertices: TVertex2DList_;
    FTriangles: TTriangle2DList_;
    FSegments: TSegment2DList_;
    FSearchSteps: integer;
    // comparison function to sort triagles by Center.X, smallest values first
    function TriangleCompareLeft(Item1, Item2: TCoreClassObject; Info: pointer): integer;
  protected
    FPrecisionSqr: double;
    procedure SetPrecision(const Value: double); virtual;
    // Create a new vertex of correct class
    function NewVertex: TVertex2D_;
    class function GetVertexClass: TVertex2DClass_; virtual;
    // Create a new triangle of correct class
    function NewTriangle: TTriangle2D_;
    class function GetTriangleClass: TTriangle2DClass_; virtual;
    // Create a new segment of correct class
    function NewSegment: TSegment2D_;
    class function GetSegmentClass: TSegment2DClass_; virtual;
    // Initialize info properties
    procedure InitializeInfo; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    // Clear the mesh
    procedure Clear; virtual;
    // Create a convex hull around all the vertices in the mesh
    procedure ConvexHull;
    // Optimize the mesh for usage in a finite element method. The triangle list
    // is sorted such that the triangles form a long chain going up and down from
    // left to right, and all vertices used are placed in the AVertices list, which
    // is also sorted by usage in the triangles. Thus, vertices connected to each
    // other usually are also have an index relatively close in the Vertices list,
    // which accomplishes that the finite element matrix is more banded than with
    // a random distribution (and gauss elimination works faster). The AVertices
    // array must be initialized, it will be cleared and then filled with all
    // vertices used by the triangles.
    procedure OptimizeForFEM(AVertices: TVertex2DList_);
    // Remove all segments that are non-functional (e.g. vertex pointers are nil,
    // or vertex1 and vertex2 point to the same vertex)
    procedure RemoveNonSegments;
    // Get the min and max location of the mesh. Returns false if there are no
    // vertices
    function BoundingBox(var AMin, AMax: TPoing2D_): boolean;
    // Returns the sum of all triangle's absolute area
    function AbsoluteArea: double;
    // Returns the sum of all triangle's signed area
    function SignedArea: double;
    // Locate the vertex that is closest to APoint. The function returns the closest
    // vertex from the vertex list to APoint. If there are no vertices in the list,
    // nil is returned. The basic algorithm is a linear search but this can
    // be overridden in descendants (to implement e.g. a quadtree approach).
    // A TTriangleFan2D_ object can be passed in Fan to speed up searching.
    function LocateClosestVertex(const APoint: TPoing2D_;
      AFan: TTriangleFan2D_ = nil): TVertex2D_; virtual;
    // List of vertices used in this mesh
    property Vertices: TVertex2DList_ read FVertices;
    // List of triangles used in this mesh
    property Triangles: TTriangle2DList_ read FTriangles;
    // List of segments used in this mesh
    property Segments: TSegment2DList_ read FSegments;
    // Precision used when generating mesh. If a point lies within precision
    // from a triangle edge, it is considered to be on it, the edge will be
    // split instead of the body.
    // When a vertex lays within Precision of another vertex, no new triangle
    // will be created, thus the vertex is skipped (not triangulated).
    property Precision: double read FPrecision write SetPrecision;
    // Number of search steps performed in linear search.
    property SearchSteps: integer read FSearchSteps;
  end;

  TTriMesh2DClass_ = class of TTriMesh2D_;

  TConvexHull_ = class(TCoreClassPersistent)
  private
    FMesh: TTriMesh2D_;
    // Add a new segment in the hull, using vertices with Idx1, Idx2
    procedure AddSegment(Idx1, Idx2: integer);
    // Is the vertex AVertex left of line V1-V2? (looking from point V1)
    function IsLeftOfLine(const V1, V2, AVertex: TVertex2D_): boolean;
    // Does AVertex violate ASegment (outside of it?)
    function SegmentViolated(ASegment: TSegment2D_; AVertex: TVertex2D_): boolean;
    // Add a new vertex to the hull. This updates the hull segments if the
    // vertex falls outside of them
    procedure AddVertexToHull(AVertex: TVertex2D_);
  public
    procedure MakeConvexHull(AMesh: TTriMesh2D_);
  end;

  // Class encapsulating 2D Planar Straightline Graphs (PSLG)
  TGraph2D_ = class(TCoreClassPersistent)
  private
    FVertices: TVertex2DList_;
    FSegments: TSegment2DList_;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    // Replaces references in the segment list to OldVertex by a reference to
    // NewVertex.
    procedure ReplaceVertex(OldVertex, NewVertex: TVertex2D_);
    // List of vertices in this PSLG
    property Vertices: TVertex2DList_ read FVertices;
    // List of segments in this PSLG
    property Segments: TSegment2DList_ read FSegments;
  end;

  // A Segment Triangle contains references for each edge to a segment, or nil
  // if there is no graph segment for this edge.
  TSegmentTriangle2D_ = class(TTriangle2D_)
  private
    FSegments: array [0 .. 2] of TSegment2D_;
  protected
    function GetSegments(Index: integer): TSegment2D_; override;
    procedure SetSegments(Index: integer; const Value: TSegment2D_); override;
  end;

  TMeshRegion_ = class(TCoreClassObject)
  private
    FWindingNumber: integer;
    FIsOuterRegion: boolean;
  public
    // Winding number of this region
    property WindingNumber: integer read FWindingNumber write FWindingNumber;
    // This region is the outer region (only one)
    property IsOuterRegion: boolean read FIsOuterRegion write FIsOuterRegion;
  end;

  TMeshRegionList_ = class(TCoreClassObjectList)
  private
    function GetItems(Index: integer): TMeshRegion_;
  public
    property Items[Index: integer]: TMeshRegion_ read GetItems; default;
  end;

  TTriangulationEvent = procedure(Sender: TCoreClassObject; const AMessage: SystemString) of object;

  // Which triangles should be removed after triangulation?
  TRemovalStyle_ = (
    rsNone,    // Remove no triagles
    rsOutside, // Remove all triangles that are connected to construction vertices
    rsEvenOdd, // Even-Odd fillrule removal
    rsNonZero, // Non-Zero fillrule removal
    rsNegative // Remove all triangles with windingnumber < 0
    );

  // Triangulates a polygon or other Planar Straightline Graphs (PSLG) into a triangular mesh.
  TTriangulationMesh2D_ = class(TTriMesh2D_)
  private
    FCornerPoints: TVertex2DList_;
    FRemovals: TTriangle2DList_;
    FRegions: TMeshRegionList_;
    FSegmentChain: TTriangleChain2D_;
    FSearchFan: TTriangleFan2D_;
    FTick: TTimeTick;
    FVertexSkipCount: integer;
    FSplitEdgeCount: integer;
    FSplitBodyCount: integer;
    FHitTests: integer;
    FAreaInitial: double;
    FCalculationTime: double;
    FOnExecutionStep: TTriangulationEvent;
    FOnPhaseComplete: TTriangulationEvent;
    FOnStatus: TTriangulationEvent;
  protected
    FBBMin, FBBMax: TPoing2D_;
    FMeshMin, FMeshMax: TPoing2D_;
    class function GetTriangleClass: TTriangle2DClass_; override;
    // perform the execution step event
    procedure DoExecutionStep(const AMessage: SystemString);
    // perform phase complete event
    procedure DoPhaseComplete(const AMessage: SystemString);
    // perform status event
    procedure DoStatus(const AMessage: SystemString);
    // Replace OldVertex by NewVertex in all segments
    procedure ReplaceVertexInSegments(Old_, New_: TVertex2D_);
    // Prepare the mesh so it can triangulate vertices by adding 4 corner points
    // and 2 initial triangles
    procedure PrepareMeshConstruction; virtual;
    // Remove the mesh construction elements that were created initially
    procedure RemoveMeshConstruction(ARemovalStyle: TRemovalStyle_); virtual;
    // Detect all the regions in the mesh, give each triangle a region index
    procedure DetectRegions; virtual;
    // Add another segment to the triangulation
    function AddSegmentToTriangulation(ASegment: TSegment2D_): boolean;
    // Add another vertex to the triangulation, subsequently dividing the mesh.
    // When False is returned, the vertex was not added (it fell on top of another vertex)
    function AddVertexToTriangulation(AVertex: TVertex2D_; Updates: TTriangle2DList_): boolean; virtual;
    // Split the triangle into 3 sub-triangles, at point AVertex on the body. The
    // point is guaranteed to lie on the triangle prior before calling this
    // method.
    procedure SplitTriangleBody(ATriangle: TTriangle2D_; AVertex: TVertex2D_; Updates: TTriangle2DList_);
    // Split the triangle into 2 on the edge (with AEdge index) at AVertex, and do the same with the triangle opposing the edge.
    // In some rare cases, this might lead to degenerate triangles at the opposing edge,
    // in this case the triangle's body is split. AVertex is guaranteed to lie in the triangle, or just on the edge.
    procedure SplitTriangleEdge(ATriangle: TTriangle2D_; AEdge: integer; AVertex: TVertex2D_; Updates: TTriangle2DList_);
    // Hittest the triangle list to find the triangle under the position where
    // AVertex is located. If a triangle was hit, it is returned in the ATriangle.
    // Result indicates where the triangle was hit. If ATriangle contains a
    // reference upon calling, it will be used as an initial guess. Set UseQuick
    // to true if the ATriangle input is expected to be far away from the hit,
    // or set to False if ATriangle is probably the one hit. If called with
    // UseQuick = false, ATriangle *must* be assigned!
    function HitTestTriangles(const APoint: TPoing2D_; var ATriangle: TTriangle2D_; UseQuick: boolean): THitTestTriangle_;
    // This routine can be called when separate triangle groups in the mesh may be no longer
    // connected (after removal). Since the normal method relies on connectedness,
    // it can fail. This method is *much* slower, simply verifies each triangle,
    // but guarantees a result is returned in these odd cases.
    function BruteForceHitTestTriangles(const APoint: TPoing2D_; var ATriangle: TTriangle2D_): THitTestTriangle_;
    // Do post-processing on the mesh.
    procedure PostProcessMesh; virtual;
    // Check triangle after it was inserted, the AEdge indicates the edge number
    // for which neighbours need to be checked.
    procedure CheckTriangleWithEdge(ATriangle: TTriangle2D_; AEdge: integer; Updates: TTriangle2DList_); virtual;
    // Generate a list of triangles that occur around AVertex. The list AList will
    // be cleared before this is done. AVertex should have a valid pointer to
    // one of the triangles it belongs to. If the result is false, AVertex didn't
    // have a pointer, or it was an invalid pointer.
    function BuildTriangleFan(AList: TTriangle2DList_; AVertex: TVertex2D_): boolean;
    // Remove ATriangle from the mesh. This also resets the neighbours so they
    // do not point to the triangle. ATriangle will be disposed of.
    procedure RemoveTriangleFromMesh(ATriangle: TTriangle2D_);
    // Reduce the chain by swapping triangles. Since this is a delaunay action,
    // we do not implement it here but in descendant.
    procedure ReduceSegmentChain(AChain: TTriangleChain2D_; ARemovals: TTriangle2DList_); virtual;
    // Initialize info properties
    procedure InitializeInfo; override;
    // Finalize info properties
    procedure FinalizeInfo; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
    // Clear all vertices, triangles and segments in the mesh, and initialize
    // all statistics.
    procedure Clear; override;
    // Add the vertices and segments of AGraph to our mesh. This doesn't triangulate
    // them yet, call Triangulate to triangulate all the graphs that have been added.
    procedure AddGraph(AGraph: TGraph2D_); virtual;
    // Triangulate the graphs that were added with AddGraph, by adding all the
    // vertices and segments in turn to the mesh. Before this is done, the mesh
    // is cleared and 4 corner points are added well outside the polygon's
    // bounding box. Between these 4 points, 2 initial triangles are added.
    // After the triangulation finishes, but before post-processing, the bounding
    // corners + triangles not part of the final mesh will be removed, unless
    // ARemovalStyle = rsNone.
    procedure Triangulate(ARemovalStyle: TRemovalStyle_ = rsOutside);
    // List of mesh regions. Regions have a winding number which indicates
    // visibility according to fill rule
    property Regions: TMeshRegionList_ read FRegions;
    // Number of vertices skipped in triangulation. Skipping happens because sometimes
    // vertices may lay almost on top of other vertices (within Precision), and
    // these vertices will be skipped.
    property VertexSkipCount: integer read FVertexSkipCount;
    // Number of triangle body splits that occurred in triangulation
    property SplitBodyCount: integer read FSplitBodyCount;
    // Number of triangle edge splits that occurred in triangulation
    property SplitEdgeCount: integer read FSplitEdgeCount;
    // The number of triangle hit tests performed.
    property HitTests: integer read FHitTests;
    // Initial area after creating the bounding box
    property AreaInitial: double read FAreaInitial;
    // Total time in seconds for triangulation (including postprocessing)
    property CalculationTime: double read FCalculationTime;
    // Connect an event to this handler to get information on each step in the execution
    property OnExecutionStep: TTriangulationEvent read FOnExecutionStep write FOnExecutionStep;
    // Connect an event to this handler to get information on completed phases
    property OnPhaseComplete: TTriangulationEvent read FOnPhaseComplete write FOnPhaseComplete;
    // Information for the status line (fast update rate)
    property OnStatus: TTriangulationEvent read FOnStatus write FOnStatus;
  end;

  TDelaunayTriangle2D_ = class(TSegmentTriangle2D_)
  private
    FSquaredRadius: double;
    FCircleCenter: TPoing2D_;
    function GetCircleCenter: TPoing2D_;
    function GetSquaredRadius: double;
  protected
    procedure CalculateMetrics; override;
  public
    // Test whether AVertex lies within the Delaunay circle of this triangle
    function VertexInCircle(AVertex: TVertex2D_): boolean;
    // Check if this triangle is in fact abiding the delaunay criterium (no neighbouring
    // triangle's opposite points inside the circle going through its 3 vertices)
    function IsDelaunay: boolean;
    // Returns the Delaunay circle center of this triangle
    property CircleCenter: TPoing2D_ read GetCircleCenter;
    // Returns the squared radius of the Delaunay circle of this triangle
    property SquaredRadius: double read GetSquaredRadius;
  end;

  TQualityTriangle2D_ = class(TDelaunayTriangle2D_)
  private
    FQuality: double;
    function GetOffCenter: TPoing2D_;
  protected
    function GetQuality: double; virtual;
    procedure CalculateMetrics; override;
  public
    // Does this triangle have an encroached segment?
    function HasEncroachedSegment: boolean;
    // Return the segment that is encroached due to APoint, or nil if none
    function EncroachedSegmentFromPoint(const APoint: TPoing2D_): TSegment2D_;
    // Calculate and return the OffCenter point for this triangle
    property OffCenter: TPoing2D_ read GetOffCenter;
    // Quality is defined as the smallest angle cosine. Larger values mean worse quality
    property Quality: double read GetQuality;
  end;

  TSortedTriangle2DList_ = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TQualityTriangle2D_;
  protected
    function DoCompare(Item1, Item2: TCoreClassObject): integer; override;
  public
    property Items[Index: integer]: TQualityTriangle2D_ read GetItems; default;
  end;

  TEncroachItem_ = class(TCoreClassObject)
  private
    FSegment: TSegment2D_;
    FEncroacher: TTriangle2D_;
    FTriangle: TTriangle2D_;
  public
    // The triangle that encroaches upon the segment
    property Encroacher: TTriangle2D_ read FEncroacher write FEncroacher;
    // The segment that was encroached
    property Segment: TSegment2D_ read FSegment write FSegment;
    // The triangle that connects to the encroached segment
    property Triangle: TTriangle2D_ read FTriangle write FTriangle;
  end;

  TEncroachItemList_ = class(TCustomSortedList)
  private
    function GetItems(Index: integer): TEncroachItem_;
  protected
    function DoCompare(Item1, Item2: TCoreClassObject): integer; override;
  public
    // Add a new item if not yet present. AEncroacher is the triangle causing
    // the encroach, ATriangle is the triangle having a segment ASegment that is
    // encroached
    procedure AddItem(AEncroacher, ATriangle: TTriangle2D_; ASegment: TSegment2D_);
    // Return the index of an item that has ATriangle as triangle, or -1 if none
    function IndexByTriangle(ATriangle: TTriangle2D_): integer;
    // Remove all items that have ATriangle as Encroacher or Triangle
    procedure RemoveAllItemsWithTriangle(ATriangle: TTriangle2D_);
    procedure RemoveAllItemsWithSegment(ASegment: TSegment2D_);
    property Items[Index: integer]: TEncroachItem_ read GetItems; default;
  end;

  // TDelaunayMesh2D_ implements a delaunay triangulation of a polygon or point cloud.
  TDelaunayMesh2D_ = class(TTriangulationMesh2D_)
  private
    FSwapCount: integer;
    FCircleCalcCount: integer;
    FDelaunayPrecision: double;
  protected
    procedure SetPrecision(const Value: double); override;
    class function GetTriangleClass: TTriangle2DClass_; override;
    // Check triangle after it was inserted, the AEdge indicates the edge number
    // for which neighbours need to be checked. See if we need to swap this
    // triangle.
    procedure CheckTriangleWithEdge(ATriangle: TTriangle2D_; AEdge: integer;
      Updates: TTriangle2DList_); override;
    // The T1 and T2 triangles should swap their common edge. However, this may not
    // be done under some circumstances. This check should evaluate these. For the
    // standard Delaunay this check ensures the triangles form a convex hull, and
    // that they are not constrained by a segment.
    function AllowSwapTriangles(T1, T2: TTriangle2D_; E1, E2: integer): boolean; virtual;
    // Reduce the chain by swapping triangle pairs
    procedure ReduceSegmentChain(AChain: TTriangleChain2D_; ARemovals: TTriangle2DList_); override;
    // Do the actual swap of triangle T1 and T2 along edges E1 and E2. This function
    // does *not* check if the swap may be made, see AllowSwapTriangles for the
    // check.
    procedure SwapTriangles(T1, T2: TTriangle2D_; E1, E2: integer; Updates: TTriangle2DList_);
    procedure InitializeInfo; override;
  public
    // Count the number of triangles that do not abide Delaunay
    function NonDelaunayTriangleCount: integer;
    // Check whether all triangles abide Delaunay
    function IsDelaunay: boolean;
    // Iterate through the triangles and try to force the non-delaunay ones
    // to adapt. This method can be called after completion of Triangulate. It
    // makes no sense to call this method more than once, unless changes are made
    // to the mesh (the procedure already contains a loop). The return is the new
    // number of non-delaunay triangles.
    function ForceDelaunay: integer;
    // Number of triangle swaps that occurred during triangulation
    property SwapCount: integer read FSwapCount;
    // Number of circle calculations that occurred during triangulation. A
    // circle calculation is used to determine the circle through the 3 points
    // of a triangle.
    property CircleCalcCount: integer read FCircleCalcCount;
  end;

  TQualityMesh2D_ = class(TDelaunayMesh2D_)
  private
    FBadTriangles: TSortedTriangle2DList_; // List of bad triangles
    FEncroached: TEncroachItemList_;       // List of encroached segments + info
    FUpdates: TTriangle2DList_;
    FSteinerPoints: TVertex2DList_;
    FSquaredBeta: double;
    FBeta: double;
    FMinimumAngleDeg: double;
    FMinimumAngleCos: double;
    FMinimumSegmentLength: double;
    FMinSegLengthSqr: double;
    FMaximumElementSize: double;
    procedure SetBeta(const Value: double);
    procedure SetMinimumAngle(const Value: double);
    procedure SetMinimumSegmentLength(const Value: double);
  protected
    class function GetTriangleClass: TTriangle2DClass_; override;
    // Post process the mesh: in this process we subdivide the triangles and
    // add Steiner points.
    procedure PostProcessMesh; override;
    procedure BuildBadTriangleList; virtual;
    procedure ProcessBadTriangleList; virtual;
    procedure UpdateLists; virtual;
    procedure SplitEncroachedSegment(AItem: TEncroachItem_); virtual;
    procedure SplitBadTriangle(ATriangle: TQualityTriangle2D_; TestOnly: boolean); virtual;
    function IsDegenerate(ASegment: TSegment2D_): boolean;
    // Is this a bad triangle? (its smallest angle is smaller than the minimum set)
    function IsBadTriangle(ATriangle: TQualityTriangle2D_): boolean;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Clear; override;
    // Refines the mesh locally around X, Y until the element under X,Y is not
    // larger than AMaximumElementSize
    procedure LocalRefine(const X, Y, AMaximumElementSize: double);
    // Returns the minimum angle found in the mesh, in degrees.
    function MinimumAngleInMesh: double;
    // Number of degenerate triangles present in the mesh (due to segment angles
    // being too small)
    function DegenerateTriangleCount: integer;
    // Specify the minimum angle in degrees that may appear within each triangle in the
    // quality triangulation. The practical upper limit for this value is around 33 degrees.
    property MinimumAngle: double read FMinimumAngleDeg write SetMinimumAngle;
    // If segments are to be split, this will not be done if the resulting segments'
    // length is smaller than this value.
    property MinimumSegmentLength: double read FMinimumSegmentLength write SetMinimumSegmentLength;
    // Maximum element size allowed (triangles with larger area will be split).
    // If no maximum size is required, then leave this value on 0 (default)
    property MaximumElementSize: double read FMaximumElementSize write FMaximumElementSize;
    // List of steiner points that were generated
    property SteinerPoints: TVertex2DList_ read FSteinerPoints;
  end;
