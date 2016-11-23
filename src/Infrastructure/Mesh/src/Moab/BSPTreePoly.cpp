#include "moab/CartVect.hpp"
#include "moab/BSPTreePoly.hpp"
#include <assert.h>
#include <stdlib.h>
#include <set>

#undef DEBUG_IDS

namespace moab {

struct BSPTreePoly::Vertex : public CartVect {
  Vertex( const CartVect& v ) : CartVect(v), usePtr(0), markVal(0)
#ifdef DEBUG_IDS
  , id(nextID++)
#endif  
  {}
  ~Vertex() { assert(!usePtr); }
  BSPTreePoly::VertexUse* usePtr;
  int markVal;
#ifdef DEBUG_IDS
  int id;
  static int nextID;
#endif
};

struct BSPTreePoly::VertexUse {
  VertexUse( Edge* edge, Vertex* vtx );
  ~VertexUse();
  
  void set_vertex( BSPTreePoly::Vertex*& vtx_ptr );
  
  BSPTreePoly::VertexUse *nextPtr, *prevPtr;
  BSPTreePoly::Vertex* vtxPtr;
  BSPTreePoly::Edge* edgePtr;
};


struct BSPTreePoly::EdgeUse {
  EdgeUse( Edge* edge );
  EdgeUse( Edge* edge, Face* face );
  ~EdgeUse();

  BSPTreePoly::EdgeUse *prevPtr, *nextPtr;
  BSPTreePoly::Edge* edgePtr;
  BSPTreePoly::Face* facePtr;
  
  inline BSPTreePoly::Vertex* start() const;
  inline BSPTreePoly::Vertex* end() const;
  int sense() const;
  
  void insert_after( BSPTreePoly::EdgeUse* prev );
  void insert_before( BSPTreePoly::EdgeUse* next );
};

struct BSPTreePoly::Edge {
  BSPTreePoly::VertexUse *startPtr, *endPtr;
  BSPTreePoly::EdgeUse *forwardPtr, *reversePtr;
#ifdef DEBUG_IDS
  int id;
  static int nextID;
#endif
  
  Edge( Vertex* vstart, Vertex* vend )
    : forwardPtr(0), reversePtr(0)
#ifdef DEBUG_IDS
  , id(nextID++)
#endif  
  {
    startPtr = new VertexUse( this, vstart );
    endPtr = new VertexUse( this, vend );
  }
  
  ~Edge();
  
  BSPTreePoly::Vertex* start() const 
    { return startPtr->vtxPtr; }
  BSPTreePoly::Vertex* end() const 
    { return endPtr->vtxPtr; }
  
  BSPTreePoly::Face* forward() const
    { return forwardPtr ? forwardPtr->facePtr : 0; }
  BSPTreePoly::Face* reverse() const
    { return reversePtr ? reversePtr->facePtr : 0; }
  
  BSPTreePoly::VertexUse* use( BSPTreePoly::Vertex* vtx ) const
    { return (vtx == startPtr->vtxPtr) ? startPtr : (vtx == endPtr->vtxPtr) ? endPtr : 0; }
  BSPTreePoly::Edge* next( BSPTreePoly::Vertex* about ) const
    { return use(about)->nextPtr->edgePtr; }
  BSPTreePoly::Edge* prev( BSPTreePoly::Vertex* about ) const
    { return use(about)->prevPtr->edgePtr; }
  
  BSPTreePoly::EdgeUse* use( BSPTreePoly::Face* face ) const
    { return (face == forwardPtr->facePtr) ? forwardPtr : (face == reversePtr->facePtr) ? reversePtr : 0; }
  BSPTreePoly::Edge* next( BSPTreePoly::Face* about ) const
    { return use(about)->nextPtr->edgePtr; }
  BSPTreePoly::Edge* prev( BSPTreePoly::Face* about ) const
    { return use(about)->prevPtr->edgePtr; }
  
  BSPTreePoly::VertexUse* other( BSPTreePoly::VertexUse* vuse ) const
    { return vuse == startPtr ? endPtr : vuse == endPtr ? startPtr : 0; }
  BSPTreePoly::EdgeUse* other( BSPTreePoly::EdgeUse* vuse ) const
    { return vuse == forwardPtr ? reversePtr : vuse == reversePtr ? forwardPtr : 0; }
  BSPTreePoly::Vertex* other( BSPTreePoly::Vertex* vtx ) const
    { return vtx == startPtr->vtxPtr ?   endPtr->vtxPtr :
             vtx ==   endPtr->vtxPtr ? startPtr->vtxPtr : 0; }
  BSPTreePoly::Vertex* common( BSPTreePoly::Edge* eother ) const
    { return start() == eother->start() || start() == eother->end() ? start() :
               end() == eother->start() ||   end() == eother->end() ?   end() : 0; }
  
  int sense( BSPTreePoly::Face* face ) const;
             
  void remove_from_vertex( BSPTreePoly::Vertex*& vtx_ptr );
  void remove_from_face( BSPTreePoly::Face*& face_ptr );
  void add_to_vertex( BSPTreePoly::Vertex* vtx_ptr );
};

struct BSPTreePoly::Face { 
  Face(Face* next) : usePtr(0), nextPtr(next) 
#ifdef DEBUG_IDS
    , id(nextID++)
#endif  
    {}
  Face() : usePtr(0), nextPtr(0) 
#ifdef DEBUG_IDS
    , id(nextID++)
#endif  
    {}
  ~Face();
  BSPTreePoly::EdgeUse* usePtr; 
  BSPTreePoly::Face* nextPtr;
#ifdef DEBUG_IDS
  int id;
  static int nextID;
#endif
  double signed_volume() const;
};

#ifdef DEBUG_IDS
int BSPTreePoly::Vertex::nextID = 1;
int BSPTreePoly::Edge::nextID = 1;
int BSPTreePoly::Face::nextID = 1;
#endif
void BSPTreePoly::reset_debug_ids() {
#ifdef DEBUG_IDS
  BSPTreePoly::Vertex::nextID = 1;
  BSPTreePoly::Edge::nextID = 1;
  BSPTreePoly::Face::nextID = 1;
#endif
}

//static void merge_edges( BSPTreePoly::Edge* keep_edge,
//                         BSPTreePoly::Edge* dead_edge );

static BSPTreePoly::Edge* split_edge( BSPTreePoly::Vertex*& new_vtx,
                                       BSPTreePoly::Edge* into_edge );

BSPTreePoly::VertexUse::VertexUse( BSPTreePoly::Edge* edge, BSPTreePoly::Vertex* vtx )
  : vtxPtr(vtx), edgePtr(edge)
{
  if (!vtx->usePtr) {
    vtx->usePtr = prevPtr = nextPtr = this;
    return;
  }
  
  nextPtr = vtx->usePtr;
  prevPtr = nextPtr->prevPtr;
  assert( prevPtr->nextPtr == nextPtr );
  nextPtr->prevPtr = this;
  prevPtr->nextPtr = this;
}

BSPTreePoly::VertexUse::~VertexUse() 
{
  if (nextPtr == this) {
    assert(prevPtr == this);
    assert(vtxPtr->usePtr == this);
    vtxPtr->usePtr = 0;
    delete vtxPtr;
  }
  else if (vtxPtr->usePtr == this)
    vtxPtr->usePtr = nextPtr;
  
  nextPtr->prevPtr = prevPtr;
  prevPtr->nextPtr = nextPtr;
  nextPtr = prevPtr = 0;
}

void BSPTreePoly::VertexUse::set_vertex( BSPTreePoly::Vertex*& vtx )
{
  if (vtxPtr) {
    if (nextPtr == prevPtr) {
      assert(nextPtr == this);
      vtxPtr->usePtr = 0;
      delete vtx;
      vtx = 0;
    }
    else {
      nextPtr->prevPtr = prevPtr;
      prevPtr->nextPtr = nextPtr;
      if (vtxPtr->usePtr == this)
        vtxPtr->usePtr = nextPtr;
    }
  }
  
  if (vtx) {
   vtxPtr = vtx;
   nextPtr = vtxPtr->usePtr->nextPtr;
   prevPtr = vtxPtr->usePtr;
   nextPtr->prevPtr = this;
   vtxPtr->usePtr->nextPtr = this;
  }
}
  

BSPTreePoly::EdgeUse::EdgeUse( BSPTreePoly::Edge* edge )
  : prevPtr(0), nextPtr(0), edgePtr(edge), facePtr(0)
{}

BSPTreePoly::EdgeUse::EdgeUse( BSPTreePoly::Edge* edge, 
                                BSPTreePoly::Face* face )
  : edgePtr(edge), facePtr(face)
{
  assert(!face->usePtr);
  face->usePtr = prevPtr = nextPtr = this;
  
  if (!face->usePtr) {
    face->usePtr = prevPtr = nextPtr = this;
    return;
  }
  
  nextPtr = face->usePtr;
  prevPtr = nextPtr->prevPtr;
  assert( prevPtr->nextPtr == nextPtr );
  nextPtr->prevPtr = this;
  prevPtr->nextPtr = this;
}

void BSPTreePoly::EdgeUse::insert_after( BSPTreePoly::EdgeUse* prev )
{
    // shouldn't already be in a face
  assert(!facePtr);
    // adjacent edges should share vertices
  assert( start() == prev->end() );
 
  facePtr = prev->facePtr;
  nextPtr = prev->nextPtr;
  prevPtr = prev;
  nextPtr->prevPtr = this;
  prevPtr->nextPtr = this;
}

void BSPTreePoly::EdgeUse::insert_before( BSPTreePoly::EdgeUse* next )
{
    // shouldn't already be in a face
  assert(!facePtr);
    // adjacent edges should share vertices
  assert( end() == next->start() );
 
  facePtr = next->facePtr;
  prevPtr = next->prevPtr;
  nextPtr = next;
  nextPtr->prevPtr = this;
  prevPtr->nextPtr = this;
}


BSPTreePoly::EdgeUse::~EdgeUse() 
{
  if (facePtr->usePtr == this) 
    facePtr->usePtr = (nextPtr == this) ? 0 : nextPtr;
    
  if (edgePtr->forwardPtr == this)
    edgePtr->forwardPtr = 0;
  if (edgePtr->reversePtr == this)
    edgePtr->reversePtr = 0;
  
  if (!edgePtr->forwardPtr && !edgePtr->reversePtr)
    delete edgePtr;
  
  nextPtr->prevPtr = prevPtr;
  prevPtr->nextPtr = nextPtr;
  nextPtr = prevPtr = 0;
}

int BSPTreePoly::EdgeUse::sense() const
{
  if (edgePtr->forwardPtr == this)
    return 1;
  else if (edgePtr->reversePtr == this)
    return -1;
  else
    return 0;
}

BSPTreePoly::Vertex* BSPTreePoly::EdgeUse::start() const
{
  if (edgePtr->forwardPtr == this)
    return edgePtr->start();
  else if (edgePtr->reversePtr == this)
    return edgePtr->end();
  else
    return 0;
}

BSPTreePoly::Vertex* BSPTreePoly::EdgeUse::end() const
{
  if (edgePtr->forwardPtr == this)
    return edgePtr->end();
  else if (edgePtr->reversePtr == this)
    return edgePtr->start();
  else
    return 0;
}

BSPTreePoly::Edge::~Edge()
{
  delete startPtr;
  delete endPtr;
  delete forwardPtr;
  delete reversePtr;
}

int BSPTreePoly::Edge::sense( BSPTreePoly::Face* face ) const
{
  if (forwardPtr && forwardPtr->facePtr == face)
    return 1;
  else if (reversePtr && reversePtr->facePtr == face)
    return -1;
  else
    return 0;
}

BSPTreePoly::Face::~Face()
{
  BSPTreePoly::EdgeUse* nextEdgeUsePtr = usePtr;
  while (nextEdgeUsePtr) {
    delete nextEdgeUsePtr; // This is tricky: ~EdgeUse() might change the value of usePtr
    if (usePtr && usePtr != nextEdgeUsePtr)
      nextEdgeUsePtr = usePtr;
    else
      nextEdgeUsePtr = 0;
  }
  usePtr = 0;
}

void BSPTreePoly::clear() {
  while (faceList) {
    Face* face = faceList;
    faceList = faceList->nextPtr;
    delete face;
  }
}

ErrorCode BSPTreePoly::set( const CartVect hex_corners[8] )
{
  clear();
  
  Vertex* vertices[8];
  for (int i = 0; i < 8; ++i)
    vertices[i] = new Vertex(hex_corners[i]);
  
  Edge* edges[12];
#ifdef DEBUG_IDS
  int start_id = Edge::nextID;
#endif
  for (int i = 0; i < 4; ++i) {
    int j= (i+1)%4;
    edges[i  ] = new Edge( vertices[i  ], vertices[j  ] );
    edges[i+4] = new Edge( vertices[i  ], vertices[i+4] );
    edges[i+8] = new Edge( vertices[i+4], vertices[j+4] );
  }
#ifdef DEBUG_IDS
  for (int i = 0; i < 12; ++i)
    edges[i]->id = start_id++;
#endif
  
  static const int face_conn[6][4] = { {  0,  5,  -8, -4 },
                                       {  1,  6,  -9, -5 },
                                       {  2,  7, -10, -6 },
                                       {  3,  4, -11, -7 },
                                       { -3, -2,  -1,-12 },
                                       {  8,  9,  10, 11 } };
  for (int i = 0; i < 6; ++i) {
    faceList = new Face(faceList);
    EdgeUse* prev = 0;
    for (int j = 0; j < 4; ++j) {
      int e = face_conn[i][j];
      if (e < 0) {
        e = (-e) % 12;
        assert(!edges[e]->reversePtr);
        if (!prev) {
          edges[e]->reversePtr = new EdgeUse( edges[e], faceList );
        }
        else {
          edges[e]->reversePtr = new EdgeUse( edges[e] );
          edges[e]->reversePtr->insert_after( prev );
        }
        prev = edges[e]->reversePtr;
      }
      else {
        assert(!edges[e]->forwardPtr);
        if (!prev) {
          edges[e]->forwardPtr = new EdgeUse( edges[e], faceList );
        }
        else {
          edges[e]->forwardPtr = new EdgeUse( edges[e] );
          edges[e]->forwardPtr->insert_after( prev );
        }
        prev = edges[e]->forwardPtr;          
      }
    }
  }
  
  return MB_SUCCESS;
}

void BSPTreePoly::get_faces( std::vector<const Face*>& face_list ) const
{
  face_list.clear();
  for (Face* face = faceList; face; face = face->nextPtr)
    face_list.push_back( face );
}

void BSPTreePoly::get_vertices( const Face* face,
                                 std::vector<CartVect>& vertices ) const
{
  vertices.clear();
  if (!face || !face->usePtr)
    return;
    
  EdgeUse* coedge = face->usePtr;
  do {
    vertices.push_back( *coedge->end() );
    coedge = coedge->nextPtr;
  } while (coedge != face->usePtr);
}

double BSPTreePoly::Face::signed_volume() const
{
  CartVect sum(0.0);
  const CartVect* base = usePtr->start();
  CartVect d1 = (*usePtr->end() - *base);
  for (EdgeUse* coedge = usePtr->nextPtr; coedge != usePtr; coedge = coedge->nextPtr) {
    CartVect d2 = (*coedge->end() - *base);
    sum += d1 * d2;
    d1 = d2;
  }
  return (1.0/6.0) * (sum % *base);
}

double BSPTreePoly::volume() const 
{
  double result = 0;
  for (Face* ptr = faceList; ptr; ptr = ptr->nextPtr)
    result += ptr->signed_volume();
  return result;
}

void BSPTreePoly::set_vertex_marks( int value )
{
  for (Face* face = faceList; face; face = face->nextPtr) {
    EdgeUse* edge = face->usePtr;
    do {
      edge->edgePtr->start()->markVal = value;
      edge->edgePtr->end()->markVal = value;
      edge = edge->nextPtr;
    } while (edge && edge != face->usePtr);
  }
}
/*
static void merge_edges( BSPTreePoly::Edge* keep_edge,
                         BSPTreePoly::Edge* dead_edge )
{
  // edges must share a vertex
  BSPTreePoly::Vertex* dead_vtx = keep_edge->common(dead_edge);
  assert(dead_vtx);
   // vertex may have only two adjacent edges
  BSPTreePoly::VertexUse* dead_vtxuse = dead_edge->use(dead_vtx);
  assert(dead_vtxuse);
  BSPTreePoly::VertexUse* keep_vtxuse = dead_vtxuse->nextPtr;
  assert(keep_vtxuse);
  assert(keep_vtxuse->edgePtr == keep_edge);
  assert(keep_vtxuse->nextPtr == dead_vtxuse);
  assert(keep_vtxuse->prevPtr == dead_vtxuse);
  assert(dead_vtxuse->prevPtr == keep_vtxuse);
  
  // kept edge now ends with the kept vertex on the dead edge
  keep_vtxuse->set_vertex( dead_edge->other(dead_vtx) );
  
  // destructors should take care of everything else
  // (including removing dead edge from face loops)
  delete dead_edge;
}
*/
static BSPTreePoly::Edge* split_edge( BSPTreePoly::Vertex*& new_vtx,
                                       BSPTreePoly::Edge* into_edge )
{
    // split edge, creating new edge
  BSPTreePoly::Edge* new_edge = new BSPTreePoly::Edge( new_vtx, into_edge->end() );
  into_edge->endPtr->set_vertex( new_vtx ); // This call might delete new_vtx
  
    // update coedge loops in faces
  if (into_edge->forwardPtr) {
    new_edge->forwardPtr = new BSPTreePoly::EdgeUse( new_edge );
    new_edge->forwardPtr->insert_after( into_edge->forwardPtr );
  }
  if (into_edge->reversePtr) {
    new_edge->reversePtr = new BSPTreePoly::EdgeUse( new_edge );
    new_edge->reversePtr->insert_before( into_edge->reversePtr );
  }
  
  return new_edge;
}

static BSPTreePoly::Face* split_face( BSPTreePoly::EdgeUse* start,
                                       BSPTreePoly::EdgeUse* end )
{
  BSPTreePoly::Face* face = start->facePtr;
  assert(face == end->facePtr);
  BSPTreePoly::Face* new_face = new BSPTreePoly::Face;
  BSPTreePoly::EdgeUse* keep_start = start->prevPtr;
  BSPTreePoly::EdgeUse* keep_end = end->nextPtr;
  for (BSPTreePoly::EdgeUse* ptr = start; ptr != keep_end; ptr = ptr->nextPtr) {
    if (face->usePtr == ptr)
      face->usePtr = keep_start;
    ptr->facePtr = new_face;
  }
  new_face->usePtr = start;
  BSPTreePoly::Edge* edge = new BSPTreePoly::Edge( start->start(), end->end() );
  edge->forwardPtr = new BSPTreePoly::EdgeUse( edge );
  edge->reversePtr = new BSPTreePoly::EdgeUse( edge );

  edge->forwardPtr->facePtr = face;
  edge->forwardPtr->prevPtr = keep_start;
  keep_start->nextPtr = edge->forwardPtr;
  edge->forwardPtr->nextPtr = keep_end;
  keep_end->prevPtr = edge->forwardPtr;
  
  edge->reversePtr->facePtr = new_face;
  edge->reversePtr->nextPtr = start;
  start->prevPtr = edge->reversePtr;
  edge->reversePtr->prevPtr = end;
  end->nextPtr = edge->reversePtr;
  
  return new_face;  
}


bool BSPTreePoly::cut_polyhedron( const CartVect& plane_normal,
                                   double plane_coeff )
{
  const double EPSILON = 1e-6; // points this close are considered coincident

    // scale epsilon rather than normalizing normal vector
  const double epsilon = EPSILON * (plane_normal % plane_normal);

    // Classify all points above/below plane and destroy any faces
    // that have no vertices below the plane.
  const int UNKNOWN = 0;
  const int ABOVE = 1;
  const int ON = 2;
  const int BELOW = 3;
  int num_above = 0;
  set_vertex_marks( UNKNOWN );
  
    // Classify all points above/below plane and 
    // split any edge that intersect the plane.
  for (Face* face = faceList; face; face = face->nextPtr) {
    EdgeUse* edge = face->usePtr;
    
    do {
      Vertex* start = edge->edgePtr->start();
      Vertex* end = edge->edgePtr->end();

      if (!start->markVal) {
        double d = plane_normal % *start + plane_coeff;
        if (d*d <= epsilon) 
          start->markVal = ON;
        else if (d < 0.0)
          start->markVal = BELOW;
        else {
          start->markVal = ABOVE;
          ++num_above;
        }
      }

      if (!end->markVal) {
        double d = plane_normal % *end + plane_coeff;
        if (d*d <= epsilon) 
          end->markVal = ON;
        else if (d < 0.0)
          end->markVal = BELOW;
        else {
          end->markVal = ABOVE;
          ++num_above;
        }
      }

      if ((end->markVal == ABOVE && start->markVal == BELOW) ||
          (end->markVal == BELOW && start->markVal == ABOVE)) {
        CartVect dir = *end - *start;
        double t = -(plane_normal % *start + plane_coeff) / (dir % plane_normal);
        Vertex* new_vtx = new Vertex( *start + t*dir );
        new_vtx->markVal = ON;
        split_edge( new_vtx, edge->edgePtr ); // This call might delete new_vtx
        end = new_vtx;
      }

      edge = edge->nextPtr;
    } while (edge && edge != face->usePtr);
  }
  
  if (!num_above)
    return false;
  
    // Split faces
  for (Face* face = faceList; face; face = face->nextPtr) {
    EdgeUse* edge = face->usePtr;
    
    EdgeUse *split_start = 0, *split_end = 0, *other_split = 0;
    do {
      if (edge->end()->markVal == ON && edge->start()->markVal != ON) {
        if (!split_start)
          split_start = edge->nextPtr;
        else if (!split_end)
          split_end = edge;
        else
          other_split = edge;
      }

      edge = edge->nextPtr;
    } while (edge && edge != face->usePtr);

      // If two vertices are on plane (but not every vertex)
      // then split the face
    if (split_end && !other_split) {
      assert(split_start);
      Face* new_face = split_face( split_start, split_end );
      new_face->nextPtr = faceList;
      faceList = new_face;
    }
  }
  
    // Destroy all faces that are above the plane
  Face** lptr = &faceList;
  while (*lptr) {
    EdgeUse* edge = (*lptr)->usePtr;
    bool some_above = false;
    do {
      if (edge->start()->markVal == ABOVE) {
        some_above = true;
        break;
      }
      edge = edge->nextPtr;
    } while (edge && edge != (*lptr)->usePtr);
    
    if (some_above) {
      Face* dead = *lptr;
      *lptr = (*lptr)->nextPtr;
      delete dead;
    }
    else {
      lptr = &((*lptr)->nextPtr);
    }
  }
  
    // Construct a new face in the cut plane
    
    // First find an edge to start at
  Edge* edge_ptr = 0;
  for (Face* face = faceList; face && !edge_ptr; face = face->nextPtr) {
    EdgeUse* co_edge = face->usePtr;
    do {
      if (0 == co_edge->edgePtr->other(co_edge)) {
        edge_ptr = co_edge->edgePtr;
        break;
      }
      co_edge = co_edge->nextPtr;
    } while (co_edge && co_edge != face->usePtr);
  }
  if (!edge_ptr) return false;
  
    // Constuct new face and first CoEdge
  faceList = new Face( faceList );
  Vertex *next_vtx, *start_vtx;
  EdgeUse* prev_coedge;
  if (edge_ptr->forwardPtr) {
    next_vtx = edge_ptr->start();
    start_vtx = edge_ptr->end();
    assert(!edge_ptr->reversePtr);
    prev_coedge = edge_ptr->reversePtr = new EdgeUse( edge_ptr, faceList );
  }
  else {
    next_vtx = edge_ptr->end();
    start_vtx = edge_ptr->start();
    prev_coedge = edge_ptr->forwardPtr = new EdgeUse( edge_ptr, faceList );
  }
  
    // Construct coedges until loop is closed
  while (next_vtx != start_vtx) {
      // find next edge adjacent to vertex with only one adjacent face
    VertexUse* this_use = edge_ptr->use( next_vtx );
    VertexUse* use = this_use->nextPtr;
    while (use != this_use) {
      if (use->edgePtr->forwardPtr == 0) {
        edge_ptr = use->edgePtr;
        assert(edge_ptr->start() == next_vtx);
        next_vtx = edge_ptr->end();
        edge_ptr->forwardPtr = new EdgeUse( edge_ptr );
        edge_ptr->forwardPtr->insert_after( prev_coedge );
        prev_coedge = edge_ptr->forwardPtr;
        break;
      }
      else if (use->edgePtr->reversePtr == 0) {
        edge_ptr = use->edgePtr;
        assert(edge_ptr->end() == next_vtx);
        next_vtx = edge_ptr->start();
        edge_ptr->reversePtr = new EdgeUse( edge_ptr );
        edge_ptr->reversePtr->insert_after( prev_coedge );
        prev_coedge = edge_ptr->reversePtr;
        break;
      }
      
      use = use->nextPtr;
      assert( use != this_use ); // failed to close loop!
    }
  }
  
  return true;
}

bool BSPTreePoly::is_valid() const
{
  std::set<Face*> list_faces;
  
  int i = 0;
  for (Face* ptr = faceList; ptr; ptr = ptr->nextPtr) {
    if (++i > 10000)
      return false;
    if (!list_faces.insert(ptr).second)
      return false;
  }
  
  std::set<Vertex*> vertices;
  for (Face* face = faceList; face; face = face->nextPtr) {
    i = 0;
    EdgeUse* coedge = face->usePtr;    
    do {
      if (++i > 10000)
        return false;
      
      if (coedge->facePtr != face)
        return false;
      
      Edge* edge = coedge->edgePtr;
      if (!edge->startPtr || !edge->endPtr)
        return false;
      
      vertices.insert( edge->start() );
      vertices.insert( edge->end() );
      
      EdgeUse* other;
      if (edge->forwardPtr == coedge)
        other = edge->reversePtr;
      else if (edge->reversePtr != coedge)
        return false;
      else
        other = edge->forwardPtr;
      if (!other)
        return false;
      if (list_faces.find( other->facePtr ) == list_faces.end())
        return false;
      
      EdgeUse* next = coedge->nextPtr;
      if (next->prevPtr != coedge)
        return false;
      if (coedge->end() != next->start())
        return false;
      
      coedge = next;
    } while (coedge != face->usePtr);
  }
  
  for (std::set<Vertex*>::iterator j = vertices.begin(); j != vertices.end(); ++j) {
    Vertex* vtx = *j;

    i = 0;
    VertexUse* use = vtx->usePtr;    
    do {
      if (++i > 10000)
        return false;
    
      if (use->vtxPtr != vtx)
        return false;
      
      Edge* edge = use->edgePtr;
      if (!edge)
        return false;
      if (edge->startPtr != use && edge->endPtr != use)
        return false;
        
      VertexUse* next = use->nextPtr;
      if (next->prevPtr != use)
        return false;
      
      use = next;
    } while (use != vtx->usePtr);
  }
  
  return true;
}

bool BSPTreePoly::is_point_contained( const CartVect& point ) const
{
  if (!faceList) // empty (zero-dimension) polyhedron
    return false;
  
    // Test that point is below the plane of each face
    // NOTE: This will NOT work for polyhedra w/ concavities
  for (Face* face = faceList; face; face = face->nextPtr) {
    Vertex *pt1, *pt2, *pt3;
    pt1 = face->usePtr->start();
    pt2 = face->usePtr->end();
    pt3 = face->usePtr->nextPtr->end();
    
    if (pt3 == pt1) // degenerate
      continue;

    CartVect norm = (*pt3 - *pt2) * (*pt1 - *pt2);
    double coeff = -(norm % *pt2);
    if (norm % point > -coeff) // if above plane
      return false;
  }
  
  return true;
}
  
} // namespace moab
