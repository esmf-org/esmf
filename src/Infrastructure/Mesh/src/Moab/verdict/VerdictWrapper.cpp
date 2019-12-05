/*
 * VerdictWrapper.cpp
 *
 *  Created on: Nov 18, 2014
 *
 */

#include "moab/Interface.hpp"
#include "moab/verdict/VerdictWrapper.hpp"
#include "Internals.hpp"
#include "moab/verdict.h"

namespace moab
{
VerdictWrapper::VerdictWrapper(Interface * mb) :mbImpl(mb) {
  // TODO Auto-generated constructor stub

}

VerdictWrapper::~VerdictWrapper() {
  // TODO Auto-generated destructor stub
}

static int possibleQuality[MBMAXTYPE][MB_QUALITY_COUNT] = {
    /*
      MB_EDGE_RATIO = 0,  // 0
      |  MB_MAX_EDGE_RATIO , // 1
      |  |  MB_SKEW,            // 2
      |  |  |  MB_TAPER,           // 3
      |  |  |  |  MB_VOLUME,          // 4
      |  |  |  |  |  MB_STRETCH,         // 5
      |  |  |  |  |  |  MB_DIAGONAL,        // 6
      |  |  |  |  |  |  |  MB_DIMENSION,       // 7
      |  |  |  |  |  |  |  |  MB_ODDY,            // 8
      |  |  |  |  |  |  |  |  |  MB_MED_ASPECT_FROBENIUS,// 9
      |  |  |  |  |  |  |  |  |  |  MB_MAX_ASPECT_FROBENIUS, // 10
      |  |  |  |  |  |  |  |  |  |  |  MB_CONDITION,       // 11
      |  |  |  |  |  |  |  |  |  |  |  |  MB_JACOBIAN,        // 12
      |  |  |  |  |  |  |  |  |  |  |  |  |  MB_SCALED_JACOBIAN, // 13
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_SHEAR,           // 14
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_SHAPE,           // 15
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_RELATIVE_SIZE_SQUARED, // 16
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_SHAPE_AND_SIZE,        // 17
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_SHEAR_AND_SIZE,        // 18
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_DISTORTION,            // 19
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_LENGTH,                // 20 only for edge
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_RADIUS_RATIO        // 21 tet
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_ASPECT_BETA      // 22 tet (very similar to 21)
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_ASPECT_RATIO   // 23 tet
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_ASPECT_GAMMA // 24 tet
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_MINIMUM_ANGLE // 25 tet
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_COLLAPSE_RATIO // 26 tet
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_WARPAGE     // 27 quad
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_AREA     // 28 quad
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  MB_MAXIMUM_ANGLE // 29 quad
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
      |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |
    */
    /*0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29*/
     {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},           //  MBVERTEX
     {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0},           //  MBEDGE
     {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 1},           //  MBTRI
     {1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1},           //  MBQUAD
     {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},           //  MBPOLYGON
     {1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0},           //  MBTET
     {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},           //  MBPYRAMID
     {0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},           //  MBPRISM
     {0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},           //  MBKNIFE
     {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},           // MBHEX
     {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},           //  MBPOLYHEDRON
     {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}            //  MBENTITYSET
};

static int numQualities[MBMAXTYPE] =
{
   0, // "MBVERTEX"
   1, // "MBEDGE", /**< Mesh Edge */
   13, // "MBTRI", /**< Triangular element (including shells) */
   22, // "MBQUAD", /**< Quadrilateral element (including shells) */
   0, // "MBPOLYGON", /**< Polygon */
   14, // "MBTET", /**< Tetrahedral element */
   0, //"MBPYRAMID", /**< Pyramid element (where are the face ids for this defined?) */
   1, // "MBPRISM", /**< Wedge element (Exodus has one, Cubit doesn't. Does Mesh need it?) */
   1, // "MBKNIFE", /**< Knife element */
   20,// "MBHEX", /**< Hexahedral element */
   0, //"MBPOLYHEDRON", /**< Polyhedron */
   0, // "MBENTITYSET", /**< MeshSet */
};
static char const * nameQuality [MB_QUALITY_COUNT] =
{
   " edge ratio",          //  MB_EDGE_RATIO = 0,  // 0
   " maximum edge ratio",  //  MB_MAX_EDGE_RATIO , // 1
   " skew",                //  MB_SKEW,            // 2
   " taper" ,              //   MB_TAPER,            // 3
   " volume" ,             //   MB_VOLUME,          // 4
   " stretch" ,            //   MB_STRETCH,         // 5
   " diagonal" ,           //   MB_DIAGONAL,        // 6
   " characteristic length",           //  MB_DIMENSION,       // 7
   " oddy" ,               //   MB_ODDY,            // 8
   " average Frobenius aspect", //  MB_MED_ASPECT_FROBENIUS,// 9
   " maximum Frobenius aspect", //   MB_MAX_ASPECT_FROBENIUS, // 10
   " condition number" ,   //   MB_CONDITION,       // 11
   " jacobian" ,           //   MB_JACOBIAN,        // 12
   " scaled jacobian" ,    //   MB_SCALED_JACOBIAN, // 13
   " shear" ,              //   MB_SHEAR,           // 14
   " shape" ,              //  MB_SHAPE,           // 15
   " relative size squared",//    MB_RELATIVE_SIZE_SQUARED, // 16
   " shape and size" ,      //   MB_SHAPE_AND_SIZE,        // 17
   " shear and size" ,      //   MB_SHEAR_AND_SIZE,        // 18
   " distortion" ,          //   MB_DISTORTION,            // 19
      // next are QuadMetricVals that are not in hex metrics
      // length for edge:
   " length"  ,             //   MB_LENGTH,                // 20 only for edge
   " radius ratio",         //   MB_RADIUS_RATIO           // 21 tet
   " aspect beta",          // MB_ASPECT_BETA              // 22 tet
   " aspect ratio",         // MB_ASPECT_RATIO,            // 23 MBTET
   " aspect gamma",         // MB_ASPECT_GAMMA             // 24 tet
   " minimum angle",        //  MB_MINIMUM_ANGLE,         // 25  MBTET
   " collapse ratio",        // MB_COLLAPSE_RATIO,        // 26  MBTET
   " warpage",               // MB_WARPAGE                // 27  MBQUAD
   " area",                   // MB_AREA                   // 28  MBQAD
   " maximum angle"          // MB_MAXIMUM_ANGLE          // 29  MBQUAD
};

static const char * nameType[MBMAXTYPE] =
{
    "MBVERTEX", /**< Mesh Vertex AKA node */
    "MBEDGE", /**< Mesh Edge */
    "MBTRI", /**< Triangular element (including shells) */
    "MBQUAD", /**< Quadrilateral element (including shells) */
    "MBPOLYGON", /**< Polygon */
    "MBTET", /**< Tetrahedral element */
    "MBPYRAMID", /**< Pyramid element (where are the face ids for this defined?) */
    "MBPRISM", /**< Wedge element (Exodus has one, Cubit doesn't. Does Mesh need it?) */
    "MBKNIFE", /**< Knife element */
    "MBHEX", /**< Hexahedral element */
    "MBPOLYHEDRON", /**< Polyhedron */
    "MBENTITYSET", /**< MeshSet */
};

ErrorCode VerdictWrapper::quality_measure(EntityHandle eh, QualityType q, double & quality,
    int num_nodes, EntityType etype, double * coords)
{
  double coordinates[27][3]; // at most 27 nodes per element

  if (0==num_nodes && NULL==coords)
  {
    etype= TYPE_FROM_HANDLE(eh);
    if (possibleQuality[etype][q]==0)
      return MB_NOT_IMPLEMENTED;

    // get coordinates of points, if not passed already
    const EntityHandle * conn = NULL;
    //int num_nodes;
    ErrorCode rval = mbImpl->get_connectivity(eh, conn, num_nodes);
    if (rval!=MB_SUCCESS)
      return rval;
    if (etype!=MBPOLYHEDRON)
    {
      rval = mbImpl->get_coords(conn, num_nodes, &(coordinates[0][0]));
      if (rval!=MB_SUCCESS)
        return rval;
    }
  }
  else
  {
    if (num_nodes > 27)
      return MB_FAILURE;
    for (int i=0; i<num_nodes; i++)
    {
      for (int j=0; j<3; j++)
        coordinates[i][j]=coords[3*i+j];
    }
  }
  VerdictFunction func=0;

  switch(etype){
  case MBHEX:
  {
    num_nodes = 8;
    switch(q) {
    case MB_EDGE_RATIO:           func = v_hex_edge_ratio; break;          // 0
    case MB_MAX_EDGE_RATIO:       func = v_hex_max_edge_ratio; break;      // 1
    case MB_SKEW:                 func = v_hex_skew; break;                // 2
    case MB_TAPER:                func = v_hex_taper; break;               // 3
    case MB_VOLUME:               func = v_hex_volume; break;              // 4
    case MB_STRETCH:              func = v_hex_stretch; break;             // 5
    case MB_DIAGONAL:             func = v_hex_diagonal; break;            // 6
    case MB_DIMENSION:            func = v_hex_dimension; break;           // 7
    case MB_ODDY:                 func = v_hex_oddy; break;                // 8
    case MB_MED_ASPECT_FROBENIUS: func = v_hex_med_aspect_frobenius; break;// 9
    case MB_MAX_ASPECT_FROBENIUS: func = v_hex_max_aspect_frobenius; break;// 10
    case MB_CONDITION:            func = v_hex_condition; break;           // 11
    case MB_JACOBIAN:             func = v_hex_jacobian; break;            // 12
    case MB_SCALED_JACOBIAN:      func = v_hex_scaled_jacobian; break;     // 13
    case MB_SHEAR:                func = v_hex_shear; break;               // 14
    case MB_SHAPE:                func = v_hex_shape; break;               // 15
    case MB_RELATIVE_SIZE_SQUARED:func = v_hex_relative_size_squared; break; // 16
    case MB_SHAPE_AND_SIZE:       func = v_hex_shape_and_size; break  ;      // 17
    case MB_SHEAR_AND_SIZE:       func = v_hex_shear_and_size; break;        // 18
    case MB_DISTORTION:           func = v_hex_distortion; break;            // 19
    default :  return MB_FAILURE;
    }
    break;
  }
  case MBEDGE:
  {
    num_nodes = 2;
    switch (q) {
    case MB_LENGTH: func = v_edge_length; break;                             // 20
    default : return MB_FAILURE;
    }
    break;
  }
  case MBTET:
  {
    num_nodes = 4;
    switch (q) {
    case MB_EDGE_RATIO:           func = v_tet_edge_ratio; break;           // 0 //! Calculates tet edge ratio metric.
    case MB_RADIUS_RATIO:         func = v_tet_radius_ratio; break;         // 21
    case MB_ASPECT_BETA:          func = v_tet_aspect_beta; break;          // 22
    case MB_ASPECT_RATIO:         func = v_tet_aspect_ratio; break;         // 23
    case MB_ASPECT_GAMMA:         func = v_tet_aspect_gamma; break;         // 24
    case MB_MAX_ASPECT_FROBENIUS: func = v_tet_aspect_frobenius; break;     // 10
    case MB_MINIMUM_ANGLE:        func = v_tet_minimum_angle; break;        // 25
    case MB_COLLAPSE_RATIO:       func = v_tet_collapse_ratio; break;       // 26
    case MB_VOLUME:               func = v_tet_volume; break;               // 4
    case MB_CONDITION:            func = v_tet_condition; break;            // 11
    case MB_JACOBIAN:             func = v_tet_jacobian; break;             // 12
    case MB_SCALED_JACOBIAN:      func = v_tet_scaled_jacobian; break;      // 13
    case MB_SHAPE:                func = v_tet_shape; break;                // 15
    case MB_RELATIVE_SIZE_SQUARED:func = v_tet_relative_size_squared; break;// 16
    case MB_SHAPE_AND_SIZE:       func = v_tet_shape_and_size; break;       // 17
    case MB_DISTORTION:           func = v_tet_distortion; break;           // 19
    default : return MB_FAILURE;
    }
    break;
  }
  case MBPRISM:
  {
    num_nodes = 6;
    switch (q) {
    case MB_VOLUME:               func = v_wedge_volume;    break;          // 4
    default : return MB_FAILURE;
    }
    break;
  }
  case MBKNIFE:
  {
    num_nodes = 7;
    switch (q) {
    case MB_VOLUME:               func = v_knife_volume;    break;          // 4
    default : return MB_FAILURE;
    }
    break;
  }
  case MBQUAD:
  {
    num_nodes = 4;
    switch (q) {
    case MB_EDGE_RATIO:           func = v_quad_edge_ratio; break;          // 0
    case MB_MAX_EDGE_RATIO:       func = v_quad_max_edge_ratio; break;      // 1
    case MB_ASPECT_RATIO:         func = v_quad_aspect_ratio; break;        // 23
    case MB_RADIUS_RATIO:         func = v_quad_radius_ratio; break;        // 21
    case MB_MED_ASPECT_FROBENIUS: func = v_quad_med_aspect_frobenius; break;// 9
    case MB_MAX_ASPECT_FROBENIUS: func = v_quad_max_aspect_frobenius; break;//10
    case MB_SKEW:                 func = v_quad_skew; break;                // 2
    case MB_TAPER:                func = v_quad_taper; break;               // 3
    case MB_WARPAGE:              func = v_quad_warpage; break;             // 27
    case MB_AREA:                 func = v_quad_area; break;                // 28
    case MB_STRETCH:              func = v_quad_stretch; break;             // 5
    case MB_MINIMUM_ANGLE:        func = v_quad_minimum_angle; break;       // 25
    case MB_MAXIMUM_ANGLE:        func = v_quad_maximum_angle; break;       // 29
    case MB_ODDY:                 func = v_quad_oddy; break;                // 8
    case MB_CONDITION:            func = v_quad_condition; break;           // 11
    case MB_JACOBIAN:             func = v_quad_jacobian; break;            // 12
    case MB_SCALED_JACOBIAN:      func = v_quad_scaled_jacobian; break;     // 13
    case MB_SHEAR:                func = v_quad_shear; break;               // 14
    case MB_SHAPE:                func = v_quad_shape; break;               // 15
    case MB_RELATIVE_SIZE_SQUARED:func = v_quad_relative_size_squared; break; // 16
    case MB_SHAPE_AND_SIZE:       func = v_quad_shape_and_size; break  ;      // 17
    case MB_SHEAR_AND_SIZE:       func = v_quad_shear_and_size; break;        // 18
    case MB_DISTORTION:           func = v_quad_distortion; break;            // 19
    default :  return MB_FAILURE;
    }
    break;
  }

  case MBTRI:
  {
    num_nodes = 3;
    switch(q) {
    case MB_EDGE_RATIO:           func = v_tri_edge_ratio; break;             // 0
    case MB_ASPECT_RATIO:         func = v_tri_aspect_ratio; break;           // 23
    case MB_RADIUS_RATIO:         func = v_tri_radius_ratio; break;           // 21
    case MB_MAX_ASPECT_FROBENIUS: func = v_tri_aspect_frobenius; break;       // 10
    case MB_AREA:                 func = v_tri_area; break;                   // 28
    case MB_MINIMUM_ANGLE:        func = v_tri_minimum_angle; break;          // 25
    case MB_MAXIMUM_ANGLE:        func = v_tri_maximum_angle; break;          // 29
    case MB_CONDITION:            func = v_tri_condition; break;              // 11
    case MB_SCALED_JACOBIAN:      func = v_tri_scaled_jacobian; break;        // 13
    // does not exist, even though it was defined in verdict.h; remove it from there too
    // case MB_SHEAR:                func = v_tri_shear; break;                  // 14
    case MB_RELATIVE_SIZE_SQUARED:func = v_tri_relative_size_squared; break;  // 16
    case MB_SHAPE:                func = v_tri_shape; break;                  // 15
    case MB_SHAPE_AND_SIZE:       func = v_tri_shape_and_size; break  ;       // 17
    case MB_DISTORTION:           func = v_tri_distortion; break;             // 19
    default :  return MB_FAILURE;
    }
    break;
  }
  default : break; // some have no measures
  }

  if (!func)
    return MB_NOT_IMPLEMENTED;
  // actual computation happens here
  quality = (*func)(num_nodes, coordinates);

  return MB_SUCCESS;
}
const char * VerdictWrapper::quality_name (QualityType q)
{
  return nameQuality[q];
}
const char * VerdictWrapper::entity_type_name(EntityType etype)
{
  return nameType[etype];
}
int VerdictWrapper::num_qualities(EntityType etype)
{
  return numQualities[etype];
}
int VerdictWrapper::possible_quality(EntityType et, QualityType q)
{
  return possibleQuality[et][q];
}
  // relative size needs a base size, that is set at global level, one for each major type (hex, tet, quad, tri)
ErrorCode VerdictWrapper::set_size(double size)
{
  // set the sizes for all of them; maybe we can set by type, this should be enough for simplicity
  v_set_hex_size (size);
  v_set_tet_size (size);
  v_set_quad_size( size );
  v_set_tri_size( size );
  return MB_SUCCESS;
}

ErrorCode VerdictWrapper::all_quality_measures(EntityHandle eh, std::map<QualityType, double> & qualities)
{
  EntityType etype = TYPE_FROM_HANDLE(eh);
  if (etype == MBPOLYHEDRON || etype == MBVERTEX || etype == MBENTITYSET)
    return MB_SUCCESS; // no quality for polyhedron or vertex or set

  double coordinates[27][3]; // at most 27 nodes per element
  // get coordinates of points, if not passed already
  const EntityHandle * conn = NULL;
  int num_nodes;
  ErrorCode rval = mbImpl->get_connectivity(eh, conn, num_nodes);
  if (rval != MB_SUCCESS)
    return rval;
  rval = mbImpl->get_coords(conn, num_nodes, &(coordinates[0][0]));
  if (rval != MB_SUCCESS)
    return rval;

  switch (etype) {
  case MBEDGE: {
    double leng = v_edge_length(2, coordinates);
    qualities[MB_LENGTH] = leng;
    break;
  }
  case MBHEX: {
    num_nodes = 8;
    HexMetricVals hexMetric;
    v_hex_quality(num_nodes, coordinates, V_HEX_ALL, &hexMetric);
    qualities[MB_EDGE_RATIO] = hexMetric.edge_ratio;
    qualities[MB_MAX_EDGE_RATIO] = hexMetric.max_edge_ratio;
    qualities[MB_SKEW] = hexMetric.skew;
    qualities[MB_TAPER] = hexMetric.taper;
    qualities[MB_VOLUME] = hexMetric.volume;
    qualities[MB_STRETCH] = hexMetric.stretch;
    qualities[MB_DIAGONAL] = hexMetric.diagonal;
    qualities[MB_DIMENSION] = hexMetric.dimension;
    qualities[MB_ODDY] = hexMetric.oddy;
    qualities[MB_MED_ASPECT_FROBENIUS] = hexMetric.med_aspect_frobenius;
    // MB_CONDITION is the same as MB_MAX_ASPECT_FROBENIUS
    qualities[MB_MAX_ASPECT_FROBENIUS] = hexMetric.condition;
    qualities[MB_CONDITION] = hexMetric.condition;
    qualities[MB_JACOBIAN] = hexMetric.jacobian;
    qualities[MB_SCALED_JACOBIAN] = hexMetric.scaled_jacobian;
    qualities[MB_SHEAR] = hexMetric.shear;
    qualities[MB_SHAPE] = hexMetric.shape;
    qualities[MB_RELATIVE_SIZE_SQUARED] = hexMetric.relative_size_squared;
    qualities[MB_SHAPE_AND_SIZE] = hexMetric.shape_and_size;
    qualities[MB_SHEAR_AND_SIZE] = hexMetric.shear_and_size;
    qualities[MB_DISTORTION] = hexMetric.distortion;
    break;
  }

  case MBTET: {
    num_nodes = 4;
    TetMetricVals tetMetrics;
    v_tet_quality(num_nodes, coordinates, V_TET_ALL, &tetMetrics);
    qualities[MB_EDGE_RATIO]=tetMetrics.edge_ratio;
    qualities[MB_RADIUS_RATIO] = tetMetrics.radius_ratio;
    qualities[MB_ASPECT_BETA] = tetMetrics.aspect_beta;
    qualities[MB_ASPECT_RATIO] = tetMetrics.aspect_ratio;
    qualities[MB_ASPECT_GAMMA] = tetMetrics.aspect_gamma;
    qualities[MB_MAX_ASPECT_FROBENIUS] = tetMetrics.aspect_frobenius;
    qualities[MB_MINIMUM_ANGLE] = tetMetrics.minimum_angle;
    qualities[MB_COLLAPSE_RATIO] = tetMetrics.collapse_ratio;
    qualities[MB_VOLUME] = tetMetrics.volume;
    qualities[MB_CONDITION] = tetMetrics.condition;
    qualities[MB_JACOBIAN] = tetMetrics.jacobian;
    qualities[MB_SCALED_JACOBIAN] = tetMetrics.scaled_jacobian;
    qualities[MB_SHAPE] = tetMetrics.shape;
    qualities[MB_RELATIVE_SIZE_SQUARED] = tetMetrics.relative_size_squared;
    qualities[MB_SHAPE_AND_SIZE] = tetMetrics.shape_and_size;
    qualities[MB_DISTORTION] = tetMetrics.distortion;
    break;
  }
  case MBPRISM: {
    num_nodes = 6;
    double volu = v_wedge_volume(num_nodes, coordinates);
    qualities[MB_VOLUME] = volu;
    break;
  }
  case MBKNIFE: {
    num_nodes = 7;
    double volu = v_knife_volume(num_nodes, coordinates);
    qualities[MB_VOLUME] = volu;
    break;
  }
  case MBQUAD: {
    num_nodes = 4;
    QuadMetricVals quadMetrics;
    v_quad_quality(num_nodes, coordinates, V_QUAD_ALL, &quadMetrics);
    qualities[MB_EDGE_RATIO] = quadMetrics.edge_ratio;
    qualities[MB_MAX_EDGE_RATIO] = quadMetrics.max_edge_ratio;
    qualities[MB_ASPECT_RATIO] = quadMetrics.aspect_ratio;   // 23
    qualities[MB_RADIUS_RATIO] = quadMetrics.radius_ratio;     // 21
    qualities[MB_MED_ASPECT_FROBENIUS] = quadMetrics.med_aspect_frobenius; // 9
    qualities[MB_MAX_ASPECT_FROBENIUS] = quadMetrics.max_aspect_frobenius; //10
    qualities[MB_SKEW] = quadMetrics.skew;     // 2
    qualities[MB_TAPER] = quadMetrics.taper;               // 3
    qualities[MB_WARPAGE] = quadMetrics.warpage;          // 27
    qualities[MB_AREA] = quadMetrics.area;           // 28
    qualities[MB_STRETCH] = quadMetrics.stretch;       // 5
    qualities[MB_MINIMUM_ANGLE] = quadMetrics.minimum_angle;   // 25
    qualities[MB_MAXIMUM_ANGLE] = quadMetrics.maximum_angle; // 29
    qualities[MB_ODDY] = quadMetrics.oddy;          // 8
    qualities[MB_CONDITION] = quadMetrics.condition;          // 11
    qualities[MB_JACOBIAN] = quadMetrics.jacobian;        // 12
    qualities[MB_SCALED_JACOBIAN] = quadMetrics.scaled_jacobian;    // 13
    qualities[MB_SHEAR] = quadMetrics.shear;     // 14
    qualities[MB_SHAPE] = quadMetrics.shape;         // 15
    qualities[MB_RELATIVE_SIZE_SQUARED] = quadMetrics.relative_size_squared; // 16
    qualities[MB_SHAPE_AND_SIZE] = quadMetrics.shape_and_size;      // 17
    qualities[MB_SHEAR_AND_SIZE] = quadMetrics.shear_and_size;    // 18
    qualities[MB_DISTORTION] = quadMetrics.distortion;      // 19
    break;
  }

  case MBTRI: {
    num_nodes = 3;
    TriMetricVals triMetrics;
    v_tri_quality(num_nodes, coordinates, V_TRI_ALL, &triMetrics);
    qualities[MB_EDGE_RATIO] = triMetrics.edge_ratio;        // 0
    qualities[MB_ASPECT_RATIO] = triMetrics.aspect_ratio;         // 23
    qualities[MB_RADIUS_RATIO] = triMetrics.radius_ratio;       // 21
    qualities[MB_MAX_ASPECT_FROBENIUS] = triMetrics.aspect_frobenius;    // 10
    qualities[MB_AREA] = triMetrics.area;             // 28
    qualities[MB_MINIMUM_ANGLE] = triMetrics.minimum_angle;       // 25
    qualities[MB_MAXIMUM_ANGLE] = triMetrics.maximum_angle;    // 29
    qualities[MB_CONDITION] = triMetrics.condition;     // 11
    qualities[MB_SCALED_JACOBIAN] = triMetrics.scaled_jacobian;    // 13
    // does not exist, even though it was defined in verdict.h; remove it from there too
    // case MB_SHEAR:                func = v_tri_shear; break;                  // 14
    qualities[MB_RELATIVE_SIZE_SQUARED] = triMetrics.relative_size_squared; // 16
    qualities[MB_SHAPE] = triMetrics.shape;           // 15
    qualities[MB_SHAPE_AND_SIZE] = triMetrics.shape_and_size;     // 17
    qualities[MB_DISTORTION] = triMetrics.distortion;          // 19
    break;
  }
  default:
    return MB_NOT_IMPLEMENTED;
  }
  return MB_SUCCESS;
}

} // end namespace
