#include "DamselUtil.hpp"
#include "moab/Range.hpp"

#include "damsel.h"

namespace moab {

damsel_entity_type DamselUtil::mtod_entity_type[] = {
    DAMSEL_ENTITY_TYPE_VERTEX,      //  MBVERTEX
    DAMSEL_ENTITY_TYPE_EDGE,  // MBEDGE
    DAMSEL_ENTITY_TYPE_TRI, // MBTRI
    DAMSEL_ENTITY_TYPE_QUAD, //   MBQUAD
    DAMSEL_ENTITY_TYPE_POLYGON, //   MBPOLYGON
    DAMSEL_ENTITY_TYPE_TET,//  MBTET
    DAMSEL_ENTITY_TYPE_PYRAMID,  //   MBPYRAMID
    DAMSEL_ENTITY_TYPE_PRISM,  //   MBPRISM
    DAMSEL_ENTITY_TYPE_UNDEFINED, // MBKNIFE
    DAMSEL_ENTITY_TYPE_HEX,  //   MBHEX,
    DAMSEL_ENTITY_TYPE_POLYHEDRON, //   MBPOLYHEDRON
    DAMSEL_ENTITY_TYPE_UNDEFINED   //   MBENTITYSET
};

EntityType DamselUtil::dtom_entity_type[] = {
    MBVERTEX,      //  MBVERTEX
    MBEDGE,  // MBEDGE
    MBTRI, // MBTRI
    MBQUAD, //   MBQUAD
    MBPOLYGON, //   MBPOLYGON
    MBTET,//  MBTET
    MBPRISM,  //   MBPRISM
    MBPYRAMID,  //   MBPYRAMID
    MBHEX,  //   MBHEX,
    MBPOLYHEDRON, //   MBPOLYHEDRON
    MBMAXTYPE,   //   MBENTITYSET
    MBMAXTYPE // MBMAXTYPE
};

damsel_data_type DamselUtil::mtod_data_type[] = {
    DAMSEL_DATA_TYPE_BYTES, // MB_TYPE_OPAQUE
    DAMSEL_DATA_TYPE_INTEGER, // MB_TYPE_INTEGER
    DAMSEL_DATA_TYPE_DOUBLE, // MB_TYPE_DOUBLE
    DAMSEL_DATA_TYPE_INVALID, // MB_TYPE_BIT
    DAMSEL_DATA_TYPE_HANDLE // MB_TYPE_HANDLE
};

DataType DamselUtil::dtom_data_type[] = {
    MB_TYPE_OPAQUE, // DAMSEL_DATA_TYPE_INVALID = 0,
    MB_TYPE_OPAQUE,   // DAMSEL_DATA_TYPE_BYTES = 1,
    MB_TYPE_INTEGER,  // DAMSEL_DATA_TYPE_INTEGER = 2,
    MB_TYPE_OPAQUE,  // DAMSEL_DATA_TYPE_INT64 = 3,
    MB_TYPE_OPAQUE,  // DAMSEL_DATA_TYPE_FLOAT = 4,
    MB_TYPE_DOUBLE,  // DAMSEL_DATA_TYPE_DOUBLE = 5,
    MB_TYPE_HANDLE,  // DAMSEL_DATA_TYPE_HANDLE = 6,
    MB_TYPE_OPAQUE,  // DAMSEL_DATA_TYPE_ID_T = 7,
    MB_TYPE_OPAQUE,  // DAMSEL_DATA_TYPE_SHORTINT = 8,
    MB_TYPE_OPAQUE  // DAMSEL_DATA_TYPE_PREDEFINED_WATERMARK = 9;
};

DamselUtil::DamselUtil() 
        : dmslLib(DAMSEL_LIBRARY_INVALID), dmslModel(DAMSEL_MODEL_INVALID),
          moabHandleType(DAMSEL_HANDLE_TYPE_INVALID) 
{}
    
    //! convert handles in a container to a range; assumes EntityHandle and Damsel
    //! entity handles are the same size
ErrorCode DamselUtil::container_to_range(damsel_model m, damsel_container &c, Range &r) 
{
  if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_SEQUENCE) {
    damsel_handle start;
    size_t count, stride;
    damsel_err_t err = DMSLcontainer_sequence_get_contents(m, c, &start, &count, &stride);
    CHK_DMSL_ERR_NM(err);
    for (damsel_handle i = start+(count-1)*stride; i >= start; i-=stride)
      r.insert(i);
  }
  else if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_VECTOR) {
    damsel_handle *handle_ptr;
    size_t count;
    damsel_err_t err = DMSLcontainer_vector_get_contents(m, c, &handle_ptr, &count);
    CHK_DMSL_ERR_NM(err);
    for (int i = count-1; i >= 0; i--)
      r.insert(handle_ptr[i]);
  }
  else if (DMSLcontainer_get_type(c) == DAMSEL_HANDLE_CONTAINER_TYPE_TREE) {
    damsel_handle_ptr node_ptr = NULL;
    damsel_container cont = NULL;
    damsel_err_t err = DMSLcontainer_tree_get_contents(m, c, &node_ptr, &cont);
    while (err.id == DMSL_OK.id && cont) {
      ErrorCode rval = container_to_range(m, c, r);
      if (MB_SUCCESS != rval) return rval;
      err = DMSLcontainer_tree_get_contents(m, c, &node_ptr, &cont);
    }
  }
      
  return MB_SUCCESS;
}

}
