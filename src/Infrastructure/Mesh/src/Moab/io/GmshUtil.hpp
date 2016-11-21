/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

#ifndef GMSH_UTIL_HPP
#define GMSH_UTIL_HPP

#include "moab/EntityType.hpp"

namespace moab {

//! Structure defining relation between MOAB and VTK element
//! types.  VTK had different types for quadratic than linear
//! elements, so a tuple of the MOAB type and number of
//! elements maps to a VTK type.
struct GmshElemType
{
  const char* name;      //!< String name for use in error messages
  unsigned gmsh_type;    //!< GMsh integer type
  EntityType mb_type;  //!< MOAB type
  unsigned num_nodes;    //!< Number of nodes (0 for polygon)
  const int* node_order; //!< Gmsh element node ordering, indexed by
                         //!< the Gmsh node position and containing
                         //!< the corresponding MOAB node position.
                         //!< NOTE: This field is NULL if MOAB and Gmsh 
                         //!< ordering is the same!
};

//! General data about GMsh files for use by read and write code.
//! \author Jason Kraftcheck
class GmshUtil 
{

public:
    //! Gmsh types, indexed by Gmsh type number.
    //! For unused Gmsh type numbers, mb_type will be MBMAXTYPE.
  static const GmshElemType gmshElemTypes[];
  
    //! Length of \ref gmshElemTypes
  static const unsigned numGmshElemType;
  
    //! Get the Gmsh type corresponding to a tuple of the MOAB type and number of nodes.
    //! num_nodes is ignored for MBPOLYGON type.  Returns -1 for unsupported types.
  static int get_gmsh_type( EntityType type, unsigned num_nodes );
};

} // namespace moab

#endif
