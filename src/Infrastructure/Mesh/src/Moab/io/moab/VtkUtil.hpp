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

#ifndef VTK_UTIL_HPP
#define VTK_UTIL_HPP

#include "moab/EntityType.hpp"

namespace moab {

//! Structure defining relation between MOAB and VTK element
//! types.  VTK had different types for quadratic than linear
//! elements, so a tuple of the MOAB type and number of
//! elements maps to a VTK type.
struct VtkElemType
{
  const char* name;            //!< String name for use in error messages
  unsigned vtk_type;           //!< VTK integer type
  EntityType mb_type;        //!< MOAB type
  unsigned num_nodes;          //!< Number of nodes (0 for polygon)
  const unsigned* node_order;  //!< VTK element node ordering, indexed by
                               //!< the VTK node position and containing
                               //!< the corresponding MOAB node position.
                               //!< NOTE: This field is NULL if MOAB and VTK 
                               //!< ordering is the same!
};

//! General data about VTK files for use by read and write code.
//! \author Jason Kraftcheck
class VtkUtil 
{

public:
    //! vtk data type names, indexed by DataType
  static const char *vtkTypeNames[];
  
    //! Vtk types, indexed by VTK type number.
    //! For unused VTK type numbers, mb_type will be MBMAXTYPE.
  static const VtkElemType vtkElemTypes[];
  
    //! Lenght of \ref vtkElemTypes
  static const unsigned numVtkElemType;
  
    //! Get the VTK type corresponding to a tuple of the MOAB type and number of nodes.
    //! num_nodes is ignored for MBPOLYGON type.
  static const VtkElemType* get_vtk_type( EntityType type, unsigned num_nodes );
};

} // namespace moab

#endif
