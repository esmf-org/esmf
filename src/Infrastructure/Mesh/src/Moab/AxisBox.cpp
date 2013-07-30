/**
 * MOAB, a Mesh-Oriented datABase, is a software component for creating,
 * storing and accessing finite element mesh data.
 * 
 * Copyright 2004 Sandia Corporation.  Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Coroporation, the U.S. Government
 * retains certain rights in this software.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 */

/**
 * \brief Class representing axis-aligned bounding box
 * \author Jason Kraftcheck (kraftche@cae.wisc.edu)
 * \date August, 2006
 */


#include "AxisBox.hpp"
#include "moab/Range.hpp"
#include <assert.h>

namespace moab {

const char* const AXIS_BOX_TAG_NAME = "AXIS_BOX";


ErrorCode AxisBox::get_tag( Tag& tag_out,
                                Interface* interface,
                                const char* tagname )
{
  assert( sizeof(AxisBox) == 6*sizeof(double) );
  
  if (!tagname)
    tagname = AXIS_BOX_TAG_NAME;
 
  return interface->tag_get_handle( tagname,
                                    sizeof(AxisBox),
                                    MB_TYPE_DOUBLE,
                                    tag_out,
                                    MB_TAG_DENSE|MB_TAG_CREAT|MB_TAG_BYTES );
}

ErrorCode AxisBox::calculate( AxisBox& box,
                                  EntityHandle set,
                                  Interface* interface )
{
  Range range;
  ErrorCode rval = interface->get_entities_by_handle( set, range );
  if (MB_SUCCESS != rval)
    return rval;
  
  return calculate( box, range, interface );
}

ErrorCode AxisBox::calculate( AxisBox& box,
                                  const Range& entities,
                                  Interface* interface )
{
  ErrorCode rval;
  Range vertices;
  Range elements;
  
  elements.merge( entities.upper_bound(MBVERTEX), entities.lower_bound(MBENTITYSET) );
  rval = interface->get_adjacencies( elements, 0, false, vertices );
  if (MB_SUCCESS != rval)
    return rval;
  
  vertices.merge( entities.begin(), entities.upper_bound(MBVERTEX) );
  
  std::vector<double> coords( 3*vertices.size() );
  rval = interface->get_coords( vertices, &coords[0] );
  if (MB_SUCCESS != rval)
    return rval;
  
  box = AxisBox();
  std::vector<double>::const_iterator i = coords.begin();
  for (; i != coords.end(); i += 3)
    box |= &*i;
  
  return MB_SUCCESS;
}
  
} // namespace moab


