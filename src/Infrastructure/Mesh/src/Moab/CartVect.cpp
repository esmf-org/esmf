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

#include "moab/CartVect.hpp"
#include <ostream>

namespace moab {

std::ostream& operator<<( std::ostream& s, const CartVect& v )
  { return s << '[' << v[0] << ' ' << v[1] << ' ' << v[2] << ']'; }
  
} // namespace moab
