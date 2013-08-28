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

#ifndef MB_UTIL_HPP
#define MB_UTIL_HPP

#include "moab/Forward.hpp"

namespace moab {

/** \struct Coord
 * \brief Structure for storing coordinate data
 */
struct  Coord
{
  double x;
  double y;
  double z;
};


/** \class Util
 *
 * \brief Utility functions for normal and centroid for entities
 */
class Util
{
public:

   
  static void normal(Interface* MB, EntityHandle handle, double& x, double& y, double& z);

  static void centroid(Interface *MB, EntityHandle handle,Coord &coord);
 // static void edge_centers(Interface *MB, EntityHandle handle, std::vector<Coord> &coords_list);

  //static void face_centers(Interface *MB, EntityHandle handle, std::vector<Coord> &coords_list);
  static ErrorCode gather_set(Interface * MB, EntityHandle & gather_set);

  static ErrorCode gather_set_entities(Interface * MB, EntityHandle & gather_set, Range & ents);

private:

  Util(){}

};

} // namespace moab

#endif
