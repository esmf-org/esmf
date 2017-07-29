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


#ifndef MB_INTERNALS_HPP
#define MB_INTERNALS_HPP

#ifdef WIN32
#pragma warning(disable : 4786)
#endif

#ifndef IS_BUILDING_MB
#error "Internals.hpp isn't supposed to be included into an application"
#endif

#include "moab/Types.hpp"
#include <assert.h>

namespace moab {

/*! Define EntityHandle for both 32 bit and 64 bit systems.
 *  The decision to use 64 bit handles must be made at compile time.
 *  \bug we should probably have an Int64 typedef
 *
 *  EntityHandle format:
 *  0xXYYYYYYY  (assuming a 32-bit handle.  Top 4 bits reserved on a 64 bit system)
 *  X - reserved for entity type.  This system can only handle 15 different types
 *  Y - Entity id space.  Max id is over 200M
 *
 *  Note that for specialized databases (such as all hex) 16 bits are not
 *  required for the entity type and the id space can be increased to over 2B.
 */
#define MB_TYPE_WIDTH 4
#define MB_ID_WIDTH (8*sizeof(EntityHandle)-MB_TYPE_WIDTH)
#define MB_TYPE_MASK ((EntityHandle)0xF << MB_ID_WIDTH)
//             2^MB_TYPE_WIDTH-1 ------^

#define MB_START_ID ((EntityID)1)        //!< All entity id's currently start at 1
#define MB_END_ID ((EntityID)MB_ID_MASK) //!< Last id is the complement of the MASK
#define MB_ID_MASK (~MB_TYPE_MASK)

//! Given a type and an id create a handle.  
inline EntityHandle CREATE_HANDLE(const unsigned type, const EntityID id, int& err) 
{
  err = 0; //< Assume that there is a real error value defined somewhere

  if (id > MB_END_ID || type > MBMAXTYPE)
  {
    err = 1;   //< Assume that there is a real error value defined somewhere
    return 1;  //<You've got to return something.  What do you return?
  }
  
  return (((EntityHandle)type) << MB_ID_WIDTH)|id;
}

inline EntityHandle CREATE_HANDLE( const unsigned type, const EntityID id )
{
  assert( id <= MB_END_ID && type <= MBMAXTYPE );
  return (((EntityHandle)type) << MB_ID_WIDTH)|id;
}

inline EntityHandle FIRST_HANDLE( unsigned type )
  { return (((EntityHandle)type) << MB_ID_WIDTH)|MB_START_ID; }

inline EntityHandle LAST_HANDLE( unsigned type )
  { return ((EntityHandle)(type+1) << MB_ID_WIDTH) - 1; }

//! Get the entity id out of the handle.
inline EntityID ID_FROM_HANDLE (EntityHandle handle)
{
  return (handle & MB_ID_MASK);
}

//! Get the type out of the handle.  Can do a simple shift because
//! handles are unsigned (therefore shifting fills with zero's)
inline EntityType TYPE_FROM_HANDLE(EntityHandle handle) 
{
  return static_cast<EntityType> (handle >> MB_ID_WIDTH);
}

} // namespace moab

#endif
