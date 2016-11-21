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

#ifndef EXOII_UTIL
#define EXOII_UTIL

//
// ExoIIUtil class: utility class for functions used by both reader
// and writer

#ifndef IS_BUILDING_MB
#error "ExoIIUtil.hpp isn't supposed to be included into an application"
#endif

#include "moab/Forward.hpp"
#include "moab/ExoIIInterface.hpp"


namespace moab {

class ExoIIUtil : public ExoIIInterface
{

  Interface* mMB;

public:
  ExoIIUtil(Interface* mdb) : mMB(mdb) {}
  ~ExoIIUtil(){}

  //! given the element name, return the type
  virtual ExoIIElementType element_name_to_type(const char* name)
  {
    return static_element_name_to_type(name);
  }

  //! get the element type of the entity; this entity can either be a meshset, 
  //! in which case it will be assumed to be a material set meshset, or an 
  //! individual entity.
  virtual  ExoIIElementType get_element_type(EntityHandle entity,
                                             Tag mid_nodes_tag, Tag geom_dimension_tag, 
                                             EntityType indiv_entity_type = MBMAXTYPE)
  {
    return static_get_element_type(mMB, entity, mid_nodes_tag, geom_dimension_tag,
                                   indiv_entity_type);    
  }

  virtual void has_mid_nodes(ExoIIElementType elem_type, int* array)
  {
    array[0] = HasMidNodes[elem_type][0]; 
    array[1] = HasMidNodes[elem_type][1]; 
    array[2] = HasMidNodes[elem_type][2]; 
    array[3] = HasMidNodes[elem_type][3]; 
  }

  virtual int has_mid_nodes(ExoIIElementType elem_type, int dimension )
  {
    return HasMidNodes[elem_type][dimension];
  }

  virtual int geometric_dimension(const ExoIIElementType elem_type) 
  {
    return ElementGeometricDimension[elem_type];
  }
  
  virtual const char* element_type_name(ExoIIElementType type)
  {
    return ElementTypeNames[type];
  }


  
//! given the element name, return the type
  static ExoIIElementType static_element_name_to_type(const char *name);

//! get the element type of the entity; this entity can either be a meshset, in which
//! case it will be assumed to be a material set meshset, or an individual entity.  If a
//! meshset, and indiv_entity_type is input, that type is used to start the search for
//! the connectivity tag which determines how many vertices per entity are defined for that meshset
  static ExoIIElementType static_get_element_type(Interface *mdbImpl,
                                           const EntityHandle entity,
                                           const Tag mid_nodes_tag,
                                           const Tag geom_dimension_tag,
                                           const EntityType indiv_entity_type = MBMAXTYPE);

//! given the number of vertices in an entity, and optionally the entity type and
//! geometric dimension, return the corresponding exodusII element type; dimension defaults
//! to 3 following TSTT convention
  static ExoIIElementType get_element_type_from_num_verts(const int num_verts, 
                                                          const EntityType entity_type = MBMAXTYPE,
                                                          const int dimension = 3);

//! the MB entity type used for each element type
  static const EntityType ExoIIElementMBEntity[];

//! names for all the element types that MB ExoII reader supports
  static const char* ElementTypeNames[];

//! number of vertices per element
  static const int VerticesPerElement[];

//! HasMidNode[elem_type][dim] = 1 denotes that elem_type has mid-nodes
//! on sub-entities of dimension dim
  static const int HasMidNodes[][4];

//! geometric dimension of each element
  static const int ElementGeometricDimension[];

};

//! postfix increment operator for EntityType
inline ExoIIElementType operator++(ExoIIElementType &type, int)
{
  return (ExoIIElementType)(((int&)type)++);
}  

//! prefix increment operator for EntityType
inline ExoIIElementType& operator++(ExoIIElementType& type)
{
  return (ExoIIElementType&)(++((int&)type));
}

} // namespace moab

#endif
