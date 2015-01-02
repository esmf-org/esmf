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


#ifdef WIN32
#ifdef _DEBUG
// turn off warnings that say they debugging identifier has been truncated
// this warning comes up when using some STL containers
#pragma warning(disable : 4786)
#endif
#endif


#include "WriteVtk.hpp"
#include "VtkUtil.hpp"
#include "SysUtil.hpp"

#include <fstream>
#include <iostream>
#include <stdio.h>
#include <assert.h>
#include <vector>
#include <set>
#include <iterator>

#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include "MBTagConventions.hpp"
#include "moab/WriteUtilIface.hpp"
#include "Internals.hpp"
#include "FileOptions.hpp"
#include "moab/Version.h"

#define INS_ID(stringvar, prefix, id) \
sprintf(stringvar, prefix, id)

namespace moab {

const int DEFAULT_PRECISION = 10;
const bool DEFAULT_STRICT = true;

WriterIface *WriteVtk::factory( Interface* iface )
  { return new WriteVtk( iface ); }

WriteVtk::WriteVtk(Interface *impl) 
    : mbImpl(impl), writeTool(0), mStrict(DEFAULT_STRICT)
{
  assert(impl != NULL);

  impl->query_interface( writeTool );
}

WriteVtk::~WriteVtk() 
{
  mbImpl->release_interface(writeTool);
}

ErrorCode WriteVtk::write_file(const char *file_name, 
                                 const bool overwrite,
                                 const FileOptions& opts,
                                 const EntityHandle *output_list,
                                 const int num_sets,
                                 const std::vector<std::string>& ,
                                 const Tag* tag_list,
                                 int num_tags,
                                 int )
{
  ErrorCode rval;

    // Get precision for node coordinates
  int precision;
  if (MB_SUCCESS != opts.get_int_option( "PRECISION", precision ))
    precision = DEFAULT_PRECISION;
  
  if (MB_SUCCESS == opts.get_null_option( "STRICT" ))
    mStrict = true;
  else if (MB_SUCCESS == opts.get_null_option( "RELAXED" ))
    mStrict = false;
  else
    mStrict = DEFAULT_STRICT;
  
    // Get entities to write
  Range nodes, elems;
  rval = gather_mesh( output_list, num_sets, nodes, elems );
  if (MB_SUCCESS != rval)
    return rval;
  
    // Honor overwrite flag
  if (!overwrite)
  {
    rval = writeTool->check_doesnt_exist( file_name );
    if (MB_SUCCESS != rval)
      return rval;
  }
  
    // Create file
  std::ofstream file( file_name );
  if (!file)
  {
    writeTool->report_error("Could not open file: %s\n", file_name );
    return MB_FILE_WRITE_ERROR;
  }
  file.precision( precision );
  
    // Write file
  if ((rval = write_header(file              )) != MB_SUCCESS ||
      (rval = write_nodes( file, nodes       )) != MB_SUCCESS ||
      (rval = write_elems( file, nodes, elems)) != MB_SUCCESS ||
      (rval = write_tags ( file, true,  nodes, tag_list, num_tags)) != MB_SUCCESS ||
      (rval = write_tags ( file, false, elems, tag_list, num_tags)) != MB_SUCCESS)
  {
    file.close();
    remove( file_name );
    return rval;
  } 
  
  return MB_SUCCESS;
}


ErrorCode WriteVtk::gather_mesh( const EntityHandle* set_list,
                                   int num_sets, 
                                   Range& nodes,
                                   Range& elems )
{
  ErrorCode rval;
  int e;
  
  if (!set_list || !num_sets)
  {
    Range a;
    rval = mbImpl->get_entities_by_handle( 0, a );
    if (MB_SUCCESS != rval) return rval;

    Range::const_iterator node_i, elem_i, set_i;
    node_i = a.lower_bound( a.begin(), a.end(), CREATE_HANDLE(    MBVERTEX, 0, e ) );
    elem_i = a.lower_bound(    node_i, a.end(), CREATE_HANDLE(      MBEDGE, 0, e ) );
    set_i  = a.lower_bound(    elem_i, a.end(), CREATE_HANDLE( MBENTITYSET, 0, e ) );
    nodes.merge( node_i, elem_i );
    elems.merge( elem_i, set_i );

      // filter out unsupported element types
    EntityType et = MBEDGE;
    for (et++; et < MBENTITYSET; et++) {
      if (VtkUtil::get_vtk_type(et, CN::VerticesPerEntity(et))) continue;
      Range::iterator 
        eit = elems.lower_bound(elems.begin(), elems.end(), CREATE_HANDLE(et, 0, e)),
        ep1it = elems.lower_bound(elems.begin(), elems.end(), CREATE_HANDLE(et+1, 0, e));
      elems.erase(eit, ep1it);
    }
  }
  else
  {
    std::set<EntityHandle> visited;
    std::vector<EntityHandle> sets;
    sets.reserve(num_sets);
    std::copy( set_list, set_list + num_sets, std::back_inserter(sets) );
    while (!sets.empty()) {
        // get next set
      EntityHandle set = sets.back();
      sets.pop_back();
        // skip sets we've already done
      if (!visited.insert(set).second)
        continue;
      
      Range a;
      rval = mbImpl->get_entities_by_handle( set, a );
      if (MB_SUCCESS != rval) return rval;
      
      Range::const_iterator node_i, elem_i, set_i;
      node_i = a.lower_bound( a.begin(), a.end(), CREATE_HANDLE(    MBVERTEX, 0, e ) );
      elem_i = a.lower_bound(    node_i, a.end(), CREATE_HANDLE(      MBEDGE, 0, e ) );
      set_i  = a.lower_bound(    elem_i, a.end(), CREATE_HANDLE( MBENTITYSET, 0, e ) );
      nodes.merge( node_i, elem_i );
      elems.merge( elem_i, set_i );
      std::copy( set_i, a.end(), std::back_inserter(sets) );
      
      a.clear();
      rval = mbImpl->get_child_meshsets( set, a );
      std::copy( a.begin(), a.end(), std::back_inserter(sets) );
    }
    
    for (Range::const_iterator ei = elems.begin(); ei != elems.end(); ++ei)
    {
      std::vector<EntityHandle> connect;
      rval = mbImpl->get_connectivity( &(*ei), 1, connect );
      if (MB_SUCCESS != rval) return rval;

      for (unsigned int i = 0; i < connect.size(); ++i)
	nodes.insert( connect[i] );
    }
  }

  if (nodes.empty())
  {
    writeTool->report_error( "Nothing to write.\n" );
    return  MB_ENTITY_NOT_FOUND;
  }
  
  return MB_SUCCESS;
}

ErrorCode WriteVtk::write_header( std::ostream& stream )
{
  stream << "# vtk DataFile Version 3.0" << std::endl;
  stream << MB_VERSION_STRING << std::endl;
  stream << "ASCII" << std::endl;
  stream << "DATASET UNSTRUCTURED_GRID" << std::endl;
  return MB_SUCCESS;
}


ErrorCode WriteVtk::write_nodes( std::ostream& stream, const Range& nodes )
{
  ErrorCode rval;
  
  stream << "POINTS " << nodes.size() << " double" << std::endl;
  
  double coords[3];
  for (Range::const_iterator i = nodes.begin(); i != nodes.end(); ++i) {
    coords[1] = coords[2] = 0.0;
    rval = mbImpl->get_coords( &*i, 1, coords );
    if (MB_SUCCESS != rval)
      return rval;
    stream << coords[0] << ' ' << coords[1] << ' ' <<coords[2] << std::endl;
  }
  
  return MB_SUCCESS;
}

ErrorCode WriteVtk::write_elems( std::ostream& stream, 
                                   const Range& nodes,
                                   const Range& elems )
{
  ErrorCode rval;

  // Get and write counts
  unsigned long num_elems, num_uses;
  num_elems = num_uses = elems.size();
  for (Range::const_iterator i = elems.begin(); i != elems.end(); ++i)
  {
    EntityType type = mbImpl->type_from_handle(*i);
    if (!VtkUtil::get_vtk_type(type, CN::VerticesPerEntity(type))) continue;
    
    std::vector<EntityHandle> connect;
    rval = mbImpl->get_connectivity( &(*i), 1, connect );
    
    if (MB_SUCCESS != rval)
      return rval;

    num_uses += connect.size();
  }
  stream << "CELLS " << num_elems << ' ' << num_uses << std::endl;
  
    // Write element connectivity
  std::vector<int> conn_data;
  std::vector<unsigned> vtk_types( elems.size() );
  std::vector<unsigned>::iterator t = vtk_types.begin();
  for (Range::const_iterator i = elems.begin(); i != elems.end(); ++i)
  {
      // Get type information for element
    EntityType type = TYPE_FROM_HANDLE(*i);

      // Get element connectivity
    std::vector<EntityHandle> connect;
    rval = mbImpl->get_connectivity( &(*i), 1, connect );
    int conn_len = connect.size();

    if (MB_SUCCESS != rval)
      return rval;

      // Get VTK type
    const VtkElemType* vtk_type = VtkUtil::get_vtk_type( type, conn_len );
    if (!vtk_type) {
        // try connectivity with 1 fewer node
      vtk_type = VtkUtil::get_vtk_type( type, conn_len-1 );
      if (vtk_type) conn_len--;
      else {
        writeTool->report_error( "Vtk file format does not support elements "
                                 "of type %s (%d) with %d nodes.\n", CN::EntityTypeName(type), 
                                 (int)type, conn_len);
        return MB_FAILURE;
      }
    }

      // Get IDs from vertex handles
    assert( conn_len > 0 );
    conn_data.resize( conn_len );
    for (int j = 0; j < conn_len; ++j)
      conn_data[j] = nodes.index( connect[j] );
    
      // Save VTK type index for later
    *t = vtk_type->vtk_type;
    ++t;
    
      // Write connectivity list
    stream << conn_len;
    if (vtk_type->node_order)
      for (int k = 0; k < conn_len; ++k)
        stream << ' ' << conn_data[vtk_type->node_order[k]];
    else
      for (int k = 0; k < conn_len; ++k)
        stream << ' ' << conn_data[k];
    stream << std::endl;
  }
   
    // Write element types
  stream << "CELL_TYPES " << num_elems << std::endl;
  for (std::vector<unsigned>::const_iterator i = vtk_types.begin(); i != vtk_types.end(); ++i)
    stream << *i << std::endl;
  
  return MB_SUCCESS;
}



ErrorCode WriteVtk::write_tags( std::ostream& stream, 
                                  bool nodes, 
                                  const Range& entities,
                                  const Tag* tag_list,
                                  int num_tags )
{
  ErrorCode rval;
  
    // The #$%@#$% MOAB interface does not have a function to retreive
    // all entities with a tag, only all entities with a specified type
    // and tag.  Define types to loop over depending on the if vertex
    // or element tag data is being written.  It seems horribly inefficient
    // to have the implementation subdivide the results by type and have
    // to call that function once for each type just to recombine the results.
    // Unfortunamtely, there doesn't seem to be any other way.
  EntityType low_type, high_type;
  if (nodes) 
  {
    low_type = MBVERTEX;
    high_type = MBEDGE;
  }
  else
  {
    low_type = MBEDGE;
    high_type = MBENTITYSET;
  }

    // Get all defined tags
  std::vector<Tag> tags;
  std::vector<Tag>::iterator i;
  rval = writeTool->get_tag_list( tags, tag_list, num_tags, false );
  if (MB_SUCCESS != rval)
    return rval;
  
    // For each tag...  
  bool entities_have_tags = false;
  for (i = tags.begin(); i != tags.end(); ++i)
  {
      // Skip tags holding entity handles -- no way to save them
    DataType dtype;
    rval = mbImpl->tag_get_data_type( *i, dtype );
    if (MB_SUCCESS != rval)
      return rval;
    if (dtype == MB_TYPE_HANDLE)
      continue;
      
      // If in strict mode, don't write tags that do not fit in any 
      // attribute type (SCALAR : 1 to 4 values, VECTOR : 3 values, TENSOR : 9 values)
    if (mStrict) {
      int count;
      rval = mbImpl->tag_get_length( *i, count );
      if (MB_SUCCESS != rval)
        return rval;
      if (count < 1 || (count > 4 && count != 9))
        continue;
    }
          
    
      // Get subset of input entities that have the tag set
    Range tagged;
    for (EntityType type = low_type; type < high_type; ++type)
    {
      Range tmp_tagged;
      rval = mbImpl->get_entities_by_type_and_tag( 0, type, &*i, 0, 1, tmp_tagged );
      if (MB_SUCCESS != rval)
        return rval;
      tmp_tagged = intersect( tmp_tagged, entities );
      tagged.merge( tmp_tagged );
    }

      // If any entities were tadged
    if (!tagged.empty())
    {
        // If this is the first tag being written for the
        // entity type, write the label marking the beginning
        // of the tag data.
      if (!entities_have_tags)
      {
        entities_have_tags = true;
        stream << (nodes ? "POINT_DATA " : "CELL_DATA ") << entities.size() << std::endl;
      }
      
        // Write the tag 
      rval = write_tag( stream, *i, entities, tagged );
      if (MB_SUCCESS != rval)
        return rval;
    }
  }
  
  return MB_SUCCESS;
}

template <typename T>
void WriteVtk::write_data( std::ostream& stream, 
                           const std::vector<T>& data,
                           unsigned vals_per_tag )
{
  typename std::vector<T>::const_iterator d = data.begin();
  const unsigned long n = data.size() / vals_per_tag;
  
  for (unsigned long i = 0; i < n; ++i)
  {
    for (unsigned j = 0; j < vals_per_tag; ++j, ++d)
    {
      if (sizeof(T) == 1) 
        stream << (unsigned int)*d << ' ';
      else
        stream << *d << ' ';
    }
    stream << std::endl;
  }
}

//template <>
//void WriteVtk::write_data<unsigned char>( std::ostream& stream, 
//                                          const std::vector<unsigned char>& data,
//                                          unsigned vals_per_tag )
//{
//  std::vector<unsigned char>::const_iterator d = data.begin();
//  const unsigned long n = data.size() / vals_per_tag;
//  
//  for (unsigned long i = 0; i < n; ++i)
//  {
//    for (unsigned j = 0; j < vals_per_tag; ++j, ++d)
//    {
//      stream << (unsigned int)*d << ' ';
//    }
//    stream << std::endl;
//  }
//}


template <typename T>
ErrorCode WriteVtk::write_tag( std::ostream& stream, Tag tag, const Range& entities, const Range& tagged,
                                 const int)
{
  ErrorCode rval;
  const unsigned long n = entities.size();
  
    // Get tag properties  

  std::string name;
  int vals_per_tag;
  if (MB_SUCCESS != mbImpl->tag_get_name( tag, name ) ||
      MB_SUCCESS != mbImpl->tag_get_length( tag, vals_per_tag ) )
    return MB_FAILURE;
  
    // Get a tag value for each entity.  Do this by initializing the
    // "data" vector with zero, and then filling in the values for
    // the entities that actually have the tag set.
  std::vector<T> data;
  data.resize( n * vals_per_tag, 0 );
  // if there is a default value for the tag, set the actual default value
  std::vector<T> def_value(vals_per_tag);
  rval = mbImpl-> tag_get_default_value( tag, &(def_value[0]));
  if (MB_SUCCESS==rval)
  {
     SysUtil::setmem(&(data[0]), &(def_value[0]), vals_per_tag*sizeof(T), n);
  }
  Range::const_iterator t = tagged.begin();
  typename std::vector<T>::iterator d = data.begin();
  for (Range::const_iterator i = entities.begin(); 
       i != entities.end() && t != tagged.end(); ++i, d += vals_per_tag)
  {
    if (*i == *t)
    {
      ++t;
      rval = mbImpl->tag_get_data( tag, &*i, 1, &*d );
      if (MB_SUCCESS != rval)
        return rval;
    }
  }
  
    // Write the tag values, one entity per line.
  write_data( stream, data, vals_per_tag );
  
  return MB_SUCCESS;
}

ErrorCode WriteVtk::write_bit_tag( std::ostream& stream, 
                                     Tag tag, 
                                     const Range& entities, 
                                     const Range& tagged )
{
  ErrorCode rval;
  const unsigned long n = entities.size();
  
    // Get tag properties  

  std::string name;
  int vals_per_tag;
  if (MB_SUCCESS != mbImpl->tag_get_name( tag, name ) ||
      MB_SUCCESS != mbImpl->tag_get_length( tag, vals_per_tag ) )
    return MB_FAILURE;

  if (vals_per_tag > 8) {
    writeTool->report_error( "Invalid tag size for bit tag \"%s\"\n", name.c_str() );
    return MB_FAILURE;
  } 
  
    // Get a tag value for each entity.  
    // Get bits for each entity and unpack into
    // one integer in the 'data' array for each bit.
    // Initialise 'data' to zero because we will skip
    // those entities for which the tag is not set.
  std::vector<unsigned short> data;
  data.resize( n * vals_per_tag, 0 );
  Range::const_iterator t = tagged.begin();
  std::vector<unsigned short>::iterator d = data.begin();
  for (Range::const_iterator i = entities.begin(); 
       i != entities.end() && t != tagged.end(); ++i)
  {
    if (*i == *t)
    {
      ++t;
      unsigned char value;
      rval = mbImpl->tag_get_data( tag, &*i, 1, &value );
      for (int j = 0; j < vals_per_tag; ++j, ++d)
        *d = (unsigned short) (value & (1<<j) ? 1 : 0);
      if (MB_SUCCESS != rval)
        return rval;
    }
    else
    {
      // if tag is not set for entity, skip values in array
      d += vals_per_tag;
    }
  }
  
    // Write the tag values, one entity per line.
  write_data( stream, data, vals_per_tag );
  
  return MB_SUCCESS;
}

ErrorCode WriteVtk::write_tag( std::ostream& s, Tag tag,
                                 const Range& entities,
                                 const Range& tagged )
{
    // Get tag properties
  std::string name;
  DataType type;
  int vals_per_tag;
  if (MB_SUCCESS != mbImpl->tag_get_name( tag, name ) ||
      MB_SUCCESS != mbImpl->tag_get_length( tag, vals_per_tag ) ||
      MB_SUCCESS != mbImpl->tag_get_data_type( tag, type ))
    return MB_FAILURE;
  
    // Skip tags of type ENTITY_HANDLE
  if (type == MB_TYPE_HANDLE)
    return MB_FAILURE;
  
    // Now that we're past the point where the name would be used in
    // an error message, remove any spaces to conform to VTK file.
  for (std::string::iterator i = name.begin(); i != name.end(); ++i)
    if (isspace(*i) || iscntrl(*i))
      *i = '_';
  
    // Write the tag desciption
  if (vals_per_tag == 3 && type == MB_TYPE_DOUBLE)
    s << "VECTORS " << name << ' ' << VtkUtil::vtkTypeNames[type] << std::endl;
  else if (vals_per_tag == 9)
    s << "TENSORS " << name << ' ' << VtkUtil::vtkTypeNames[type] << std::endl;
  else
    s << "SCALARS " << name << ' ' << VtkUtil::vtkTypeNames[type] << ' '
    << vals_per_tag << std::endl << "LOOKUP_TABLE default" << std::endl;
  
    // Write the tag data
  switch (type) 
  {
    case MB_TYPE_OPAQUE:  return write_tag<unsigned char>(s, tag, entities, tagged, 0 );
    case MB_TYPE_INTEGER: return write_tag<          int>(s, tag, entities, tagged, 0 );
    case MB_TYPE_DOUBLE:  return write_tag<       double>(s, tag, entities, tagged, 0 );
    case MB_TYPE_BIT:     return write_bit_tag(s, tag, entities, tagged );
    default:              return MB_FAILURE;
  }
}

} // namespace moab
