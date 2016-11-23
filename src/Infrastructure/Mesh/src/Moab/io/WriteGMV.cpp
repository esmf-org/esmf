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

#include "WriteGMV.hpp"

#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include "MBTagConventions.hpp"
#include "moab/WriteUtilIface.hpp"
#include <fstream>
#include <assert.h>

namespace moab {

const char *WriteGMV::gmvTypeNames[] = {
  "",
  "line",
  "tri",
  "quad",
  "",
  "tet",
  "pyramid",
  "prism",
  "",
  "hex",
  "",
  ""
};

WriterIface* WriteGMV::factory( Interface* iface )
  { return new WriteGMV( iface ); }

WriteGMV::WriteGMV(Interface *impl) 
    : mbImpl(impl)
{
  assert(impl != NULL);

  impl->query_interface( mWriteIface );

  // initialize in case tag_get_handle fails below
  mMaterialSetTag  = 0;
  mDirichletSetTag = 0;
  mNeumannSetTag   = 0;
  mHasMidNodesTag  = 0;
  mGeomDimensionTag= 0;
  mGlobalIdTag= 0;

  //! get and cache predefined tag handles
  // initialize in case tag_get_handle fails below
  //! get and cache predefined tag handles
  int zero = 0, negone = -1;
  impl->tag_get_handle(MATERIAL_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mMaterialSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);

  impl->tag_get_handle(DIRICHLET_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mDirichletSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);

  impl->tag_get_handle(NEUMANN_SET_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mNeumannSetTag, MB_TAG_SPARSE|MB_TAG_CREAT, &negone);

  impl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                       mGlobalIdTag, MB_TAG_SPARSE|MB_TAG_CREAT, &zero);

  int dum_val_array[] = {-1, -1, -1, -1};
  impl->tag_get_handle(HAS_MID_NODES_TAG_NAME, 4, MB_TYPE_INTEGER,
                       mHasMidNodesTag, MB_TAG_SPARSE|MB_TAG_CREAT, dum_val_array);
}

WriteGMV::~WriteGMV() 
{
  mbImpl->release_interface(mWriteIface);
}

ErrorCode WriteGMV::write_file(const char *file_name,
                                 const EntityHandle output_set,
                                 const int user_dimension,
                                 const bool mesh,
                                 const bool poly_mesh) 
{
    // general function for writing a mesh
  
  ErrorCode result = MB_SUCCESS;

    // initialize file

  if (mesh) {
    result = local_write_mesh(file_name, output_set, user_dimension, true, false);
    if (MB_SUCCESS != result) return result;
  }
  
  if (poly_mesh) {
    result = local_write_mesh(file_name, output_set, user_dimension, false, true);
    if (MB_SUCCESS != result) return result;
  }
  
  return result;
}

ErrorCode WriteGMV::write_file( const char* filename,
                                  const bool ,
                                  const FileOptions& /*opts*/,
                                  const EntityHandle* output_sets,
                                  const int num_output_sets,
                                  const std::vector<std::string>& ,
                                  const Tag*,
                                  int,
                                  int dimension )
{
  EntityHandle output_set = 0;
  if (output_sets && num_output_sets > 0)
  {
    if (num_output_sets > 1)
      return MB_FAILURE;
    output_set = output_sets[0];
  }
  
  if (dimension == 0)
  {
    mbImpl->get_dimension( dimension );
  }
  
  return write_file( filename, output_set, dimension, true, true );
}
  

ErrorCode WriteGMV::local_write_mesh(const char *file_name,
                                       const EntityHandle output_set,
                                       const int user_dimension,
                                       const bool mesh,
                                       const bool poly_mesh)
{
  std::ofstream ofile;
  ErrorCode result;

  if (mesh) {
      // need to insert ".gmv"
    std::string tmp_name(file_name);
    tmp_name += ".gmv";
    ofile.open(tmp_name.c_str());
  }
  else if (poly_mesh) {
      // need to insert ".poly.gmv"
    std::string tmp_name(file_name);
    tmp_name += ".poly.gmv";
    ofile.open(tmp_name.c_str());
  }

  ofile << "gmvinput ascii" << std::endl;
  
    // get elements to be output
  Range dum_range, elements, all_verts;
  EntityType otype;
  if (poly_mesh) {
    result = mbImpl->get_entities_by_type(output_set, MBPOLYGON, elements, true);
    if (MB_SUCCESS != result) return result;
  }
  else {
    for (otype = CN::TypeDimensionMap[user_dimension].first;
         otype <= CN::TypeDimensionMap[user_dimension].second; otype++) {
      if (otype == MBPOLYGON || otype == MBPOLYHEDRON) continue;
      dum_range.clear();
      result = mbImpl->get_entities_by_type(output_set, otype, dum_range, true);
      if (MB_SUCCESS != result) return result;

      std::copy(dum_range.begin(), dum_range.end(), range_inserter(elements));
    }
  }
  
    // gather the vertices in these elements
  result = mbImpl->get_adjacencies(elements, 0, false, all_verts, Interface::UNION);
  if (MB_SUCCESS != result) return result;
  
  int num_verts = all_verts.size();
  
    // allocate coordinate arrays and put pointers to them in a list
  double *xcoord = new double[num_verts];
  double *ycoord = new double[num_verts];
  double *zcoord = new double[num_verts];
  std::vector<double*> coord_arrays;
  coord_arrays.push_back(xcoord);
  coord_arrays.push_back(ycoord);
  coord_arrays.push_back(zcoord);
  
    // fill them in, writing id tags at the same time
  result = mWriteIface->get_node_coords(3, num_verts, all_verts, mGlobalIdTag, 1, coord_arrays);
  if (MB_SUCCESS != result) return result;

  int i, j;
  
    //========================================
    // WRITE COORDINATE DATA TO FILE HERE

  ofile << "nodev " << num_verts << std::endl;
  for (i = 0; i < num_verts; i++) 
    ofile << xcoord[i] << " " << ycoord[i] << " " << zcoord[i] << std::endl;
  

    //========================================

  delete [] xcoord;
  delete [] ycoord;
  delete [] zcoord;

    // iterate over types in selected dimension

  std::vector<int> connect;
  std::vector<EntityHandle> connecth;

  if (mesh) {
    Range sub_range;
    
    ofile << "cells " << elements.size() << std::endl;
  
    for (otype = CN::TypeDimensionMap[user_dimension].first;
         otype <= CN::TypeDimensionMap[user_dimension].second; otype++) {

      if (otype == MBPOLYGON || otype == MBPOLYHEDRON) continue;
      
        // get the first element of this type in the range, and one past the last
      Range::iterator lower =
        Range::lower_bound(elements.begin(),
                             elements.end(),
                             CREATE_HANDLE(otype, MB_START_ID, i));
      Range::iterator upper =
        Range::lower_bound(elements.begin(),
                             elements.end(),
                             CREATE_HANDLE(otype+1, MB_START_ID, i));
      
      if (lower == upper) continue;
    
        // copy these elements into a subrange
      sub_range.clear();
      std::copy(lower, upper, range_inserter(sub_range));

        // make sure the connectivity array is big enough
      int verts_per = CN::VerticesPerEntity(otype);
      if (connect.size() < verts_per*sub_range.size())
        connect.resize(verts_per*sub_range.size());
    
        // get the connectivity
      result = mWriteIface->get_element_connect(sub_range.size(),
                                              verts_per,
                                              mGlobalIdTag, sub_range,
                                              mGlobalIdTag, 1, &connect[0]);
      if (MB_SUCCESS != result) return result;

        //========================================
        // WRITE CONNECTIVITY DATA TO FILE HERE

      for (i = 0; i < (int) sub_range.size(); i++) {
        ofile << gmvTypeNames[otype] << " " << verts_per << std::endl;
        for (j = i*verts_per; j < (int) (i+1)*verts_per; j++)
          ofile << connect[j] << " ";
        ofile << std::endl;
      }

        //========================================
    }
  }
  
  else if (poly_mesh) {
  
      // write polygons/hedra, if any
    Range polygons, polyhedra;
    result = mbImpl->get_entities_by_type(output_set, MBPOLYGON, polygons, true);
    if (MB_SUCCESS != result) return result;
  
    result = mbImpl->get_entities_by_type(output_set, MBPOLYHEDRON, polyhedra, true);
    if (MB_SUCCESS != result) return result;

    if (polygons.size() == 0) return result;
  
      // mark polyhedra with global ids
    result = mWriteIface->assign_ids(polyhedra, mGlobalIdTag, 1);
    if (MB_SUCCESS != result) return result;

    ofile << "faces " << polygons.size() << " " << polyhedra.size() << std::endl;

    for (Range::iterator rit = polygons.begin(); rit != polygons.end(); ++rit) {
        // get the vertices
      connecth.clear();
      result = mbImpl->get_connectivity(&(*rit), 1, connecth, true);
      if (MB_SUCCESS != result) return result;

      if (0 == connecth.size()) continue;
    
        // get the polyhedra, if any
      if (user_dimension == 3) {
        polyhedra.clear();
        result = mbImpl->get_adjacencies(Range(*rit, *rit), 3, false, polyhedra);
        if (MB_SUCCESS != result) return result;
    
          // put them in the connect array
        connecth.push_back((polyhedra.size() > 0 ? *polyhedra.begin() : 0));
        connecth.push_back((polyhedra.size() > 1 ? *polyhedra.rbegin() : 0));
      }
    
        // replace handles with ids
      connect.resize(connecth.size()+2);

        // pre-set polyhedra ids in case there aren't any
      connect[connecth.size()] = 0;
      connect[connecth.size()+1] = 0;
      result = mbImpl->tag_get_data(mGlobalIdTag, &connecth[0], 
                                    connecth.size()-2+polyhedra.size(),
                                    &connect[0]);
      if (MB_SUCCESS != result) return result;
    
        // write the data
      ofile << connecth.size()-2;
    
      for (i = 0; i < (int)connecth.size(); i++)
        ofile << " " << connect[i];

      ofile << std::endl;
    }
  }

  ofile << std::endl << "endgmv" << std::endl;
  
  ofile.close();
  
  return MB_SUCCESS;
}

} // namespace moab

