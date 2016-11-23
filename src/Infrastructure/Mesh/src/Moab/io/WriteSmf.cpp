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

#include "WriteSmf.hpp"

#include <fstream>
#include <iostream>
#include <stdio.h>
#include <assert.h>
#include <vector>
#include <set>
#include <iterator>
#include <algorithm>

#include "moab/Interface.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"
#include "MBTagConventions.hpp"
#include "moab/WriteUtilIface.hpp"
#include "Internals.hpp"
#include "moab/FileOptions.hpp"

namespace moab {

const int DEFAULT_PRECISION = 10;
//const bool DEFAULT_STRICT = true;

WriterIface *WriteSmf::factory(Interface* iface)
{
  return new WriteSmf(iface);
}

WriteSmf::WriteSmf(Interface* impl)
  : mbImpl(impl), writeTool(0)
{
  assert(impl != NULL);
  impl->query_interface(writeTool);
}

WriteSmf::~WriteSmf()
{
  mbImpl->release_interface(writeTool);
}

ErrorCode WriteSmf::write_file(const char *file_name,
                               const bool overwrite,
                               const FileOptions& opts,
                               const EntityHandle *output_list,
                               const int num_sets,
                               const std::vector<std::string>& /* qa_list */,
                               const Tag* /* tag_list */,
                               int /* num_tags */,
                               int /* export_dimension */)
{
  ErrorCode rval;

  // Get precision for node coordinates
  int precision;
  if (MB_SUCCESS != opts.get_int_option("PRECISION", precision))
    precision = DEFAULT_PRECISION;

  // Honor overwrite flag
  if (!overwrite) {
    rval = writeTool->check_doesnt_exist(file_name);
    if (MB_SUCCESS != rval)
      return rval;
  }

  // Create file
  std::ofstream file(file_name);
  if (!file) {
    MB_SET_ERR(MB_FILE_WRITE_ERROR, "Could not open file: " << file_name);
  }
  file.precision(precision);

  // Get entities to write
  Range triangles;
  if (!output_list || !num_sets) {
    rval = mbImpl->get_entities_by_type(0, MBTRI, triangles, false);
    if (MB_SUCCESS != rval)
      return rval;

  // Somehow get all the nodes from this range, order them, uniquify, then use binary search
  }
  else {
    // Get all triangles from output sets
    for (int i = 0; i < num_sets; i++)
      rval = mbImpl->get_entities_by_type(output_list[i], MBTRI, triangles, false);
  }
  // Use an array with all the connectivities in the triangles; it will be converted later to ints
  int numTriangles = triangles.size();
  int array_alloc = 3 * numTriangles; // Allocated size of 'array'
  EntityHandle* array = new EntityHandle[array_alloc]; // ptr to working array of result handles
  // Fill up array with node handles; reorder and uniquify
  if (!array)
     return MB_MEMORY_ALLOCATION_FAILED;
  int fillA = 0;
  for (Range::const_iterator e = triangles.begin(); e != triangles.end(); ++e) {
      const EntityHandle* conn;
      int conn_len;
      rval = mbImpl->get_connectivity(*e, conn, conn_len);
      if (MB_SUCCESS != rval) {
        delete[] array;
        return rval;
      }
      if (3 != conn_len) {
        delete[] array;
        return MB_INVALID_SIZE;
      }

      for (int i = 0; i < conn_len; ++i)
        array[fillA++] = conn[i];
  }
  if (fillA != array_alloc) {
    delete[] array;
    return MB_INVALID_SIZE;
  }

  std::sort(array, array + array_alloc);
  int numNodes = std::unique(array, array + array_alloc) - array;

  file << "#$SMF 1.0\n";
  file << "#$vertices " << numNodes << std::endl;
  file << "#$faces " << numTriangles << std::endl;
  file << "# \n";
  file << "# output from MOAB \n";
  file << "# \n";

  // Output first the nodes
  // num nodes??
  // Write the nodes
  double coord[3];
  for (int i = 0; i < numNodes; i++) {
    EntityHandle node_handle = array[i];

    rval = mbImpl->get_coords(&node_handle, 1, coord);
    if (rval != MB_SUCCESS) {
      delete[] array;
      return rval;
    }

    file << "v " << coord[0] << " " << coord[1] << " " << coord[2] << std::endl;
  }
  // Write faces now
  // Leave a blank line for cosmetics
  file << " \n";
  for (Range::const_iterator e = triangles.begin(); e != triangles.end(); ++e) {
    const EntityHandle* conn;
    int conn_len;
    rval = mbImpl->get_connectivity(*e, conn, conn_len);
    if (MB_SUCCESS != rval) {
      delete[] array;
      return rval;
    }
    if (3 != conn_len) {
      delete[] array;
      return MB_INVALID_SIZE;
    }
    file << "f ";
    for (int i = 0; i < conn_len; ++i) {
      int indexInArray = std::lower_bound(array, array + numNodes, conn[i]) - array;
      file << indexInArray + 1 << " ";
    }
    file << std::endl;
  }

  file.close();
  delete[] array;
  return MB_SUCCESS;
}

} // namespace moab
