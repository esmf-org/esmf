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

/**
 * \class ReadSms
 * \brief Sms (http://www.geuz.org/sms) file reader
 * \author Jason Kraftcheck
 */

#include "ReadSms.hpp"
#include "FileTokenizer.hpp" // For file tokenizer
#include "Internals.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/Range.hpp"
#include "MBTagConventions.hpp"
#include "MBParallelConventions.h"
#include "moab/CN.hpp"

#include <errno.h>
#include <string.h>
#include <map>
#include <set>
#include <iostream>

#define CHECK(a) if (MB_SUCCESS != result) { \
  std::cerr << a << std::endl; \
  return result; \
}

#define CHECKN(a) if (n != (a)) return MB_FILE_WRITE_ERROR

namespace moab {

ReaderIface* ReadSms::factory(Interface* iface)
{
  return new ReadSms(iface);
}

ReadSms::ReadSms(Interface* impl)
  : mdbImpl(impl), globalId(0), paramCoords(0), geomDimension(0), setId(0)
{
  mdbImpl->query_interface(readMeshIface);
}

ReadSms::~ReadSms()
{
  if (readMeshIface) {
    mdbImpl->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

ErrorCode ReadSms::read_tag_values(const char* /* file_name */,
                                   const char* /* tag_name */,
                                   const FileOptions& /* opts */,
                                   std::vector<int>& /* tag_values_out */,
                                   const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode ReadSms::load_file(const char* filename,
                             const EntityHandle* /* file_set */,
                             const FileOptions& /* opts */,
                             const ReaderIface::SubsetList* subset_list,
                             const Tag* file_id_tag)
{
  if (subset_list) {
    MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "Reading subset of files not supported for Sms");
  }

  setId = 1;

  // Open file
  FILE* file_ptr = fopen(filename, "r");
  if (!file_ptr) {
    MB_SET_ERR(MB_FILE_DOES_NOT_EXIST, filename << ": " << strerror(errno));
  }

  const ErrorCode result = load_file_impl(file_ptr, file_id_tag);
  fclose(file_ptr);

  return result;
}

ErrorCode ReadSms::load_file_impl(FILE* file_ptr, const Tag* file_id_tag)
{
  bool warned = false;
  double dum_params[] = {0.0, 0.0, 0.0};

  int zero = 0;
  ErrorCode result = mdbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                             globalId, MB_TAG_DENSE | MB_TAG_CREAT, &zero);
  CHECK("Failed to create gid tag.");

  result = mdbImpl->tag_get_handle("PARAMETER_COORDS", 3, MB_TYPE_DOUBLE,
                                   paramCoords, MB_TAG_DENSE | MB_TAG_CREAT);
  CHECK("Failed to create param coords tag.");

  int negone = -1;
  result = mdbImpl->tag_get_handle(GEOM_DIMENSION_TAG_NAME, 1, MB_TYPE_INTEGER,
                                   geomDimension, MB_TAG_SPARSE | MB_TAG_CREAT, &negone);
  CHECK("Failed to create geom dim tag.");

  int n;
  char line[256], all_line[1024];
  int file_type;

  if (fgets(all_line, sizeof(all_line), file_ptr) == NULL) { return MB_FAILURE;}
  if (sscanf(all_line, "%s %d", line, &file_type) != 2) { return MB_FAILURE;}

  if (3 == file_type) {
    result = read_parallel_info(file_ptr);
    CHECK("Failed to read parallel info.");
  }

  int nregions, nfaces, nedges, nvertices, npoints;
  n = fscanf(file_ptr, "%d %d %d %d %d", &nregions, &nfaces, &nedges,
         &nvertices, &npoints);CHECKN(5);
  if (nregions < 0 || nfaces < 0 || nedges < 0 || nvertices < 0 || npoints < 0)
    return MB_FILE_WRITE_ERROR;

  // Create the vertices
  std::vector<double*> coord_arrays;
  EntityHandle vstart = 0;
  result = readMeshIface->get_node_coords(3, nvertices, MB_START_ID,
                                          vstart, coord_arrays);
  CHECK("Failed to get node arrays.");

  if (file_id_tag) {
    result = add_entities(vstart, nvertices, file_id_tag);MB_CHK_ERR(result);
  }

  EntityHandle this_gent, new_handle;
  std::vector<EntityHandle> gentities[4];
  int gent_id, dum_int;
  int gent_type, num_connections;

  for (int i = 0; i < nvertices; i++) {
    n = fscanf(file_ptr, "%d", &gent_id);
    CHECKN(1);
    if (!gent_id) continue;

    n = fscanf(file_ptr, "%d %d %lf %lf %lf", &gent_type, &num_connections,
               coord_arrays[0] + i, coord_arrays[1] + i, coord_arrays[2] + i);
    CHECKN(5);

    result = get_set(gentities, gent_type, gent_id, geomDimension, this_gent, file_id_tag);MB_CHK_ERR(result);

    new_handle = vstart + i;
    result = mdbImpl->add_entities(this_gent, &new_handle, 1);
    CHECK("Adding vertex to geom set failed.");

    switch (gent_type) {
      case 1:
        n = fscanf(file_ptr, "%le", dum_params);
        CHECKN(1);
        result = mdbImpl->tag_set_data(paramCoords, &new_handle, 1, dum_params);
        CHECK("Failed to set param coords tag for vertex.");
        break;
      case 2:
        n = fscanf(file_ptr, "%le %le %d", dum_params, dum_params + 1, &dum_int);
        CHECKN(3);
        dum_params[2] = dum_int;
        result = mdbImpl->tag_set_data(paramCoords, &new_handle, 1, dum_params);
        CHECK("Failed to set param coords tag for vertex.");
        break;
      default:
        break;
    }
  } // End of reading vertices

// *******************************
//  Read Edges
// *******************************

  int vert1, vert2, num_pts;
  std::vector<EntityHandle> everts(2);
  EntityHandle estart, *connect;
  result = readMeshIface->get_element_connect(nedges, 2, MBEDGE, 1, estart, connect);
  CHECK("Failed to create array of edges.");

  if (file_id_tag) {
    result = add_entities(estart, nedges, file_id_tag);
    if (MB_SUCCESS != result)
      return result;
  }

  for (int i = 0; i < nedges; i++) {
    n = fscanf(file_ptr, "%d",&gent_id);
    CHECKN(1);
    if (!gent_id) continue;

    n = fscanf(file_ptr, "%d %d %d %d %d", &gent_type, &vert1, &vert2,
           &num_connections, &num_pts);
    CHECKN(5);
    if (vert1 < 1 || vert1 > nvertices)
      return MB_FILE_WRITE_ERROR;
    if (vert2 < 1 || vert2 > nvertices)
      return MB_FILE_WRITE_ERROR;

    connect[0] = vstart + vert1 - 1;
    connect[1] = vstart + vert2 - 1;
    if (num_pts > 1 && !warned) {
      std::cout << "Warning: num_points > 1 not supported; choosing last one." << std::endl;
      warned = true;
    }

    result = get_set(gentities, gent_type, gent_id, geomDimension, this_gent, file_id_tag);
    CHECK("Problem getting geom set for edge.");

    new_handle = estart + i;
    result = mdbImpl->add_entities(this_gent, &new_handle, 1);
    CHECK("Failed to add edge to geom set.");

    connect += 2;

    for (int j = 0; j < num_pts; j++) {
      switch (gent_type) {
        case 1:
            n = fscanf(file_ptr, "%le", dum_params);
            CHECKN(1);
            result = mdbImpl->tag_set_data(paramCoords, &new_handle, 1, dum_params);
            CHECK("Failed to set param coords tag for edge.");
            break;
        case 2:
            n = fscanf(file_ptr, "%le %le %d", dum_params, dum_params + 1, &dum_int);
            CHECKN(3);
            dum_params[2] = dum_int;
            result = mdbImpl->tag_set_data(paramCoords, &new_handle, 1, dum_params);
            CHECK("Failed to set param coords tag for edge.");
            break;
        default:
            break;
      }
    }
  } // End of reading edges

// *******************************
//  Read Faces
// *******************************
  std::vector<EntityHandle> bound_ents, bound_verts, new_faces;
  int bound_id;
  Range shverts;
  new_faces.resize(nfaces);
  int num_bounding;

  for (int i = 0; i < nfaces; i++) {
    n = fscanf(file_ptr, "%d", &gent_id);
    CHECKN(1);
    if (!gent_id)
      continue;

    n = fscanf(file_ptr, "%d %d", &gent_type, &num_bounding);
    CHECKN(2);

    result = get_set(gentities, gent_type, gent_id, geomDimension, this_gent, file_id_tag);
    CHECK("Problem getting geom set for face.");

    bound_ents.resize(num_bounding + 1);
    bound_verts.resize(num_bounding);
    for (int j = 0; j < num_bounding; j++) {
      n = fscanf(file_ptr, "%d ", &bound_id);
      CHECKN(1);
      if (0 > bound_id) bound_id = abs(bound_id);
      assert(0 < bound_id && bound_id <= nedges);
      if (bound_id < 1 || bound_id > nedges)
        return MB_FILE_WRITE_ERROR;
      bound_ents[j] = estart + abs(bound_id) - 1;
    }

    // Convert edge-based model to vertex-based one
    for (int j = 0; j < num_bounding; j++) {
      if (j == num_bounding - 1) bound_ents[j + 1] = bound_ents[0];
      result = mdbImpl->get_adjacencies(&bound_ents[j], 2, 0, false, shverts);
      CHECK("Failed to get vertices bounding edge.");
      assert(shverts.size() == 1);
      bound_verts[j] = *shverts.begin();
      shverts.clear();
    }

    result = mdbImpl->create_element((EntityType)(MBTRI + num_bounding - 3),
                                     &bound_verts[0], bound_verts.size(),
                                     new_faces[i]);
    CHECK("Failed to create edge.");

    result = mdbImpl->add_entities(this_gent, &new_faces[i], 1);
    CHECK("Failed to add edge to geom set.");

    int num_read = fscanf(file_ptr, "%d", &num_pts);
    if (!num_pts || !num_read) continue;

    for (int j = 0; j < num_pts; j++) {
      switch (gent_type) {
        case 1:
          n = fscanf(file_ptr, "%le", dum_params); CHECKN(1);
          result = mdbImpl->tag_set_data(paramCoords, &new_faces[i], 1, dum_params);
          CHECK("Failed to set param coords tag for face.");
          break;
        case 2:
          n = fscanf(file_ptr, "%le %le %d", dum_params, dum_params + 1, &dum_int);
          CHECKN(3);
          dum_params[2] = dum_int;
          result = mdbImpl->tag_set_data(paramCoords, &new_faces[i], 1, dum_params);
          CHECK("Failed to set param coords tag for face.");
          break;
        default:
          break;
      }
    }
  } // End of reading faces

  if (file_id_tag) {
    result = readMeshIface->assign_ids(*file_id_tag, &new_faces[0], new_faces.size(), 1);
    if (MB_SUCCESS != result)
      return result;
  }

// *******************************
//  Read Regions
// *******************************
  int sense[MAX_SUB_ENTITIES];
  bound_verts.resize(MAX_SUB_ENTITIES);

  std::vector<EntityHandle> regions;
  if (file_id_tag)
    regions.resize(nregions);
  for (int i = 0; i < nregions; i++) {
    n = fscanf(file_ptr, "%d", &gent_id); CHECKN(1);
    if (!gent_id) continue;
    result = get_set(gentities, 3, gent_id, geomDimension, this_gent, file_id_tag);
    CHECK("Couldn't get geom set for region.");
    n = fscanf(file_ptr, "%d", &num_bounding); CHECKN(1);
    bound_ents.resize(num_bounding);
    for (int j = 0; j < num_bounding; j++) {
      n = fscanf(file_ptr, "%d ", &bound_id); CHECKN(1);
      assert(abs(bound_id) < (int)new_faces.size() + 1 && bound_id);
      if (!bound_id || abs(bound_id) > nfaces)
        return MB_FILE_WRITE_ERROR;
      sense[j] = (bound_id < 0) ? -1 : 1;
      bound_ents[j] = new_faces[abs(bound_id) - 1];
    }

    EntityType etype;
    result = readMeshIface->get_ordered_vertices(&bound_ents[0], sense,
                                                 num_bounding,
                                                 3, &bound_verts[0], etype);
    CHECK("Failed in get_ordered_vertices.");

    // Make the element
    result = mdbImpl->create_element(etype, &bound_verts[0],
                                     CN::VerticesPerEntity(etype), new_handle);
    CHECK("Failed to create region.");

    result = mdbImpl->add_entities(this_gent, &new_handle, 1);
    CHECK("Failed to add region to geom set.");

    if (file_id_tag)
      regions[i] = new_handle;

    n = fscanf(file_ptr, "%d ", &dum_int); CHECKN(1);
  } // End of reading regions

  if (file_id_tag) {
    result = readMeshIface->assign_ids(*file_id_tag, &regions[0], regions.size(), 1);
    if (MB_SUCCESS != result)
      return result;
  }

  return MB_SUCCESS;
}

ErrorCode ReadSms::get_set(std::vector<EntityHandle> *sets,
                           int set_dim, int set_id,
                           Tag dim_tag,
                           EntityHandle &this_set,
                           const Tag* file_id_tag)
{
  ErrorCode result = MB_SUCCESS;

  if (set_dim < 0 || set_dim > 3)
    return MB_FILE_WRITE_ERROR;

  if ((int)sets[set_dim].size() <= set_id ||
      !sets[set_dim][set_id]) {
    if ((int)sets[set_dim].size() <= set_id)
      sets[set_dim].resize(set_id + 1, 0);

    if (!sets[set_dim][set_id]) {
      result = mdbImpl->create_meshset(MESHSET_SET,
                                       sets[set_dim][set_id]);
      if (MB_SUCCESS != result)
        return result;
      result = mdbImpl->tag_set_data(globalId,
                                     &sets[set_dim][set_id], 1,
                                     &set_id);
      if (MB_SUCCESS != result)
        return result;
      result = mdbImpl->tag_set_data(dim_tag,
                                     &sets[set_dim][set_id], 1,
                                     &set_dim);
      if (MB_SUCCESS != result)
        return result;

      if (file_id_tag) {
        result = mdbImpl->tag_set_data(*file_id_tag,
                                       &sets[set_dim][set_id], 1,
                                       &setId);
        ++setId;
      }
    }
  }

  this_set = sets[set_dim][set_id];

  return result;
}

ErrorCode ReadSms::read_parallel_info(FILE *file_ptr)
{
  //ErrorCode result;

  // Read partition info
  int nparts, part_id, num_ifaces, num_corner_ents;
  int num_read = fscanf(file_ptr, "%d %d %d %d", &nparts, &part_id, &num_ifaces, &num_corner_ents);
  if (!num_read)
    return MB_FAILURE;

  // Read interfaces
  int iface_id, iface_dim, iface_own, num_iface_corners;
  //EntityHandle this_iface;
  std::vector<int> *iface_corners = NULL;
  for (int i = 0; i < num_ifaces; i++) {
    num_read = fscanf(file_ptr, "%d %d %d %d", &iface_id, &iface_dim, &iface_own,
                      &num_iface_corners);
    if (!num_read)
      return MB_FAILURE;

    //result = get_set(sets, iface_dim, iface_id, dim_tag, iface_own, this_iface);
    //CHECK("Failed to make iface set.");

    // Read the corner ids and store them on the set for now
    iface_corners = new std::vector<int>(num_iface_corners);
    for (int j = 0; j < num_iface_corners; j++) {
      num_read = fscanf(file_ptr, "%d", &(*iface_corners)[j]);
      if (!num_read) {
        delete iface_corners;
        return MB_FAILURE;
      }
    }

    //result = tag_set_data(ifaceCornerTag, &this_iface, 1,
                          //&iface_corners);
    //CHECK("Failed to set iface corner tag.");

    delete iface_corners;
    iface_corners = NULL;
  }

  // Interface data has been read
  return MB_SUCCESS;
}

ErrorCode ReadSms::add_entities(EntityHandle start,
                                EntityHandle count,
                                const Tag* file_id_tag)
{
  if (!count || !file_id_tag)
    return MB_FAILURE;

  Range range;
  range.insert(start, start + count - 1);
  return readMeshIface->assign_ids(*file_id_tag, range, 1);
}

} // namespace moab
