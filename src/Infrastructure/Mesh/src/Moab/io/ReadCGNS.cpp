/**
 * \class ReadCGNS
 * \brief Template for writing a new reader in MOAB
 *
 */

#include "ReadCGNS.hpp"
#include "Internals.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/Range.hpp"
#include "moab/FileOptions.hpp"
#include "MBTagConventions.hpp"
#include "MBParallelConventions.h"
#include "moab/CN.hpp"

#include <cstdio>
#include <assert.h>
#include <errno.h>
#include <map>
#include <set>

#include <iostream>
#include <cmath>

namespace moab
{

ReaderIface* ReadCGNS::factory(Interface* iface)
{
  return new ReadCGNS(iface);
}

ReadCGNS::ReadCGNS(Interface* impl)
  : fileName(NULL), mesh_dim(0), mbImpl(impl), globalId(0), boundary(0)
{
  mbImpl->query_interface(readMeshIface);
}

ReadCGNS::~ReadCGNS()
{
  if (readMeshIface) {
    mbImpl->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}

ErrorCode ReadCGNS::read_tag_values(const char* /* file_name */,
                                    const char* /* tag_name */,
                                    const FileOptions& /* opts */,
                                    std::vector<int>& /* tag_values_out */,
                                    const SubsetList* /* subset_list */)
{
  return MB_NOT_IMPLEMENTED;
}

ErrorCode ReadCGNS::load_file(const char* filename,
                              const EntityHandle * /*file_set*/,
                              const FileOptions& opts,
                              const ReaderIface::SubsetList* subset_list,
                              const Tag* file_id_tag)
{
  int num_material_sets = 0;
  const int* material_set_list = 0;

  if (subset_list) {
    if (subset_list->tag_list_length > 1 &&
        !strcmp(subset_list->tag_list[0].tag_name, MATERIAL_SET_TAG_NAME)) {
      MB_SET_ERR(MB_UNSUPPORTED_OPERATION, "CGNS supports subset read only by material ID");
    }
    material_set_list = subset_list->tag_list[0].tag_values;
    num_material_sets = subset_list->tag_list[0].num_tag_values;
  }

  ErrorCode result;

  geomSets.clear();
  result = mbImpl->tag_get_handle(GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER,
                                  globalId, MB_TAG_DENSE | MB_TAG_CREAT, 0);
  if (MB_SUCCESS != result)
    return result;

  // Create set for more convenient check for material set ids
  std::set<int> blocks;
  for (const int* mat_set_end = material_set_list + num_material_sets;
       material_set_list != mat_set_end; ++material_set_list)
    blocks.insert(*material_set_list);

  // Map of ID->handle for nodes
  std::map<long, EntityHandle> node_id_map;

  // Save filename to member variable so we don't need to pass as an argument
  // to called functions
  fileName = filename;

  // Process options; see src/FileOptions.hpp for API for FileOptions class, and doc/metadata_info.doc for
  // a description of various options used by some of the readers in MOAB
  result = process_options(opts);MB_CHK_SET_ERR(result, fileName << ": problem reading options");

  // Open file
  int filePtr = 0;

  cg_open(filename, CG_MODE_READ, &filePtr);

  if (filePtr <= 0) {
    MB_SET_ERR(MB_FILE_DOES_NOT_EXIST, fileName << ": fopen returned error");
  }

  // Read number of verts, elements, sets
  long num_verts = 0, num_elems = 0, num_sets = 0;
  int num_bases = 0, num_zones = 0, num_sections = 0;

  char zoneName[128];
  cgsize_t size[3];

  mesh_dim = 3; // Default to 3D

  // Read number of bases;
  cg_nbases(filePtr, &num_bases);

  if (num_bases > 1) {
    MB_SET_ERR(MB_NOT_IMPLEMENTED, fileName << ": support for number of bases > 1 not implemented");
  }

  for (int indexBase = 1; indexBase <= num_bases; ++indexBase) {
    // Get the number of zones/blocks in current base.
    cg_nzones(filePtr, indexBase, &num_zones);

    if (num_zones > 1) {
      MB_SET_ERR(MB_NOT_IMPLEMENTED, fileName << ": support for number of zones > 1 not implemented");
    }

    for (int indexZone = 1; indexZone <= num_zones; ++indexZone) {
      // Get zone name and size.
      cg_zone_read(filePtr, indexBase, indexZone, zoneName, size);

      // Read number of sections/Parts in current zone.
      cg_nsections(filePtr, indexBase, indexZone, &num_sections);

      num_verts = size[0];
      num_elems = size[1];
      num_sets = num_sections;

      std::cout << "\nnumber of nodes = " << num_verts;
      std::cout << "\nnumber of elems = " << num_elems;
      std::cout << "\nnumber of parts = " << num_sets << std::endl;

      // //////////////////////////////////
      // Read Nodes

      // Allocate nodes; these are allocated in one shot, get contiguous handles starting with start_handle,
      // and the reader is passed back double*'s pointing to MOAB's native storage for vertex coordinates
      // for those verts
      std::vector<double*> coord_arrays;
      EntityHandle handle = 0;
      result = readMeshIface->get_node_coords(3, num_verts, MB_START_ID, handle,
                                              coord_arrays);MB_CHK_SET_ERR(result, fileName << ": Trouble reading vertices");

      // Fill in vertex coordinate arrays
      cgsize_t beginPos = 1, endPos = num_verts;

      // Read nodes coordinates.
      cg_coord_read(filePtr, indexBase, indexZone, "CoordinateX",
                    RealDouble, &beginPos, &endPos, coord_arrays[0]);
      cg_coord_read(filePtr, indexBase, indexZone, "CoordinateY",
                    RealDouble, &beginPos, &endPos, coord_arrays[1]);
      cg_coord_read(filePtr, indexBase, indexZone, "CoordinateZ",
                    RealDouble, &beginPos, &endPos, coord_arrays[2]);

      // CGNS seems to always include the Z component, even if the mesh is 2D.
      // Check if Z is zero and determine mesh dimension.
      // Also create the node_id_map data.
      double sumZcoord = 0.0;
      double eps = 1.0e-12;
      for (long i = 0; i < num_verts; ++i, ++handle) {
        int index = i + 1;

        node_id_map.insert(std::pair<long, EntityHandle>(index, handle)).second;

        sumZcoord += *(coord_arrays[2] + i);
      }
      if (std::abs(sumZcoord) <= eps) mesh_dim = 2;

      // Create reverse map from handle to id
      std::vector<int> ids(num_verts);
      std::vector<int>::iterator id_iter = ids.begin();
      std::vector<EntityHandle> handles(num_verts);
      std::vector<EntityHandle>::iterator h_iter = handles.begin();
      for (std::map<long, EntityHandle>::iterator i = node_id_map.begin();
          i != node_id_map.end(); ++i, ++id_iter, ++h_iter) {
        *id_iter = i->first;
        * h_iter = i->second;
      }
      // Store IDs in tags
      result = mbImpl->tag_set_data(globalId, &handles[0], num_verts, &ids[0]);
      if (MB_SUCCESS != result)
        return result;
      if (file_id_tag) {
        result = mbImpl->tag_set_data(*file_id_tag, &handles[0], num_verts, &ids[0]);
        if (MB_SUCCESS != result)
          return result;
      }
      ids.clear();
      handles.clear();

      // //////////////////////////////////
      // Read elements data

      EntityType ent_type;

      long section_offset = 0;

      // Define which mesh parts are volume families.
      // Mesh parts with volumeID[X] = 0 are boundary parts.
      std::vector<int> volumeID(num_sections, 0);

      for (int section = 0; section < num_sections; ++section) {
        ElementType_t elemsType;
        int iparent_flag, nbndry;
        char sectionName[128];
        int verts_per_elem;

        int cgSection = section + 1;

        cg_section_read(filePtr, indexBase, indexZone, cgSection, sectionName,
                        &elemsType, &beginPos, &endPos, &nbndry, &iparent_flag);

        size_t section_size = endPos - beginPos + 1;

        // Read element description in current section

        switch (elemsType) {
          case BAR_2:
            ent_type = MBEDGE;
            verts_per_elem = 2;
            break;
          case TRI_3:
            ent_type = MBTRI;
            verts_per_elem = 3;
            if (mesh_dim == 2)
              volumeID[section] = 1;
            break;
          case QUAD_4:
            ent_type = MBQUAD;
            verts_per_elem = 4;
            if (mesh_dim == 2)
              volumeID[section] = 1;
            break;
          case TETRA_4:
            ent_type = MBTET;
            verts_per_elem = 4;
            if (mesh_dim == 3)
              volumeID[section] = 1;
              break;
          case PYRA_5:
            ent_type = MBPYRAMID;
            verts_per_elem = 5;
            if (mesh_dim == 3) volumeID[section] = 1;
              break;
          case PENTA_6:
            ent_type = MBPRISM;
            verts_per_elem = 6;
            if (mesh_dim == 3) volumeID[section] = 1;
              break;
          case HEXA_8:
            ent_type = MBHEX;
            verts_per_elem = 8;
            if (mesh_dim == 3) volumeID[section] = 1;
            break;
          case MIXED:
            ent_type = MBMAXTYPE;
            verts_per_elem = 0;
            break;
          default:
            MB_SET_ERR(MB_INDEX_OUT_OF_RANGE, fileName << ": Trouble determining element type");
        }

        if (elemsType == TETRA_4 || elemsType == PYRA_5 || elemsType == PENTA_6 || elemsType == HEXA_8 ||
            elemsType == TRI_3   || elemsType == QUAD_4 || ((elemsType == BAR_2) && mesh_dim == 2)) {
          // Read connectivity into conn_array directly

          cgsize_t iparentdata;
          cgsize_t connDataSize;

          // Get number of entries on the connectivity list for this section
          cg_ElementDataSize(filePtr, indexBase, indexZone, cgSection, &connDataSize);

          // Need a temporary vector to later cast to conn_array.
          std::vector<cgsize_t> elemNodes(connDataSize);

          cg_elements_read(filePtr, indexBase, indexZone, cgSection, &elemNodes[0], &iparentdata);

          // //////////////////////////////////
          // Create elements, sets and tags

          create_elements(sectionName, file_id_tag,
                          ent_type, verts_per_elem, section_offset, section_size , elemNodes);
        } // Homogeneous mesh type
        else if (elemsType == MIXED) {
          // We must first sort all elements connectivities into continuous vectors

          cgsize_t connDataSize;
          cgsize_t iparentdata;

          cg_ElementDataSize(filePtr, indexBase, indexZone, cgSection, &connDataSize);

          std::vector< cgsize_t > elemNodes(connDataSize);

          cg_elements_read(filePtr, indexBase, indexZone, cgSection, &elemNodes[0], &iparentdata);

          std::vector<cgsize_t> elemsConn_EDGE;
          std::vector<cgsize_t> elemsConn_TRI, elemsConn_QUAD;
          std::vector<cgsize_t> elemsConn_TET, elemsConn_PYRA, elemsConn_PRISM, elemsConn_HEX;
          cgsize_t count_EDGE, count_TRI, count_QUAD;
          cgsize_t count_TET, count_PYRA, count_PRISM, count_HEX;

          // First, get elements count for current section

          count_EDGE = count_TRI = count_QUAD = 0;
          count_TET = count_PYRA = count_PRISM = count_HEX = 0;

          int connIndex = 0;
          for (int i = beginPos; i <= endPos; i++) {
            elemsType = ElementType_t(elemNodes[connIndex]);

            // Get current cell node count.
            cg_npe(elemsType, &verts_per_elem);

            switch (elemsType) {
              case BAR_2:
                count_EDGE += 1;
                break;
              case TRI_3:
                count_TRI += 1;
                break;
              case QUAD_4:
                count_QUAD += 1;
                break;
              case TETRA_4:
                count_TET += 1;
                break;
              case PYRA_5:
                count_PYRA += 1;
                break;
              case PENTA_6:
                count_PRISM += 1;
                break;
              case HEXA_8:
                count_HEX += 1;
                break;
              default:
                MB_SET_ERR(MB_INDEX_OUT_OF_RANGE, fileName << ": Trouble determining element type");
            }

            connIndex += (verts_per_elem + 1); // Add one to skip next element descriptor
          }

          if (count_EDGE  > 0) elemsConn_EDGE.resize(count_EDGE * 2);
          if (count_TRI   > 0) elemsConn_TRI.resize(count_TRI * 3);
          if (count_QUAD  > 0) elemsConn_QUAD.resize(count_QUAD * 4);
          if (count_TET   > 0) elemsConn_TET.resize(count_TET * 4);
          if (count_PYRA  > 0) elemsConn_PYRA.resize(count_PYRA * 5);
          if (count_PRISM > 0) elemsConn_PRISM.resize(count_PRISM * 6);
          if (count_HEX   > 0) elemsConn_HEX.resize(count_HEX * 8);

          // Grab mixed section elements connectivity

          int idx_edge, idx_tri, idx_quad;
          int idx_tet, idx_pyra, idx_prism, idx_hex;
          idx_edge = idx_tri = idx_quad = 0;
          idx_tet = idx_pyra = idx_prism = idx_hex = 0;

          connIndex = 0;
          for (int i = beginPos; i <= endPos; i++) {
            elemsType = ElementType_t(elemNodes[connIndex]);

            // Get current cell node count.
            cg_npe(elemsType, &verts_per_elem);

            switch (elemsType) {
              case BAR_2:
                for (int j = 0; j < 2; ++j)
                  elemsConn_EDGE[idx_edge + j] = elemNodes[connIndex + j + 1];
                idx_edge += 2;
                break;
              case TRI_3:
                for (int j = 0; j < 3; ++j)
                  elemsConn_TRI[idx_tri + j] = elemNodes[connIndex + j + 1];
                idx_tri += 3;
                break;
              case QUAD_4:
                for (int j = 0; j < 4; ++j)
                  elemsConn_QUAD[idx_quad + j] = elemNodes[connIndex + j + 1];
                idx_quad += 4;
                break;
              case TETRA_4:
                for (int j = 0; j < 4; ++j)
                  elemsConn_TET[idx_tet + j] = elemNodes[connIndex + j + 1];
                idx_tet += 4;
                break;
              case PYRA_5:
                for (int j = 0; j < 5; ++j)
                  elemsConn_PYRA[idx_pyra + j] = elemNodes[connIndex + j + 1];
                idx_pyra += 5;
                break;
              case PENTA_6:
                for (int j = 0; j < 6; ++j)
                  elemsConn_PRISM[idx_prism + j] = elemNodes[connIndex + j + 1];
                idx_prism += 6;
                break;
              case HEXA_8:
                for (int j = 0; j < 8; ++j)
                  elemsConn_HEX[idx_hex + j] = elemNodes[connIndex + j + 1];
                idx_hex += 8;
                break;
              default:
                MB_SET_ERR(MB_INDEX_OUT_OF_RANGE, fileName << ": Trouble determining element type");
            }

            connIndex += (verts_per_elem + 1); // Add one to skip next element descriptor
          }

          // //////////////////////////////////
          // Create elements, sets and tags

          if (count_EDGE > 0)
            create_elements(sectionName, file_id_tag, MBEDGE, 2, section_offset, count_EDGE, elemsConn_EDGE);

          if (count_TRI > 0)
            create_elements(sectionName, file_id_tag, MBTRI, 3, section_offset, count_TRI, elemsConn_TRI);

          if (count_QUAD > 0)
            create_elements(sectionName, file_id_tag, MBQUAD, 4, section_offset, count_QUAD, elemsConn_QUAD);

          if (count_TET > 0)
            create_elements(sectionName, file_id_tag, MBTET, 4, section_offset, count_TET, elemsConn_TET);

          if (count_PYRA > 0)
            create_elements(sectionName, file_id_tag, MBPYRAMID, 5, section_offset, count_PYRA, elemsConn_PYRA);

          if (count_PRISM > 0)
            create_elements(sectionName, file_id_tag, MBPRISM, 6, section_offset, count_PRISM, elemsConn_PRISM);

          if (count_HEX > 0)
            create_elements(sectionName, file_id_tag, MBHEX, 8, section_offset, count_HEX, elemsConn_HEX);
        } // Mixed mesh type
      } // num_sections

      cg_close(filePtr);

      return result;
    } // indexZone for
  } // indexBase for

  return MB_SUCCESS;
}

ErrorCode ReadCGNS::create_elements(char *sectionName,
                                    const Tag *file_id_tag,
                                    const EntityType &ent_type,
                                    const int& verts_per_elem,
                                    long &section_offset,
                                    int elems_count,
                                    const std::vector<cgsize_t>& elemsConn)
{
  ErrorCode result;

  // Create the element sequence; passes back a pointer to the internal storage for connectivity and the
  // starting entity handle
  EntityHandle* conn_array;
  EntityHandle handle = 0;

  result = readMeshIface->get_element_connect(elems_count, verts_per_elem, ent_type, 1, handle,
                                              conn_array);MB_CHK_SET_ERR(result, fileName << ": Trouble reading elements");

  memcpy(conn_array, &elemsConn[0], elemsConn.size() * sizeof(EntityHandle));

  // Notify MOAB of the new elements
  result = readMeshIface->update_adjacencies(handle, elems_count, verts_per_elem, conn_array);
  if (MB_SUCCESS != result)
    return result;

  // //////////////////////////////////
  // Create sets and tags

  Range elements(handle, handle + elems_count - 1);

  // Store element IDs

  std::vector<int> id_list(elems_count);

  // Add 1 to offset id to 1-based numbering
  for (cgsize_t i = 0; i < elems_count; ++i)
    id_list[i] = i + 1 + section_offset;
  section_offset += elems_count;

  create_sets(sectionName, file_id_tag, ent_type, elements, id_list, 0);

  return MB_SUCCESS;
}

ErrorCode ReadCGNS::create_sets(char *sectionName,
                                const Tag *file_id_tag,
                                EntityType /*element_type*/,
                                const Range& elements,
                                const std::vector<int>& set_ids,
                                int /*set_type*/)
{
  ErrorCode result;

  result = mbImpl->tag_set_data(globalId, elements, &set_ids[0]);
  if (MB_SUCCESS != result)
    return result;

  if (file_id_tag) {
    result = mbImpl->tag_set_data(*file_id_tag, elements, &set_ids[0]);
    if (MB_SUCCESS != result)
      return result;
  }

  EntityHandle set_handle;

  Tag tag_handle;

  const char* setName = sectionName;

  mbImpl->tag_get_handle(setName, 1, MB_TYPE_INTEGER, tag_handle, MB_TAG_SPARSE | MB_TAG_CREAT);

  // Create set
  result = mbImpl->create_meshset(MESHSET_SET, set_handle);MB_CHK_SET_ERR(result, fileName << ": Trouble creating set");

  //// Add dummy values to current set
  //std::vector<int> tags(set_ids.size(), 1);
  //result = mbImpl->tag_set_data(tag_handle, elements, &tags[0]);
  //if (MB_SUCCESS != result) return result;

  // Add them to the set
  result = mbImpl->add_entities(set_handle, elements);MB_CHK_SET_ERR(result, fileName << ": Trouble putting entities in set");

  return MB_SUCCESS;
}

ErrorCode ReadCGNS::process_options(const FileOptions & opts)
{
  // Mark all options seen, to avoid compile warning on unused variable
  opts.mark_all_seen();

  return MB_SUCCESS;
}

} // namespace moab
