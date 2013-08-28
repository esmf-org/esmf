#include <iostream>
#include <fstream>
#include <vector>
#include <cstdlib>
#include <sstream>
#include "assert.h"

#include "ReadIDEAS.hpp"
#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "moab/ReadUtilIface.hpp"
#include "FileTokenizer.hpp"
#include "MBTagConventions.hpp"
#include "moab/Range.hpp"
#include "moab/CN.hpp"

namespace moab {

ReaderIface* ReadIDEAS::factory( Interface* iface )
  { return new ReadIDEAS( iface ); }

ReadIDEAS::ReadIDEAS(Interface* impl)
    : MBI(impl)
{
  impl->query_interface(readMeshIface);
}


ErrorCode ReadIDEAS::read_tag_values( const char* /* file_name */,
                                      const char* /* tag_name */,
                                      const FileOptions& /* opts */,
                                      std::vector<int>& /* tag_values_out */,
                                      const SubsetList* /* subset_list */ )
{
  return MB_NOT_IMPLEMENTED;
}


ErrorCode ReadIDEAS::load_file(const char* fname, 
                               const EntityHandle* , 
                               const FileOptions& /*options*/,
                               const ReaderIface::SubsetList* subset_list,
                               const Tag* file_id_tag ) {

  if (subset_list) {
    readMeshIface->report_error( "Reading subset of files not supported for IDEAS." );
    return MB_UNSUPPORTED_OPERATION;
  }

  file.open( fname );
  if (!file.good()) {
    readMeshIface->report_error("Failed to open file: %s", fname);
    return MB_FILE_DOES_NOT_EXIST;
  }

  ErrorCode rval;

  char line[10000];
  file.getline(line, 10000);
  char* liter = line;
  while (*liter && isspace(*liter))
    ++liter;
  if (*liter != '-') return MB_FAILURE;
  ++liter;
  if (*liter != '1') return MB_FAILURE;
  while (*++liter)
    if (!isspace(*liter))
      return MB_FAILURE;

  EntityHandle first_vertex = 0;
  std::string s;
  while (! file.eof() ) {
    file.getline(line, 10000);
    s = line;
    unsigned int header_id = (unsigned int) strtol(line, NULL, 10);

    // create vertices
    if( DOUBLE_PRECISION_NODES0==header_id ||
        DOUBLE_PRECISION_NODES1==header_id ) {
        if (first_vertex) // multiple vertex blocks?
          return MB_FAILURE;
        rval = create_vertices( first_vertex, file_id_tag );
    }
    // create elements
    else if(ELEMENTS0==header_id ||
            ELEMENTS1==header_id ||
            ELEMENTS2==header_id) {
        if (!first_vertex) // need to read vertices first
          return MB_FAILURE;
        rval = create_elements( first_vertex, file_id_tag );
    }
    // skip everything else
    else {
      rval = skip_header(); 
      if (MB_SUCCESS != rval)
        return MB_FAILURE;
    }
  }

  file.close();
  return MB_SUCCESS;
}

ErrorCode ReadIDEAS::skip_header() {

  // Go until finding a pair of -1 lines
  char *ctmp;
  char line[10000];
  std::string s;

  int end_of_block = 0;

  long int il;

  while (file.getline(line, 10000)) {
 
    il = std::strtol(line, &ctmp, 10);
    if (il == -1) {
      s = ctmp;
      if (s.empty()) end_of_block++;
    }
    else end_of_block = 0;

    if (end_of_block >= 2)
      return MB_SUCCESS;

  }

  return MB_SUCCESS;
}



ErrorCode ReadIDEAS::create_vertices(EntityHandle& first_vertex,
                                     const Tag* file_id_tag) {

  // Read two lines: first has some data, second has coordinates
  char line1[10000], line2[10000];
  int il1, il2;
  char *ctmp1, *ctmp2;
  std::string s1, s2;

  ErrorCode rval;

  int top_of_block = file.tellg();
  unsigned int num_verts = 0;

  for (;;) {

    // read both lines
    if (!file.getline(line1, 10000))
      return MB_FAILURE;
    if (!file.getline(line2, 10000)) 
      return MB_FAILURE;
   
    // Check if we are at the end of the block
    il1 = std::strtol(line1, &ctmp1, 10);
    il2 = std::strtol(line2, &ctmp2, 10);
    if ((il1 == -1) && (il2 == -1)) {
      s1 = ctmp1;
      s2 = ctmp2;
      if ((s1.empty()) && (s2.empty())) break;     
    }
    num_verts++;
  }

  file.seekg( top_of_block );
  
  std::vector<double*> arrays;
  rval = readMeshIface->get_node_coords( 3, num_verts, 0, first_vertex, arrays );
  if (MB_SUCCESS != rval)
    return rval;

  Range verts;
  verts.insert( first_vertex, first_vertex + num_verts - 1 );
  
  double *x = arrays[0];
  double *y = arrays[1];
  double *z = arrays[2];

  // For now, assume ids are sequential and begin with 1
  Tag id_tag;
  int zero = 0;
  rval = MBI->tag_get_handle( GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, id_tag,
                              MB_TAG_DENSE|MB_TAG_CREAT, &zero); 
  if (MB_SUCCESS != rval && MB_ALREADY_ALLOCATED != rval) 
    return rval;
  const int beginning_node_id = 1;
  int node_id = beginning_node_id;;

  for (unsigned int i = 0; i < num_verts; i++) {

    if (!file.getline(line1, 10000))
      return MB_FAILURE;
    if (!file.getline(line2, 10000))
      return MB_FAILURE;

    // Get the id out of the 1st line. Check the assumption that node ids are
    // sequential and begin with 1.
    if(node_id != std::strtol(line1, &ctmp1, 10))
      return MB_NOT_IMPLEMENTED;
    else
      ++node_id;   

    // Get the doubles out of the 2nd line
    x[i] = std::strtod(line2,   &ctmp2);
    y[i] = std::strtod(ctmp2+1, &ctmp2);
    z[i] = std::strtod(ctmp2+1, NULL);
  }

  if (!file.getline(line1, 10000))
    return MB_FAILURE;
  if (!file.getline(line2, 10000))
    return MB_FAILURE;

  // Tag the nodes with ids
  rval = readMeshIface->assign_ids( id_tag, verts, beginning_node_id );
  if (MB_SUCCESS != rval)
    return rval;
  if (file_id_tag) {
    rval = readMeshIface->assign_ids( *file_id_tag, verts, beginning_node_id );
    if (MB_SUCCESS != rval)
      return rval;
  }

  return MB_SUCCESS;
}


ErrorCode ReadIDEAS::create_elements(EntityHandle vstart,
                                     const Tag* file_id_tag) {

  char line1[10000], line2[10000];
  int il1, il2;
  char *ctmp1, *ctmp2;
  std::string s1, s2;
  ErrorCode rval;
  EntityHandle handle;

  Tag mat_tag, phys_tag, id_tag;
  rval = MBI->tag_get_handle( MAT_PROP_TABLE_TAG , 1, MB_TYPE_INTEGER, mat_tag, MB_TAG_DENSE|MB_TAG_CREAT); 
  if (MB_SUCCESS != rval && MB_ALREADY_ALLOCATED != rval) return rval;
  rval = MBI->tag_get_handle( PHYS_PROP_TABLE_TAG, 1, MB_TYPE_INTEGER, phys_tag, MB_TAG_DENSE|MB_TAG_CREAT); 
  if (MB_SUCCESS != rval && MB_ALREADY_ALLOCATED != rval) return rval;
  const int zero = 0;
  rval = MBI->tag_get_handle( GLOBAL_ID_TAG_NAME, 1, MB_TYPE_INTEGER, id_tag, MB_TAG_DENSE|MB_TAG_CREAT, &zero); 
  if (MB_SUCCESS != rval && MB_ALREADY_ALLOCATED != rval) return rval;
 
  for (;;) {

    if (!file.getline(line1, 10000) || !file.getline(line2, 10000))
      return MB_FAILURE;

    // Check if we are at the end of the block
    il1 = std::strtol(line1, &ctmp1, 10);
    il2 = std::strtol(line2, &ctmp2, 10);
    if ((il1 == -1) && (il2 == -1)) {
      s1 = ctmp1;
      s2 = ctmp2;
      if ((s1.empty()) && (s2.empty())) 
        return MB_SUCCESS;     
    }

    // The first line describes attributes of the element other than connectivity.
    const int element_id = strtol(line1+1,  &ctmp1, 10);
    const int ideas_type = strtol(line1+11, &ctmp1, 10);
    const int phys_table = strtol(line1+21, &ctmp1, 10);
    const int mat_table  = strtol(line1+31, &ctmp1, 10);

    // Determine the element type.
    EntityType mb_type;
    if     (TRI0 ==ideas_type || TRI1 ==ideas_type) mb_type = MBTRI;
    else if(QUAD0==ideas_type || QUAD1==ideas_type) mb_type = MBQUAD;
    else if(TET  ==ideas_type)                      mb_type = MBTET;
    else if(HEX  ==ideas_type)                      mb_type = MBHEX;
    else if(WEDGE==ideas_type)                      mb_type = MBPRISM;
    else {
      std::cout << "IDEAS element type not yet added to MOAB reader." << std::endl;
      return MB_NOT_IMPLEMENTED;
    }

    // Get the connectivity out of the 2nd line
    std::stringstream ss(line2);
    const int n_conn = CN::VerticesPerEntity(mb_type);
    EntityHandle conn[CN::MAX_NODES_PER_ELEMENT];
    EntityHandle vert;
    for(int i=0; i<n_conn; ++i) {
      ss >> vert;
      conn[i] = vstart + vert - 1;
    }

    // Make the element. According to the Gmsh 2.2.3 source code, the IDEAS
    // canonical numbering is the same as MBCN.
    rval = MBI->create_element( mb_type, conn, n_conn, handle);
    if(MB_SUCCESS != rval) return rval;

    // If the phys set does not already exist, create it.
    Range phys_sets;
    EntityHandle phys_set;
    const void* const phys_set_id_val[] = {&phys_table};
    rval = MBI->get_entities_by_type_and_tag( 0, MBENTITYSET, &phys_tag, 
                                              phys_set_id_val, 1, phys_sets );
    if(MB_SUCCESS != rval) return rval;
    if(phys_sets.empty()) {
      rval = MBI->create_meshset( MESHSET_SET, phys_set );
      if(MB_SUCCESS != rval) return rval;
      rval = MBI->tag_set_data( phys_tag, &phys_set, 1, &phys_table);
      if(MB_SUCCESS != rval) return rval;
    } else if(1==phys_sets.size()) {
      phys_set = phys_sets.front();
    } else {
      return MB_MULTIPLE_ENTITIES_FOUND;
    }
    rval = MBI->add_entities( phys_set, &handle, 1 );
    if(MB_SUCCESS != rval) return rval;

    // If the material set does not already exist, create it.
    Range mat_sets;
    EntityHandle mat_set;
    const void* const mat_set_id_val[] = {&mat_table};
    rval = MBI->get_entities_by_type_and_tag( 0, MBENTITYSET, &mat_tag, 
                                              mat_set_id_val, 1, mat_sets );
    if(MB_SUCCESS != rval) return rval;
    if(mat_sets.empty()) {
      rval = MBI->create_meshset( MESHSET_SET, mat_set );
      if(MB_SUCCESS != rval) return rval;
      rval = MBI->tag_set_data( mat_tag, &mat_set, 1, &mat_table);
      if(MB_SUCCESS != rval) return rval;
    } else if(1==mat_sets.size()) {
      mat_set = mat_sets.front();
    } else {
      return MB_MULTIPLE_ENTITIES_FOUND;
    }
    rval = MBI->add_entities( mat_set, &handle, 1 );
    if(MB_SUCCESS != rval) return rval;

    // Tag the element with its id
    rval = MBI->tag_set_data( id_tag, &handle, 1, &element_id);
    if(MB_SUCCESS != rval) return rval;
    if (file_id_tag) {
      rval = MBI->tag_set_data( *file_id_tag, &handle, 1, &element_id );
      if(MB_SUCCESS != rval) return rval;
    }
  }
  
    //return MB_FAILURE;
}

} // namespace moab
