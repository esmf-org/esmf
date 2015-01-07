/**
 * \class ReadTemplate
 * \brief Template for writing a new reader in MOAB
 *
 */

#include "ReadTemplate.hpp"
#include "moab/Interface.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/Range.hpp"
#include "FileOptions.hpp"

#include <cstdio>
#include <assert.h>

namespace moab {

ReaderIface* ReadTemplate::factory( Interface* iface )
  { return new ReadTemplate(iface); }

ReadTemplate::ReadTemplate(Interface* impl)
    : mbImpl(impl)
{
  mbImpl->query_interface(readMeshIface);
}

ReadTemplate::~ReadTemplate()
{
  if (readMeshIface) {
    mbImpl->release_interface(readMeshIface);
    readMeshIface = 0;
  }
}


ErrorCode ReadTemplate::read_tag_values( const char* /* file_name */,
                                     const char* /* tag_name */,
                                     const FileOptions& /* opts */,
                                     std::vector<int>& /* tag_values_out */,
                                     const SubsetList* /* subset_list */ )
{
  return MB_NOT_IMPLEMENTED;
}


ErrorCode ReadTemplate::load_file( const char* filename, 
                               const EntityHandle *file_set,
                               const FileOptions& opts,
                               const ReaderIface::SubsetList* subset_list,
                                   const Tag* /*file_id_tag*/ )
{
  if (subset_list) {
      // see src/moab/ReaderIface.hpp, definition of SubsetList struct; this basically specifies
      // an integer tag and tag values for sets to read on this proc, or a part number and total # parts
      // for reading a trivial partition of entities
  }

    // save filename to member variable so we don't need to pass as an argument
    // to called functions
  fileName = filename;

    // process options; see src/FileOptions.hpp for API for FileOptions class, and doc/metadata_info.doc for
    // a description of various options used by some of the readers in MOAB
  ErrorCode result = process_options(opts);
  if (MB_SUCCESS != result) {
    readMeshIface->report_error( "%s: problem reading options\n", fileName);
    return result;
  }

    // Open file; filePtr is member of ReadTemplate, change to whatever mechanism is used to identify file
  FILE* filePtr = fopen( fileName, "r" );
  if (!filePtr)
  {
    readMeshIface->report_error( "%s: fopen returned error.\n", fileName);
    return MB_FILE_DOES_NOT_EXIST;
  }
  
    // read number of verts, elements, sets
  long num_verts = 0, num_elems = 0, num_sets = 0;

    // read_ents keeps a running set of entities read from this file, including vertices, elements, and sets;
    // these will get added to file_set (if input) at the end of the read
  Range read_ents;
  
    // start_vertex is passed back so we know how to convert indices from the file into vertex handles; most
    // of the time this is done by adding start_vertex to the (0-based) index; if the index is 1-based, you also
    // need to subtract one; see read_elements for details
  EntityHandle start_vertex;
  result = read_vertices(num_verts, start_vertex, read_ents);
  if (MB_SUCCESS != result) return result;

    // create/read elements; this template assumes that all elements are the same type, so can be read in a single
    // call to read_elements, and kept track of with a single start_elem handle.  If there are more entity types,
    // might have to keep these start handles in an array/vector.  start_elem is only really needed if you're reading
    // sets later, and need to convert some file-based index to an entity handle
  EntityHandle start_elem;
  result = read_elements(num_elems, start_vertex, start_elem, read_ents);
  if (MB_SUCCESS != result) return result;

    // read/create entity sets; typically these sets have some tag identifying what they're for, see doc/metadata_info.doc
    // for examples of different kinds of sets and how they're marked
  result = create_sets(num_sets, start_vertex, num_verts, start_elem, num_elems, read_ents);
  if (MB_SUCCESS != result) return result;

    // finally, add all read_ents into the file set, if one was input
  if (file_set && *file_set) {
    result = mbImpl->add_entities(*file_set, read_ents);
    if (MB_SUCCESS != result) return result;
  }
  
  return result;
}

ErrorCode ReadTemplate::read_vertices(int num_verts, EntityHandle &start_vertex, Range &read_ents) 
{
    // allocate nodes; these are allocated in one shot, get contiguous handles starting with start_handle,
    // and the reader is passed back double*'s pointing to MOAB's native storage for vertex coordinates
    // for those verts
  std::vector<double*> coord_arrays;
  ErrorCode result = readMeshIface->get_node_coords( 3, num_verts, 1, start_vertex, coord_arrays );
  if (MB_SUCCESS != result) {
    readMeshIface->report_error("%s: Trouble reading vertices\n", fileName);
    return result;
  }

    // fill in vertex coordinate arrays
  double *x = coord_arrays[0], 
         *y = coord_arrays[1],
         *z = coord_arrays[2];
  for(long i = 0; i < num_verts; ++i) {
      // read x/y/z; do something with them for now to avoid warning
    if (x || y || z) {}
    
  }

  if (num_verts) read_ents.insert(start_vertex, start_vertex + num_verts - 1);
  
  return result;
}

//! read/create elements
ErrorCode ReadTemplate::read_elements(int num_elems, EntityHandle start_vertex,
                                      EntityHandle &start_elem, Range &read_ents) 
{
    // get the entity type being read
  EntityType ent_type = MBHEX;

    // get the number of vertices per entity
  int verts_per_elem = 8;
  
    // Create the element sequence; passes back a pointer to the internal storage for connectivity and the
    // starting entity handle
  EntityHandle* conn_array;
  ErrorCode result = readMeshIface->get_element_connect( num_elems, verts_per_elem, ent_type,
                                                         1, start_elem, conn_array );
  if (MB_SUCCESS != result) {
    readMeshIface->report_error("%s: Trouble reading elements\n", fileName);
    return result;
  }

    // read connectivity into conn_array directly
  for (long i = 0; i < num_elems; i++) {
      // read connectivity
  }

    // convert file-based connectivity indices to vertex handles in-place; be careful, if indices are smaller than handles,
    // need to do from the end of the list so we don't overwrite data
    //
    // here, we assume indices are smaller than handles, just for demonstration; create an integer-type pointer to connectivity
    // initialized to same start of connectivity array
  int *ind_array = reinterpret_cast<int*>(conn_array);
    // OFFSET is value of first vertex index in file; most files are 1-based, but some might be 0-based
  int OFFSET = 1;
  for (long i = num_elems*verts_per_elem-1; i >= 0; i--) {
    conn_array[i] = ind_array[i] + start_vertex + OFFSET;

      // this assert assumes last handle in read_ents is highest vertex handle in this file
    assert(conn_array[i] >= start_vertex && conn_array[i] <= *read_ents.rbegin());
  }
  
    // notify MOAB of the new elements
  result = readMeshIface->update_adjacencies(start_elem, num_elems, verts_per_elem, conn_array);
  if (MB_SUCCESS != result) return result;

    // add elements to read_ents
  if (num_elems) read_ents.insert(start_elem, start_elem+num_elems-1);
  
  return MB_SUCCESS;
}

//! read/create sets
    ErrorCode ReadTemplate::create_sets(int num_sets, EntityHandle /*start_vertex*/, int /*num_verts*/, 
                                        EntityHandle /*start_elem*/, int /*num_elems*/, Range &read_ents)
{ 
  ErrorCode result = MB_SUCCESS;
  EntityHandle this_set;
  
  for (int i = 0; i < num_sets; i++) {
      // create set
    result = mbImpl->create_meshset(MESHSET_SET, this_set);
    if (MB_SUCCESS != result) {
      readMeshIface->report_error("%s: Trouble creating set.\n", fileName);
      return result;
    }

    Range set_ents;
      // read/compute what's in this set; REMEMBER TO CONVERT THESE TO MOAB HANDLES

      // add them to the set
    result = mbImpl->add_entities(this_set, set_ents);
    if (MB_SUCCESS != result) {
      readMeshIface->report_error("%s: Trouble putting entities in set.\n", fileName);
      return result;
    }

      // add the new set to read_ents
    read_ents.insert(this_set);
  }
    
  return MB_SUCCESS;
}

ErrorCode ReadTemplate::process_options(const FileOptions &opts) 
{
    // mark all options seen, to avoid compile warning on unused variable
  opts.mark_all_seen();
  return MB_SUCCESS;
}
    

} // namespace moab
