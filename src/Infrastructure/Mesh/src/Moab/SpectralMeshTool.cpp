#include "moab/SpectralMeshTool.hpp"
#include "moab/Interface.hpp"
#include "Internals.hpp"
#include "moab/ReadUtilIface.hpp"
#include "moab/CN.hpp"

#include <cmath>
#include <assert.h>

namespace moab 
{
const short int SpectralMeshTool::permute_array[] = 
{0, 1, 13, 25, 3, 2, 14, 26, 7, 6, 18, 30, 11, 10, 22, 34};

  // lin_permute_array does the same to get the linear vertices of the coarse quad
const short int SpectralMeshTool::lin_permute_array[] =
{0, 25, 34, 11};

Tag SpectralMeshTool::spectral_vertices_tag(const bool create_if_missing) 
{
  ErrorCode rval = MB_SUCCESS;
  if (!svTag && create_if_missing) {
    if (!spectralOrder) {
        // should already be a spectral order tag...
      MB_SET_ERR_RET_VAL("Spectral order must be set before creating spectral vertices tag", 0);
    }
        
      // create it
    std::vector<EntityHandle> dum_val(spectralOrderp1*spectralOrderp1, 0);
    rval = mbImpl->tag_get_handle("SPECTRAL_VERTICES", spectralOrderp1*spectralOrderp1, MB_TYPE_HANDLE,
                                  svTag, MB_TAG_DENSE | MB_TAG_CREAT, &dum_val[0]);
  }
      
  return (rval == MB_SUCCESS ? svTag : 0);
}

      /** \brief Return tag used to store spectral order
       * \param create_if_missing If true, will create this tag if it doesn't exist already
       */
Tag SpectralMeshTool::spectral_order_tag(const bool create_if_missing)
{
  ErrorCode rval = MB_SUCCESS;
  if (!soTag && create_if_missing) {
        
      // create it
    int dum = 0;
    rval = mbImpl->tag_get_handle("SPECTRAL_ORDER", 1, MB_TYPE_INTEGER,
                                  soTag, MB_TAG_DENSE | MB_TAG_CREAT, &dum);
  }
      
  return (rval == MB_SUCCESS ? soTag : 0);
}
  
    /** \brief Convert representation from coarse to fine
     * Each element in set, or in interface if set is not input, is converted to fine elements, using
     * vertices in SPECTRAL_VERTICES tagged array
     * \param spectral_set Set containing spectral elements
     */
ErrorCode SpectralMeshTool::convert_to_fine(EntityHandle /* spectral_set */) 
{
  return MB_NOT_IMPLEMENTED;
}
    
  
    /** \brief Convert representation from fine to coarse
     * Each element in set, or in interface if set is not input, is converted to coarse elements, with
     * fine vertices put into SPECTRAL_VERTICES tagged array.  NOTE: This function assumes that each
     * order^d (fine) elements comprise each coarse element, and are in order of fine elements in each
     * coarse element.  If order is input as 0, looks for a SPECTRAL_ORDER tag on the mesh.
     * \param order Order of the spectral mesh
     * \param spectral_set Set containing spectral elements
     */
ErrorCode SpectralMeshTool::convert_to_coarse(int order, EntityHandle spectral_set) 
{
  if (order) spectralOrder = order;
  if (!spectralOrder) {
    MB_SET_ERR(MB_FAILURE, "Spectral order must be set or input before converting to spectral mesh");
  }
  
  Range tmp_ents, ents;
  ErrorCode rval = mbImpl->get_entities_by_handle(spectral_set, tmp_ents);
  if (MB_SUCCESS != rval || ents.empty()) return rval;
  
    // get the max-dimensional elements from it
  ents = tmp_ents.subset_by_dimension(3);
  if (ents.empty()) ents = tmp_ents.subset_by_dimension(2);
  if (ents.empty()) ents = tmp_ents.subset_by_dimension(1);
  if (ents.empty()) {
    MB_SET_ERR(MB_FAILURE, "Can't find any entities for conversion");
  }
    
    // get a ptr to connectivity
  if (ents.psize() != 1) {
    MB_SET_ERR(MB_FAILURE, "Entities must be in one chunk for conversion");
  }
  EntityHandle *conn;
  int count, verts_per_e;
  rval = mbImpl->connect_iterate(ents.begin(), ents.end(), conn, verts_per_e, count);
  if (MB_SUCCESS != rval || count != (int)ents.size()) return rval;

  Range tmp_range;
  return create_spectral_elems(conn, ents.size(), CN::Dimension(TYPE_FROM_HANDLE(*ents.begin())), tmp_range);
}

template <class T>
ErrorCode SpectralMeshTool::create_spectral_elems(const T *conn, int num_fine_elems, int dim,
                                                  Range &output_range, int start_idx, Range *local_gids) 
{
  assert(spectralOrder && num_fine_elems);
  
    // now create num_coarse_elems
    // compute the number of local elems, accounting for coarse or fine representation
    // spectral_unit is the # fine elems per coarse elem, or spectralOrder^2
  int spectral_unit = spectralOrder*spectralOrder;
  int num_coarse_elems = num_fine_elems / spectral_unit;

  EntityHandle *new_conn;
  EntityHandle start_elem;
  ReadUtilIface *rmi;
  ErrorCode rval = mbImpl->query_interface(rmi);
  if (MB_SUCCESS != rval) return rval;
  
  int verts_per_felem = spectralOrderp1*spectralOrderp1,
      verts_per_celem = std::pow((double)2.0, dim);

  rval = rmi->get_element_connect(num_coarse_elems, verts_per_celem,
                                  (2 == dim ? MBQUAD : MBHEX), 0,
                                  start_elem, new_conn);MB_CHK_SET_ERR(rval, "Failed to create elems");

  output_range.insert(start_elem, start_elem + num_coarse_elems - 1);

    // read connectivity into that space

    // permute_array takes a 4*order^2-long vector of integers, representing the connectivity of order^2
    // elems (fine elems in a coarse elem), and picks out the ids of the vertices necessary
    // to get a lexicographically-ordered array of vertices in a spectral element of that order
    //assert(verts_per_felem == (sizeof(permute_array)/sizeof(unsigned int)));
  
    // we're assuming here that elems was empty on input
  int count;
  EntityHandle *sv_ptr = NULL;
  rval = mbImpl->tag_iterate(spectral_vertices_tag(true), output_range.begin(), output_range.end(), count,
                            (void*&)sv_ptr);MB_CHK_SET_ERR(rval, "Failed to get SPECTRAL_VERTICES ptr");
  assert(count == num_coarse_elems);
  int f = start_idx, fs = 0, fl = 0;
  for (int c = 0; c < num_coarse_elems; c++) {
    for (int i = 0; i < verts_per_celem; i++)
      new_conn[fl+i] = conn[f+lin_permute_array[i]];
    fl += verts_per_celem;
    for (int i = 0; i < verts_per_felem; i++) 
      sv_ptr[fs+i] = conn[f+permute_array[i]];
    f += verts_per_celem*spectral_unit;
    fs += verts_per_felem;
  }
  if (local_gids) 
    std::copy(sv_ptr, sv_ptr+verts_per_felem*num_coarse_elems, range_inserter(*local_gids));

  return MB_SUCCESS;
}

// force instantiation of a few specific types      
template ErrorCode SpectralMeshTool::create_spectral_elems<int>(const int *conn, int num_fine_elems, int dim,
                                                                Range &output_range, int start_idx, Range *local_gids);
template ErrorCode SpectralMeshTool::create_spectral_elems<EntityHandle>(const EntityHandle *conn, int num_fine_elems, int dim,
                                                                         Range &output_range, int start_idx, Range *local_gids);
} // namespace moab

