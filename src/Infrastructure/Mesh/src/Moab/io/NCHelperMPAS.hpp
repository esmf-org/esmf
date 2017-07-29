//-------------------------------------------------------------------------
// Filename      : NCHelperMPAS.hpp
//
// Purpose       : Climate NC file helper for MPAS grid
//
// Creator       : Danqing Wu
//-------------------------------------------------------------------------

#ifndef NCHELPERMPAS_HPP
#define NCHELPERMPAS_HPP

#include "NCHelper.hpp"

namespace moab {

//! Child helper class for MPAS grid
class NCHelperMPAS : public UcdNCHelper
{
public:
  NCHelperMPAS(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet);
  static bool can_read_file(ReadNC* readNC);

private:
  //! Implementation of NCHelper::init_mesh_vals()
  virtual ErrorCode init_mesh_vals();
  //! Implementation of NCHelper::check_existing_mesh()
  virtual ErrorCode check_existing_mesh();
  //! Implementation of NCHelper::create_mesh()
  virtual ErrorCode create_mesh(Range& faces);
  //! Implementation of NCHelper::get_mesh_type_name()
  virtual std::string get_mesh_type_name() { return "MPAS"; }

  //! Implementation of UcdNCHelper::read_ucd_variables_to_nonset_allocate()
  virtual ErrorCode read_ucd_variables_to_nonset_allocate(std::vector<ReadNC::VarData>& vdatas,
                                                         std::vector<int>& tstep_nums);
#ifdef MOAB_HAVE_PNETCDF
  //! Implementation of UcdNCHelper::read_ucd_variables_to_nonset_async()
  virtual ErrorCode read_ucd_variables_to_nonset_async(std::vector<ReadNC::VarData>& vdatas,
                                                      std::vector<int>& tstep_nums);
#else
  //! Implementation of UcdNCHelper::read_ucd_variables_to_nonset()
  virtual ErrorCode read_ucd_variables_to_nonset(std::vector<ReadNC::VarData>& vdatas,
                                                std::vector<int>& tstep_nums);
#endif

#ifdef MOAB_HAVE_MPI
  //! Redistribute local cells after trivial partition (e.g. Zoltan partition, if applicable)
  ErrorCode redistribute_local_cells(int start_cell_index);
#endif

  //! Create local vertices
  ErrorCode create_local_vertices(const std::vector<int>& vertices_on_local_cells, EntityHandle& start_vertex);

  //! Create local edges (optional)
  ErrorCode create_local_edges(EntityHandle start_vertex, const std::vector<int>& num_edges_on_local_cells);

  //! Create local cells without padding (cells are divided into groups based on the number of edges)
  ErrorCode create_local_cells(const std::vector<int>& vertices_on_local_cells,
                                        const std::vector<int>& num_edges_on_local_cells,
                                        EntityHandle start_vertex, Range& faces);

  //! Create local cells with padding (padded cells will have the same number of edges)
  ErrorCode create_padded_local_cells(const std::vector<int>& vertices_on_local_cells,
                                      EntityHandle start_vertex, Range& faces);

  //! Create gather set vertices
  ErrorCode create_gather_set_vertices(EntityHandle gather_set, EntityHandle& gather_set_start_vertex);

  //! Create gather set edges (optional)
  ErrorCode create_gather_set_edges(EntityHandle gather_set, EntityHandle gather_set_start_vertex);

  //! Create gather set cells without padding (cells are divided into groups based on the number of edges)
  ErrorCode create_gather_set_cells(EntityHandle gather_set, EntityHandle gather_set_start_vertex);

  //! Create gather set cells with padding (padded cells will have the same number of edges)
  ErrorCode create_padded_gather_set_cells(EntityHandle gather_set, EntityHandle gather_set_start_vertex);

private:
  int maxEdgesPerCell;
  int numCellGroups;
  bool createGatherSet;
  std::map<EntityHandle, int> cellHandleToGlobalID;
  Range facesOwned;
};

} // namespace moab

#endif
