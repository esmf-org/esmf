//-------------------------------------------------------------------------
// Filename      : NCHelperHOMME.hpp
//
// Purpose       : Climate NC file helper for HOMME grid
//
// Creator       : Danqing Wu
//-------------------------------------------------------------------------

#ifndef NCHELPERHOMME_HPP
#define NCHELPERHOMME_HPP

#include "NCHelper.hpp"

namespace moab {

//! Child helper class for HOMME grid (CAM_SE)
class NCHelperHOMME : public UcdNCHelper
{
public:
  NCHelperHOMME(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet);
  static bool can_read_file(ReadNC* readNC, int fileId);

private:
  //! Implementation of NCHelper::init_mesh_vals()
  virtual ErrorCode init_mesh_vals();
  //! Implementation of NCHelper::check_existing_mesh()
  virtual ErrorCode check_existing_mesh();
  //! Implementation of NCHelper::create_mesh()
  virtual ErrorCode create_mesh(Range& faces);
  //! Implementation of NCHelper::get_mesh_type_name()
  virtual std::string get_mesh_type_name() { return "CAM_SE"; }

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

private:
  int _spectralOrder; // Read from variable 'np'
  int connectId; // For connectivity file
  bool isConnFile; // Is the data file being read actually a connectivity file in disguise?
};

} // namespace moab

#endif
