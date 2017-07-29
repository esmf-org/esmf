//-------------------------------------------------------------------------
// Filename      : NCHelperFV.hpp
//
// Purpose       : Climate NC file helper for Finite Volume grid
//
// Creator       : Danqing Wu
//-------------------------------------------------------------------------

#ifndef NCHELPERFV_HPP
#define NCHELPERFV_HPP

#include "NCHelper.hpp"

namespace moab {

//! Child helper class for Finite Volume grid (CAM_FV)
class NCHelperFV : public ScdNCHelper
{
public:
  NCHelperFV(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: ScdNCHelper(readNC, fileId, opts, fileSet) {}
  static bool can_read_file(ReadNC* readNC, int fileId);

private:
  virtual ErrorCode init_mesh_vals();
  virtual std::string get_mesh_type_name() { return "CAM_FV"; }
};

} // namespace moab

#endif
