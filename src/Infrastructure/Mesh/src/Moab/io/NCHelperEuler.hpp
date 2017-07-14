//-------------------------------------------------------------------------
// Filename      : NCHelperEuler.hpp
//
// Purpose       : Climate NC file helper for Eulerian Spectral grid
//
// Creator       : Danqing Wu
//-------------------------------------------------------------------------

#ifndef NCHELPEREULER_HPP
#define NCHELPEREULER_HPP

#include "NCHelper.hpp"

#ifdef WIN32
#ifdef size_t
#undef size_t
#endif
#endif

namespace moab {

//! Child helper class for Eulerian Spectral grid (CAM_EUL)
class NCHelperEuler : public ScdNCHelper
{
public:
  NCHelperEuler(ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: ScdNCHelper(readNC, fileId, opts, fileSet) {}

  static bool can_read_file(ReadNC* readNC, int fileId);

private:
  virtual ErrorCode init_mesh_vals();
  virtual std::string get_mesh_type_name() { return "CAM_EUL"; }
};

} // namespace moab

#endif
