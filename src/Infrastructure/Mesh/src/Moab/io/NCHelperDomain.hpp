//-------------------------------------------------------------------------
// Filename      : NCHelperFV.hpp
//
// Purpose       : Climate NC file helper for Domain grid

//-----------------------
#ifndef NCHELPERDOMAIN_HPP
#define NCHELPERDOMAIN_HPP

#include "NCHelper.hpp"

namespace moab
{

//! Child helper class for Domain grid
class NCHelperDomain : public ScdNCHelper
{
  public:
    NCHelperDomain( ReadNC* readNC, int fileId, const FileOptions& opts, EntityHandle fileSet )
        : ScdNCHelper( readNC, fileId, opts, fileSet )
    {
    }
    static bool can_read_file( ReadNC* readNC, int fileId );

    ErrorCode create_mesh( Range& faces );

  private:
    virtual ErrorCode init_mesh_vals();
    virtual std::string get_mesh_type_name()
    {
        return "DOMAIN";
    }

    int nv;     // number of vertices per cell
    int nvDim;  // index of nv dim
};

#endif /* NCHELPERDOMAIN_HPP */
}  // namespace moab
