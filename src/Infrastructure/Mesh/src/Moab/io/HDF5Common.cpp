#include <HDF5Common.hpp>
#include <H5Spublic.h>
#include <assert.h>

namespace moab {

bool HDF5_can_append_hyperslabs()
{
  hsize_t dim = 100;
  hid_t space = H5Screate_simple( 1, &dim, NULL );
  bool result = false;

  HDF5_Error_Func_Type fptr = 0;
  void* fdata = 0;
#if defined(H5Eget_auto_vers) && H5Eget_auto_vers > 1
  if (0 <= H5Eget_auto( H5E_DEFAULT, &fptr, &fdata ))
    H5Eset_auto( H5E_DEFAULT, 0, 0 );
#else
  if (0 <= H5Eget_auto( &fptr, &fdata ))
    H5Eset_auto( 0, 0 );
#endif

  
  hsize_t start = 1, count = 5;
  H5Sselect_hyperslab( space, H5S_SELECT_SET, &start, 0, &count, 0 );
  start = 20;
  if (0 <= H5Sselect_hyperslab( space, H5S_SELECT_APPEND, &start, 0, &count, 0 ))
    result = true;

  if (fptr) {
#if defined(H5Eset_auto_vers) && H5Eset_auto_vers > 1
    H5Eset_auto( H5E_DEFAULT, fptr, fdata );
#else
    H5Eset_auto( fptr, fdata );
#endif
  }
  H5Sclose( space );
  
  return result;
}

}
