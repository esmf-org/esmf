/*
 * NCWriteGCRM.hpp
 *
 *  nc write helper for GCRM type data (CAM)
 *  Created on: April 9, 2014
 *
 */

#ifndef NCWRITEGCRM_HPP_
#define NCWRITEGCRM_HPP_

#include "NCWriteHelper.hpp"

namespace moab {

class NCWriteGCRM: public UcdNCWriteHelper
{
public:
  NCWriteGCRM(WriteNC* writeNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: UcdNCWriteHelper(writeNC, fileId, opts, fileSet) {}

  virtual ~NCWriteGCRM();

private:
  //! Implementation of NCWriteHelper::collect_mesh_info()
  virtual ErrorCode collect_mesh_info();

  //! Collect data for specified variables
  virtual ErrorCode collect_variable_data(std::vector<std::string>& var_names, std::vector<int>& tstep_nums);

  //! Implementation of NCWriteHelper::write_nonset_variables()
  virtual ErrorCode write_nonset_variables(std::vector<WriteNC::VarData>& vdatas, std::vector<int>& tstep_nums);
};

} // namespace moab

#endif
