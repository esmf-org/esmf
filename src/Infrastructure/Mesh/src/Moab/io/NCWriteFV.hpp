/*
 * NCWriteFV.hpp
 *
 *  nc write helper for FV type data (CAM)
 *  Created on: April 9, 2014
 *
 */

#ifndef NCWRITEFV_HPP_
#define NCWRITEFV_HPP_

#include "NCWriteHelper.hpp"

namespace moab {

class NCWriteFV: public ScdNCWriteHelper
{
public:
  NCWriteFV(WriteNC* writeNC, int fileId, const FileOptions& opts, EntityHandle fileSet)
: ScdNCWriteHelper(writeNC, fileId, opts, fileSet) {}

  virtual ~NCWriteFV();
};

} // namespace moab

#endif // NCWRITEFV_HPP_
