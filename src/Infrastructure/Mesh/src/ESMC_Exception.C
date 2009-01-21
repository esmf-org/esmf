// $Id: ESMC_Exception.C,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_Exception.h>


namespace ESMCI {
namespace MESH {

std::vector<std::string> TraceBack::traceBuf;

std::string TraceBack::StackTrace() {
  std::string res("** STACKTRACE **:\n");
  for (int i = traceBuf.size() - 1; i >= 0; --i) {
    res += traceBuf[i] + "\n";
  }
  return res;
}

std::string EmptyTrace::StackTrace() {
  return "Trace Not implemented in EmptyTrace";
}

} // namespace
} // namespace
