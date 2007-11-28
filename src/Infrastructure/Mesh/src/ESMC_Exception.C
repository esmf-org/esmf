// $Id: ESMC_Exception.C,v 1.3 2007/11/28 16:42:40 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <ESMC_Exception.h>


namespace ESMC {

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
