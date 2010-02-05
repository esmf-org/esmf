//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/ESMCI_Exception.h>


namespace ESMCI {


static std::vector<std::string> &get_traceBuf() {
  static std::vector<std::string> traceBuf;

  return traceBuf;
}

TraceBack::~TraceBack() {
  if (std::uncaught_exception()) {
    get_traceBuf().push_back(std::string(funcName));
  }
}

std::string TraceBack::StackTrace() {
  std::string res("** STACKTRACE **:\n");
  for (int i = get_traceBuf().size() - 1; i >= 0; --i) {
    res += get_traceBuf()[i] + "\n";
  }
  return res;
}

std::string EmptyTrace::StackTrace() {
  return "Trace Not implemented in EmptyTrace";
}

} // namespace
