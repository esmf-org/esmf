// $Id$
//
// Earth System Modeling Framework
// Copyright 2002-2019, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================
#include <Mesh/include/Legacy/ESMCI_Exception.h>

//-----------------------------------------------------------------------------
// leave the following line as-is; it will insert the cvs ident string
// into the object file for tracking purposes.
static const char *const version = "$Id$";
//-----------------------------------------------------------------------------


namespace ESMCI {


static std::vector<std::string> &get_traceBuf() {
  static std::vector<std::string> traceBuf;

  return traceBuf;
}

TraceBack::~TraceBack() {
// TODO: Temporary work-around until we find solution for exception handling:
// gjt: Commented out the following lines because they were causing SEGV on
// gjt: PGI 10.3 and 10.4 (suspect all PGI 10.x versions) in ESMF applications
// gjt: that were linked following the current rules in Linux.pgi.default.  
//  if (std::uncaught_exception()) {
//    get_traceBuf().push_back(std::string(funcName));
//  }
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
