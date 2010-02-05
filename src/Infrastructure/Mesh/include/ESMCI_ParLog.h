//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMCI_ParLog_h
#define ESMCI_ParLog_h

#include <iostream>
#include <fstream>
#include <map>

namespace ESMCI {

class ParLog {
public:
// If called (first time) with a fstem, it opens a file
// fstem.rank, otherwise this arg is ignored.
static ParLog *instance(const std::string &fstem = "PARLOG");
static std::ofstream &stream() { return ParLog::instance()->of; }
static void flush() { ParLog::instance()->of.flush(); }
private:
static ParLog *classInstance;
ParLog(const std::string &fname);
std::ofstream of;
ParLog(const ParLog&);
ParLog &operator=(const ParLog&);

};

} // namespace 

#endif
