// $Id: ESMC_ParLog.h,v 1.1.2.2 2009/01/21 21:25:22 cdeluca Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.


// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_ParLog_h
#define ESMC_ParLog_h

#include <iostream>
#include <fstream>
#include <map>

namespace ESMCI {
namespace MESH {

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
} // namespace 

#endif
