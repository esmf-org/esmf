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
#ifndef ESMCI_WRITEWEIGHTSPAR_H_
#define ESMCI_WRITEWEIGHTSPAR_H_

#include <string>

namespace ESMCI {

class IWeights;

/*
 * Gather the weights to a row-order and distributed decomposition
 * so that each processor will output a chunk of the netcdf file.
 * The user MUST call this prior to writing the weights.
 */
void GatherForWrite(IWeights &w);

enum {NCMATPAR_ORDER_SEQ = 0, NCMATPAR_ORDER_INTERLEAVE};


/*
 * Write a weight file, in parallel.  You must provide the original 
 * grid files, as these are included in the weights file.
 */
void WriteNCMatFilePar(const std::string &src_ncfile,
                    const std::string &dst_ncfile,
                    const std::string &outfile,
                    const IWeights &w,
                    int ordering = NCMATPAR_ORDER_INTERLEAVE
                    );

} // namespace

#endif /*ESMC_WRITEWEIGHTSPAR_H_*/
