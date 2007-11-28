// $Id: ESMC_WriteWeights.h,v 1.1 2007/11/28 16:23:22 dneckels Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2007, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

//
//-----------------------------------------------------------------------------
#ifndef ESMC_WRITEWEIGHTS_H_
#define ESMC_WRITEWEIGHTS_H_


#include <mesh/ESMC_ParEnv.h>

#include <string>

namespace ESMC {

class IWeights;

/*
 * Write the weights to a parallel netcdf file.
 */
void WriteIWeights(const IWeights &w, const std::string &fname);

/*
 * Read in the interpolation weights.  If weights is non-empty, the weights
 * from the file are just appended.
 */
void ReadIWeights(IWeights &w,
                  const std::string &fname,
                  UInt nproc = Par::Size(),
                  UInt rank = Par::Rank());

/*
 * Write a Script type matrix file, with dest and src grid info.
 * SEQ = number weights so all of one index is first, then the next, etc..
 * INTERLEAVE = weights interleaved.
 */

enum {NCMAT_ORDER_SEQ = 0};

void WriteNCMatFile(const std::string &src_ncfile,
                    const std::string &dst_ncfile,
                    const std::string &outfile,
                    const IWeights &w,
                    int ordering = NCMAT_ORDER_SEQ
                    );

} // namespace

#endif /*ESMC_WRITEWEIGHTS_H_*/
