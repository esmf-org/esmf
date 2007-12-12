// $Id: ESMCI_Null.h,v 1.1 2007/12/12 02:06:55 rosalind Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2008, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// ESMC internal include file.
// Define "null" values to be used as place holders for non-present optional 
// arguments  in f77 C-Fortran interfaces.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// prevent this file from being read more than once
#ifndef ESMCI_Null_H
#define ESMCI_Null_H

#define ESMC_F77_Int_Null -99999
#define ESMC_F77_Float_Null -1.e-99
#define ESMC_F77_Char_Null "!@#$"

//-----------------------------------------------------------------------------

#endif  // ESMCI_Arg_H

