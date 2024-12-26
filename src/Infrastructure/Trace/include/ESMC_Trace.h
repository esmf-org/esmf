// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2023, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// This file is part of the pure C public ESMC API
//-----------------------------------------------------------------------------

//-------------------------------------------------------------------------
// (all lines below between the !BOP and !EOP markers will be included in
//  the automated document processing.)
//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMC_Trace_H
#define ESMC_Trace_H

//-----------------------------------------------------------------------------
// ESMC_Trace - Public C interface to the ESMF Trace class
//
// The code in this file defines the public C Trace class and declares method 
// signatures (prototypes).  The companion file {\tt ESMC\_Trace.C} contains
// the definitions (full code bodies) for the Trace methods.
//-----------------------------------------------------------------------------

#ifdef __cplusplus
extern "C" {
#endif

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TraceRegionEnter - Enter a trace region
//
// !INTERFACE:
int ESMC_TraceRegionEnter(
  const char* name  // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Enter a named trace region.
//
//  The arguments are:
//  \begin{description}
//  \item[name] 
//    Name of the trace region.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE: ESMC_TraceRegionExit - Exit a trace region
//
// !INTERFACE:
int ESMC_TraceRegionExit(
  const char* name  // in
);
// !RETURN VALUE:
//  Return code; equals ESMF_SUCCESS if there are no errors.
//
// !DESCRIPTION:
//
//  Exit a named trace region.
//
//  The arguments are:
//  \begin{description}
//  \item[name] 
//    Name of the trace region.
//  \end{description}
//
//EOP
//-----------------------------------------------------------------------------

#ifdef __cplusplus
} // extern "C"
#endif

#endif  // ESMC_Trace_H
