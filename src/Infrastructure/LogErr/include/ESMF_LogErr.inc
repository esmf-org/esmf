#if 0
 $Id: ESMF_LogErr.inc,v 1.4 2004/05/13 22:48:24 cpboulder Exp $

 Earth System Modeling Framework
 Copyright 2002-2003, University Corporation for Atmospheric Research,
 Massachusetts Institute of Technology, Geophysical Fluid Dynamics
 Laboratory, University of Michigan, National Centers for Environmental
 Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
 NASA Goddard Space Flight Center.
 Licensed under the GPL.
 ------------------------------------------------------------------------------

 This file is included in framework source code and will give filename and
 line numbers where messages were generated from.  For user code in which
 no preprocessing step is preferred, the routines will report the messages
 but no line or file information.  If user code wants to generate this,
 they will have to include this file and use a preprocessing step.

#endif

#include "ESMF_ErrConstants.inc"
#include "ESMF_LogConstants.inc"
#ifndef PARCH_IRIX64

#define ESMF_LogWrite() ESMF_LogWrite()


#endif
