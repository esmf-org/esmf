#if 0
 $Id: ESMF_LogMacros.inc,v 1.6 2004/05/11 22:04:41 cpboulder Exp $

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
#define ESMC_LogWrite(...) \
ESMC_LogWrite(__LINE__,__FILE__,__VA_ARGS__)



#define ESMC_LogFoundError(rc,...) \
ESMC_LogFoundError(rc,__LINE__,__FILE__,__VA_ARGS__)

