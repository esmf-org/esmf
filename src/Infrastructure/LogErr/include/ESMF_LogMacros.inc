#if 0
 $Id: ESMF_LogMacros.inc,v 1.7 2004/05/12 17:47:25 cpboulder Exp $

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
#define ESMC_LogWrite(args...) \
ESMC_LogWrite(__LINE__,__FILE__,args)



#define ESMC_LogFoundError(rc,args...) \
ESMC_LogFoundError(rc,__LINE__,__FILE__,args)
