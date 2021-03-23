#if 0
 $Id$

 Earth System Modeling Framework
 Copyright 2002-2021, University Corporation for Atmospheric Research,
 Massachusetts Institute of Technology, Geophysical Fluid Dynamics
 Laboratory, University of Michigan, National Centers for Environmental
 Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
 NASA Goddard Space Flight Center.
 Licensed under the University of Illinois-NCSA License.
 ------------------------------------------------------------------------------

 This file is included in framework source code and will give filename and
 line numbers where messages were generated from.  For user code in which
 no preprocessing step is preferred, the routines will report the messages
 but no line or file information.  If user code wants to generate this,
 they will have to include this file and use a preprocessing step.

#endif

#ifndef ESMF_LOGMACROS_INC
#define ESMF_LOGMACROS_INC

#ifndef ESMF_FILENAME
#define ESMF_FILENAME __FILE__
#endif

#ifndef ESMC_FILENAME
#define ESMC_FILENAME __FILE__
#endif

#define ESMF_CONTEXT  line=__LINE__,file=ESMF_FILENAME,method=ESMF_METHOD
#define ESMC_CONTEXT  __LINE__,ESMC_FILENAME,ESMC_METHOD

#endif
