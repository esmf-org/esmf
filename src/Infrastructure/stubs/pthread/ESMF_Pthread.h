// $Id: ESMF_Pthread.h,v 1.6.4.1 2010/02/05 20:02:07 svasquez Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2010, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//-------------------------------------------------------------------------

// Purpose of this file is to provide a wrapper around the pthread library
// header. This allows to switch from a real pthread library implementation
// to pthread_stubs in a single place.

//-------------------------------------------------------------------------
// these lines prevent this file from being read more than once if it
// ends up being included multiple times

#ifndef ESMF_PTHREAD_H
#define ESMF_PTHREAD_H

#ifndef ESMF_NO_PTHREADS
#include <pthread.h>    // use a real pthread library implementation
typedef pthread_mutex_t esmf_pthread_mutex_t;
typedef pthread_cond_t  esmf_pthread_cond_t;
typedef pthread_t       esmf_pthread_t;
#else
typedef struct{}        esmf_pthread_mutex_t;
typedef struct{}        esmf_pthread_cond_t;
typedef int             esmf_pthread_t;
#endif

#endif  // ESMF_PTHREAD_H
