// $Id: ESMC_FTable_F.C,v 1.3 2003/04/14 14:51:38 nscollins Exp $
//
// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.
//
//==============================================================================
//
//==============================================================================
//
// This file contains Fortran interface code to link F90 and C++.
//
//------------------------------------------------------------------------------
// INCLUDES
//------------------------------------------------------------------------------
#include <stdio.h>
#include <string.h>
#include "ESMC.h"
#include "ESMC_Base.h"
#include "ESMC_Comp.h"
#include "ESMC_FTable.h"
#include "ESMC_Array.h"
//------------------------------------------------------------------------------
//BOP
// !DESCRIPTION:
//
// The {\tt Component} implementation language is Fortran 90, but the
// routines which register function and data addresses to be used later
// in callback code must be implemented in C++.  These routines here
// allow the F90 to call the C++ support routines.
//
//
//EOP

// make a copy of a string, trim off trailing blanks, and make sure
// it's null terminated.  if phase > 0, add it as a 0 filled 3 digit
// number. this char string must be deleted when finished.
static void newtrim(char *c, int clen, int *phase, char **newc) {
     char *cp, *ctmp, *ctmpp;
     int hasphase = 0;
     int pad=4;

     //printf("in newtrim, c = '%s', clen = %d\n", c, clen);
     if ((phase != NULL) && (*phase > 0))  {
         pad = 8;
         hasphase++;
     }

     ctmp = new char[clen+pad];
     strncpy(ctmp, c, clen);
     (ctmp)[clen] = '\0';
     for (cp = &ctmp[clen-1]; *cp == ' '; cp--)   // trim() trailing blanks
         *cp = '\0';
   
     if (hasphase) {
         ctmpp = new char[strlen(ctmp) + pad];
         sprintf(ctmp, "%s%03d", ctmp, *phase);
         delete[] ctmp;
         *newc = ctmpp;
     } else
 	 *newc = ctmp;

     //printf("out newtrim, newc = '%s'\n", *newc);
     return;

}


// these interface subroutine names MUST be in lower case
extern "C" {

     // no need for explicit create methods - call the native class 
     // constructor and destructor methods directly.
     void FTN(c_esmc_ftablecreate)(ESMC_FTable **ptr, int *status) {
         (*ptr) = new ESMC_FTable;
         (*status) = (*ptr != NULL) ? ESMF_SUCCESS : ESMF_FAILURE;
     }

     void FTN(c_esmc_ftabledestroy)(ESMC_FTable **ptr, int *status) {
         delete (*ptr);
         *ptr = 0;
         *status = ESMF_SUCCESS;
     }
  
     // call a function 
     void FTN(c_esmc_ftablecallentrypoint)(ESMC_FTable **ptr, char *type, 
                                         int *phase, int *status, int slen) {
         int funcrc;
         char *name;

         newtrim(type, slen, phase, &name);
         //printf("after newtrim, name = '%s'\n", name);

         // TODO: two return codes here - one is whether we could find
         // the right function to call; the other is the actual return code
         // from the user function itself.

         *status = (*ptr)->ESMC_FTableCallVFuncPtr(name, &funcrc);

         delete[] name;
     }

     // get and set routines for both function and data pointers.
     // index them by name.
     void FTN(c_esmc_ftablegetentrypoint)(ESMC_FTable **ptr, char *type, 
                       void **func, enum ftype *ftype, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableGetFuncPtr(name, func, ftype);

         delete[] name;
     }

     void FTN(c_esmc_ftablegetinternalstate)(ESMC_FTable **ptr, char *type,
                       void **data, enum dtype *dtype, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableGetDataPtr(name, data, dtype);

         delete[] name;
     }

     void FTN(c_esmc_ftablesetentrypoint)(ESMC_FTable **ptr, char *type,
                                           void *func, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableSetFuncPtr(name, func);

         delete[] name;
     }

     void FTN(c_esmc_ftablesetargs)(ESMC_FTable **ptr, char *type,
                            int *acount, void **alist, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableSetFuncArgs(name, *acount, alist);

         delete[] name;
     }

     void FTN(c_esmc_ftablesetgridargs)(ESMC_FTable **ptr, char *type,
                         int *phase, void *comp, 
                         void *importstate, void *exportstate,
	                 void *clock, int *status, int slen) {

         char *fname;
         int acount = 5;
         void *alist[5];

         newtrim(type, slen, phase, &fname);
         //printf("after newtrim, name = '%s'\n", fname);

         alist[0] = (void *)comp;
         alist[1] = (void *)importstate;
         alist[2] = (void *)exportstate;
         alist[3] = (void *)clock;
         alist[4] = (void *)status;

         *status = (*ptr)->ESMC_FTableSetFuncArgs(fname, acount, alist);

         delete[] fname;
     }

     void FTN(c_esmc_ftablesetcplargs)(ESMC_FTable **ptr, char *type,
                                      int *phase, void *comp, 
                                      //void **statelist, void *clock, 
                                      void *statelist, void *clock, 
                                      int *status, int slen) {

         char *fname;
         int acount = 4;
         void *alist[4];

         newtrim(type, slen, phase, &fname);
         //printf("after newtrim, name = '%s'\n", fname);

         alist[0] = (void *)comp;
         //alist[1] = (void *)(*statelist);
         alist[1] = (void *)statelist;
         alist[2] = (void *)clock;
         alist[3] = (void *)status;

         *status = (*ptr)->ESMC_FTableSetFuncArgs(fname, acount, alist);

         delete[] fname;
     }


     void FTN(c_esmc_ftablesetinternalstate)(ESMC_FTable **ptr, char *type,
                        void *data, enum dtype *dtype, int *status, int slen) {
         char *name;

         newtrim(type, slen, NULL, &name);
         //printf("after newtrim, name = '%s'\n", name);

         *status = (*ptr)->ESMC_FTableSetDataPtr(name, data, *dtype);

         delete[] name;
     }

};


