// Earth System Modeling Framework
// Copyright 2002-2003, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the GPL.

// ESMC Array method implementation (body) file

//-----------------------------------------------------------------------------
//
// !DESCRIPTION:
//
// The code in this file implements the C++ Array Communication methods 
// declared in the companion file ESMC_ArrayComm.h.  
//
// The {\tt ESMF\_Array} object allows C++ to emulate the richer
// Fortran language Array operations.  It allows strided access, 
// subsetting operations, known dimension sizes, and typed access 
// to arrays instead of just a starting address to a block of memory.  
//
//-----------------------------------------------------------------------------
//

// for printf
#include <iostream.h>   // for cout
#include <stdio.h>
#include <string.h>
#include <assert.h>
// associated class definition file
#include "ESMC_Array.h"
#include "ESMC_DELayout.h"
#include "ESMC_Grid.h"        // grid info

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_ArrayComm.C,v 1.12 2004/04/28 23:11:47 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Array routines
//
//

//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayHalo - update the halo of an Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayHalo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *delayout,     // in  - layout (temporarily)
      ESMC_AxisIndex *ai_global, // in  - do we need?  jw
      int global_dimlengths[],   // in
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      ESMC_Logical periodic[] ) {  // in  - logical, size ESMF_MAXGRIDDIM 
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;

    printf("ESMC_ArrayHalo no longer supported; use ESMF_FieldHalo instead\n");
    return rc;

 } // end ESMC_ArrayHalo



//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayGather - gather a distributed Array onto 1 DE
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayGather(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *delayout,     // in  - layout (temporarily)
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      int global_dimlengths[],   // in  - array of global dimensions
      int local_maxlength[],     // in  - array of maximum counts on any DE per dim
      int deid,                  // in  - the DE to collect the data on
      ESMC_Array **Array_out) {  // out - new Array on all DE's with the global data
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    int thisde;
    int i_exc, j_exc;
    int counts[ESMF_MAXDIM];
    ESMC_Array *gathered;

//  allocate global-sized array on 1 DE and fill with distributed data
//  from each current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * global_dimlengths[i];
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);
      counts[i] = global_dimlengths[i];
    }

    delayout->ESMC_DELayoutGet(NULL, NULL, NULL, NULL, NULL, &thisde,
                                  NULL, NULL, NULL, NULL);

    // create array with global data buffer
    if (thisde == deid) {
        gathered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);

        // call something which will do a receive
 //jw       delayout->ESMC_DELayoutGatherArray(this->base_addr, global_dimlengths, 
 //jw                                             decompids, size_decomp, 
 //jw                                             // FIXME: localAxisCounts should be an arg
 //jw                                             NULL, local_maxlength,
 //jw                                             //localAxisCounts, local_maxlength,
 //jw                                             ai_comp, ai_total, 
 //jw                                             this->kind, gathered->base_addr);
        //gathered->ESMC_ArrayPrint();
    } else {
        // call something which will do a send
 //jw       delayout->ESMC_DELayoutGatherArray(this->base_addr, global_dimlengths, 
 //jw                                             decompids, size_decomp, 
 //jw                                             // FIXME: localAxisCounts should be an arg
 //jw                                             NULL, local_maxlength,
 //jw                                             //localAxisCounts, local_maxlength,
 //jw                                             ai_comp, ai_total, 
 //jw                                             this->kind, NULL);
      } 


    if (thisde == deid)
       *Array_out = gathered;
    else
       *Array_out = NULL;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayGather


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayScatter - scatter a single Array onto N distributed DEs
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *delayout,     // in  - layout (temporarily)
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      int deid,                  // in  - the DE the original Array is on
      ESMC_Array **Array_out) {  // out - new Array on all DE's with the global data
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    int thisde;
    int i_exc, j_exc;
    float *fp, *fp0;
    int *ip, *ip0;
    int counts[ESMF_MAXDIM];
    ESMC_Array *scattered;

#if 0
    // TODO: this is simply a copy of gather - it needs to be fleshed out
    // and completed.

//  allocate global-sized array on 1 DE and fill with distributed data
//  from each current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * ai_comp[i].max;
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);
      counts[i] = ai_comp[i].max;
    }

    delayout->ESMC_DELayoutGet(NULL, NULL, NULL, NULL, NULL, &thisde,
                                  NULL, NULL, NULL, NULL);

    // switch based on datatype  TODO: this might be a good place to use templates
    switch (this->type) {
      case ESMF_DATA_REAL:
        // create array with global data buffer
        if (thisde == deid) {
          scattered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);
          // allocate global array from this size
          fp = (float *)(scattered->base_addr);

          // call layoutscatter to fill this array
          fp0 = (float *)this->base_addr;

          // call something which will do a receive
          delayout->ESMC_DELayoutScatterArrayF(fp0, decompids, size_decomp, 
                                                  ai_comp, ai_total, fp);
        } else {
          // call something which will do a send
          delayout->ESMC_DELayoutScatterArrayF(fp0, decompids, size_decomp, 
                                                  ai_comp, ai_total, fp);
        } 

      break;

      case ESMF_DATA_INTEGER:
        // create array with global data
        if (thisde == deid) {
          scattered = ESMC_ArrayCreate(this->rank, this->type, this->kind, counts);
          // allocate global array from this size
          ip = (int *)(scattered->base_addr);

          // call layoutscatter to fill this array

          // call something which will do a receive
          ip0 = (int *)this->base_addr;
          delayout->ESMC_DELayoutScatterArrayI(ip0, decompids, size_decomp, 
                                                  ai_comp, ai_total, ip);
        } else {
          // call something which will do a send
          delayout->ESMC_DELayoutScatterArrayI(ip0, decompids, size_decomp, 
                                                  ai_comp, ai_total, ip);
        }
      break;
      default:
        printf("no code to handle data type %d yet\n", this->type);
      break;
    }

    //scattered->ESMC_ArrayPrint();

    *Array_out = scattered;
    rc = ESMF_SUCCESS;
#endif

    *Array_out = NULL;
    rc = ESMF_FAILURE;

    return rc;

 } // end ESMC_ArrayScatter


//-----------------------------------------------------------------------------
//BOP
// !IROUTINE:  ESMC_ArrayRedist - general redistribution of an Array
//
// !INTERFACE:
      int ESMC_Array::ESMC_ArrayRedist(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMC_DELayout *delayout,     // in  - layout (temporarily)
      int global_start[],        // in  - array of global starting positions
      int global_dimlengths[],   // in  - array of global dimensions
      int rank_trans[],          // in  - translation of old ranks to new
                                 //       Array
      int size_rank_trans,       // in  - size of rank_trans array
      int olddecompids[],        // in  - decomposition identifier for each
                                 //       axis for the original Array
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the redistributed Array
      int size_decomp,           // in  - size of decomp arrays
      ESMC_Array *RedistArray) { // out - Redistributed Array
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMF_FAILURE;
    int i, j, k, l, m;     // general counter vars
    void *vp;

//  allocate global-sized array on each DE and fill with distributed data
//  from current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * global_dimlengths[i];
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);  // jw?
    }

    // allocate global array from this size
    vp = (void *)(new char[gsize * ESMC_DataKindSize(this->kind)]);

    // call layoutgather to fill this array
//jw    delayout->ESMC_DELayoutGatherArray(this->base_addr, global_dimlengths, 
//jw                                          olddecompids, size_decomp, NULL, NULL,
//jw                                          this->ai_comp, this->ai_comp, 
//jw                                          this->kind, vp);

    // switch based on array rank
    switch (this->rank) {
      case 1:
        printf("no code to handle array rank %d yet\n", this->rank);
      break;
      case 2:
        {
         //  copy decomposed piece of global array into new Array
          int gmax[2];
          int lmax[2];
          int lstart[2];
          gmax[rank_trans[0]-1] = 1;
          for (i=1; i<this->rank; i++) {
            int i_new = rank_trans[i]-1;
            gmax[i_new] = global_dimlengths[i-1];
          }
          for (i=0; i<this->rank; i++) {
            lmax[i] = RedistArray->ai_comp[i].max 
                    - RedistArray->ai_comp[i].min + 1;
            lstart[i] = global_start[i] + RedistArray->ai_comp[i].min;
          }
          int *ip2 = (int *)RedistArray->base_addr;
          int *ip = (int *)vp;
          int local, global;
          for (j=0; j<lmax[1]; j++) {
            for (i=0; i<lmax[0]; i++) {
              local  = lmax[0]*j + i;
              global = gmax[1]*(j+lstart[1]) +
                       gmax[0]*(i+lstart[0]);
              ip2[local] = ip[global];
            }
          }
        }
      break;
      case 3:
        {
          //  copy decomposed piece of global array into new Array
          int gmax[3];
          int lmax[3];
          int lstart[3];
          gmax[rank_trans[0]-1] = 1;
          for (i=1; i<this->rank; i++) {
            int i_new = rank_trans[i]-1;
            gmax[i_new] = global_dimlengths[i-1];  
          }
          for (i=0; i<this->rank; i++) {
            lmax[i] = RedistArray->ai_total[i].max
                    - RedistArray->ai_total[i].min + 1;
            lstart[i] = global_start[i] + RedistArray->ai_total[i].min;
          }
          int *ip2 = (int *)RedistArray->base_addr;
          int *ip = (int *)vp;
          int local, global;
          for (k=0; k<lmax[2]; k++) {
            for (j=0; j<lmax[1]; j++) {
              for (i=0; i<lmax[0]; i++) {
                local  = lmax[1]*lmax[0]*k +
                         lmax[0]*j + i;
                global = gmax[2]*gmax[1]*(k+lstart[2]) + 
                         gmax[1]*(j+lstart[1]) +
                         gmax[0]*(i+lstart[0]);
                ip2[local] = ip[global];
              }
            }
          }
        }
      break;
      case 4:
        {
          // call allgatherv to fill this array or if Earl works out a method

          //  copy decomposed piece of global array into new Array
          int gmax[4];
          int lmax[4];
          int lstart[4];
          gmax[rank_trans[0]] = 1;
          for (i=1; i<this->rank; i++) {
            int i_new = rank_trans[i];
            gmax[i_new] = global_dimlengths[i-1];
          }
          for (i=0; i<this->rank; i++) {
            lmax[i] = RedistArray->ai_total[i].max
                    - RedistArray->ai_total[i].min + 1;
            lstart[i] = global_start[i] + RedistArray->ai_total[i].min;
          }
          int *ip2 = (int *)RedistArray->base_addr;
          int *ip = (int *)vp;
          int local, global;
          for (l=0; l<lmax[3]; l++) {
            for (k=0; k<lmax[2]; k++) {
              for (j=0; j<lmax[1]; j++) {
                for (i=0; i<lmax[0]; i++) {
                  local  = lmax[2]*lmax[1]*lmax[0]*l +
                           lmax[1]*lmax[0]*k + 
                           lmax[0]*j + i;
                  global = gmax[3]*gmax[2]*gmax[1]*(l+lstart[3]) +
                           gmax[2]*gmax[1]*(k+lstart[2]) + 
                           gmax[1]*(j+lstart[1]) +
                           gmax[0]*(i+lstart[0]);
                  ip2[local] = ip[global];
                }
              }
            }
          }
        }
      break;
      case 5:
        printf("no code to handle array rank %d yet\n", this->rank);
      break;
      default:
        printf("no code to handle array rank %d yet\n", this->rank);
      break;
    }

    // deallocate global array
    delete [] (char *)vp;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayRedist


