// $Id: ESMC_InternArrayComm.C,v 1.11.2.2 2009/01/21 21:25:22 cdeluca Exp $
// Earth System Modeling Framework
// Copyright 2002-2009, University Corporation for Atmospheric Research, 
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
// Laboratory, University of Michigan, National Centers for Environmental 
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.

// ESMC Array method implementation (body) file

#define ESMF_FILENAME "ESMF_InternArrayComm.C"
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
#include <stdio.h>
#include <string.h>
#include <assert.h>
// associated class definition file
#include "ESMC_InternArray.h"
#include "ESMC_LogErr.h"
#include "ESMC_DELayout.h"
#include "ESMC_InternGrid.h"        // igrid info

//-----------------------------------------------------------------------------
 // leave the following line as-is; it will insert the cvs ident string
 // into the object file for tracking purposes.
 static const char *const version = 
            "$Id: ESMC_InternArrayComm.C,v 1.11.2.2 2009/01/21 21:25:22 cdeluca Exp $";
//-----------------------------------------------------------------------------

//
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
//
// This section includes all the Array routines
//
//

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayHalo"
//BOP
// !IROUTINE:  ESMC_ArrayHalo - update the halo of an Array
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayHalo(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMCI::DELayout *delayout, // in  - layout (temporarily)
      ESMC_AxisIndex *ai_global, // in  - do we need?  jw
      int global_dimlengths[],   // in
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      ESMC_Logical periodic[] ) {  // in  - logical, size ESMF_MAXIGRIDDIM 
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMC_RC_NOT_IMPL;

    ESMC_LogDefault.ESMC_LogMsgFoundError(rc,
    "ESMC_ArrayHalo no longer supported; use ESMF_FieldHalo instead\n", &rc);

    return rc;

 } // end ESMC_ArrayHalo



#if 1
//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_newDELayoutGatherArray"
//BOP
// !IROUTINE:  ESMC_DELayoutGatherArray - all gather a distributed array
//
// !INTERFACE:
//      int ESMC_newDELayout::ESMC_newDELayoutGatherArray(
static int ESMC_newDELayoutGatherArray(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMCI::DELayout *delayout,
      int root,
      void *DistArray,           // in  - distributed array
      int global_dimlength[],    // in
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the array
      int size_decomp,           // in  - size of decomp arrays
      int localDimCounts[],      // in  -
      int localMaxDimCount[],    // in  -
      ESMC_AxisIndex *AIPtr,     // in  - pointer to array of AxisIndex
                                 //       structures for exclusive data
      ESMC_AxisIndex *AIPtr2,    // in  - pointer to array of AxisIndex
                                 //       structures for total data
      ESMC_TypeKind datatype,    // in  - real, integer, *4, *8
      void *GlobalArray) {       // out - global array
//
// !DESCRIPTION:
//    returns an array of AxisIndex types representing the decomposition of
//    an arbitrary number of axis by a layout
//
//EOP

  int rc = ESMC_RC_NOT_IMPL;
  int i, j, k, l, m;     // general counter vars

  // get layout size
  int nx, ny, nde, ncount[2];

  // TODO: remove dependency on deprecated DELayout  
  delayout->getDeprecated(&nde, NULL, NULL, NULL, 0, NULL,
                            NULL, NULL, ncount, 2);
  nx = ncount[0];
  ny = ncount[1];
  
  int rankx, ranky;
  int bytesperitem = ESMC_TypeKindSize(datatype);
 

  // switch based on array rank
  switch (size_decomp) {
    case 2:
      {
        // figure out which ranks are decomposed and figure out the
        // number of separate data chunks per rank and size of data
        // chunks
        int rmax[2];
        int rsize[2];
        int rsize_tot[2];
        int rskip[2];
        int rbreak[1];
        int rbcount = 0;
        for (i=0; i<size_decomp; i++) {
          rmax[i] = global_dimlength[i];
          rsize[i] = AIPtr[i].max - AIPtr[i].min + 1;
          rsize_tot[i] = AIPtr2[i].max - AIPtr2[i].min + 1;
          if (decompids[i] == 0) {
            rbreak[rbcount]=i;
            rbcount++;
          }
          if (decompids[i] == 1) {
            rankx = i;
          }
          if (decompids[i] == 2) {
            ranky = i;
            rbreak[rbcount]=i;
            rbcount++;
          }
        }
        rskip[0] = 1;
        for (i=1; i<size_decomp; i++)  
          rskip[i] = rskip[i-1]*rmax[i-1];
         
        // loop over ranks, skipping the first decomposed one, loading
        // up chunks of data to gather
        int k, j_tot, jMax, displsX, displsY;
        void *sendbuf, *recvbuf;
        int sendcount;
        int* recvcounts = new int[nde];
        int* displs = new int[nde];
        if (localMaxDimCount == NULL) {
          jMax = rsize[rbreak[0]];
        } else {
          jMax = localMaxDimCount[rbreak[0]];
        }
        for (int j=0; j<jMax; j++) {
          j_tot = j + AIPtr[ranky].min;
          sendbuf = DistArray;  // void * at this point, cast to char * to increment
          sendbuf = (void *)((char *)sendbuf + 
                        (j_tot*rsize_tot[rankx] + AIPtr[rankx].min) * bytesperitem);
  
          sendcount = rsize[rankx];
          if (j >= rsize[rbreak[0]]) 
            sendcount = 0;
          
          recvbuf = GlobalArray;   // ditto comment above
          recvbuf = (void *)((char *)recvbuf + (j*rmax[rankx]) * bytesperitem);
          displsY = 0;
          for (int ky=0; ky<ny; ky++) {
            displsX = 0;
            for (int kx=0; kx<nx; kx++) {
              k = ky*nx + kx;
              if (localDimCounts == NULL) 
                recvcounts[k] = rsize[rankx];
              else {
                recvcounts[k] = localDimCounts[rankx*nde + k];
                if (j >= localDimCounts[ranky*nde + k]) recvcounts[k] = 0;
              }
              displs[k] = displsX + displsY;
              displsX  += recvcounts[k];
            }
            if (localDimCounts == NULL) 
              displsY += rsize[ranky] * rskip[ranky];
            else 
              displsY += localDimCounts[ranky*nde + k] * rskip[ranky];
            
          }
          // call layout gather routine
          delayout->ESMC_DELayoutGatherV((void *)sendbuf, (void *)recvbuf,
            recvcounts, displs, datatype, root);
          //comm.ESMC_CommAllGatherV(sendbuf, sendcount, recvbuf, recvcounts, 
          //                         displs, datatype);
        }
        delete [] recvcounts;
        delete [] displs;
      }
    break;
    case 3:
      {
        // figure out which ranks are decomposed and figure out the
        // number of separate data chunks per rank and size of data
        // chunks
        int rmax[3];
        int rsize[3];
        int rskip[3];
        int rbreak[2];
        int rbcount = 0;
        for (i=0; i<size_decomp; i++) {
          rmax[i] = global_dimlength[i];
          rsize[i] = AIPtr[i].max - AIPtr[i].min + 1;
          if (decompids[i] == 0) {
            rbreak[rbcount]=i;
            rbcount++;
          }
          if (decompids[i] == 1) {
            rankx = i;
          }
          if (decompids[i] == 2) {
            ranky = i;
            rbreak[rbcount]=i;
            rbcount++;
          }
        }
        rskip[0] = 1;
        for (i=1; i<size_decomp; i++) {
          rskip[i] = rskip[i-1]*rmax[i-1];
        }
        // loop over ranks, skipping the first decomposed one, loading
        // up chunks of data to gather
        int k, iMax, jMax, displsX, displsY;
        void *sendbuf, *recvbuf;
        int sendcount;
        int* recvcounts = new int[nde];
        int* displs = new int[nde];
        if (localMaxDimCount == NULL) {
          iMax = rsize[rbreak[1]];
        } else {
          iMax = localMaxDimCount[rbreak[1]];
        }
        if (localMaxDimCount == NULL) {
          jMax = rsize[rbreak[0]];
        } else {
          jMax = localMaxDimCount[rbreak[0]];
        }
        for (i=0; i<iMax; i++) {
          for (int j=0; j<jMax; j++) {
            sendbuf = DistArray;  // void * must be cast to char * to increment
            sendbuf = (void *)((char *)sendbuf + 
                    (j*rsize[rankx] + i*rsize[rankx]*rsize[rbreak[0]]) * bytesperitem);
            sendcount = rsize[rankx];
            if (i >= rsize[rbreak[1]] || j >= rsize[rbreak[0]]) {
              sendcount = 0;
            }
            recvbuf = GlobalArray;   // ditto comment above
            recvbuf = (void *)((char *)recvbuf + 
                       (j*rmax[rankx] + i*rmax[rankx]*rmax[rbreak[0]]) * bytesperitem);
            displsY = 0;
            for (int ky=0; ky<ny; ky++) {
              displsX = 0;
              for (int kx=0; kx<nx; kx++) {
                k = ky*nx + kx;
                if (localDimCounts == NULL) {
                  recvcounts[k] = rsize[rankx];
                } else {
                  recvcounts[k] = localDimCounts[rankx*nde + k];
                  if (j >= localDimCounts[ranky*nde + k]) recvcounts[k] = 0;
                }
                displs[k] = displsX + displsY;
                displsX  += recvcounts[k];
              }
              if (localDimCounts == NULL) {
                displsY += rsize[ranky] * rskip[ranky];
              } else {
                displsY += localDimCounts[ranky*nde + k] * rskip[ranky];
              }
            }
          // call layout gather routine
          //comm.ESMC_CommAllGatherV(sendbuf, sendcount, recvbuf, recvcounts, 
//                                   displs, datatype);
          }
        }
        delete [] recvcounts;
        delete [] displs;
      }
      break;
    default:
      printf("no code to handle array rank %d yet\n", size_decomp);
      break;
  }

  rc = ESMF_SUCCESS;
  return rc;

 } // end ESMC_newDELayoutGatherArray
#endif

//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayGather"
//BOP
// !IROUTINE:  ESMC_ArrayGather - gather a distributed Array onto 1 DE
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayGather(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMCI::DELayout *delayout,     // in  - layout (temporarily)
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      int local_axislengths[],   // in
      int global_dimlengths[],   // in  - array of global dimensions
      int local_maxlength[],     // in  - array of maximum counts on any DE per dim
      int deid,                  // in  - the DE to collect the data on
      ESMC_InternArray **Array_out) {  // out - new Array on all DE's with the global data
//
// !DESCRIPTION:
//     
//
//EOP

    int rc = ESMC_RC_NOT_IMPL;
    int i, j, k, l, m;     // general counter vars
    int thisde;
    int i_exc, j_exc;
    int counts[ESMF_MAXDIM];
    ESMC_InternArray *gathered;

//  allocate global-sized array on 1 DE and fill with distributed data
//  from each current Array
    int gsize=1;
    int lsize=1;
    for (i=0; i<rank; i++) {
      gsize = gsize * global_dimlengths[i];
      lsize = lsize * (ai_comp[i].max - ai_comp[i].min+1);
      counts[i] = global_dimlengths[i];
    }

    // TODO: remove dependency on deprecated DELayout  
    delayout->getDeprecated(NULL, NULL, NULL, NULL, 0, &thisde,
                                  NULL, NULL, NULL, 0);

    // create array with global data buffer
    //printf("arraygather: %d, %d\n", thisde, deid);
    if (thisde == deid) {
      //printf("arraygather: I am root\n");
        gathered = ESMC_InternArrayCreate(this->rank, this->kind, counts);

        // call something which will do a receive
        ESMC_newDELayoutGatherArray(delayout, deid, this->base_addr,
                                    global_dimlengths, decompids, size_decomp, 
                                    local_axislengths, local_maxlength,
                                    ai_comp, ai_total, 
                                    this->kind, gathered->base_addr);
        //gathered->ESMC_ArrayPrint();
    } else {
      //printf("arraygather: I am not root\n");
        // call something which will do a send
       ESMC_newDELayoutGatherArray(delayout, deid, this->base_addr,
                                   global_dimlengths, decompids, size_decomp, 
                                   local_axislengths, local_maxlength,
                                   ai_comp, ai_total, this->kind, NULL);
    }

    if (thisde == deid)
       *Array_out = gathered;
    else
       *Array_out = NULL;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayGather


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayScatter"
//BOP
// !IROUTINE:  ESMC_ArrayScatter - scatter a single Array onto N distributed DEs
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayScatter(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMCI::DELayout *delayout,     // in  - layout (temporarily)
      int decompids[],           // in  - decomposition identifier for each
                                 //       axis for the Array
      int size_decomp,           // in  - size of decomp array
      int deid,                  // in  - the DE the original Array is on
      ESMC_InternArray **Array_out) {  // out - new Array on all DE's with the global data
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMC_RC_NOT_IMPL;
    int i, j, k, l, m;     // general counter vars
    int thisde;
    int i_exc, j_exc;
    ESMC_R4 *fp, *fp0;
    int *ip, *ip0;
    int counts[ESMF_MAXDIM];
    ESMC_InternArray *scattered;

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

    // TODO: remove dependency on deprecated DELayout
    delayout->getDeprecated(NULL, NULL, NULL, NULL, 0, &thisde,
                                  NULL, NULL, NULL, 0);

    // switch based on datatype  TODO: this might be a good place to use templates
    switch (this->kind) {
       case ESMF_TYPEKIND_R4:
        // create array with global data buffer
        if (thisde == deid) {
          scattered = ESMC_InternArrayCreate(this->rank, this->kind, counts);
          // allocate global array from this size
          fp = (ESMC_R4 *)(scattered->base_addr);

          // call layoutscatter to fill this array
          fp0 = (ESMC_R4 *)this->base_addr;

          // call something which will do a receive
          delayout->ESMC_DELayoutScatterArrayF(fp0, decompids, size_decomp, 
                                                  ai_comp, ai_total, fp);
        } else {
          // call something which will do a send
          delayout->ESMC_DELayoutScatterArrayF(fp0, decompids, size_decomp, 
                                                  ai_comp, ai_total, fp);
        } 

      case ESMF_TYPEKIND_I4:
        // create array with global data
        if (thisde == deid) {
          scattered = ESMC_InternArrayCreate(this->rank, this->kind, counts);
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
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
                                          "unsupported data type", &rc);
        //printf("no code to handle TypeKind %d yet\n", this->kind);
        return (rc);
    }

    //scattered->ESMC_ArrayPrint();

    *Array_out = scattered;
    rc = ESMF_SUCCESS;
#endif

    *Array_out = NULL;
    rc = ESMC_RC_NOT_IMPL;

    return rc;

 } // end ESMC_ArrayScatter


//-----------------------------------------------------------------------------
#undef  ESMC_METHOD
#define ESMC_METHOD "ESMC_ArrayRedist"
//BOP
// !IROUTINE:  ESMC_ArrayRedist - general redistribution of an Array
//
// !INTERFACE:
      int ESMC_InternArray::ESMC_ArrayRedist(
//
// !RETURN VALUE:
//    int error return code
//
// !ARGUMENTS:
      ESMCI::DELayout *delayout,     // in  - layout (temporarily)
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
      ESMC_InternArray *RedistArray) { // out - Redistributed Array
//
// !DESCRIPTION:
//      
//     
//
//EOP
// !REQUIREMENTS:  XXXn.n, YYYn.n

    int rc = ESMC_RC_NOT_IMPL;
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
    vp = (void *)(new char[gsize * ESMC_TypeKindSize(this->kind)]);

    // call layoutgather to fill this array
//jw    delayout->ESMC_DELayoutGatherArray(this->base_addr, global_dimlengths, 
//jw                                          olddecompids, size_decomp, NULL, NULL,
//jw                                          this->ai_comp, this->ai_comp, 
//jw                                          this->kind, vp);

    // switch based on array rank
    switch (this->rank) {
      case 1:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
                                          "unsupported data rank", &rc);
        //printf("no code to handle array rank %d yet\n", this->rank);
        return (rc);
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
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
                                          "unsupported data rank", &rc);
        //printf("no code to handle array rank %d yet\n", this->rank);
        return (rc);
      break;
      default:
        ESMC_LogDefault.ESMC_LogMsgFoundError(ESMC_RC_NOT_IMPL,
                                          "unsupported data rank", &rc);
        //printf("no code to handle array rank %d yet\n", this->rank);
        return (rc);
      break;
    }

    // deallocate global array
    delete [] (char *)vp;

    rc = ESMF_SUCCESS;
    return rc;

 } // end ESMC_ArrayRedist


