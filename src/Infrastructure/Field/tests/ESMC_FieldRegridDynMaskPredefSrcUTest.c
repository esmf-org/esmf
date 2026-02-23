// $Id$
//
// Earth System Modeling Framework
// Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
// Massachusetts Institute of Technology, Geophysical Fluid Dynamics
// Laboratory, University of Michigan, National Centers for Environmental
// Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
// NASA Goddard Space Flight Center.
// Licensed under the University of Illinois-NCSA License.
//
//==============================================================================

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ESMF header
#include "ESMC.h"

// ESMF Test header
#include "ESMC_Test.h"

//==============================================================================
//BOP
// !PROGRAM: ESMC_FieldGridGridRegridUTest - Check ESMC_FieldRegrid functionality
//
// !DESCRIPTION:
//
//EOP
//-----------------------------------------------------------------------------

int main() {

  // Test variables
  char name[80];
  char failMsg[80];
  int result = 0;
  int rc;

  // VM 
  ESMC_VM vm;
  int localPet, petCount, p;

  // variables to make grid and field
  int *maxIndex_src, *maxIndex_dst;
  ESMC_InterArrayInt i_maxIndex_src;
  ESMC_InterArrayInt i_maxIndex_dst;
  int dimcount = 2;
  enum ESMC_CoordSys_Flag coordsys = ESMC_COORDSYS_SPH_DEG;
  enum ESMC_TypeKind_Flag typekind = ESMC_TYPEKIND_R8;
  enum ESMC_IndexFlag indexflag = ESMC_INDEX_GLOBAL;
  int perdim = 1;
  int poledim = 2;
  double srcMaskValue_r8 = 2000.0;
  double def_value_r8=170;
  float srcMaskValue_r4 = 2000.0;
  float def_value_r4=170;
  ESMC_Grid grid_src, grid_dst;
  ESMC_Field field_src, field_dst;
  ESMC_RouteHandle routehandle;
  ESMC_DynamicMask dynamicMask;
  enum ESMC_DynamicMaskPredef_Flag predefFlag = ESMC_DYNAMICMASKPREDEF_SRC;
  int src_nx = 20;
  int src_ny = 20;
  int dst_nx = 27;
  int dst_ny = 27;

  int total_points;
  total_points = dst_nx*dst_ny;
  double xmin,xmax,ymin, ymax;
  double dx_src, dy_src;
  double dx_dst, dy_dst;
  dx_src = 360.0/(double)(src_nx);
  dy_src = 180.0/(double)(src_ny);
  dx_dst = 360.0/(double)(dst_nx);
  dy_dst = 180.0/(double)(dst_ny);

  ESMC_TestStart(__FILE__, __LINE__, 0);
  vm = ESMC_VMGetGlobal(&rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  rc = ESMC_VMGet(vm, &localPet, &petCount, NULL, NULL, NULL, NULL);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  maxIndex_src = (int *)malloc(dimcount*sizeof(int));
  maxIndex_dst = (int *)malloc(dimcount*sizeof(int));
  maxIndex_src[0] = src_nx;
  maxIndex_src[1] = src_ny;
  maxIndex_dst[0] = dst_nx;
  maxIndex_dst[1] = dst_ny;
  rc = ESMC_InterArrayIntSet(&i_maxIndex_src, maxIndex_src, dimcount);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  rc = ESMC_InterArrayIntSet(&i_maxIndex_dst, maxIndex_dst, dimcount);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  grid_src = ESMC_GridCreate1PeriDim(&i_maxIndex_src, NULL, &perdim, &poledim, &coordsys, &typekind, &indexflag, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  grid_dst = ESMC_GridCreate1PeriDim(&i_maxIndex_dst, NULL, &perdim, &poledim, &coordsys, &typekind, &indexflag, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  rc = ESMC_GridAddCoord(grid_src, ESMC_STAGGERLOC_CENTER);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  rc = ESMC_GridAddCoord(grid_dst, ESMC_STAGGERLOC_CENTER);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  int *exLBound_src = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_src = (int *)malloc(dimcount*sizeof(int));
  int *exLBound_dst = (int *)malloc(dimcount*sizeof(int));
  int *exUBound_dst = (int *)malloc(dimcount*sizeof(int));

  double *gridXCoord_src = (double *)ESMC_GridGetCoord(grid_src, 1,
    ESMC_STAGGERLOC_CENTER, NULL, exLBound_src, exUBound_src, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  double *gridYCoord_src = (double *)ESMC_GridGetCoord(grid_src, 2,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  xmin=-180.0;
  xmax=180.0-dx_src;
  ymin=-90.0;
  ymax=90.0;
  p = 0;
  dx_src = 360.0/(double)(src_nx-1);
  dy_src = 180.0/(double)(src_ny-1);
  for (int i1=exLBound_src[1]; i1<=exUBound_src[1]; ++i1) {
    for (int i0=exLBound_src[0]; i0<=exUBound_src[0]; ++i0) {
      if (exLBound_src[1] == i0) {
         gridXCoord_src[p] = xmin;
      }
      else if (exUBound_src[1] == i0) {
         gridXCoord_src[p] = xmax;
      }
      else {
         gridXCoord_src[p] = xmin + (double)(i0-1)*dx_src;
      }

      if (exLBound_src[0] == i1) {
         gridYCoord_src[p] = ymin;
      }
      else if (exUBound_src[0] == i1) {
         gridYCoord_src[p] = ymax;
      }
      else {
         gridYCoord_src[p] = ymin + (double)(i1-1)*dy_src;
      }
      ++p;
    }
  }

  double *gridXCoord_dst = (double *)ESMC_GridGetCoord(grid_dst, 1,
    ESMC_STAGGERLOC_CENTER, NULL, exLBound_dst, exUBound_dst, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  double *gridYCoord_dst = (double *)ESMC_GridGetCoord(grid_dst, 2,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  xmin=-180.0;
  xmax=180.0-dx_dst;
  ymin=-90.0;
  ymax=90.0;
  dx_dst = 360.0/(double)(dst_nx-1);
  dy_dst = 180.0/(double)(dst_ny-1);
  p = 0;
  for (int i1=exLBound_dst[1]; i1<=exUBound_dst[1]; ++i1) {
    for (int i0=exLBound_dst[0]; i0<=exUBound_dst[0]; ++i0) {
      if (exLBound_dst[1] == i0) {
         gridXCoord_dst[p] = xmin;
      }
      else if (exUBound_dst[1] == i0) {
      
         gridXCoord_dst[p] = xmax;
      }
      else {
         gridXCoord_dst[p] = xmin + (double)(i0-1)*dx_dst;
      }

      if (exLBound_dst[0] == i1) {
         gridYCoord_dst[p] = ymin;
      }
      else if (exUBound_dst[0] == i1) {
         gridYCoord_dst[p] = ymax;
      }
      else {
         gridYCoord_dst[p] = ymin + (double)(i1-1)*dy_dst;
      }
      ++p;
    }
  }

  field_src = ESMC_FieldCreateGridTypeKind(grid_src, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  field_dst = ESMC_FieldCreateGridTypeKind(grid_dst, ESMC_TYPEKIND_R8,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstfield", &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  //----------------------------------------------------------------------------
  // NEX_UTest
  strcpy(name, "Test R8R8R8 dynamic mask");
  strcpy(failMsg, "R8R8R8 dynamic masking was not correctly applied");

  double * srcfieldptr_r8 = (double *)ESMC_FieldGetPtr(field_src, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  double * dstfieldptr_r8 = (double *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  p = 0;
  for (int i1=exLBound_src[1]; i1<=exUBound_src[1]; ++i1) {
    for (int i0=exLBound_src[0]; i0<=exUBound_src[0]; ++i0) {
		 if (i1 < exUBound_src[1]/3) {
			 srcfieldptr_r8[p] = def_value_r8; }
		 else { 
			srcfieldptr_r8[p] = srcMaskValue_r8;}
		++p;
    }
  }
  p = 0;
  for (int i1=exLBound_dst[1]; i1<=exUBound_dst[1]; ++i1) {
    for (int i0=exLBound_dst[0]; i0<=exUBound_dst[0]; ++i0) {
      dstfieldptr_r8[p] = -10.0;
      ++p;
    }
  }

  int handleAllElem = 1;
  rc = ESMC_DynamicMaskPredefinedSetR8R8R8(&dynamicMask, predefFlag, &handleAllElem, &srcMaskValue_r8, NULL);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  enum ESMC_RegridMethod_Flag regridmethod = ESMC_REGRIDMETHOD_BILINEAR;
  enum ESMC_PoleMethod_Flag polemethod = ESMC_POLEMETHOD_ALLAVG;
  enum ESMC_LineType_Flag linetype = ESMC_LINETYPE_GREAT_CIRCLE;
  enum ESMC_UnmappedAction_Flag unmappedaction = ESMC_UNMAPPEDACTION_ERROR;

  int srcTermProcessing = 0;
  rc = ESMC_FieldRegridStore(field_src, field_dst, NULL, NULL, &routehandle,
                             &regridmethod, &polemethod, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, &srcTermProcessing, NULL, NULL,
                             NULL, NULL, NULL);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  srcfieldptr_r8 = (double *)ESMC_FieldGetPtr(field_src, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  p = 0;
  for (int i1=exLBound_src[1]; i1<=exUBound_src[1]; ++i1) {
    for (int i0=exLBound_src[0]; i0<=exUBound_src[0]; ++i0) {
		 if (i1 < exUBound_src[1]/3) {
			 srcfieldptr_r8[p] = def_value_r8; }
		 else { 
			srcfieldptr_r8[p] = srcMaskValue_r8;}
		++p;
    }
  }
  rc = ESMC_FieldRegrid(field_src,field_dst,routehandle,NULL,&dynamicMask);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  dstfieldptr_r8 = (double *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  int count_def = 0;
  int count_undef = 0;
  double delta = 0.00001;
  dstfieldptr_r8 = (double *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  p = 0;
  for (int i1=exLBound_dst[1]; i1<=exUBound_dst[1]; ++i1) {
	  for (int i0=exLBound_dst[0]; i0<=exUBound_dst[0]; ++i0) {
        if (def_value_r8 - delta < dstfieldptr_r8[p] & dstfieldptr_r8[p] < def_value_r8 + delta) {count_def=count_def+1;}
        if (srcMaskValue_r8 - delta < dstfieldptr_r8[p] & dstfieldptr_r8[p] < srcMaskValue_r8 + delta) {count_undef=count_undef+1;}
		  ++p;
	  }
  }

  ESMC_Test((count_def+count_undef == dst_nx*dst_ny), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // NEX_UTest
  strcpy(name, "Test R8R8R8V dynamic mask");
  strcpy(failMsg, "R8R8R8V dynamic masking was not correctly applied");

  srcfieldptr_r8 = (double *)ESMC_FieldGetPtr(field_src, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  p = 0;
  for (int i1=exLBound_src[1]; i1<=exUBound_src[1]; ++i1) {
    for (int i0=exLBound_src[0]; i0<=exUBound_src[0]; ++i0) {
		 if (i1 < exUBound_src[1]/3) {
			 srcfieldptr_r8[p] = def_value_r8; }
		 else { 
			srcfieldptr_r8[p] = srcMaskValue_r8;}
		++p;
    }
  }
  rc = ESMC_DynamicMaskPredefinedSetR8R8R8V(&dynamicMask, predefFlag, &handleAllElem, &srcMaskValue_r8, NULL);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  rc = ESMC_FieldRegrid(field_src,field_dst,routehandle,NULL,&dynamicMask);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  dstfieldptr_r8 = (double *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  count_def = 0;
  count_undef = 0;
  dstfieldptr_r8 = (double *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  p = 0;
  for (int i1=exLBound_dst[1]; i1<=exUBound_dst[1]; ++i1) {
	  for (int i0=exLBound_dst[0]; i0<=exUBound_dst[0]; ++i0) {
        if (def_value_r8 - delta < dstfieldptr_r8[p] & dstfieldptr_r8[p] < def_value_r8 + delta) {count_def=count_def+1;}
        if (srcMaskValue_r8 - delta < dstfieldptr_r8[p] & dstfieldptr_r8[p] < srcMaskValue_r8 + delta) {count_undef=count_undef+1;}
		  ++p;
	  }
  }
  ESMC_Test((count_def+count_undef == dst_nx*dst_ny), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  // Now test R4
  rc =ESMC_FieldRegridRelease(&routehandle);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  rc = ESMC_FieldDestroy(&field_src);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  rc = ESMC_FieldDestroy(&field_dst);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  field_src = ESMC_FieldCreateGridTypeKind(grid_src, ESMC_TYPEKIND_R4,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "srcfield", &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  field_dst = ESMC_FieldCreateGridTypeKind(grid_dst, ESMC_TYPEKIND_R4,
    ESMC_STAGGERLOC_CENTER, NULL, NULL, NULL, "dstfield", &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  //----------------------------------------------------------------------------
  // NEX_UTest
  strcpy(name, "Test R4R8R4 dynamic mask");
  strcpy(failMsg, "R4R8R4 dynamic masking was not correctly applied");

  float * srcfieldptr_r4 = (float*)ESMC_FieldGetPtr(field_src, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  float * dstfieldptr_r4 = (float*)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  p = 0;
  for (int i1=exLBound_src[1]; i1<=exUBound_src[1]; ++i1) {
    for (int i0=exLBound_src[0]; i0<=exUBound_src[0]; ++i0) {
		 if (i1 < exUBound_src[1]/3) {
			 srcfieldptr_r4[p] = def_value_r4; }
		 else { 
			srcfieldptr_r4[p] = srcMaskValue_r4;}
		++p;
    }
  }
  p = 0;
  for (int i1=exLBound_dst[1]; i1<=exUBound_dst[1]; ++i1) {
    for (int i0=exLBound_dst[0]; i0<=exUBound_dst[0]; ++i0) {
      dstfieldptr_r4[p] = -10.0;
      ++p;
    }
  }

  rc = ESMC_DynamicMaskPredefinedSetR4R8R4(&dynamicMask, predefFlag, &handleAllElem, &srcMaskValue_r4, NULL);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  srcTermProcessing = 0;
  rc = ESMC_FieldRegridStore(field_src, field_dst, NULL, NULL, &routehandle,
                             &regridmethod, &polemethod, NULL, NULL, NULL,
                             NULL, NULL, NULL, NULL, NULL,
                             NULL, NULL, &srcTermProcessing, NULL, NULL,
                             NULL, NULL, NULL);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  srcfieldptr_r4 = (float *)ESMC_FieldGetPtr(field_src, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  p = 0;
  for (int i1=exLBound_src[1]; i1<=exUBound_src[1]; ++i1) {
    for (int i0=exLBound_src[0]; i0<=exUBound_src[0]; ++i0) {
		 if (i1 < exUBound_src[1]/3) {
			 srcfieldptr_r4[p] = def_value_r4; }
		 else { 
			srcfieldptr_r4[p] = srcMaskValue_r4;}
		++p;
    }
  }
  rc = ESMC_FieldRegrid(field_src,field_dst,routehandle,NULL,&dynamicMask);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  dstfieldptr_r4 = (float *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  count_def = 0;
  count_undef = 0;
  delta = 0.0001;
  dstfieldptr_r4= (float *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  p = 0;
  for (int i1=exLBound_dst[1]; i1<=exUBound_dst[1]; ++i1) {
	  for (int i0=exLBound_dst[0]; i0<=exUBound_dst[0]; ++i0) {
        if (def_value_r4 - delta < dstfieldptr_r4[p] & dstfieldptr_r4[p] < def_value_r4 + delta) {count_def=count_def+1;}
        if (srcMaskValue_r4 - delta < dstfieldptr_r4[p] & dstfieldptr_r4[p] < srcMaskValue_r4 + delta) {count_undef=count_undef+1;}
		  ++p;
	  }
  }
  ESMC_Test((count_def+count_undef == dst_nx*dst_ny), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  //----------------------------------------------------------------------------
  // NEX_UTest
  strcpy(name, "Test R4R8R4V dynamic mask");
  strcpy(failMsg, "R4R8R4V dynamic masking was not correctly applied");

  rc = ESMC_DynamicMaskPredefinedSetR4R8R4V(&dynamicMask, predefFlag, &handleAllElem, &srcMaskValue_r4, NULL);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  p = 0;
  for (int i1=exLBound_src[1]; i1<=exUBound_src[1]; ++i1) {
    for (int i0=exLBound_src[0]; i0<=exUBound_src[0]; ++i0) {
		 if (i1 < exUBound_src[1]/3) {
			 srcfieldptr_r4[p] = def_value_r4; }
		 else { 
			srcfieldptr_r4[p] = srcMaskValue_r4;}
		++p;
    }
  }
  rc = ESMC_FieldRegrid(field_src,field_dst,routehandle,NULL,&dynamicMask);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  dstfieldptr_r4 = (float *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);

  count_def = 0;
  count_undef = 0;
  delta = 0.0001;
  dstfieldptr_r4= (float *)ESMC_FieldGetPtr(field_dst, 0, &rc);
  if (ESMC_LogMsgFoundError(rc, "Checking rc", __LINE__, __FILE__, "main()", NULL))
    ESMC_FinalizeWithFlag(ESMC_END_ABORT);
  p = 0;
  for (int i1=exLBound_dst[1]; i1<=exUBound_dst[1]; ++i1) {
	  for (int i0=exLBound_dst[0]; i0<=exUBound_dst[0]; ++i0) {
        if (def_value_r4 - delta < dstfieldptr_r4[p] & dstfieldptr_r4[p] < def_value_r4 + delta) {count_def=count_def+1;}
        if (srcMaskValue_r4 - delta < dstfieldptr_r4[p] & dstfieldptr_r4[p] < srcMaskValue_r4 + delta) {count_undef=count_undef+1;}
		  ++p;
	  }
  }
  ESMC_Test((count_def+count_undef == dst_nx*dst_ny), name, failMsg, &result, __FILE__, __LINE__, 0);
  //----------------------------------------------------------------------------

  ESMC_TestEnd(__FILE__, __LINE__, 0);
  return 0;
}
