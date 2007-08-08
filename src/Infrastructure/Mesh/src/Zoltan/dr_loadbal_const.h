/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: dr_loadbal_const.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/08/08 22:43:50 $
 *    Revision: 1.18 $
 ****************************************************************************/


#ifndef _DR_LOADBAL_CONST_H_
#define _DR_LOADBAL_CONST_H_

#include "dr_input_const.h"

#ifdef __cplusplus

  #include "zoltan_cpp.h"

  extern "C" {

#else

#endif

#define ZOLTAN_STRUCT struct Zoltan_Struct *

extern int setup_zoltan(ZOLTAN_STRUCT, int, PROB_INFO_PTR, MESH_INFO_PTR); 

extern int run_zoltan(ZOLTAN_STRUCT, int, PROB_INFO_PTR, MESH_INFO_PTR,
                      PARIO_INFO_PTR); 

extern int migrate_elements(int, MESH_INFO_PTR, ZOLTAN_STRUCT,
                            int, int, 
                            int, ZOLTAN_ID_PTR, ZOLTAN_ID_PTR, int *, int *,
                            int, ZOLTAN_ID_PTR, ZOLTAN_ID_PTR, int *, int *);

extern ELEM_INFO *search_by_global_id(MESH_INFO *, int, int *);

extern ZOLTAN_OBJ_SIZE_FN migrate_elem_size;
extern ZOLTAN_OBJ_SIZE_MULTI_FN migrate_elem_size_multi;

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif /* _DR_LOADBAL_CONST_H_ */
