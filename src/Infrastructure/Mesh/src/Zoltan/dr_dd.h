/*****************************************************************************
 * Zoltan Library for Parallel Applications                                  *
 * Copyright (c) 2000,2001,2002, Sandia National Laboratories.               *
 * For more info, see the README file in the top-level Zoltan directory.     *  
 *****************************************************************************/
/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: dr_dd.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/08/08 22:43:49 $
 *    Revision: 1.2 $
 ****************************************************************************/

#ifndef __DR_DD_H
#define __DR_DD_H

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
void destroy_elem_dd();
#endif

extern int build_elem_dd(MESH_INFO_PTR);
extern int update_elem_dd(MESH_INFO_PTR);
extern int update_hvertex_proc(MESH_INFO_PTR);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif  /* __DR_DD_H */
