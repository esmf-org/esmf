/*****************************************************************************
 * CVS File Information :
 *    $RCSfile: migtags_const.h,v $
 *    $Author: dneckels $
 *    $Date: 2007/08/08 22:43:53 $
 *    Revision: 1.6 $
 ****************************************************************************/

#ifndef __MIGTAGS_CONST_H
#define __MIGTAGS_CONST_H

#ifdef __cplusplus
/* if C++, define the rest of this header file as extern C */
extern "C" {
#endif


extern int Zoltan_Oct_migrate_objects(ZZ *zz, pOctant *octs, int *newpids, int nocts,
			       int *nsentags, 
			       pRegion *import_tags, int *nrectags, 
			       float *c2, float *c3, int *counter3, 
			       int *counter4);

extern int Zoltan_Oct_fix_tags(ZZ *, ZOLTAN_ID_PTR *, ZOLTAN_ID_PTR *,
                        int **, int **, int, pRegion);

#ifdef __cplusplus
} /* closing bracket for extern "C" */
#endif

#endif /* __MIGTAGS_CONST_H */
