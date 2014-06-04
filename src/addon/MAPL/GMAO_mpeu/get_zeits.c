 /*  +-======-+ 
  Copyright (c) 2003-2007 United States Government as represented by 
  the Admistrator of the National Aeronautics and Space Administration.  
  All Rights Reserved.
  
  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
  
  Government Agency: National Aeronautics and Space Administration
  Government Agency Original Software Designation: GSC-15354-1
  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
  Government Agency Point of Contact for Original Software:  
  			Dale Hithon, SRA Assistant, (301) 286-2691
  
 +-======-+   */
/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !ROUTINE: get_zeits - a C interface to times for Fortran calls
!
! !DESCRIPTION:
!
! !INTERFACE: */
 /*
  System times() dependencies:
 */


#include <sys/types.h>
#include <sys/times.h>

#include <time.h>             /* CLK_TCK is usually here */

#if !defined(CLK_TCK)
#  include <limits.h>         /* if not, try here */
#endif

#if !defined(CLK_TCK)
#define CLK_TCK  CLOCKS_PER_SEC
#endif


/*
Kept the difference for reference.
=======
#if defined(__osf__) || defined(sysAIX)
#  include <time.h>
#else
#  include <limits.h>
>>>>>>> 1.1.2.2
*/

 /*
  The default is FORTRAN_UNDERSCORE_, but not explicitly used.
 */

#ifdef _UNICOS
#  define FORTRAN_CAPS_
#endif

#ifdef FORTRAN_CAPS_
#  define	get_zeits_		GET_ZEITS
#  define	get_ztick_		GET_ZTICK
#endif

#ifdef FORTRAN_SAME
#  define	get_zeits_		get_zeits
#  define	get_ztick_		get_ztick
#endif


 /*  Prototype: */

   void get_zeits_(double *zts);
   void get_ztick_(double *tic);

/*!REVISION HISTORY:
! 	12Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
! 	06Jul99 - J.W. Larson <jlarson@dao> - support for AIX platform
!EOP */

/*  Implementations: */

void get_zeits_(double *zts)
{

  struct tms tm;
  double secs;

  secs=1./CLK_TCK;

  zts[0]=times(&tm)*secs;
  zts[1]=tm.tms_utime*secs;
  zts[2]=tm.tms_stime*secs;
  zts[3]=tm.tms_cutime*secs;
  zts[4]=tm.tms_cstime*secs;

}

void get_ztick_(double *tic)
{
  tic[0]=1./CLK_TCK;
}
