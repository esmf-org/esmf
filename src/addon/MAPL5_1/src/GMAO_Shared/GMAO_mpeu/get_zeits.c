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
  System times() and sysconf() dependencies:
 */

#include <sys/types.h>
#include <sys/times.h>
#include <unistd.h>           /* This is required by sysconf() */

 /*
  The default is FORTRAN_UNDERSCORE_, but not explicitly used.
 */

/*
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
*/

 /*  Prototype: */

   void get_zeits_(double *zts);
   void get_ztick_(double *tic);

/*!REVISION HISTORY:
! 	12Mar98 - Jing Guo <guo@thunder> - initial prototype/prolog/code
! 	06Jul99 - J.W. Larson <jlarson@dao> - support for AIX platform
!	20Jun12 - Jing Guo <jing.guo@nasa.gov> -
!               . Fixed absolete CLK_TCK value, which was incorrectly
!                 defined to CLOCKS_PER_SEC, a useless and fixed wrong
!		  constant 1000000.  The correct value is to get from
!                 system function sysconf(_SC_CLK_TCK).  
!               . Removed unneccesary code.  Hope it is still portable.
!		  The objective is to support Fortran usages through a
!		  C-Binding module.
!EOP */

/*  Implementations: */

void get_zeits_(double *zts)
{

  struct tms tm;
  double secs;

  secs=1./sysconf(_SC_CLK_TCK);	/* seconds per clock tick */

  zts[0]=times(&tm)*secs;	/* (real time in ticks)/hertz */
  zts[1]=tm.tms_utime*secs;	/* (process user ticks)/hertz */
  zts[2]=tm.tms_stime*secs;	/* (process system ticks)/hertz */
  zts[3]=tm.tms_cutime*secs;	/* (child process(es) user ticks)/hertz */
  zts[4]=tm.tms_cstime*secs;	/* (child process(es) system ticks)/hertz */

}

void get_ztick_(double *tic)
{
  tic[0]=1./sysconf(_SC_CLK_TCK);
}
