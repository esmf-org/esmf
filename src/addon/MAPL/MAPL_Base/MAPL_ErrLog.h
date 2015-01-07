! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 

!  $Id: MAPL_ErrLog.h,v 1.8 2011-04-13 14:20:32 atrayano Exp $ 

! The error logging may eventually evolve into a module based
! on the ESMF logger.  For now these macros provide simple
! traceback capability. 

#ifndef MAPL_ErrLog_DONE


#define MAPL_ErrLog_DONE

#ifdef RETURN_
#undef RETURN_
#endif
#ifdef VERIFY_
#undef VERIFY_
#endif
#ifdef ASSERT_
#undef ASSERT_
#endif

#ifdef IGNORE_
#undef IGNORE_
#endif
 
#define IGNORE_(a) continue

#ifdef I_AM_MAIN

#define VERIFY_(A) if(MAPL_VRFY(A,Iam,__LINE__))call MAPL_Abort
#define ASSERT_(A) if(MAPL_ASRT(A,Iam,__LINE__))call MAPL_Abort

#else

#ifdef ANSI_CPP

#define RETURN_(...)   if(MAPL_RTRN(__VA_ARGS__,Iam,__LINE__,RC))return
#define VERIFY_(...)   if(MAPL_VRFY(__VA_ARGS__,Iam,__LINE__,RC))return
#define ASSERT_(...)   if(MAPL_ASRT(__VA_ARGS__,Iam,__LINE__,RC))return

#else

#define RETURN_(A)     if(MAPL_RTRN(A,Iam,__LINE__,RC))return
#define VERIFY_(A)     if(MAPL_VRFY(A,Iam,__LINE__,RC))return
#define ASSERT_(A)     if(MAPL_ASRT(A,Iam,__LINE__,RC))return

#endif
#endif


#endif
