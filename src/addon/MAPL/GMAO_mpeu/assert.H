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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !MODULE: assert.H - an #include section of ASSERT() macro for Fortran
!
! !DESCRIPTION:
!
! !INTERFACE:
!
!	#define NDEBUG
!	#include "assert.H"
!	...
!		use m_die,only : assert_
!	...
!		ASSERT( <Fortran expression> )
!		ALWAYS_ASSERT( <Fortran expression> )
!
! !BUGS
!	This macro requires Fortran friendly cpp() for macro processing.
!
! !REVISION HISTORY:
!	19Jan01 - Jing Guo
!		- Merged in Tom Clune`s new ASSERT macros.
! 	28Aug00	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- modified
!		- added the prolog for a brief documentation
!	before  - Tom Clune
!		- Created for MP PSAS
!EOP ___________________________________________________________________

	! This implementation allows multiple-"#include" in the same
	! source file.

#ifndef ALWAYS_ASSERT

#define ALWAYS_ASSERT(EX) If (.not. (EX) ) call assert_("EX",__FILE__,__LINE__)
#endif


#ifndef ALWAYS_ASSERT_NOMSG
#define ALWAYS_ASSERT_NOMSG(EX) If(.not.(EX))call assert_(__FILE__,__LINE__)
#endif

#ifndef ALWAYS_ASSERT_MSG
#define ALWAYS_ASSERT_MSG(EX,msg) If(.not.(EX))call assert_(msg,__FILE__,__LINE__)
#endif


	! ASSERT(EX) and ASSERT_MSG(EX) are always redefined, such that
	! one can toggle #define and #undef NDEBUG in the same source
	! file.

#ifdef ASSERT
#undef ASSERT
#endif

#ifdef ASSERT_NOMSG
#undef ASSERT_NOMSG
#endif

#ifdef ASSERT_MSG
#undef ASSERT_MSG
#endif

#ifdef NDEBUG
		! Skip assertion: EX

#define ASSERT(EX)
#define ASSERT_NOMSG(EX)
#define ASSERT_MSG(EX,msg)

#else

#define ASSERT(EX)         ALWAYS_ASSERT(EX)
#define ASSERT_NOMSG(EX)   ALWAYS_ASSERT_NOMSG(EX)
#define ASSERT_MSG(EX,msg) ALWAYS_ASSERT_MSG(EX,msg)

#endif
