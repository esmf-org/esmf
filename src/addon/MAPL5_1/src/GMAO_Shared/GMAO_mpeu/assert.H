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
