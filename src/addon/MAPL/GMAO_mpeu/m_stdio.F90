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
!-----------------------------------------------------------------------
!BOP
!
! !MODULE: m_stdio - a F90 module defines std. I/O parameters
!
! !DESCRIPTION:
!	Define system dependent I/O parameters.
!
! !INTERFACE:

	module m_stdio
	implicit none
	private

	public	:: stdin	! a unit linked to UNIX stdin
	public	:: stdout	! a unit linked to UNIX stdout
	public	:: stderr	! a unit linked to UNIX stderr

	public	:: LEN_FILENAME

! !REVISION HISTORY:
!	10oct96 - Jing G.	- Defined
!EOP
!_______________________________________________________________________

!    Defines standard i/o units.

	integer, parameter :: stdin  = 5
	integer, parameter :: stdout = 6

#ifdef	sysHP_UX
		! Special setting for HP-UX

	integer, parameter :: stderr = 7
#else
		! Generic setting for UNIX other than HP-UX

	integer, parameter :: stderr = 0
#endif

	integer, parameter :: LEN_FILENAME = 128

!-----------------------------------------------------------------------
end module m_stdio
!.
