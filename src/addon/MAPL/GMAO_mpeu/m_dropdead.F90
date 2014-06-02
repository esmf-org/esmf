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
! !MODULE: m_dropdead - An abort() with a style
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_dropdead
      implicit none
      private	! except

      public	:: die	! terminate a program with a condition

      interface die; module procedure	&
	die_,	&
	diex_
      end interface

! !REVISION HISTORY:
! 	20Feb97 - Jing Guo <guo@eramus> - defined template
!EOP
!_______________________________________________________________________

contains

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
! !IROUTINE: die_ - Clean up and raise an exception to the OS
!
! !DESCRIPTION:
!
!   A call to die() exits the program with minimum information for
!   both the user and the operating system.
!
! !INTERFACE:

    subroutine die_(where)
      use m_stdio, only : stderr
      use m_mpif90,only : MP_comm_world
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_abort
      implicit none
      character(len=*),intent(in) :: where	! where it is called

! !REVISION HISTORY:
! 	20Feb97 - Jing Guo <guo@eramus> - defined template
!
!EOP
!_______________________________________________________________________

  character(len=*),parameter :: myname_='die.'
  integer :: myrank,ier

	!-------------------------------------------------
	! MPI_ should have been initialized for this call
	!-------------------------------------------------

    call MP_comm_rank(MP_comm_world,myrank,ier)

	! a message for the users:

    write(stderr,'(z3.3,5a)') myrank,'.',myname_,	&
      ': from ',trim(where),'()'

	! raise a condition to the OS

    call MP_abort(MP_comm_world,2,ier)

end subroutine die_
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!-----------------------------------------------------------------------
!BOP
!
! !IROUTINE: diex_ - Clean up and raise an exception to the OS
!
! !DESCRIPTION:
!
!   A call to die() exits the program with minimum information for
!   both the user and the operating system.  This implementation,
!   however, may be used in conjunction with with a source preprocessor
!   to produce more detailed location information.
!
! !INTERFACE:

    subroutine diex_(where,fnam,line)
      use m_stdio, only : stderr
      use m_mpif90,only : MP_comm_world
      use m_mpif90,only : MP_comm_rank
      use m_mpif90,only : MP_abort
      implicit none
      character(len=*),intent(in) :: where	! where it is called
      character(len=*),intent(in) :: fnam
      integer,intent(in) :: line

! !REVISION HISTORY:
! 	20Feb97 - Jing Guo <guo@eramus> - defined template
!
!EOP
!_______________________________________________________________________

  character(len=*),parameter :: myname_='die.'
  integer :: myrank,ier
  character(len=16) :: lineno

	!-------------------------------------------------
	! MPI_ should have been initialized for this call
	!-------------------------------------------------

    call MP_comm_rank(MP_comm_world,myrank,ier)

	! a message for the users:

    write(lineno,'(i16)') line

    write(stderr,'(z3.3,9a)') myrank,'.',myname_,	&
      ': from ',trim(where),'()',	&
      ', line ',trim(adjustl(lineno)),	&
      ' of file ',fnam

	! raise a condition to the OS

    call MP_abort(MP_comm_world,2,ier)

end subroutine diex_
!=======================================================================
end module m_dropdead
!.
