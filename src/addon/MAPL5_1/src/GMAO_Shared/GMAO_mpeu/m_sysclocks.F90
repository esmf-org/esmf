!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_sysclocks - a standard binding to system clocks in C
!
! !DESCRIPTION:
!
! !INTERFACE:
!#include "regime.H"

    module m_sysclocks
      implicit none
      private	! except

      public:: sysclocks_get

      interface sysclocks_get
        subroutine sysclocks_get(times) bind(C,name='get_zeits_')
	  ! void get_zeits_(double *zts){}
	  use, intrinsic:: ISO_C_BINDING, only: C_DOUBLE
	  implicit none
	  real(C_DOUBLE),dimension(*),intent(out):: times
        end subroutine sysclocks_get
      end interface

! !REVISION HISTORY:
! 	29Jun12	-  <jguo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_sysclocks'

!_UTC_program tc_sysclocks
!_UTC_  ! -- a testcase of this module --
!_UTC_  use m_sysclocks, only: get_times => sysclocks_get ! => get_zeits_
!_UTC_
!_UTC_  implicit none
!_UTC_  character(len=*),parameter :: myname='tc_sysclocks'
!_UTC_  real*8,dimension(5):: times
!_UTC_  integer:: i,k
!_UTC_
!_UTC_  call get_times(times) 
!_UTC_  print'(2a,f15.4,4f10.4)',myname,'(.begin.): ',times
!_UTC_  k=0
!_UTC_  do i=1,100000000
!_UTC_    k=2*sin(i*1.)
!_UTC_  enddo
!_UTC_  call get_times(times) 
!_UTC_  if(k>5) print'(2a,i2)',myname,'(.-bad-.): ',k
!_UTC_  print'(2a,f15.4,4f10.4)',myname,'(.-end-.): ',times
!_UTC_end program tc_sysclocks
end module m_sysclocks
