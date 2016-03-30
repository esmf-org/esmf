!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_allgatherv -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_allgatherv(sbuf,scount ,        stype,	&
			      rbuf,rcounts,rdispls,rtype,	&
	comm,ierror)
      use m_mpi0,only : mpi0_initialized
      implicit none
      integer,dimension(*),intent(in) :: sbuf
      integer,		   intent(in) :: scount
      integer,		   intent(in) :: stype

      integer,dimension(*),intent(out):: rbuf
      integer,dimension(*),intent(in) :: rcounts
      integer,dimension(*),intent(in) :: rdispls
      integer,		   intent(in) :: rtype

      integer,		   intent(in) :: comm
      integer,		   intent(out):: ierror

! !REVISION HISTORY:
! 	29Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_allgatherv'

  if(.not.mpi0_initialized) call mpi_init(ierror)

  call mpi0_copy( sbuf              ,scount    ,stype,		&
		  rbuf(rdispls(1)+1),rcounts(1),rtype,ierror)

end subroutine mpi_allgatherv
