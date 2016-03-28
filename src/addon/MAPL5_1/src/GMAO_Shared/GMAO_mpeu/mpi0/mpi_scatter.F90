!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_scatter -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_scatter(sbuf,scount,stype,rbuf,rcount,rtype,	&
	root,comm,ierror)
      use m_mpi0,only : mpi0_initialized
      implicit none
      integer,dimension(*),intent(in) :: sbuf
      integer,		   intent(in) :: scount
      integer,		   intent(in) :: stype

      integer,dimension(*),intent(out):: rbuf
      integer,             intent(in) :: rcount
      integer,		   intent(in) :: rtype

      integer,		   intent(in) :: root
      integer,		   intent(in) :: comm
      integer,		   intent(out):: ierror

! !REVISION HISTORY:
! 	29Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_scatter'

  if(.not.mpi0_initialized) call mpi_init(ierror)

  call mpi0_copy(sbuf,scount,stype,rbuf,rcount,rtype,ierror)

end subroutine mpi_scatter
