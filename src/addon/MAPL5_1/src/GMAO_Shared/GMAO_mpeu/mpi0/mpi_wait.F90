!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_wait
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_wait( request, status, ierror)

      use m_mpi0,only : mpi0_initialized, MPI_STATUS_SIZE
      implicit none
      integer,intent(in) :: request
      integer,dimension(MPI_STATUS_SIZE),intent(in) :: status
      integer,		   intent(out):: ierror

! !REVISION HISTORY:
! 	22May03	- Will Sawyer, sawyer@dao.gsfc.nasa.gov
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_wait'

  if(.not.mpi0_initialized) call mpi_init(ierror)

  ! no-op

end subroutine mpi_wait
