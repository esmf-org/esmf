!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_waitall -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_waitall( count, array_of_requests, array_of_statuses, ierror)

      use m_mpi0,only : mpi0_initialized, MPI_STATUS_SIZE
      implicit none
      integer,             intent(in) :: count
      integer,dimension(*),intent(in) :: array_of_requests
      integer,dimension(MPI_STATUS_SIZE,*),intent(in) :: array_of_statuses
      integer,		   intent(out):: ierror

! !REVISION HISTORY:
! 	04Jun02	- Tom Clune <Thomas.L.Clune.1@gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_isendallgather'

  if(.not.mpi0_initialized) call mpi_init(ierror)

  ! no-op

end subroutine mpi_waitall
