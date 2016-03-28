!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_recv -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_recv( buf, count, type, pe, tag, comm, status, ierror)

      use m_mpi0,only : mpi0_initialized, MPI_STATUS_SIZE
      implicit none
      integer,dimension(*),intent(in) :: buf
      integer,		   intent(in) :: count
      integer,		   intent(in) :: type

      integer,             intent(in) :: pe
      integer,             intent(in) :: tag
      integer,		   intent(in) :: comm
      integer,             intent(out):: status(MPI_STATUS_SIZE)
      integer,		   intent(out):: ierror

! !REVISION HISTORY:
! 	04Jun02	- Tom Clune <Thomas.L.Clune.1@gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_isendallgather'

  if(.not.mpi0_initialized) call mpi_init(ierror)

  ! need to set up a buffer
  ! for now - never call this routine
  call mpi_abort(comm, -1, ierror)

end subroutine mpi_recv
