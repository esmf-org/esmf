!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_sendrecv -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_sendrecv( sbuf, scount, stype, dest, stag,	&
    			     rbuf, rcount, rtype, srcs, rtag,	&
			     comm, ierror)

      use m_mpi0,only : mpi0_initialized, MPI_STATUS_SIZE
      implicit none
      integer,dimension(*),intent(in) :: sbuf
      integer,		   intent(in) :: scount
      integer,		   intent(in) :: stype
      integer,             intent(in) :: dest
      integer,             intent(in) :: stag

      integer,dimension(*),intent(out) :: rbuf
      integer,		   intent(in) :: rcount
      integer,		   intent(in) :: rtype
      integer,             intent(in) :: srcs
      integer,             intent(in) :: rtag

      integer,		   intent(in) :: comm
      integer,		   intent(out):: ierror

! !REVISION HISTORY:
! 	14Nov06	- Jing Guo <jguo@gmao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_sendrecv'

  if(.not.mpi0_initialized) call mpi_init(ierror)

  ! No real implementation for now.  Thus it should never been actually
  ! used.
  call mpi_abort(comm, -1, ierror)

end subroutine mpi_sendrecv
