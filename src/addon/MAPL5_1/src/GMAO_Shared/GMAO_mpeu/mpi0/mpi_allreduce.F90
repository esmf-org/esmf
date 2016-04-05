!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_allreduce -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_allreduce( sbuf,rbuf,count,dtype,op,	&
	comm,ierror)
      use m_mpi0,only : mpi0_initialized
      implicit none
      integer,dimension(*),intent(in) :: sbuf
      integer,dimension(*),intent(out):: rbuf
      integer,		   intent(in) :: count
      integer,		   intent(in) :: dtype

      integer,		   intent(in) :: op
      integer,		   intent(in) :: comm
      integer,		   intent(out):: ierror

! !REVISION HISTORY:
! 	29Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_allreduce'

  if(.not.mpi0_initialized) call mpi_init(ierror)

  call mpi0_copy(sbuf,count,dtype,rbuf,count,dtype,ierror)

end subroutine mpi_allreduce
