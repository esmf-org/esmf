!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_reduce_scatter -
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine mpi_reduce_scatter( sbuf,rbuf,count,dtype,op,	&
	comm,ierror)
      use m_mpi0,only : mpi0_initialized
      implicit none
      integer,dimension(*),intent(in) :: sbuf
      integer,dimension(*),intent(out):: rbuf
      integer,dimension(*),intent(in) :: count
      integer,		   intent(in) :: dtype

      integer,		   intent(in) :: op
      integer,		   intent(in) :: comm
      integer,		   intent(out):: ierror

! !REVISION HISTORY:
! 	29Sep99	- Jing Guo <guo@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_reduce_scatter'

  if(.not.mpi0_initialized) call mpi_init(ierror)

  call mpi0_copy(sbuf,count(1),dtype,rbuf,count(1),dtype,ierror)

end subroutine mpi_reduce_scatter
