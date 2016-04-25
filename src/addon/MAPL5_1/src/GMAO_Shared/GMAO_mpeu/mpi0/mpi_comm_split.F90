!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!       NASA/GSFC, Data Assimilation Office, Code 910.3, GEOS/DAS      !
!BOP -------------------------------------------------------------------
!
! !ROUTINE: mpi_comm_split -
!
! !DESCRIPTION:
!
! !INTERFACE:

  subroutine mpi_comm_split(comm,color,key,newcomm,ier)
    use m_mpi0,only : mpi0_initialized
    implicit none
    integer,intent(in) :: comm
    integer,intent(in) :: color
    integer,intent(in) :: key
    integer,intent(out) :: newcomm
    integer,intent(out) :: ier

! !REVISION HISTORY:
! 	22May03	- Will Sawyer <sawyer@dao.gsfc.nasa.gov>
!		- initial prototype/prolog/code
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='mpi_comm_split'

  if(.not.mpi0_initialized) call mpi_init(ier)

  newcomm=comm
  ier=0
end subroutine mpi_comm_split

