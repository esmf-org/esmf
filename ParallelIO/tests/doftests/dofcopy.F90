!
! Copy an old style dof text file into the newer netcdf format file
!
program dofcopy
#ifndef NO_MPIMOD
  use mpi
#endif
  use pio
  
  implicit none
#ifdef NO_MPIMOD
#include <mpif.h>
#endif
  character(len=256) :: infile, outfile
  integer :: ndims
  integer, pointer :: gdims(:)
  integer(kind=PIO_Offset_kind), pointer :: compmap(:)
  integer :: ierr, mype, npe
  integer :: comm=MPI_COMM_WORLD
  logical :: Mastertask
  integer :: stride=3
  integer :: rearr = PIO_REARR_SUBSET
  type(iosystem_desc_t) :: iosystem
  type(io_desc_t) :: iodesc

  call MPI_Init(ierr)
  call CheckMPIreturn(__LINE__,ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, mype, ierr)
  call CheckMPIreturn(__LINE__,ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npe,  ierr)
  call CheckMPIreturn(__LINE__,ierr)
  if(mype==0) then
     Mastertask=.true.
  else
     Mastertask=.false.
  endif

  CALL get_command_argument(1, infile)
  
  call pio_readdof(trim(infile), ndims, gdims, compmap, MPI_COMM_WORLD)

  if(mype < npe) then
     call pio_init(mype, comm, npe/stride, 0, stride, PIO_REARR_SUBSET, iosystem)

     call PIO_InitDecomp(iosystem, PIO_INT, gdims, compmap, iodesc, rearr=rearr)
     write(outfile, *) trim(infile)//".nc"
     call PIO_write_nc_dof(iosystem, outfile, PIO_64BIT_DATA, iodesc, ierr) 
     call PIO_finalize(iosystem, ierr)
  endif


  call MPI_Finalize(ierr)
contains
  !=============================================
  !  CheckMPIreturn:
  !
  !      Check and prints an error message
  !  if an error occured in a MPI subroutine.
  !=============================================
  subroutine CheckMPIreturn(line,errcode)
#ifndef NO_MPIMOD
    use mpi
#endif
    implicit none
#ifdef NO_MPIMOD
#include <mpif.h>
#endif
    integer, intent(in) :: errcode
    integer, intent(in) :: line
    character(len=MPI_MAX_ERROR_STRING) :: errorstring

    integer :: errorlen

    integer :: ierr

    if (errcode .ne. MPI_SUCCESS) then
       call MPI_Error_String(errcode,errorstring,errorlen,ierr)
       write(*,*) errorstring(1:errorlen)
    end if
  end subroutine CheckMPIreturn


end program dofcopy
