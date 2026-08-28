#include "config.h"
!>
!! @file
!! Internal code for compiler workarounds, aborts and debug functions.
!<
module pio_support
  use pio_kinds
  use iso_c_binding
#ifndef NO_MPIMOD
  use mpi !_EXTERNAL
#endif
  implicit none
  private
#ifdef NO_MPIMOD
  include 'mpif.h'    ! _EXTERNAL
#endif
  public :: piodie
  public :: CheckMPIreturn
  public :: pio_readdof
  public :: pio_writedof
  public :: pio_write_nc_dof
  public :: pio_read_nc_dof
  public :: replace_c_null

  logical, public :: Debug=.FALSE.            !< debug mode
  logical, public :: DebugIO=.FALSE.          !< IO debug mode
  logical, public :: DebugAsync=.FALSE.       !< async debug mode
  integer,private,parameter :: versno = 1001

  character(len=*), parameter :: modName='pio_support'

contains
  !> Remove null termination (C-style) from strings for Fortran.
  !<
  subroutine replace_c_null(istr, ilen)
    use iso_c_binding, only : C_NULL_CHAR
    character(len=*),intent(inout) :: istr
    integer(kind=pio_offset_kind), optional, intent(in) :: ilen
    integer :: i, slen
    if(present(ilen)) then
       slen = int(ilen)
    else
       slen = len(istr)
    endif
    do i=1,slen
       if(istr(i:i) == C_NULL_CHAR) exit
    end do
    istr(i:slen)=''
  end subroutine replace_c_null

  !>
  !! Abort the model for abnormal termination.
  !!
  !! @param file File where piodie is called from.
  !! @param line Line number where it is called.
  !! @param msg,msg2,msg3,ival1,ival2,ival3,mpirank : Optional
  !! argument for error messages.
  !! @author Jim Edwards
  !<
  subroutine piodie (file,line, msg, ival1, msg2, ival2, msg3, ival3, mpirank)
    implicit none
    character(len=*), intent(in) :: file
    integer,intent(in) :: line
    character(len=*), intent(in), optional :: msg,msg2,msg3
    integer,intent(in),optional :: ival1,ival2,ival3, mpirank

    character(len=*), parameter :: subName=modName//'::pio_die'
    integer :: ierr, myrank=-1

    if(present(mpirank)) myrank=mpirank

    if (present(ival3)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ', &
            msg,ival1,msg2,ival2,msg3,ival3
    else if (present(msg3)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ', &
            msg,ival1,msg2,ival2,msg3
    else if (present(ival2)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ',msg,ival1,msg2,ival2
    else if (present(msg2)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ',msg,ival1,msg2
    else if (present(ival1)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ',msg,ival1
    else if (present(msg)) then
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': ',msg
    else
       write(6,*) subName,':: myrank=',myrank,': ERROR: ',file,':',line,': (no message)'
    endif


#if defined(CPRXLF) && !defined(BGQ)
    close(5)    ! needed to prevent batch jobs from hanging in xl__trbk
    call xl__trbk()
#endif

    ! passing an argument of 1 to mpi_abort will lead to a STOPALL output 
    ! error code of 257
    call mpi_abort (MPI_COMM_WORLD, 1, ierr)  

#ifdef CPRNAG
    stop
#else
    call abort
#endif

  end subroutine piodie

  !>
  !! Check and prints an error message if an error occured in an MPI
  !! subroutine.
  !!
  !! @param locmesg Message to output
  !! @param errcode MPI error code
  !! @param file The file where the error message originated.
  !! @param line The line number where the error message originated.
  !! @author Jim Edwards
  !<
  subroutine CheckMPIreturn(locmesg, errcode, file, line)

    character(len=*), intent(in) :: locmesg
    integer(i4), intent(in) :: errcode
    character(len=*),optional :: file
    integer, intent(in),optional :: line
    character(len=MPI_MAX_ERROR_STRING) :: errorstring

    integer(i4) :: errorlen

    integer(i4) :: ierr
    if (errcode .ne. MPI_SUCCESS) then
       call MPI_Error_String(errcode,errorstring,errorlen,ierr)
       write(*,*) TRIM(ADJUSTL(locmesg))//errorstring(1:errorlen)
       if(present(file).and.present(line)) then
          call piodie(file,line)
       endif
    end if
  end subroutine CheckMPIreturn

  !>
  !! Fortran interface to write a mapping file.
  !!
  !! @param file : The file where the decomp map will be written.
  !! @param gdims : The global dimensions of the data array as stored in memory.
  !! @param DOF : The multidimensional array of indexes that describes how
  !! data in memory are written to a file.
  !! @param comm : The MPI comm index.
  !! @author T Craig
  !<
  subroutine pio_writedof (file, gdims, DOF, comm)
    implicit none
    character(len=*),intent(in) :: file
    integer, intent(in) :: gdims(:)
    integer(PIO_OFFSET_KIND)  ,intent(in) :: dof(:)
    integer         ,intent(in) :: comm
    integer :: err
    integer :: ndims


    interface
       integer(c_int) function PIOc_writemap_from_f90(file, ndims, gdims, maplen, map, f90_comm) &
            bind(C,name="PIOc_writemap_from_f90")
         use iso_c_binding
         character(C_CHAR), intent(in) :: file
         integer(C_INT), value, intent(in) :: ndims
         integer(C_INT), intent(in) :: gdims(*)
         integer(C_SIZE_T), value, intent(in) :: maplen 
         integer(C_SIZE_T), intent(in) :: map(*)
         integer(C_INT), value, intent(in) :: f90_comm
       end function PIOc_writemap_from_f90
    end interface
    ndims = size(gdims)
    err = PIOc_writemap_from_f90(trim(file)//C_NULL_CHAR, ndims, gdims, int(size(dof),C_SIZE_T), dof, comm)

  end subroutine pio_writedof

  !>
  !! Fortran interface to write a netcdf format mapping file.
  !!
  !! @param ios : The iosystem structure
  !! @param filename : The file where the decomp map will be written.
  !! @param cmode : The netcdf creation mode.
  !! @param iodesc : The io descriptor structure
  !! @param title : An optional title to add to the netcdf attributes
  !! @param history : An optional history to add to the netcdf attributes
  !! @param fortran_order : Optional logical - Should multidimensional arrays be written in fortran order?
  !! @param ret : Return code 0 if success
  !<

  subroutine pio_write_nc_dof(ios, filename, cmode, iodesc, ret, title, history, fortran_order)
    use pio_types, only : iosystem_desc_t, io_desc_t
    type(iosystem_desc_t) :: ios
    character(len=*) :: filename
    integer :: cmode
    type(io_desc_t) :: iodesc
    integer :: ret
    character(len=*), optional :: title
    character(len=*), optional :: history
    logical, optional :: fortran_order
    
    interface
       integer(c_int) function PIOc_write_nc_decomp(iosysid, filename, cmode, &
            ioid, title, history, fortran_order) &
            bind(C,name="PIOc_write_nc_decomp") 
         use iso_c_binding
         integer(C_INT), value :: iosysid
         character(kind=c_char) :: filename
         integer(C_INT), value :: cmode
         integer(c_int), value :: ioid
         character(kind=c_char) :: title
         character(kind=c_char) :: history
         integer(c_int), value :: fortran_order
       end function PIOc_write_nc_decomp
    end interface
    character(len=:), allocatable :: ctitle, chistory
    integer :: nl
    integer :: forder
    integer :: i

    
    if(present(title)) then
       ctitle = trim(title)//C_NULL_CHAR
    else
       ctitle = C_NULL_CHAR
    endif

    if(present(history)) then
       chistory = trim(history)//C_NULL_CHAR
    else
       chistory = C_NULL_CHAR
    endif

    if(present(fortran_order)) then
       if(fortran_order) then
          forder = 1
       else
          forder = 0
       endif
    endif
    nl = len_trim(filename)
    ret = PIOc_write_nc_decomp(ios%iosysid, filename(:nl)//C_NULL_CHAR, cmode, iodesc%ioid, ctitle, chistory, forder)
    
  end subroutine pio_write_nc_dof



  !>
  !! Fortran interface to read a mapping file.
  !!
  !! @param file The file from where the decomp map is read.
  !! @param ndims The number of dimensions of the data.
  !! @param gdims The actual dimensions of the data (pointer to an
  !! integer array of length ndims).
  !! @param DOF Pointer to an integer array where the Decomp map will
  !! be stored.
  !! @param comm MPI comm index
  !! @author T Craig
  !<
  subroutine pio_readdof (file, ndims, gdims, DOF, comm)
    implicit none
    character(len=*),intent(in) :: file
    integer(PIO_OFFSET_KIND),pointer:: dof(:)
    integer         ,intent(in) :: comm
    integer, intent(out) :: ndims
    integer, pointer :: gdims(:)
    integer(PIO_OFFSET_KIND) :: maplen
    integer :: ierr
    type(C_PTR) :: tgdims, tmap
    interface
       integer(C_INT) function PIOc_readmap_from_f90(file, ndims, gdims, maplen, map, f90_comm) &
            bind(C,name="PIOc_readmap_from_f90") 
         use iso_c_binding
         character(C_CHAR), intent(in) :: file
         integer(C_INT), intent(out) :: ndims
         type(C_PTR), intent(out) :: gdims
         integer(C_SIZE_T), intent(out) :: maplen
         type(C_PTR) :: map
         integer(C_INT), value, intent(in) :: f90_comm
       end function PIOc_readmap_from_f90
    end interface
    ierr = PIOc_readmap_from_f90(trim(file)//C_NULL_CHAR, ndims, tgdims, maplen, tmap, comm);

    call c_f_pointer(tgdims, gdims, (/ndims/))
    call c_f_pointer(tmap, DOF, (/maplen/))
  end subroutine pio_readdof

  !>
  !! Fortran interface to read a netcdf format mapping file.
  !!
  !! @param ios : The iosystem structure
  !! @param filename : The file where the decomp map will be written.
  !! @param iodesc : The io descriptor structure returned
  !! @param ret : Return code 0 if success
  !! @param title : An optional title to add to the netcdf attributes
  !! @param history : An optional history to add to the netcdf attributes
  !! @param fortran_order : An optional logical - should arrays be read in fortran order
  !<

  subroutine pio_read_nc_dof(ios, filename, iodesc, ret, title, history, fortran_order)
    use pio_types, only : iosystem_desc_t, io_desc_t
    type(iosystem_desc_t) :: ios
    character(len=*) :: filename
    type(io_desc_t) :: iodesc
    integer :: ret
    character(len=*), optional :: title
    character(len=*), optional :: history
    logical, optional :: fortran_order
    
    interface
       integer(c_int) function PIOc_read_nc_decomp(iosysid, filename, ioid, &
            title, history, fortran_order) &
            bind(C,name="PIOc_read_nc_decomp") 
         use iso_c_binding
         integer(C_INT), value :: iosysid
         character(kind=c_char) :: filename
         integer(c_int)        :: ioid
         character(kind=c_char) :: title
         character(kind=c_char) :: history
         integer(c_int), value :: fortran_order
       end function PIOc_read_nc_decomp
    end interface
    character(len=:), allocatable :: ctitle, chistory
    integer :: nl
    integer :: forder

    nl = len_trim(filename)
    ret = PIOc_read_nc_decomp(ios%iosysid, filename(:nl)//C_NULL_CHAR, iodesc%ioid, title, history, forder)
    if(present(fortran_order)) then
       if(forder /= 0) then
          fortran_order = .true.
       else
          fortran_order = .true.
       endif
    endif
  end subroutine pio_read_nc_dof

end module pio_support
