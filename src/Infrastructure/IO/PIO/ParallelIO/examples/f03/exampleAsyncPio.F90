#include "config.h"
!> @file
!! A simple Fortran example for Async use of the ParallelIO Library.
module pioAsyncExample

    use pio, only : PIO_init, PIO_rearr_box, iosystem_desc_t, file_desc_t
    use pio, only : PIO_finalize, PIO_noerr, PIO_iotype_netcdf, PIO_createfile
    use pio, only : PIO_int,var_desc_t, PIO_redef, PIO_def_dim, PIO_def_var, PIO_enddef
    use pio, only : PIO_closefile, io_desc_t, PIO_initdecomp, PIO_write_darray
    use pio, only : PIO_freedecomp, PIO_clobber, PIO_read_darray, PIO_syncfile, PIO_OFFSET_KIND
    use pio, only : PIO_nowrite, PIO_openfile, pio_set_log_level
    use mpi
    implicit none

    private

    !> @brief Length of the data array we are using.  This is then
    !! divided among MPI processes.
    integer, parameter :: LEN = 16

    !> @brief Value used for array that will be written to netcdf file.
    integer, parameter :: VAL = 42

    !> @brief Error code if anything goes wrong.
    integer, parameter :: ERR_CODE = 99

    !> @brief A class to hold example code and data.
    !! This class contains the data and functions to execute the
    !! example.
    type, public :: pioExampleClass

        !> @brief Compute task comm
        integer, allocatable :: comm(:)

        !> @brief true if this is an iotask
        logical :: iotask

        !> @brief Rank of processor running the code.
        integer :: myRank

        !> @brief Number of processors participating in MPI communicator.
        integer :: ntasks

        !> @brief Number of processors performing I/O.
        integer :: niotasks

        !> @brief Stride in the mpi rank between io tasks.
        integer :: stride

        !> @brief Start index of I/O processors.
        integer :: optBase

        !> @brief The ParallelIO system set up by @ref PIO_init.
        type(iosystem_desc_t), allocatable :: pioIoSystem(:)

        !> @brief Contains data identifying the file.
        type(file_desc_t)     :: pioFileDesc

        !> @brief The netCDF variable ID.
        type(var_desc_t)      :: pioVar

        !> @brief An io descriptor handle that is generated in @ref PIO_initdecomp.
        type(io_desc_t)       :: iodescNCells

        !> @brief Specifies the flavor of netCDF output.
        integer               :: iotype

        !> @brief The netCDF dimension ID.
        integer               :: pioDimId

        !> @brief 1-based index of start of this processors data in full data array.
        integer               :: ista

        !> @brief Size of data array for this processor.
        integer               :: isto

        !> @brief Number of elements handled by each processor.
        integer               :: arrIdxPerPe

        !> @brief The length of the dimension of the netCDF variable.
        integer, dimension(1) :: dimLen

        !> @brief Buffer to hold sample data that is written to netCDF file.
        integer, allocatable  :: dataBuffer(:)

        !> @brief Buffer to read data into.
        integer, allocatable  :: readBuffer(:)

        !> @brief Array describing the decomposition of the data.
        integer, allocatable  :: compdof(:)

        !> @brief Name of the sample netCDF file written by this example.
        character(len=255) :: fileName

    contains

        !> @brief Initialize MPI, ParallelIO, and example data.
        !! Initialize the MPI and ParallelIO libraries. Also allocate
        !! memory to write and read the sample data to the netCDF file.
        procedure,  public  :: init

        !> @brief Create the decomposition for the example.
        !! This subroutine creates the decomposition for the example.
        procedure,  public  :: createDecomp

        !> @brief Create netCDF output file.
        !! This subroutine creates the netCDF output file for the example.
        procedure,  public  :: createFile

        !> @brief Define the netCDF metadata.
        !! This subroutine defines the netCDF dimension and variable used
        !! in the output file.
        procedure,  public  :: defineVar

        !> @brief Write the sample data to the output file.
        !! This subroutine writes the sample data array to the netCDF
        !! output file.
        procedure,  public  :: writeVar

        !> @brief Read the sample data from the output file.
        !! This subroutine reads the sample data array from the netCDF
        !! output file.
        procedure,  public  :: readVar

        !> @brief Close the netCDF output file.
        !! This subroutine closes the output file used by this example.
        procedure,  public  :: closeFile

        !> @brief Clean up resources.
        !! This subroutine cleans up resources used in the example. The
        !! ParallelIO and MPI libraries are finalized, and memory
        !! allocated in this example program is freed.
        procedure,  public  :: cleanUp

        !> @brief Handle errors.
        !! This subroutine is called if there is an error.
        procedure,  private :: errorHandle

    end type pioExampleClass

contains

    !> @brief Initialize MPI, ParallelIO, and example data.
    !! Initialize the MPI and ParallelIO libraries. Also allocate
    !! memory to write and read the sample data to the netCDF file.
    subroutine init(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this
        integer :: io_comm
        integer :: ierr,i
        integer :: procs_per_component(1), io_proc_list(1)
        integer, allocatable :: comp_proc_list(:,:)

        !
        ! initialize MPI
        !

        call MPI_Init(ierr)
        call MPI_Comm_rank(MPI_COMM_WORLD, this%myRank, ierr)
        call MPI_Comm_size(MPI_COMM_WORLD, this%ntasks , ierr)

        if(this%ntasks < 2) then
           print *,"ERROR: not enough tasks specified for example code"
           call mpi_abort(mpi_comm_world, -1 ,ierr)
        endif

        !
        ! set up PIO for rest of example
        !

        this%stride        = 1
        this%optBase       = 0
        this%iotype        = PIO_iotype_netcdf
        this%fileName      = "examplePio_f90.nc"
        this%dimLen(1)     = LEN

        this%niotasks = 1 ! keep things simple - 1 iotask

!        io_proc_list(1) = 0
        io_proc_list(1) = this%ntasks-1
        this%ntasks = this%ntasks - this%niotasks

        procs_per_component(1) = this%ntasks
        allocate(comp_proc_list(this%ntasks,1))
        do i=1,this%ntasks
           comp_proc_list(i,1) = i - 1
!           comp_proc_list(i,1) = i
        enddo

        allocate(this%pioIOSystem(1), this%comm(1))

        call PIO_init(this%pioIOSystem,      & ! iosystem
             MPI_COMM_WORLD,             & ! MPI communicator
             procs_per_component,        & ! number of tasks per component model
             comp_proc_list,             & ! list of procs per component
             io_proc_list,               & ! list of io procs
             PIO_REARR_BOX,              & ! rearranger to use (currently only BOX is supported)
             this%comm,                  & ! comp_comm to be returned
             io_comm)                      ! io_comm to be returned
        if (io_comm /= MPI_COMM_NULL) then
           this%iotask = .true.
           return
        endif
        this%iotask = .false.
        call MPI_Comm_rank(this%comm(1), this%myRank, ierr)
        call MPI_Comm_size(this%comm(1), this%ntasks , ierr)

        !
        ! set up some data that we will write to a netcdf file
        !

        this%arrIdxPerPe = LEN / this%ntasks

        if (this%arrIdxPerPe < 1) then
            call this%errorHandle("Not enough work to distribute among pes", ERR_CODE)
        endif

        this%ista = this%myRank * this%arrIdxPerPe + 1
        this%isto = this%ista + (this%arrIdxPerPe - 1)

        allocate(this%compdof(this%ista:this%isto))
        allocate(this%dataBuffer(this%ista:this%isto))
        allocate(this%readBuffer(this%ista:this%isto))

        this%compdof(this%ista:this%isto) = (/(i, i=this%ista,this%isto, 1)/)
        this%dataBuffer(this%ista:this%isto) = this%myRank + VAL
        this%readBuffer(this%ista:this%isto) = 0

    end subroutine init

    subroutine createDecomp(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this
        integer :: ierr

        call PIO_initdecomp(this%pioIoSystem(1), PIO_int, this%dimLen, this%compdof(this%ista:this%isto), &
            this%iodescNCells)

    end subroutine createDecomp

    subroutine createFile(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: retVal

        retVal = PIO_createfile(this%pioIoSystem(1), this%pioFileDesc, this%iotype, trim(this%fileName), PIO_clobber)

        call this%errorHandle("Could not create "//trim(this%fileName), retVal)

    end subroutine createFile

    subroutine defineVar(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: retVal

        retVal = PIO_def_dim(this%pioFileDesc, 'x', this%dimLen(1) , this%pioDimId)
        call this%errorHandle("Could not define dimension x", retVal)

        retVal = PIO_def_var(this%pioFileDesc, 'foo', PIO_int, (/this%pioDimId/), this%pioVar)
        call this%errorHandle("Could not define variable foo", retVal)

        retVal = PIO_enddef(this%pioFileDesc)
        call this%errorHandle("Could not end define mode", retVal)

    end subroutine defineVar

    subroutine writeVar(this)

    implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: retVal

        call PIO_write_darray(this%pioFileDesc, this%pioVar, this%iodescNCells, this%dataBuffer(this%ista:this%isto), retVal)
        call this%errorHandle("Could not write foo", retVal)
        call PIO_syncfile(this%pioFileDesc)

    end subroutine writeVar

    subroutine readVar(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: retVal

        call PIO_read_darray(this%pioFileDesc, this%pioVar, this%iodescNCells,  this%readBuffer, retVal)
        call this%errorHandle("Could not read foo", retVal)

    end subroutine readVar

    subroutine closeFile(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        call PIO_closefile(this%pioFileDesc)

    end subroutine closeFile

    subroutine cleanUp(this)

        implicit none

        class(pioExampleClass), intent(inout) :: this

        integer :: ierr

        deallocate(this%compdof)
        deallocate(this%dataBuffer)
        deallocate(this%readBuffer)

        call PIO_freedecomp(this%pioIoSystem(1), this%iodescNCells)
        call PIO_finalize(this%pioIoSystem(1), ierr)

    end subroutine cleanUp

    subroutine errorHandle(this, errMsg, retVal)

        implicit none

        class(pioExampleClass), intent(inout) :: this
        character(len=*),       intent(in)    :: errMsg
        integer,                intent(in)    :: retVal
        integer :: lretval
        if (retVal .ne. PIO_NOERR) then
            write(*,*) retVal,errMsg
            call PIO_closefile(this%pioFileDesc)
            call mpi_abort(this%comm(1),retVal, lretval)
        end if

    end subroutine errorHandle

end module pioAsyncExample

!> @brief Main execution of example code.
!! This is an example program for the ParallelIO library.
!!
!! This program creates a netCDF output file with the ParallelIO
!! library, then writes and reads some data to and from the file.
!!
!! This example does the following:
!!
!! - initialization initializes the MPI library, initializes the
!!   ParallelIO library with @ref PIO_init. Then allocate memory for a
!!   data array of sample data to write, and an array to read the data
!!   back into. Also allocate an array to hold decomposition
!!   information.
!!
!! - creation of decomposition by calling @ref PIO_initdecomp.
!!
!! - creation of netCDF file with @ref PIO_createfile.
!!
!! - define netCDF metadata with @ref PIO_def_dim and @ref
!!   PIO_def_var. Then end define mode with @ref PIO_enddef.
!!
!! - write the sample data with @ref PIO_write_darray. Then sync the
!!   file with @ref PIO_syncfile.
!!
!! - read the sample data with @ref PIO_read_darray.
!!
!! - close the netCDF file with @ref PIO_closefile.
!!
!! - clean up local memory, ParallelIO library resources with @ref
!!   PIO_freedecomp and @ref PIO_finalize, and MPI library resources.
!!
program main

    use pioAsyncExample, only : pioExampleClass
    use pio, only : pio_set_log_level
#ifdef TIMING
    use perf_mod, only : t_initf, t_finalizef, t_prf
#endif

    implicit none

    type(pioExampleClass) :: pioExInst
    integer :: ierr
#ifdef TIMING
    call t_initf('timing.nl')
#endif
    call pioExInst%init()
    if (.not. pioExInst%iotask) then
       call pioExInst%createDecomp()
       call pioExInst%createFile()
       call pioExInst%defineVar()
       call pioExInst%writeVar()
       call pioExInst%readVar()
       call pioExInst%closeFile()
       call pioExInst%cleanUp()
    endif
#ifdef TIMING
    call t_prf()
    call t_finalizef()
#endif
    call MPI_Finalize(ierr)


end program main
