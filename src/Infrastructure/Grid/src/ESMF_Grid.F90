! $Id: ESMF_Grid.F90,v 1.2 2002/10/23 21:19:07 nscollins Exp $
!-------------------------------------------------------------------------
!
! ESMF Grid module
!
! This code covered by the GNU public license.  See licence file for details.
! NCAR, 2002.
!

!-------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the top level Grid defined type
!  and functions which operate on Grids.  A corresponding
!  C++ interface exists in the companion ../interface directory.
!
! Most of the Grid functions are actually implemented in the lower
!  level types PhysGrid (the physical grid, the actual coordinates)
!  or the DistGrid (the distributed grid, which manages the decomposition).
!
!  See the ESMF Grids design document for more details.
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!
!
!-------------------------------------------------------------------------
#include "ESMF_Macros.inc"
!
! module definition

      module ESMF_GridMod

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section contains the basic defined type for grids, and the
!  internal subroutines and functions which operate on them.
!
!

!-------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_GridMod
      
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_PhysGridMod
      use ESMF_DistGridMod

! !PUBLIC TYPES:
      implicit none

      integer, parameter :: ESMF_MaxGrids = 32

      type ESMF_Grid
      sequence
      private
   
        type (ESMF_Base) :: base
        integer :: grid_state
       
        type (ESMF_PhysGrid) :: physgrid
        integer :: phys_state

         type (ESMF_DistGrid) :: distgrid
         integer :: dist_state

      end type


! !PUBLIC MEMBER FUNCTIONS:
!
! 
!  function ESMF_GridInit() (interface only)
!  function ESMF_GridInitNew(
!  function ESMF_GridInitFastest(
!
! or
!
!  function ESMF_GridCreate() (interface only)
!  function ESMF_GridCreateNew(
!  function ESMF_GridCreateFastest(
!  subroutine ESMF_GridDestroy(grid, rc)
!  subroutine ESMF_GridConstruct(grid, rc)
!  subroutine ESMF_GridDestruct(grid, rc)
!
!  subroutine ESMF_GridGet(grid, what?, rc)
!
!  subroutine ESMF_GridCheckpoint(grid, iospec, rc)
!  function ESMF_GridRestore(name, iospec, rc)
!  subroutine ESMF_GridWrite(grid, subset, iospec, rc)
!  function ESMF_GridRead(name, iospec, rc)

!  subroutine ESMF_GridValidate(grid, options, rc)
!  subroutine ESMF_GridPrint(grid, options, rc)
!

!  specify what the subset is to do
!  function ESMF_SubsetCreate() (interface only)
!  function ESMF_SubsetCreate(type, origin, range, dim)
!  function ESMF_SubsetCreate(type, ll, ur, in_out)
!  function ESMF_SubsetCreate(type, dim, origin, thickness)


! !PRIVATE MEMBER FUNCTIONS:
!EOP
 
!


!-------------------------------------------------------------------------
!
! interface blocks here
!
!-------------------------------------------------------------------------

      contains



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section includes all the Grid Create and Destroy routines
!
!

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_GridCreateNew(gridtype, dimcount, dimlist, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateNew        ! newly created grid 
!
! !ARGUMENTS:
      integer, intent(in) :: gridtype              ! good luck
      integer, intent(in) :: dimcount              ! 2d, 3d, etc
      integer, dimension (1:ESMF_MAXDIM), intent(in) :: dimlist  ! grid dims
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Returns rc=OK if the grid was created without error.
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        type(ESMF_Grid) o !! = ESMF_Allocate()  ! how does this work?

        ESMF_GridCreateNew = o;
        end function ESMF_GridCreateNew


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid   ! grid to be destroyed
      integer, intent(out), optional :: rc          ! return code
!
! !DESCRIPTION:
!     Returns rc=OK if the grid was destroyed without error.
!     Releases all resources associated with this grid.
!
! !REQUIREMENTS: 
!EOP

!
! code goes here
!
        end subroutine ESMF_GridDestroy


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GridConstruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid   ! grid to be constructed
      integer, intent(out), optional :: rc          ! return code
!
! !DESCRIPTION:
!      ESMF routine to initialize the contents of a Grid type.
!      The corresponding internal routine is Destruct.
!
! !REQUIREMENTS: internal
!EOP

!
! code goes here
!
        end subroutine ESMF_GridConstruct



!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid              ! grid to be deleted
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      release all resources except the Grid datatype itself.
!
!
! !REQUIREMENTS: internal
!EOP

!
! code goes here
!
        end subroutine ESMF_GridDestruct



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section is I/O for Grids
!
!

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GridCheckpoint(grid, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid                ! grid to save
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        end subroutine ESMF_GridCheckpoint


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_GridRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! grid name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Grid from the last call to Checkpoint.
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        end function ESMF_GridRestore


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GridWrite(grid, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid          ! grid to save
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see Checkpoint/Restore for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec defined type. 
!
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        end subroutine ESMF_GridWrite


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_GridRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! grid name to read
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        end function ESMF_GridRead


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_GridValidate(grid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid            ! grid to be checked
      character (len = *), intent(in) :: options       ! select validate options
      integer, intent(out), optional :: rc             ! return code
!
! !DESCRIPTION:
!      Routine to validate the internal state of a grid.
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        end subroutine ESMF_GridValidate

!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_GridPrint(grid, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid              ! grid to be printed
      character (len = *), intent(in) :: options         ! select print options
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      Routine to print information about a grid.
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        end subroutine ESMF_GridPrint

        end module ESMF_GridMod

