! $Id: ESMF_DistGrid.F90,v 1.2 2002/10/23 21:19:07 nscollins Exp $
!-------------------------------------------------------------------------
!
! ESMF DistGrid module
!
! This code covered by the GNU public license.  See licence file for details.
! NCAR, 2002.
!

!-------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the top level DistGrid defined type
!  and functions which operate on DistGrids.  A corresponding
!  C++ interface exists in the companion ../interface directory.
!
!  < more here about what's in a DistGrid >
!
!
!  See the ESMF Grid design document for more details.
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!
!
!-------------------------------------------------------------------------
#include "ESMF_Macros.inc"
!
! module definition

      module ESMF_DistGridMod

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section contains the basic defined type for the decomposition of
!  a single grid into multiple subgrids for parallel processing, and the
!  internal subroutines and functions which operate on them.
!
!

!-------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_DistGridMod
      
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod

! !PUBLIC TYPES:
      implicit none

      type ESMF_DistGrid
      sequence
      private
   
        type (ESMF_Base) :: base
        integer :: dgrid_state
       
      end type


! !PUBLIC MEMBER FUNCTIONS:
!
! 
!  function ESMF_DistGridInit() (interface only)
!  function ESMF_DistGridInitNew(
!  function ESMF_DistGridInitFastest(
!
! or
!
!  function ESMF_DistGridCreate() (interface only)
!  function ESMF_DistGridCreateNew(
!  function ESMF_DistGridCreateFastest(
!  subroutine ESMF_DistGridDestroy(grid, rc)
!  subroutine ESMF_DistGridConstruct(grid, rc)
!  subroutine ESMF_DistGridDestruct(grid, rc)
!
!  subroutine ESMF_DistGridGet(grid, what?, rc)
!
!  subroutine ESMF_DistGridCheckpoint(grid, iospec, rc)
!  function ESMF_DistGridRestore(name, iospec, rc)
!  subroutine ESMF_DistGridWrite(grid, subset, iospec, rc)
!  function ESMF_DistGridRead(name, iospec, rc)

!  subroutine ESMF_DistGridValidate(grid, options, rc)
!  subroutine ESMF_DistGridPrint(grid, options, rc)
!


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
! This section includes all the DistGrid Create and Destroy routines
!
!

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_DistGridCreateNew(gridtype, dimcount, dimlist, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateNew        ! newly created grid 
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
        type(ESMF_DistGrid) o !! = ESMF_Allocate()  ! how does this work?

        ESMF_DistGridCreateNew = o;
        end function ESMF_DistGridCreateNew


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DistGridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: grid   ! grid to be destroyed
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
        end subroutine ESMF_DistGridDestroy


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DistGridConstruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: grid   ! grid to be constructed
      integer, intent(out), optional :: rc          ! return code
!
! !DESCRIPTION:
!      ESMF routine to initialize the contents of a DistGrid type.
!      The corresponding internal routine is Destruct.
!
! !REQUIREMENTS: internal
!EOP

!
! code goes here
!
        end subroutine ESMF_DistGridConstruct



!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DistGridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: grid              ! grid to be deleted
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      release all resources except the DistGrid datatype itself.
!
!
! !REQUIREMENTS: internal
!EOP

!
! code goes here
!
        end subroutine ESMF_DistGridDestruct



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section is I/O for DistGrids
!
!

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DistGridCheckpoint(grid, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: grid                ! grid to save
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
        end subroutine ESMF_DistGridCheckpoint


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_DistGridRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! grid name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a DistGrid from the last call to Checkpoint.
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        end function ESMF_DistGridRestore


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DistGridWrite(grid, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: grid          ! grid to save
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
        end subroutine ESMF_DistGridWrite


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_DistGridRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridRead
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
        end function ESMF_DistGridRead


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_DistGridValidate(grid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: grid            ! grid to be checked
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
        end subroutine ESMF_DistGridValidate

!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_DistGridPrint(grid, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: grid              ! grid to be printed
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
        end subroutine ESMF_DistGridPrint

        end module ESMF_DistGridMod

