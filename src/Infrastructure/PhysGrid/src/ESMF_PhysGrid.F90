! $Id: ESMF_PhysGrid.F90,v 1.1 2002/10/04 17:36:30 nscollins Exp $
!-------------------------------------------------------------------------
!
! ESMF PhysGrid module
!
! This code covered by the GNU public license.  See licence file for details.
! NCAR, 2002.
!

!-------------------------------------------------------------------------
!
! !PURPOSE:
!
! The code in this file implements the top level PhysGrid defined type
!  and functions which operate on PhysGrids.  A corresponding
!  C++ interface exists in the companion ../interface directory.
!
!  < more here about what's in a PhysGrid >
!
!
!  See the ESMF Grid design document for more details.
!
! (all lines between the !BOP and !EOP markers will be included in the
! automated document processing.)
!
!
!-------------------------------------------------------------------------
!
! module definition

      module ESMF_PhysGridMod

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section contains the basic defined type for physical grids, and the
!  internal subroutines and functions which operate on them.
!
!

!-------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_PhysGridMod
      
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod

! !PUBLIC TYPES:
      implicit none

      type ESMF_PhysGrid
      sequence
      private
   
        type (ESMF_Base) :: base
        integer :: pgrid_state
       
      end type


! !PUBLIC MEMBER FUNCTIONS:
!
! 
!  function ESMF_PhysGridInit() (interface only)
!  function ESMF_PhysGridInitNew(
!  function ESMF_PhysGridInitFastest(
!
! or
!
!  function ESMF_PhysGridCreate() (interface only)
!  function ESMF_PhysGridCreateNew(
!  function ESMF_PhysGridCreateFastest(
!  subroutine ESMF_PhysGridDestroy(grid, rc)
!  subroutine ESMF_PhysGridConstruct(grid, rc)
!  subroutine ESMF_PhysGridDestruct(grid, rc)
!
!  subroutine ESMF_PhysGridGet(grid, what?, rc)
!
!  subroutine ESMF_PhysGridCheckpoint(grid, iospec, rc)
!  function ESMF_PhysGridRestore(name, iospec, rc)
!  subroutine ESMF_PhysGridWrite(grid, subset, iospec, rc)
!  function ESMF_PhysGridRead(name, iospec, rc)

!  subroutine ESMF_PhysGridValidate(grid, options, rc)
!  subroutine ESMF_PhysGridPrint(grid, options, rc)
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
! This section includes all the PhysGrid Create and Destroy routines
!
!

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_PhysGridCreateNew(gridtype, dimcount, dimlist, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridCreateNew        ! newly created grid 
!
! !ARGUMENTS:
      integer, intent(in) :: gridtype              ! good luck
      integer, intent(in) :: dimcount              ! 2d, 3d, etc
      integer, dimension (1:ESMF_MaxDim), intent(in) :: dimlist  ! grid dims
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
        type(ESMF_PhysGrid) o !! = ESMF_Allocate()  ! how does this work?

        ESMF_PhysGridCreateNew = o;
        end function ESMF_PhysGridCreateNew


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_PhysGridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: grid   ! grid to be destroyed
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
        end subroutine ESMF_PhysGridDestroy


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_PhysGridConstruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: grid   ! grid to be constructed
      integer, intent(out), optional :: rc          ! return code
!
! !DESCRIPTION:
!      ESMF routine to initialize the contents of a PhysGrid type.
!      The corresponding internal routine is Destruct.
!
! !REQUIREMENTS: internal
!EOP

!
! code goes here
!
        end subroutine ESMF_PhysGridConstruct



!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_PhysGridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: grid              ! grid to be deleted
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!      release all resources except the PhysGrid datatype itself.
!
!
! !REQUIREMENTS: internal
!EOP

!
! code goes here
!
        end subroutine ESMF_PhysGridDestruct



!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
! This section is I/O for PhysGrids
!
!

!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_PhysGridCheckpoint(grid, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: grid                ! grid to save
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
        end subroutine ESMF_PhysGridCheckpoint


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_PhysGridRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! grid name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a PhysGrid from the last call to Checkpoint.
!
! !REQUIREMENTS:
!EOP

!
! code goes here
!
        end function ESMF_PhysGridRestore


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_PhysGridWrite(grid, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: grid          ! grid to save
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
        end subroutine ESMF_PhysGridWrite


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_PhysGridRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_PhysGrid) :: ESMF_PhysGridRead
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
        end function ESMF_PhysGridRead


!-------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_PhysGridValidate(grid, options, rc)
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: grid            ! grid to be checked
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
        end subroutine ESMF_PhysGridValidate

!-------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_PhysGridPrint(grid, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_PhysGrid), intent(in) :: grid              ! grid to be printed
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
        end subroutine ESMF_PhysGridPrint

        end module ESMF_PhysGridMod

