! $Id: ESMF_Grid.F90,v 1.130 2004/01/28 21:46:48 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
!     ESMF Grid Module
      module ESMF_GridMod
!
!==============================================================================
!
! This file contains the Grid class definition and all Grid class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!!#include "ESMF_Grid.h"   ! unnecessary?
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_GridMod - Grid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_Grid} class.  This class
! provides a unified interface for both {\tt ESMF\_PhysGrid} and 
! {\tt ESMF\_DistGrid} information for model grids.  
! Functions for defining and computing {\tt ESMF\_Grid}
! information are available through this class.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod        ! ESMF base class
      use ESMF_IOMod          ! ESMF I/O class
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_DataMapMod     ! ESMF data map class
      use ESMF_DELayoutMod    ! ESMF layout class
      use ESMF_ArrayBaseMod
      use ESMF_ArrayExpandMod
      use ESMF_DistGridMod    ! ESMF distributed grid class
      use ESMF_PhysCoordMod   ! ESMF physical coord class
      use ESMF_PhysGridMod    ! ESMF physical grid class
      use ESMF_GridTypesMod   ! ESMF basic grid types and primitives
      use ESMF_LogRectGridMod ! ESMF logically rectangular grid routines
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

    public ESMF_GridDestroy
    public ESMF_GridGetCoord
    public ESMF_GridSetCoord
    public ESMF_GridGetDE
    public ESMF_GridGetAllAxisIndex
    public ESMF_GridGlobalToLocalIndex
    public ESMF_GridLocalToGlobalIndex
    public ESMF_GridGet
    public ESMF_GridSet
    !public ESMF_GridGetMask
    public ESMF_GridGetCellMask
    public ESMF_GridSetMask
    !public ESMF_GridGetMetric
    public ESMF_GridSetMetric
    public ESMF_GridBoxIntersectRecv
    public ESMF_GridBoxIntersectSend
    public ESMF_GridValidate
    public ESMF_GridPrint
    public ESMF_GridComputeDistance
    !public ESMF_GridSearch

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Grid.F90,v 1.130 2004/01/28 21:46:48 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_GridCreate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridCreateEmpty
         module procedure ESMF_GridCreateRead
         module procedure ESMF_GridCreateCopy
         module procedure ESMF_GridCreateCutout
         module procedure ESMF_GridCreateDiffRes
         module procedure ESMF_GridCreateExchange

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Grid} create
!     methods.

!EOP
      end interface
!
!------------------------------------------------------------------------------

!BOP
! !INTERFACE:
      interface ESMF_GridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetCoordFromArray
         module procedure ESMF_GridSetCoordFromBuffer
#if (PARCH != linux_intel)
         ! don't include the following two in the interface def
         ! for the intel build.  fails on jazz.
         module procedure ESMF_GridSetCoordSpecd
         module procedure ESMF_GridSetCoordUniform
#endif
         module procedure ESMF_GridSetCoordCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     coordinates as part of a {\tt ESMF\_Grid}.

!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_GridSetMask

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetMaskFromArray
         module procedure ESMF_GridSetMaskFromBuffer
         module procedure ESMF_GridSetMaskCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     logical masks as part of a {\tt ESMF\_Grid}.

!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_GridSetMetric

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_GridSetMetricFromArray
         module procedure ESMF_GridSetMetricFromBuffer
         module procedure ESMF_GridSetMetricCompute
         module procedure ESMF_GridSetMetricCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     metrics as part of a {\tt Grid}.

!EOP
      end interface
!
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_GridSearch
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_GridSearchPoint
!         module procedure ESMF_GridSearchList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     search a {\tt ESMF\_Grid} for point(s).
!!
!!EOPI
!      end interface
!!
!------------------------------------------------------------------------------

!    < add other interfaces here>

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Grid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateEmpty - Create a new Grid with no contents

! !INTERFACE:
      function ESMF_GridCreateEmpty(name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateEmpty
!
! !ARGUMENTS:
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object and constructs its
!     internals, but does not fill in any contents.  Return a pointer to
!     the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateEmpty%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateEmpty: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateEmpty: Grid construct"
        return
      endif

!     Set return values.
      ESMF_GridCreateEmpty%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateEmpty

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateRead - Create a new Grid read in from a file

! !INTERFACE:
      function ESMF_GridCreateRead(gridStructure, iospec, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateRead
!
! !ARGUMENTS:
      integer, intent(in) :: gridStructure
      type(ESMF_IOSpec), intent(in) :: iospec   ! file specs
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and reads a {\tt ESMF\_Grid} in from a file.  Return a pointer to
!     the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridStructure]
!          Grid structure specification.
!     \item[iospec]
!          File I/O specification.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(ESMF_GridCreateRead%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridCreateRead routines based on GridStructure

      select case(gridStructure)

      !-------------
      !  ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridCreateRead: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        ESMF_GridCreateRead = ESMF_LRGridCreateRead(iospec, name, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridCreateRead: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridCreateRead: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridCreateRead: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridCreateRead: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridCreateRead: error in creation'
        return
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateCopy - Create a new Grid by copying another Grid

! !INTERFACE:
      function ESMF_GridCreateCopy(gridIn, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateCopy
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: gridIn
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and copies attributes from another {\tt ESMF\_Grid}.  Return a
!     pointer to the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridIn]
!          {\tt ESMF\_Grid} to be copied.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(ESMF_GridCreateCopy%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridCreateCopy routines based on GridStructure

      select case(gridIn%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridCreateCopy: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        ESMF_GridCreateCopy = ESMF_LRGridCreateCopy(gridIn, name, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridCreateCopy: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridCreateCopy: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridCreateCopy: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridCreateCopy: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridCreateCopy: error in creation'
        return
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateCutout - Create a new Grid as a subset of an existing Grid

! !INTERFACE:
      function ESMF_GridCreateCutout(gridIn, min, max, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateCutout
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: gridIn
      integer, dimension(:), intent(in) :: min
      integer, dimension(:), intent(in) :: max
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and copies a region from an existing {\tt ESMF\_Grid}.
!     Return a pointer to the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridIn]
!          {\tt ESMF\_Grid} to be partially copied.
!     \item[min]
!          Minimum global indices for the region of the grid to be cutout.
!     \item[max]
!          Maximum global indices for the region of the grid to be cutout.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(ESMF_GridCreateCutout%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridCreateCutout routines based on GridStructure

      select case(gridIn%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridCreateCutout: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        ESMF_GridCreateCutout = ESMF_LRGridCreateCutout(gridIn, min, max, &
                                                        name, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridCreateCutout: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridCreateCutout: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridCreateCutout: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridCreateCutout: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridCreateCutout: error in creation'
        return
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateCutout

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateDiffRes - Create a new Grid by coarsening or refining an existing Grid

! !INTERFACE:
      function ESMF_GridCreateDiffRes(gridIn, resolution, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateDiffRes
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: gridIn
      integer, dimension(:), intent(in) :: resolution
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and creates a {\tt ESMF\_Grid} by either coarsening or refining an
!     existing {\tt ESMF\_Grid}.  Return a pointer to the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridIn]
!          Source {\tt ESMF\_Grid} to be coarsened or refined.
!     \item[resolution]
!          Integer resolution factors in each direction.
!          Note:  The above arguments assume refinement by factor if positive
!          and coarsening by absolute value of the factor if negative.  For
!          example, resolution(1)=4 indicates the new {\tt ESMF\_Grid} will be
!          four times as resolved in the first direction as the source
!          {\tt ESMF\_Grid}, whereas resolution(2)=-3 means the new
!          {\tt ESMF\_Grid} will sample every third point in the second 
!          direction.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(ESMF_GridCreateDiffRes%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridCreateDiffRes routines based on GridStructure

      select case(gridIn%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridCreateDiffRes: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        ESMF_GridCreateDiffRes = &
          ESMF_LRGridCreateDiffRes(gridIn, resolution, name, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridCreateDiffRes: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridCreateDiffRes: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridCreateDiffRes: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridCreateDiffRes: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridCreateDiffRes: error in creation'
        return
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateDiffRes

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateExchange - Create a new Grid from the intersection of two existing grids

! !INTERFACE:
      function ESMF_GridCreateExchange(gridIn1, gridIn2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateExchange
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: gridIn1
      type(ESMF_Grid), intent(in) :: gridIn2
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and creates a new {\tt ESMF\_Grid} from the intersection of two
!     existing {\tt ESMF\_Grids}.  Return a pointer to the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[gridIn1]
!          First source {\tt ESMF\_Grid}.
!     \item[gridIn2]
!          Second source {\tt ESMF\_Grid}.
!     \item[{[name]}]
!          New {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(ESMF_GridCreateExchange%ptr)

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridCreateExchange routines based on GridStructure

      select case(gridIn1%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridCreateExchange: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        ESMF_GridCreateExchange = ESMF_LRGridCreateExchange(gridIn1, gridIn2, &
                                                            name, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridCreateExchange: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridCreateExchange: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridCreateExchange: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridCreateExchange: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridCreateExchange: error in creation'
        return
      endif

!     Set return values.
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateExchange

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridDestroy - Free all resources associated with a Grid 

! !INTERFACE:
      subroutine ESMF_GridDestroy(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys a {\tt ESMF\_Grid} object previously allocated
!     via an {\tt ESMF\_GridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          The class to be destroyed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! If already destroyed or never created, return ok
      if (.not. associated(grid%ptr)) then
        print *, "GridDestroy called on uninitialized or destroyed Grid"
        if(rcpresent) rc = ESMF_FAILURE   ! should this really be an error?
        return
      endif

      ! Call GridDestruct routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridDestroy: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        call ESMF_LRGridDestruct(grid%ptr, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridDestroy: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridDestroy: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridDestroy: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridDestroy: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      ! If error write message and return.
      if(status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridDestroy from grid destruct"
        return
      endif

      ! free field memory.
      deallocate(grid%ptr, stat=status)
      if(status .NE. 0) then
        print *, "ERROR in ESMF_GridDestroy: Grid deallocate"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetCoord - Get the coordinates of a Grid

! !INTERFACE:
      subroutine ESMF_GridGetCoord(grid, physGridId, relloc, centerCoord, &
                                   cornerCoord, faceCoord, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in), optional :: physGridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_Array), dimension(:), pointer, optional :: centerCoord
      type(ESMF_Array), dimension(:,:), pointer, optional :: cornerCoord
      type(ESMF_Array), optional :: faceCoord
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Determines the appropriate physGrid to query from either a physGridId or
!     relloc and returns the requested information.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be queried.
!     \item[{[physGridId]}]
!          Identifier for the {\tt ESMF\_PhysGrid} to be queried.
!     \item[{[relloc]}]
!          Relative location of the {\tt ESMF\_PhysGrid} to be queried.
!          Note: either the physGridId or relloc must be specified.  If both
!                are, the physGridId will take precedence.
!     \item[{[centerCoord]}]
!          Coordinates of each cell center.  The dimension index should
!          be defined first (e.g. x = coord(1,i,j), y=coord(2,i,j)).
!     \item[{[cornerCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the corner index.  Corners can
!          be numbered in either clockwise or counter-clockwise direction,
!          but must be numbered consistently throughout grid.
!     \item[{[faceCoord]}]
!          Coordinates of corners of each cell.  The dimension index should
!          be defined first, followed by the face index.  Faces should
!          be numbered consistently with corners.  For example, face 1 should
!          correspond to the face between corners 1,2.
!     \item[{[total]}]
!          Logical. If TRUE, return the total coordinates including internally
!          generated boundary cells. If FALSE return the
!          computational cells (which is what the user will be expecting.)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: physIdUse
      logical :: rellocIsValid, physIdIsValid

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initialize other variables
      physIdUse = -1
      rellocIsValid = .false.
      physIdIsValid = .false.

      ! Either the relative location or physGridId must be present and valid
      if (present(relloc)) then
!        rellocIsValid = ESMF_RelLocIsValid(relloc)  TODO: assume OK if there for now
        rellocIsValid = .true.
      endif
      if (present(physGridId)) then
        if ((physGridId.ge.1) .and. (physGridId.le.grid%ptr%numPhysGrids)) then
          physIdIsValid = .true.
          physIdUse = physGridId
       endif
      endif
      if (.not.(rellocIsValid .or. physIdIsValid)) then
        print *, "ERROR in ESMF_GridGetCoord: need either relloc or PhysGridId"
        return
      endif

      ! If there is a relloc but no physGrid id, then get the id from the relloc
      if (rellocIsValid .and. .not.(physIdIsValid)) then
        call ESMF_GridGetPhysGridID(grid%ptr, relloc, physIdUse, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetCoord: get PhysGrid id"
          return
        endif
        if (physIdUse.eq.-1) then
          print *, "ERROR in ESMF_GridGetCoord: no PhysGrid corresponding", &
                   " to relloc"
          return
        endif
      endif

      ! Call PhysGridGet with valid PhysGrid
      if (present(centerCoord)) then
        call ESMF_PhysGridGetLocations(grid%ptr%physGrids(physIdUse), &
                                       locationArray=centerCoord, &
                                       total=total, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetCoord: PhysGrid get locations"
          return
        endif
      endif
      if (present(cornerCoord)) then
        call ESMF_PhysGridGetRegions(grid%ptr%physGrids(physIdUse), &
                                     vertexArray=cornerCoord, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetCoord: PhysGrid get regions"
          return
        endif
      endif
      ! TODO: face coords

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCoord

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordFromArray - Set the coordinates of a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetCoordFromArray(grid, array, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the coordinates exist already and are being
!     passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item[{[id]}]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetDE - Get DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_GridGetDE(grid, distGridId, physGridId, relloc, &
                                MyDE, localCellCount, localCellCountPerDim, &
                                globalStartPerDim, globalAIPerDim, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(in), optional :: distGridId
      integer, intent(in), optional :: physGridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(inout), optional :: MyDE
      integer, intent(inout), optional :: localCellCount
      integer, dimension(:), intent(inout), optional :: localCellCountPerDim
      integer, dimension(:), intent(inout), optional :: globalStartPerDim
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM), intent(inout), &
                        optional :: globalAIPerDim
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[MyDE]}]
!          Identifier for this {\tt ESMF\_DE}.
!     \item[{[localCellCount]}]
!          Local (on this {\tt ESMF\_DE}) number of cells.
!     \item[{[localCellCountPerDim]}]
!          Local (on this {\tt ESMF\_DE}) number of cells per axis.
!     \item[{[globalStartPerDim]}]
!          Global index of starting counts for each dimension.
!     \item[{[globalAIPerDim]}]
!          Global axis indices for each dimension.
!     \item[{[total]}]
!          Logical flag to indicate getting DistGrid information for total cells.
!          The default is the computational regime.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridGetDE routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridGetDE: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        call ESMF_LRGridGetDE(grid, distGridId, physGridId, relloc, MyDE, &
                              localCellCount, localCellCountPerDim, &
                              globalStartPerDim, globalAIPerDim, total, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridGetDE: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridGetDE: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridGetDE: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridGetDE: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridGetDE: error in get'
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetDE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGetAllAxisIndex - Get all axis indices for a DistGrid

! !INTERFACE:
      subroutine ESMF_GridGetAllAxisIndex(grid, globalAI, distGridId, &
                                          physGridId, relloc, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: globalAI
      integer, intent(in), optional :: distGridId
      integer, intent(in), optional :: physGridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[globalAI]
!          Global axis indices on all DE's.
!     \item[{[total]}]
!          Logical flag for whether the axis indices should be for total
!          cells or not.  Default is false, which infers computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridGetAllAxisIndex routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridGetAllAxisIndex: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        call ESMF_LRGridGetAllAxisIndex(grid, globalAI, distGridId, &
                                        physGridId, relloc, total, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridGetAllAxisIndex: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridGetAllAxisIndex: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridGetAllAxisIndex: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridGetAllAxisIndex: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridGetAllAxisIndex: error in get'
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetAllAxisIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_GridGlobalToLocalIndex(grid, distGridId, physGridId, &
                                             relloc, global1D, local1D, &
                                             global2D, local2D, &
                                             globalAI1D, localAI1D, &
                                             globalAI2D, localAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(in), optional :: distGridId
      integer, intent(in), optional :: physGridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) :: global1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: local1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) :: global2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: local2D
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) :: globalAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) ::  localAI2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_DistGrid} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be used.
!     \item[{[global1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of global identifiers to be
!          translated.  Infers translating between positions in memory.
!     \item[{[local1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of local identifiers
!          corresponding to global identifiers.
!     \item[{[global2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of global identifiers to be
!          translated.  Infers translating between indices in ij space.
!     \item[{[local2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of local identifiers
!          corresponding to global identifiers.
!     \item[{[globalAI1D]}]
!          One-dimensional array of global AxisIndices to be translated.
!     \item[{[localAI1D]}]
!          One-dimensional array of local AxisIndices corresponding to global AIs.
!     \item[{[globalAI2D]}]
!          Two-dimensional array of global AxisIndices to be translated.
!     \item[{[localAI2D]}]
!          Two-dimensional array of local AxisIndices corresponding to global AIs.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridGlobalToLocalIndex routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridGlobalToLocalIndex: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        call ESMF_LRGridGlobalToLocalIndex(grid, distGridId, physGridId, &
                                           relloc, global1D, local1D, &
                                           global2D, local2D, &
                                           globalAI1D, localAI1D, &
                                           globalAI2D, localAI2D, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridGlobalToLocalIndex: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridGlobalToLocalIndex: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridGlobalToLocalIndex: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridGlobalToLocalIndex: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridGlobalToLocalIndex: error in gtolindex'
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGlobalToLocalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridLocalToGlobalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_GridLocalToGlobalIndex(grid, distGridId, physGridId, &
                                             relloc, local1D, global1D, &
                                             local2D, global2D, &
                                             localAI1D, globalAI1D, &
                                             localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      integer, intent(in), optional :: distGridId
      integer, intent(in), optional :: physGridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer(ESMF_KIND_I4), dimension(:), optional, intent(in) ::  local1D
      integer(ESMF_KIND_I4), dimension(:), optional, intent(out) :: global1D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(in) ::  local2D
      integer(ESMF_KIND_I4), dimension(:,:), optional, intent(out) :: global2D
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) ::  localAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) :: globalAI2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_DistGrid} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be used.
!     \item[{[local1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of local identifiers to be
!          translated.  Infers translating between positions in memory.
!     \item[{[global1D]}]
!          One-dimensional {\tt ESMF\_LocalArray} of global identifiers
!          corresponding to local identifiers.
!     \item[{[local2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of local identifiers to be
!          translated.  Infers translating between indices in ij space.
!     \item[{[global2D]}]
!          Two-dimensional {\tt ESMF\_LocalArray} of global identifiers
!          corresponding to local identifiers.
!     \item[{[localAI1D]}]
!          One-dimensional array of local AxisIndices to be translated.
!     \item[{[globalAI1D]}]
!          One-dimensional array of global AxisIndices corresponding to local AIs.
!     \item[{[localAI2D]}]
!          Two-dimensional array of local AxisIndices to be translated.
!     \item[{[globalAI2D]}]
!          Two-dimensional array of global AxisIndices corresponding to local AIs.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Call GridLocalToGlobalIndex routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridLocalToGlobalIndex: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        call ESMF_LRGridLocalToGlobalIndex(grid, distGridId, physGridId, &
                                           relloc, local1D, global1D, &
                                           local2D, global2D, &
                                           localAI1D, globalAI1D, &
                                           localAI2D, globalAI2D, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridLocalToGlobalIndex: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridLocalToGlobalIndex: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridLocalToGlobalIndex: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
        print *, "ERROR in ESMF_GridLocalToGlobalIndex: Invalid grid structure"
        status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridLocalToGlobalIndex: error in specific call'
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridLocalToGlobalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordFromBuffer - Set the coordinates of a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetCoordFromBuffer(Grid, buffer, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real(ESMF_KIND_R8), dimension (:), pointer :: buffer
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the coordinates exist already and are being
!     passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item[id]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetCoordCopy - Copies coordinates from one grid to another

! !INTERFACE:
      subroutine ESMF_GridSetCoordCopy(grid, gridIn, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: gridIn
      integer, intent(in) :: id
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies the coordinates of a {\tt ESMF\_Grid} from another Grid.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[gridIn]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item[id]
!          Identifier for which set of coordinates are being set:
!             1  center\_x
!             2  center\_y
!             3  corner\_x
!             4  corner\_y
!             5  face\_x
!             6  face\_y 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetCoordCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridGet - Gets a variety of information about the grid

! !INTERFACE:
      subroutine ESMF_GridGet(grid, distGridId, physGridId, relloc, &
                              numDims, &
                              horzGridKind, vertGridKind, &
                              horzStagger, vertStagger, &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder, &
                              minGlobalCoordPerDim, maxGlobalCoordPerDim, &
                              minLocalCoordPerDim, maxLocalCoordPerDim, &
                              globalCellCountPerDim, &
                              globalStartPerDEPerDim, maxLocalCellCountPerDim, &
                              cellCountPerDEPerDim, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in), optional :: distGridId
      integer, intent(in), optional :: physGridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(out), optional :: numDims
      type(ESMF_GridKind), intent(out), optional :: horzGridKind
      type(ESMF_GridKind), intent(out), optional :: vertGridKind
      type(ESMF_GridStagger), intent(out), optional :: horzStagger
      type(ESMF_GridStagger), intent(out), optional :: vertStagger
      type(ESMF_CoordSystem), intent(out), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(out), optional :: vertCoordSystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordOrder
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minGlobalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxGlobalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minLocalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxLocalCoordPerDim
      integer, intent(out), dimension(:), optional :: globalCellCountPerDim
      integer, intent(out), dimension(:,:), optional :: globalStartPerDEPerDim
      integer, intent(out), dimension(:), optional :: maxLocalCellCountPerDim
      integer,              dimension(:,:), pointer, &
                            optional :: cellCountPerDEPerDim
      type(ESMF_Logical), intent(out), dimension(:), optional :: periodic
      character(len = *), intent(out), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version sets a variety of information about a {\tt ESMF\_Grid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[{[horzGridKind]}]
!          Integer specifier to denote horizontal grid type.
!     \item[{[vertGridKind]}]
!          Integer specifier to denote vertical grid type.
!     \item[{[horzStagger]}]
!          Integer specifier to denote horizontal grid stagger.
!     \item[{[vertStagger]}]
!          Integer specifier to denote vertical grid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[coordOrder]}]
!          Integer specifier to denote coordinate ordering.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[minLocalCoordPerDim]}]
!          Array of minimum local physical coordinates in each direction.
!     \item[{[maxLocalCoordPerDim]}]
!          Array of maximum local physical coordinates in each direction.
!     \item[{[globalCellCountPerDim]}]
!          Array of numbers of global grid increments in each direction.
!     \item[{[globalStartPerDEPerDim]}]
!          Array of global starting locations for each DE and in each direction.
!     \item[{[maxLocalCellCountPerDim]}]
!          Array of maximum grid counts on any DE in each direction.
!     \item[{[cellCountPerDEPerDim]}]
!          2-D array of grid counts on each DE and in each direction.
!     \item[{[periodic]}]
!          Returns the periodicity along the coordinate axes - logical array.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i, distGridIdUse, physGridIdUse
      type(ESMF_GridType), pointer :: gridp
      type(ESMF_PhysCoord) :: coord

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
 
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_GridGet called with invalid grid object"
        return
      endif

      gridp => grid%ptr

      ! if present, gets information from the grid derived type
      if(present(numDims        )) numDims         = gridp%numDims
      if(present(horzGridKind   )) horzGridKind    = gridp%horzGridKind
      if(present(vertGridKind   )) vertGridKind    = gridp%vertGridKind
      if(present(horzStagger    )) horzStagger     = gridp%horzStagger
      if(present(vertStagger    )) vertStagger     = gridp%vertStagger
      if(present(horzCoordSystem)) horzCoordSystem = gridp%horzCoordSystem
      if(present(vertCoordSystem)) vertCoordSystem = gridp%vertCoordSystem
      if(present(coordOrder     )) coordOrder      = gridp%coordOrder

      ! get name from base obj
      if (present(name)) then
        call ESMF_GetName(gridp%base, name, status)
        if(status .ne. ESMF_SUCCESS) then
           print *, "ERROR in ESMF_GridGetName"
           return
        endif
      endif

      ! Get global coordinate extents
      if(present(minGlobalCoordPerDim)) then
        do i = 1,size(minGlobalCoordPerDim)  !TODO: check array size vs gridrank
          minGlobalCoordPerDim(i) = gridp%minGlobalCoordPerDim(i)
        enddo
      endif
      if(present(maxGlobalCoordPerDim)) then
        do i = 1,size(maxGlobalCoordPerDim)   !TODO: check array size vs gridrank
          maxGlobalCoordPerDim(i) = gridp%maxGlobalCoordPerDim(i)
        enddo
      endif

      ! get the periodicity
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            periodic(i) = gridp%periodic(i)
         enddo
      endif

      ! if DistGrid info is being queried, make sure there is a valid distGridId
      if(present(globalCellCountPerDim)   .or. &
         present(globalStartPerDEPerDim)  .or. &
         present(maxLocalCellCountPerDim) .or. &
         present(cellCountPerDEPerDim)) then

! TODO: add code to get distgridId from relloc or physgridId, test for the
!       presence of at least one of these optional arguments
        if (present(distGridId)) distGridIdUse = distGridId
      endif

      ! Get distgrid info with global coordinate counts
      if (present(globalCellCountPerDim) .or. present(globalStartPerDEPerDim) &
                                     .or. present(maxLocalCellCountPerDim)) then
        call ESMF_DistGridGet(gridp%distgrids(distGridIdUse), &
                              globalCellCountPerDim=globalCellCountPerDim, &
                              globalStartPerDEPerDim=globalStartPerDEPerDim, &
                              maxLocalCellCountPerDim=maxLocalCellCountPerDim, &
                              rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGet: DistGrid get"
          return
        endif
      endif
      if(present(cellCountPerDEPerDim)) then
        ! TODO: check size of cellCountPerDEPerDim
        call ESMF_DistGridGetAllCounts(gridp%distgrids(distGridIdUse)%ptr, &
                                       cellCountPerDEPerDim, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGet: DistGrid get all counts"
          return
        endif
      endif

      ! if PhysGrid info is being queried, make sure there is a valid physGridId
      if(present(minLocalCoordPerDim) .or. present(maxLocalCoordPerDim)) then
! TODO: add code to get physgridId from relloc, test for the presence of at least
!       one of these optional arguments
        if (present(physGridId)) physGridIdUse = physGridId
      endif

      ! Get physcoord info
      if(present(minLocalCoordPerDim)) then
        do i = 1,size(minLocalCoordPerDim)  !TODO: check array size vs gridrank
                                            !TODO: add query to vertical physgrid
          coord = gridp%physgrids(physGridIdUse)%ptr%coords(i)
          call ESMF_PhysCoordGetExtents(coord, minVal=minLocalCoordPerDim(i), &
                                        rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridGet: physcoord get extents"
            return
          endif
        enddo
      endif
      if(present(maxLocalCoordPerDim)) then
        do i = 1,size(maxLocalCoordPerDim)  !TODO: check array size vs gridrank
                                            !TODO: add query to vertical physgrid
          coord = gridp%physgrids(physGridIdUse)%ptr%coords(i)
          call ESMF_PhysCoordGetExtents(coord, maxVal=maxLocalCoordPerDim(i), &
                                        rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridGet: physcoord get extents"
            return
          endif
        enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSet - Sets a variety of information about the grid

! !INTERFACE:
      subroutine ESMF_GridSet(grid, horzGridKind, vertGridKind, &
                              horzStagger, vertStagger, &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder, minGlobalCoordPerDim, &
                              maxGlobalCoordPerDim, periodic, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      type(ESMF_GridKind), intent(in), optional :: horzGridKind
      type(ESMF_GridKind), intent(in), optional :: vertGridKind
      type(ESMF_GridStagger), intent(in), optional :: horzStagger
      type(ESMF_GridStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: maxGlobalCoordPerDim
      type(ESMF_Logical), intent(in), optional :: periodic(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version sets a variety of information about a {\tt ESMF\_Grid}, depending
!     on a list of optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[{[horzGridKind]}]
!          Integer specifier to denote horizontal grid type.
!     \item[{[vertGridKind]}]
!          Integer specifier to denote vertical grid type.
!     \item[{[horzStagger]}]
!          Integer specifier to denote horizontal grid stagger.
!     \item[{[vertStagger]}]
!          Integer specifier to denote vertical grid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[coordOrder]}]
!          Integer specifier to denote coordinate ordering.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum global physical coordinates in each direction.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum global physical coordinates in each direction.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! loop index

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     if present, set information filling in grid derived type
      if(present(horzGridKind)) grid%horzGridKind = horzGridKind
      if(present(vertGridKind)) grid%vertGridKind = vertGridKind
      if(present(horzStagger)) grid%horzStagger = horzStagger
      if(present(vertStagger)) grid%vertStagger = vertStagger
      if(present(horzCoordSystem)) grid%horzCoordSystem = horzCoordSystem
      if(present(vertCoordSystem)) grid%vertCoordSystem = vertCoordSystem
      if(present(coordOrder)) grid%coordOrder = coordOrder
      if (present(periodic)) then
         do i=1,ESMF_MAXGRIDDIM
            if (i > size(periodic)) exit
            grid%periodic(i) = periodic(i)
         enddo
      endif

      if (present(minGlobalCoordPerDim)) then
   !      if (size(minGlobalCoordPerDim) .gt. ESMF_MAXGRIDDIM) exit  ! TODO
         do i=1,size(minGlobalCoordPerDim)
            grid%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
         enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
   !      if (size(maxGlobalCoordPerDim) .gt. ESMF_MAXGRIDDIM) exit  ! TODO
         do i=1,size(maxGlobalCoordPerDim)
            grid%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
         enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridSet

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_GridGetCellMask - Retrieves cell identifier mask for a Grid

! !INTERFACE:
      subroutine ESMF_GridGetCellMask(grid, maskArray, physGridId, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_Array), intent(inout) :: maskArray
      integer, intent(in), optional :: physGridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of get retrieves an {\tt ESMF\_Array} of cell types for an
!     {\tt ESMF\_Grid} from a corresponding {\tt ESMF\_PhysGrid}.
!     This mask is intended for internal use to indicate which cells are in
!     the computational regime (cellType=0), a ghost region (cellType=1), or a
!     halo region (cellType=2).
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[maskArray]
!          {\tt ESMF\_Array} to contain the internally-used cell array denoting
!          whether cells are in the computational regime, a ghost region, or a
!          halo region.
!     \item[{[physGridId]}]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[{[relloc]}]
!          Relative location in grid cell for this PhysGrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: physIdUse
      logical :: rellocIsValid, physIdIsValid

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initialize other variables
      physIdUse = -1
      rellocIsValid = .false.
      physIdIsValid = .false.

      ! Either the relative location or PhysGridId must be present and valid
      if (present(relloc)) then
!        rellocIsValid = ESMF_RelLocIsValid(relloc)  TODO: assume OK if there for now
        rellocIsValid = .true.
      endif
      if (present(physGridId)) then
        if ((physGridId.ge.1) .and. (physGridId.le.grid%ptr%numPhysGrids)) then
          physIdIsValid = .true.
          physIdUse = physGridId
       endif
      endif
      if (.not.(rellocIsValid .or. physIdIsValid)) then
        print *, "ERROR in ESMF_GridGetCellMask: ", &
                 "need either relloc or physGridId"
        return
      endif
      ! If there is a relloc but no PhysGrid id, then get the id from the relloc
      if (rellocIsValid .and. .not.(physIdIsValid)) then
        call ESMF_GridGetPhysGridId(grid%ptr, relloc, physIdUse, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridGetCellMask: get PhysGrid id"
          return
        endif
        if (physIdUse.eq.-1) then
          print *, "ERROR in ESMF_GridGetCellMask: ", &
                   "no PhysGrid corresponding to relloc"
          return
        endif
      endif

      ! call PhysGrid with the valid Id
      call ESMF_PhysGridGetMask(grid%ptr%physGrids(physIdUse), maskArray, id=1, &
                                rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetCellMask: PhysGrid get mask"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_GridGetCellMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMaskFromArray - Set a mask in a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetMaskFromArray(grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the logical mask data exists already and is
!     being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item [{[name]}]
!           {\tt LMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMaskFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMaskFromBuffer - Set a mask in a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetMaskFromBuffer(grid, buffer, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real, dimension (:), pointer :: buffer
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the logical mask data exists already and is
!     being passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt ESMF\_Mask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMaskFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetLMaskFromMMask - Set a logical mask in a Grid from an existing multiplicative mask

! !INTERFACE:
      subroutine ESMF_GridSetLMaskFromMMask(grid, mmask, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: mmask
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the logical mask data will be
!     created from an existing multiplicative mask.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[mmask]
!          Multiplicative mask identifier.
!     \item [{[name]}]
!           {\tt ESMF\_LMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetLMaskFromMMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMaskCopy - Copies a mask from one grid to another.

! !INTERFACE:
      subroutine ESMF_GridSetMaskCopy(grid, gridIn, name, nameIn, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: gridIn
      character (len=*), intent(in), optional :: name
      character (len=*), intent(in), optional :: nameIn
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a logical mask for a {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[gridIn]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [{[name]}]
!           {\tt ESMF\_Mask} name to be set.
!     \item [{[nameIn]}]
!           {\tt ESMF\_Mask} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMaskCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskFromArray - Set a multiplicative mask in a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetMMaskFromArray(grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the multiplicative mask data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item [{[name]}]
!           {\tt ESMF\_MMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskFromBuffer - Set a multiplicative mask in a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetMMaskFromBuffer(grid, buffer, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real, dimension (:), pointer :: buffer
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the multiplicative mask data exists already
!     and is being passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt ESMF\_MMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskFromLMask - Set a multiplicative mask in a Grid from an existing logical mask

! !INTERFACE:
      subroutine ESMF_GridSetMMaskFromLMask(grid, lmask, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: lmask
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the multiplicative mask data will be
!     created from an existing logical mask.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[lmask]
!          Logical mask identifier.
!     \item [{[name]}]
!           {\tt ESMF\_MMask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskFromLMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMMaskCopy - Copies a multiplicative mask from one grid to another.

! !INTERFACE:
      subroutine ESMF_GridSetMMaskCopy(grid, gridIn, name, nameIn, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: gridIn
      character (len=*), intent(in), optional :: name
      character (len=*), intent(in), optional :: nameIn
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a multiplicative mask for a {\tt ESMF\_Grid} from another
!     {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[gridIn]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [{[name]}]
!           {\tt ESMF\_MMask} name to be set.
!     \item [{[nameIn]}]
!           {\tt ESMF\_MMask} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMMaskCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetricFromArray - Set a metric for a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_GridSetMetricFromArray(grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the metric data exists already and is being
!     passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item [{[name]}]
!           {\tt ESMF\_Metric} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetricFromBuffer - Set a metric for a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_GridSetMetricFromBuffer(grid, buffer, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real, dimension (:), pointer :: buffer
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the metric data exists already and is being
!     passed in as a raw data buffer.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[buffer]
!          Raw data buffer.
!     \item [{[name]}]
!           {\tt ESMF\_Metric} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetricCompute - Compute a metric for a Grid

! !INTERFACE:
      subroutine ESMF_GridSetMetricCompute(grid, name, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: id
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes a metric for a {\tt ESMF\_Grid} via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[id]
!          Identifier for predescribed metrics.  TODO: make list
!     \item [{[name]}]
!           {\tt ESMF\_Metric} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricCompute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridSetMetricCopy - Copies a metric from one grid to another

! !INTERFACE:
      subroutine ESMF_GridSetMetricCopy(grid, name, gridIn, nameIn, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: name  ! TODO: optional?
      type(ESMF_Grid), intent(in) :: gridIn
      character (len=*), intent(in) :: nameIn  ! TODO: optional?
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a metric for a {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item [name]
!           {\tt ESMF\_Metric} name to be set.
!     \item[gridIn]
!          Pointer to a {\tt ESMF\_Grid} whose coordinates are to be copied.
!     \item [nameIn]
!           {\tt ESMF\_Metric} name to be copied.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

!
!  code goes here
!
      end subroutine ESMF_GridSetMetricCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridBoxIntersectRecv - Determine a DomainList covering a box

! !INTERFACE:
      subroutine ESMF_GridBoxIntersectRecv(grid, &
                                           localMinPerDim, localMaxPerDim, &
                                           domainList, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      real(ESMF_KIND_R8), dimension(:), intent(in) :: localMinPerDim
                                                         ! array of local mins
      real(ESMF_KIND_R8), dimension(:), intent(in) :: localMaxPerDim
                                                         ! array of local maxs
      type(ESMF_DomainList), intent(inout) :: domainList ! domain list
      logical, intent(in), optional :: total             ! flag to indicate
                                                         ! total cells in the
                                                         ! domainList
      integer, intent(out), optional :: rc               ! return code

! !DESCRIPTION:
!     This routine computes the DomainList necessary to cover a given "box"
!     described by an array of min/max's.  This routine is for the case of
!     a DE that is part of a destination Grid determining which DEs it will
!     receive data from.

!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Source {\tt ESMF\_Grid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[localMinPerDim]
!          Array of local minimum coordinates, one per rank of the array,
!          defining the "box."
!     \item[localMaxPerDim]
!          Array of local maximum coordinates, one per rank of the array,
!          defining the "box."
!     \item[domainList]
!          Resulting {\tt ESMF\_DomainList} containing the set of
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[{[total]}]
!          Logical flag to indicate the domainList should use total cells
!          instead of computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      if (.not. associated(grid%ptr)) then
        print *, "Empty or Uninitialized Grid"
        return
      endif

!     Call intersect routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridBoxIntersectRecv: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        call ESMF_LRGridBoxIntersectRecv(grid, localMinPerDim, localMaxPerDim, &
                                         domainList, total, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridBoxIntersectRecv: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridBoxIntersectRecv: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridBoxIntersectRecv: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
         print *, "ERROR in ESMF_GridBoxIntersectRecv: Invalid grid structure"
         status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, "ERROR in ESMF_GridBoxIntersectRecv: ", &
                 "error in specific intersect code."
        return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridBoxIntersectRecv

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridBoxIntersectSend - Determine a DomainList covering a box

! !INTERFACE:
      subroutine ESMF_GridBoxIntersectSend(dstGrid, srcGrid, &
                                           localMinPerDim, localMaxPerDim, &
                                           myAI, domainList, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: dstGrid
      type(ESMF_Grid) :: srcGrid
      real(ESMF_KIND_R8), dimension(:), intent(in) :: localMinPerDim
                                                         ! array of local mins
      real(ESMF_KIND_R8), dimension(:), intent(in) :: localMaxPerDim
                                                         ! array of local maxs
      type(ESMF_AxisIndex), dimension(:), intent(in) :: myAI
      type(ESMF_DomainList), intent(inout) :: domainList ! domain list
      integer, intent(out), optional :: rc               ! return code

! !DESCRIPTION:
!     This routine computes the DomainList necessary to cover a given "box"
!     described by an array of min/max's.  This routine is for the case of
!     a DE that is part of a source Grid determining which DEs it will send
!     its data to.
!
!     The arguments are:
!     \begin{description}
!     \item[dstGrid]
!          Destination {\tt ESMF\_Grid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[srcGrid]
!          Source {\tt ESMF\_Grid} to use to calculate the resulting
!          {\tt ESMF\_DomainList}.
!     \item[localMinPerDim]
!          Array of local minimum coordinates, one per rank of the array,
!          defining the "box."
!     \item[localMaxPerDim]
!          Array of local maximum coordinates, one per rank of the array,
!          defining the "box."
!     \item[myAI]
!          {\tt ESMF\_AxisIndex} for this DE on the sending (source)
!          {\tt ESMF\_Grid}, assumed to be in global indexing.
!     \item[domainList]
!          Resulting {\tt ESMF\_DomainList} containing the set of
!          {\tt ESMF\_Domains} necessary to cover the box.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      if ((.not. associated(dstGrid%ptr)) .or. &
          (.not. associated(srcGrid%ptr))) then
        print *, "Empty or Uninitialized Grid"
        return
      endif

!     Call intersect routines based on GridStructure

      select case(srcGrid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        print *, "ERROR in ESMF_GridBoxIntersectSend: ", &
                 "GridStructureUnknown not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        call ESMF_LRGridBoxIntersectSend(dstGrid, srcGrid, &
                                         localMinPerDim, localMaxPerDim, &
                                         myAI, domainList, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridBoxIntersectSend: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridBoxIntersectSend: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridBoxIntersectSend: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
         print *, "ERROR in ESMF_GridBoxIntersectSend: Invalid grid structure"
         status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, "ERROR in ESMF_GridBoxIntersectSend: ", &
                 "error in specific intersect code."
        return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridBoxIntersectSend

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridValidate - Check internal consistency of a Grid

! !INTERFACE:
      subroutine ESMF_GridValidate(grid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that a {\tt ESMF\_Grid} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[opt]}]
!          Validation options.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      if (.not. associated(grid%ptr)) then
        print *, "Empty or Uninitialized Grid"
        return
      endif

!     Call validate routines based on GridStructure

      select case(grid%ptr%gridStructure%gridStructure)

      !-------------
      ! ESMF_GridStructure_Unknown
      case(0)
        ! FIXME:  field needs to know the grid is ok -
        !print *, "ERROR in ESMF_GridValidate: ", &
        !         "GridStructureUnknown not supported"
        !status = ESMF_FAILURE
        print *, "Grid created, but Structure unknown"
        status = ESMF_SUCCESS

      !-------------
      ! ESMF_GridStructure_LogRect
      case(1)
        call ESMF_LRGridValidate(grid, opt, status)

      !-------------
      ! ESMF_GridStructure_LogRectBlock
      case(2)
        print *, "ERROR in ESMF_GridValidate: ", &
                 "GridStructureLogRectBlock not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_Unstruct
      case(3)
        print *, "ERROR in ESMF_GridValidate: ", &
                 "GridStructureUnstruct not supported"
        status = ESMF_FAILURE

      !-------------
      ! ESMF_GridStructure_User
      case(4)
        print *, "ERROR in ESMF_GridValidate: ", &
                 "GridStructureUser not supported"
        status = ESMF_FAILURE

      !-------------
      case default
         print *, "ERROR in ESMF_GridValidate: Invalid grid structure"
         status = ESMF_FAILURE
      end select

      if (status /= ESMF_SUCCESS) then
        rc = status
        print *, 'ERROR in ESMF_GridValidate: error in specific validate'
        return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridPrint - Print the contents of a Grid

! !INTERFACE:
      subroutine ESMF_GridPrint(grid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      character (len=*), intent(in) :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Print information about a {\t ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[opt]
!          Print options that control the type of information and level of
!          detail.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      character(len=ESMF_MAXSTR) :: name, str
      type(ESMF_GridType), pointer :: gp
      integer :: i
      integer :: status

      if (present(rc)) rc = ESMF_FAILURE

      print *, "********Begin Grid Print:"
      if (.not. associated(grid%ptr)) then
        print *, "Empty or Uninitialized Grid"
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      !TODO: complete prints

      gp => grid%ptr
  !    call ESMF_StatusString(gp%gridStatus, str, rc)
  !    print *, "Grid status = ", trim(str)

      if (gp%gridStatus /= ESMF_GridStatus_Ready) then
        if (present(rc)) rc = ESMF_FAILURE
        return
      endif

      call ESMF_GetName(gp%base, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridGetName"
        return
      endif
      print *, "  Name = '",  trim(name), "'"

      ! TODO: add calls to //physgrid-Done\\ and distgrid prints

      ! Print the Associated physgrids
      print *, 'PhysGrids associated with this grid:'
      do i=1, gp%numPhysGrids
        call ESMF_PhysGridPrint(gp%physgrids(i), 'no-opt')
      enddo

      ! Print the DistGrid
      print *, 'DistGrids associated with this Grid:'
      do i=1, gp%numDistGrids
        call ESMF_DistGridPrint(gp%distgrids(i), 'no-opt')
      enddo

      print *, "*********End Grid Print"

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_GridPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridComputeDistance - Compute distance between points
!
! !INTERFACE:
      function ESMF_GridComputeDistance(x1, y1, x2, y2, coordSystem, rc)

! !RETURN VALUE:
      real(ESMF_KIND_R8) :: ESMF_GridComputeDistance

! !ARGUMENTS:

      real(ESMF_KIND_R8), intent(in) :: x1      ! x,y coordinates of two points
      real(ESMF_KIND_R8), intent(in) :: y1      ! between which the distance is
      real(ESMF_KIND_R8), intent(in) :: x2      ! to be computed
      real(ESMF_KIND_R8), intent(in) :: y2
      type(ESMF_CoordSystem) :: coordSystem    ! coordinate system in which the
                                                ! points are given
      integer, optional :: rc                   ! return code

! !DESCRIPTION:
!     This routine computes the distance between two points given the
!     coordinates of the two points.
!
!     The arguments are:
!     \begin{description}
!     \item[x1,y1,x2,y2]
!          Coordinates of two points between which to compute distance.
!     \item[coordSystem]
!          Coordinate system in which the points are given
!          (e.g. spherical, Cartesian)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  SSSn.n, GGGn.n
!EOP

      integer :: status
!
!     branch to appropriate PhysGrid routine to compute
!     distance
!
      status = ESMF_SUCCESS

      if(coordSystem .eq. ESMF_CoordSystem_Spherical) then
        ESMF_GridComputeDistance = &
          ESMF_PhysGridCompDistSpherical(x1, y1, x2, y2, rc=status)
      elseif(coordSystem .eq. ESMF_CoordSystem_Cartesian) then
        ESMF_GridComputeDistance = &
          ESMF_PhysGridCompDistCartesian(x1, y1, x2, y2, rc=status)
      else
        print *,'Distance in coordinate system not yet supported'
        status = ESMF_FAILURE
      endif
!
!     set return code and exit
!
      if (present(rc)) rc = status
      return

      end function ESMF_GridComputeDistance

!------------------------------------------------------------------------------
!!BOP
!! !IROUTINE: ESMF_GridSearchPoint - Search the grid for a cell containing point
!
! !INTERFACE:
!      subroutine ESMF_GridSearchPoint(dstAdd, x, y, DEID, searchGrid, &
!                                      physGridID, rc)
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) :: dstAdd       ! location in grid of grid cell
!                                            ! containing search point
!      real (kind=?), intent(in) :: x        ! x coordinates of search point 
!      real (kind=?), intent(in) :: y        ! y coordinates of search point 
!      integer, intent(in) :: DEID           ! DE which owns the search point
!      type(ESMF_Grid), intent(in) :: searchGrid
!                                            ! grid to search for location of point
!      integer, intent(in), optional :: physGridID
!                                            ! id of the subgrid to search
!                                            ! (if more than one subgrid)
!      integer, intent(out), optional :: rc  ! return code
!
!!
!! !DESCRIPTION:
!!     This routine searches for the location in the grid of a grid cell 
!!     containing the point given by the input x,y coordinates.
!!
!!     The arguments are:
!!     \begin{description}
!!     \item[dstAdd]
!!          Address of grid cell containing the search point.
!!     \item[x]
!!          X coordinates of search point.
!!     \item[y]
!!          Y coordinates of search point.
!!     \item[DEID]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[searchGrid]
!!          ESMF {\tt ESMF\_Grid} to search for location.
!!     \item[{[physGridID]}]
!!          If more than one {\tt ESMF\_PhysGrid} is contained in 
!!          {\tt ESMF\_Grid}, choose which grid to search (default is 1st
!!          {\tt ESMF\_PhysGrid}?).
!!     \item[{[rc]}]
!!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!!     \end{description}
!!
!!EOP
!! !REQUIREMENTS:  SSSn.n, GGGn.n
!
!      integer :: status                           ! Error status
!      logical :: rcpresent                        ! Return code present
!
!!     Initialize return code
!      status = ESMF_FAILURE
!      rcpresent = .FALSE.
!      if(present(rc)) then
!        rcpresent=.TRUE.
!        rc = ESMF_FAILURE
!      endif
!
!!     Call Search routines based on GridStructure
!
!      select case(grid%ptr%gridStructure%gridStructure)
!
!      !-------------
!      ! ESMF_GridStructure_Unknown
!      case(0)
!        print *, "ERROR in ESMF_GridSearch: ", &
!                 "GridStructureUnknown not supported"
!        status = ESMF_FAILURE
!
!      !-------------
!      ! ESMF_GridStructure_LogRect
!      case(1)
!        call ESMF_LRGridSearchPoint(dstAdd, x, y, DEID, searchGrid, &
!                                    physGridID, status)
!
!      !-------------
!      ! ESMF_GridStructure_LogRectBlock
!      case(2)
!        print *, "ERROR in ESMF_GridSearch: ", &
!                 "GridStructureLogRectBlock not supported"
!        status = ESMF_FAILURE
!
!      !-------------
!      ! ESMF_GridStructure_Unstruct
!      case(3)
!        print *, "ERROR in ESMF_GridSearch: ", &
!                 "GridStructureUnstruct not supported"
!        status = ESMF_FAILURE
!
!      !-------------
!      ! ESMF_GridStructure_User
!      case(4)
!        print *, "ERROR in ESMF_GridSearch: ", &
!                 "GridStructureUser not supported"
!        status = ESMF_FAILURE
!
!      !-------------
!      case default
!         print *, "ERROR in ESMF_GridSearch: Invalid grid structure"
!         status = ESMF_FAILURE
!      end select
!
!      if (status /= ESMF_SUCCESS) then
!        rc = status
!        print *, 'ERROR in ESMF_GridSearch: error in search'
!        return
!      endif
!
!      if (present(rc)) rc = ESMF_SUCCESS
!
!      end subroutine ESMF_GridSearchPoint
!
!------------------------------------------------------------------------------

      end module ESMF_GridMod
