! $Id: ESMF_LogRectGrid.F90,v 1.6 2004/01/28 21:46:48 nscollins Exp $
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
!     ESMF LogRectGrid Module
      module ESMF_LogRectGridMod
!
!==============================================================================
!
! This file contains the Grid class definition and all Grid class
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!!#include "ESMF_Grid.h"  ! unneeded
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_LogRectGridMod - LogRectGrid class
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_LogRectGrid} class.  
! This class provides a unified interface for both {\tt ESMF\_PhysGrid} and 
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

    public ESMF_LRGridCreateRead
    public ESMF_LRGridCreateCopy
    public ESMF_LRGridCreateCutout
    public ESMF_LRGridCreateDiffRes
    public ESMF_LRGridCreateExchange
    public ESMF_LRGridAddPhysGrid
    public ESMF_LRGridGetCoord
    public ESMF_LRGridSetCoord
    public ESMF_LRGridGetDE            ! access DistGrid from above
    public ESMF_LRGridGetAllAxisIndex  ! access DistGrid from above
    public ESMF_LRGridGlobalToLocalIndex
    public ESMF_LRGridLocalToGlobalIndex
    public ESMF_LRGridGet
    public ESMF_LRGridSet
    public ESMF_LRGridSetCellMask
    !public ESMF_LRGridGetMask
    public ESMF_LRGridSetMask
    !public ESMF_LRGridGetMetric
    public ESMF_LRGridSetMetric
    public ESMF_LRGridSetBoundingBoxes
    public ESMF_LRGridValidate
    public ESMF_LRGridBoxIntersectRecv
    public ESMF_LRGridBoxIntersectSend
    public ESMF_LRGridDestruct
    !public ESMF_LRGridSearch

!------------------------------------------------------------------------------
!
! !PUBLIC DATA MEMBERS:

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_LogRectGrid.F90,v 1.6 2004/01/28 21:46:48 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_LRGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_LRGridConstructSpecd
         module procedure ESMF_LRGridConstructUniform

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct a
!     complete {\tt ESMF\_Grid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_LRGridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_LRGridSetCoordFromArray
         module procedure ESMF_LRGridSetCoordFromBuffer
#if (PARCH != linux_intel)
         ! don't include the following two in the interface def
         ! for the intel build.  fails on jazz.
         module procedure ESMF_LRGridSetCoordSpecd
         module procedure ESMF_LRGridSetCoordUniform
#endif
         module procedure ESMF_LRGridSetCoordCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     coordinates as part of a {\tt ESMF\_Grid}.

!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_LRGridSetMask

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_LRGridSetMaskFromArray
         module procedure ESMF_LRGridSetMaskFromBuffer
         module procedure ESMF_LRGridSetMaskFromMask
         module procedure ESMF_LRGridSetMaskCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     logical masks as part of a {\tt ESMF\_Grid}.

!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_LRGridSetMetric

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_LRGridSetMetricFromArray
         module procedure ESMF_LRGridSetMetricFromBuffer
         module procedure ESMF_LRGridSetMetricCompute
         module procedure ESMF_LRGridSetMetricCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     metrics as part of a {\tt Grid}.

!EOP
      end interface
!
!------------------------------------------------------------------------------
!!BOPI
!! !INTERFACE:
!      interface ESMF_LRGridSearch
!
!! !PRIVATE MEMBER FUNCTIONS:
!         module procedure ESMF_LRGridSearchPoint
!         module procedure ESMF_LRGridSearchList
!
!! !DESCRIPTION:
!!     This interface provides a single entry point for methods that
!!     search a {\tt ESMF\_Grid} for point(s).
!!
!!EOPI
!      end interface
!!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
       interface ESMF_LRGridAddPhysGrid

! !PRIVATE MEMBER FUNCTIONS:
          module procedure ESMF_LRGridAddPhysGridSpecd
          module procedure ESMF_LRGridAddPhysGridUniform
          module procedure ESMF_LRGridAddVertPhysGridUni

! !DESCRIPTION:
!     This interface provides a single entry point for methods that
!     search a {\tt ESMF\_Grid} for point(s).

!EOPI
       end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
       interface ESMF_LRGridSetBoundingBoxes

! !PRIVATE MEMBER FUNCTIONS:
          module procedure ESMF_LRGridSetBBoxesUni
          module procedure ESMF_LRGridSetBBoxesSpecd

! !DESCRIPTION:
!     This interface provides a single entry point for methods that
!     set bounding boxes for all the DEs in a {\tt ESMF\_Grid}.

!EOPI
       end interface
!
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
! !IROUTINE: ESMF_GridCreateLogRectUniform - Create a new uniform Grid

! !INTERFACE:
      function ESMF_GridCreateLogRectUniform(numDims, counts, &
                                             minGlobalCoordPerDim, &
                                             maxGlobalCoordPerDim, deltaPerDim, &
                                             horzGridKind, vertGridKind, &
                                             horzStagger, vertStagger, &
                                             horzCoordSystem, vertCoordSystem, &
                                             dimNames, dimUnits, &
                                             coordOrder, coordIndex, periodic, &
                                             layout, decompIds, &
                                             name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateLogRectUniform
!
! !ARGUMENTS:
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: &
                                                            maxGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: deltaPerDim
      type(ESMF_GridKind), intent(in), optional :: horzGridKind
      type(ESMF_GridKind), intent(in), optional :: vertGridKind
      type(ESMF_GridStagger), intent(in), optional :: horzStagger
      type(ESMF_GridStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      type(ESMF_CoordIndex), intent(in), optional :: coordIndex
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      type(ESMF_DELayout), intent(in), optional :: layout
      integer, dimension(:), intent(in), optional :: decompIds
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and internally generates the {\tt ESMF\_Grid}.  Return a pointer to
!     the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[numDims]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[minGlobalCoordPerDim]
!          Array of minimum physical coordinates in each dimension.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum physical coordinates in each direction.
!     \item[{[deltaPerDim]}]
!          Array of constant physical increments in each direction.
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
!     \item[{[coordIndex]}]
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate axes.
!     \item[{[layout]}]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[decompIds]}]
!          Identifier for which Grid axes are decomposed.
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
      nullify(ESMF_GridCreateLogRectUniform%ptr)

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
        print *, "ERROR in ESMF_GridCreateLogRectUniform: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_LRGridConstructUniform(grid, numDims, counts, minGlobalCoordPerDim, &
                                       maxGlobalCoordPerDim, deltaPerDim, &
                                       horzGridKind, vertGridKind, &
                                       horzStagger, vertStagger, &
                                       horzCoordSystem, vertCoordSystem, &
                                       dimNames, dimUnits, &
                                       coordOrder, coordIndex, periodic, &
                                       name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateLogRectUniform: Grid construct"
        return
      endif

!     if layout information available, call grid distribute method
      if (present(layout)) then
        call ESMF_LRGridDistributeUniform(grid, layout, decompIds, name, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridCreateLogRectUniform: Grid distribute"
          return
        endif
      endif

!     Set return values.
      ESMF_GridCreateLogRectUniform%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateLogRectUniform

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateLogRect - Create a new specified LogRect Grid internally

! !INTERFACE:
      function ESMF_GridCreateLogRect(numDims, counts, minGlobalCoordPerDim, &
                                      delta1, delta2, delta3, &
                                      coord1, coord2, coord3, &
                                      horzGridKind, vertGridKind, &
                                      horzStagger, vertStagger, &
                                      horzCoordSystem, vertCoordSystem, &
                                      dimNames, dimUnits, &
                                      coordOrder, coordIndex, periodic, &
                                      layout, decompIds, &
                                      countsPerDEDim1, countsPerDEDim2, &
                                      name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateLogRect
!
! !ARGUMENTS:
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord3
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: &
                                                 minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta3
      type(ESMF_GridKind), intent(in), optional :: horzGridKind
      type(ESMF_GridKind), intent(in), optional :: vertGridKind
      type(ESMF_GridStagger), intent(in), optional :: horzStagger
      type(ESMF_GridStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      character (len=*), dimension(numDims), intent(in), optional :: dimNames
      character (len=*), dimension(numDims), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      type(ESMF_CoordIndex), intent(in), optional :: coordIndex
      type(ESMF_Logical), dimension(numDims), intent(in), optional :: periodic
      type(ESMF_DELayout), intent(in), optional :: layout
      integer, dimension(:), intent(in), optional :: decompIds
      integer, dimension(:), intent(in), optional :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and internally generates the {\tt ESMF\_Grid}.  Return a pointer to
!     the new {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[numDims]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[{[minGlobalCoordsPerDim]}]
!          Array of minimum physical coordinate in each direction.
!     \item[{[layout]}]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[delta1]}]
!          Array of physical increments between nodes in the first direction.
!     \item[{[delta2]}]
!          Array of physical increments between nodes in the second direction.
!     \item[{[delta3]}]
!          Array of physical increments between nodes in the third direction.
!     \item[{[coord1]}]
!          Array of physical coordinates in the first direction.
!     \item[{[coord2]}]
!          Array of physical coordinates in the second direction.
!     \item[{[coord3]}]
!          Array of physical coordinates in the third direction.
!     \item[{[decompIds]}]
!          Identifier for which Grid axes are decomposed.
!     \item[{[countsPerDEDim1]}]
!          Array of number of grid increments per DE in the first direction.
!     \item[{[countsPerDEDim2]}]
!          Array of number of grid increments per DE in the second direction.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
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
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_GridType), pointer :: grid    ! Pointer to new grid
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateLogRect%ptr)

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
        print *, "ERROR in ESMF_GridCreateLogRect: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_LRGridConstructSpecd(grid, numDims, coord1, coord2, coord3, &
                                     minGlobalCoordPerDim=minGlobalCoordPerDim, &
                                     delta1=delta1, delta2=delta2, delta3=delta3, &
                                     horzGridKind=horzGridKind, &
                                     vertGridKind=vertGridKind, &
                                     horzStagger=horzStagger, &
                                     vertStagger=vertStagger, &
                                     horzCoordSystem=horzCoordSystem, &
                                     vertCoordSystem=vertCoordSystem, &
                                     dimNames=dimNames, dimunits=dimUnits, &
                                     coordOrder=coordOrder, coordIndex=coordIndex, &
                                     periodic=periodic, name=name, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateLogRect: Grid construct"
        return
      endif

!     if layout information available, call grid distribute method
      if (present(layout)) then
        call ESMF_LRGridDistributeSpecd(grid, layout, countsPerDEDim1, &
                                        countsPerDEDim2, decompIds, name, rc)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridCreateLogRect: Grid distribute"
          return
        endif
      endif

!     Set return values.
      ESMF_GridCreateLogRect%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateLogRect

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridCreateRead - Create a new Grid read in from a file

! !INTERFACE:
      function ESMF_LRGridCreateRead(iospec, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_LRGridCreateRead
!
! !ARGUMENTS:
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

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateRead%ptr)

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
        print *, "ERROR in ESMF_LRGridCreateRead: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateRead: Grid construct"
        return
      endif

!     Set return values.
      ESMF_LRGridCreateRead%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateRead

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridCreateCopy - Create a new Grid by copying another Grid

! !INTERFACE:
      function ESMF_LRGridCreateCopy(gridIn, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_LRGridCreateCopy
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

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateCopy%ptr)

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
        print *, "ERROR in ESMF_LRGridCreateCopy: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateCopy: Grid construct"
        return
      endif

!     Set return values.
      ESMF_LRGridCreateCopy%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridCreateCutout - Create a new Grid as a subset of an existing Grid

! !INTERFACE:
      function ESMF_LRGridCreateCutout(gridIn, min, max, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_LRGridCreateCutout
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

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateCutout%ptr)

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
        print *, "ERROR in ESMF_LRGridCreateCutout: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateCutout: Grid construct"
        return
      endif

!     Set return values.
      ESMF_LRGridCreateCutout%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateCutout

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridCreateDiffRes - Create a new Grid by coarsening or refining an existing Grid

! !INTERFACE:
      function ESMF_LRGridCreateDiffRes(gridIn, resolution, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_LRGridCreateDiffRes
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

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateDiffRes%ptr)

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
        print *, "ERROR in ESMF_LRGridCreateDiffRes: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateDiffRes: Grid construct"
        return
      endif

!     Set return values.
      ESMF_LRGridCreateDiffRes%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateDiffRes

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridCreateExchange - Create a new Grid from the intersection of two existing grids

! !INTERFACE:
      function ESMF_LRGridCreateExchange(gridIn1, gridIn2, name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_LRGridCreateExchange
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

      type(ESMF_GridType), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateExchange%ptr)

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
        print *, "ERROR in ESMF_LRGridCreateExchange: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateExchange: Grid construct"
        return
      endif

!     Set return values.
      ESMF_LRGridCreateExchange%ptr => grid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateExchange

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridConstructUniform - Construct a uniform Grid

! !INTERFACE:
      subroutine ESMF_LRGridConstructUniform(grid, numDims, counts, &
                                             minGlobalCoordPerDim, &
                                             maxGlobalCoordPerDim, &
                                             deltaPerDim, &
                                             horzGridKind, vertGridKind, &
                                             horzStagger, vertStagger, &
                                             horzCoordSystem, vertCoordSystem, &
                                             dimNames, dimUnits, &
                                             coordOrder, coordIndex, &
                                             periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: &
                                                            maxGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: &
                                                            deltaPerDim
      type(ESMF_GridKind), intent(in), optional :: horzGridKind
      type(ESMF_GridKind), intent(in), optional :: vertGridKind
      type(ESMF_GridStagger), intent(in), optional :: horzStagger
      type(ESMF_GridStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      character(len=*), dimension(:), intent(in), optional :: dimNames
      character(len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      type(ESMF_CoordIndex), intent(in), optional :: coordIndex
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_Grid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_GridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_GridCreate}, which calls
!     {\tt ESMF\_GridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid}
!     \item[numDims]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[minGlobalCoordPerDim]
!          Array of minimum physical coordinates in each dimension.
!     \item[maxGlobalCoordPerDim]
!          Array of maximum physical coordinates in each direction.
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
!     \item[{[coordIndex]}]
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate axes.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i
      type (ESMF_LogRectGrid), target :: lrgrid

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridConstructUniform: Grid construct"
        return
      endif

!     Set the Grid name if present, otherwise construct a default one
      if (present(name)) then
         call ESMF_SetName(grid%base, name, "Grid", status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructNew: Setname"
            return
         endif
      endif

!     Fill in logRectGrid derived type with subroutine arguments
      do i = 1,numDims
        lrgrid%countPerDim(i) = counts(i)
        lrgrid%deltaPerDim(i) = &
          (maxGlobalCoordPerDim(i)-minGlobalCoordPerDim(i))/real(counts(i))
      enddo
      grid%gridSpecific%logRectGrid => lrgrid

!     Fill in grid derived type with subroutine arguments
      grid%numDims       = numDims
      grid%gridStructure = ESMF_GridStructure_LogRect
      if(present(horzGridKind   )) grid%horzGridKind    = horzGridKind
      if(present(vertGridKind   )) grid%vertGridKind    = vertGridKind
      if(present(horzStagger    )) grid%horzStagger     = horzStagger
      if(present(vertStagger    )) grid%vertStagger     = vertStagger
      if(present(horzCoordSystem)) grid%horzCoordSystem = horzCoordSystem
      if(present(vertCoordSystem)) grid%vertCoordSystem = vertCoordSystem
      if(present(coordOrder     )) grid%coordOrder      = coordOrder
      if(present(coordIndex     )) grid%coordIndex      = coordIndex

!     Set dimension names and units for each dimension
      if (present(dimNames)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(dimNames)) exit
          grid%dimNames(i) = dimNames(i)
        enddo
      endif
      if (present(dimUnits)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(dimUnits)) exit
          grid%dimUnits(i) = dimUnits(i)
        enddo
      endif

!     Set periodic flags for each dimension
      if (present(periodic)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(periodic)) exit
          grid%periodic(i) = periodic(i)
        enddo
      endif

!     Set global domain limits
      if (size(minGlobalCoordPerDim) > ESMF_MAXGRIDDIM) then
        print *,'ESMF_LRGridConstruct: minGlobalCoordPerDim too big'
        return
      endif
      do i=1,size(minGlobalCoordPerDim)
        grid%minGlobalCoordPerDim(i) = minGlobalCoordPerDim(i)
      enddo

      if (present(maxGlobalCoordPerDim)) then
        if (size(maxGlobalCoordPerDim) > ESMF_MAXGRIDDIM) then
          print *,'ESMF_LRGridConstruct: maxGlobalCoordPerDim too big'
          return
        endif
        do i=1,size(maxGlobalCoordPerDim)
          grid%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDim(i)
        enddo
      endif

      grid%gridStatus = ESMF_GridStatus_Init

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridConstructUniform

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridConstructSpecd - Construct a specified Grid

! !INTERFACE:
      subroutine ESMF_LRGridConstructSpecd(grid, numDims, &
                                           coord1, coord2, coord3, &
                                           minGlobalCoordPerDim, &
                                           delta1, delta2, delta3, &
                                           horzGridKind, vertGridKind, &
                                           horzStagger, vertStagger, &
                                           horzCoordSystem, vertCoordSystem, &
                                           dimNames, dimUnits, &
                                           coordOrder, coordIndex, &
                                           periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord3
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: &
                                                         minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta3
      type(ESMF_GridKind), intent(in), optional :: horzGridKind
      type(ESMF_GridKind), intent(in), optional :: vertGridKind
      type(ESMF_GridStagger), intent(in), optional :: horzStagger
      type(ESMF_GridStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      character (len=*), dimension(numDims), intent(in), optional :: dimNames
      character (len=*), dimension(numDims), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      type(ESMF_CoordIndex), intent(in), optional :: coordIndex
      type(ESMF_Logical), dimension(numDims), intent(in), optional :: periodic
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_Grid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_GridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_GridCreate}, which calls
!     {\tt ESMF\_GridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid}
!     \item[numDims]
!          Number of grid dimensions.
!     \item[{[coord1]}]
!          Array of physical coordinates in the first direction.
!     \item[{[coord2]}]
!          Array of physical coordinates in the second direction.
!     \item[{[coord3]}]
!          Array of physical coordinates in the third direction.
!     \item[{[minGlobalCoordPerDim]}]
!          Array of minimum physical coordinate in each direction.
!     \item[{[delta1]}]
!          Array of physical increments between nodes in the first direction.
!     \item[{[delta2]}]
!          Array of physical increments between nodes in the second direction.
!     \item[{[delta3]}]
!          Array of physical increments between nodes in the third direction.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
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
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i
      integer, dimension(numDims) :: counts
      real(ESMF_KIND_R8), dimension(numDims) :: minGlobalCoordPerDimUse, &
                                                maxGlobalCoordPerDimUse
      real(ESMF_KIND_R8), dimension(:), pointer :: coordsUse1, coordsUse2, &
                                                   coordsUse3
      type(ESMF_LocalArray), dimension(:), pointer :: coords
      type(ESMF_LogRectGrid), target :: lrgrid

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      call ESMF_GridConstructNew(grid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridConstructSpecd: Grid construct"
        return
      endif

!     Set the Grid name if present, otherwise construct a default one
      if (present(name)) then
         call ESMF_SetName(grid%base, name, "Grid", status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_GridConstructNew: Setname"
            return
         endif
      endif

      allocate(coords(numDims), stat=status)

!     Two ways to make a grid in this method: by coordinates or by minima and deltas
!     by coordinates:
      if (present(coord1)) then    ! for now, assume if coord1 array is there then
                                   ! using coords for all directions
        allocate(coordsUse1(size(coord1)))
        do i = 1,size(coord1)
          coordsUse1(i) = coord1(i)
        enddo
        coords(1) = ESMF_LocalArrayCreate(coordsUse1, ESMF_DATA_COPY, status)
        counts(1) = size(coord1) - 1   !TODO: coords indicate corner points
                                       !      or center points?
        minGlobalCoordPerDimUse(1) = minval(coord1)
        maxGlobalCoordPerDimUse(1) = maxval(coord1)
        if (present(coord2)) then
          if (numDims.le.1) then
            print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                     "numDims not consistent with coords arrays"
            return
          endif
          allocate(coordsUse2(size(coord2)))
          do i = 1,size(coord2)
            coordsUse2(i) = coord2(i)
          enddo
          coords(2) = ESMF_LocalArrayCreate(coordsUse2, ESMF_DATA_COPY, status)
          counts(2) = size(coord2) - 1
          minGlobalCoordPerDimUse(2) = minval(coord2)
          maxGlobalCoordPerDimUse(2) = maxval(coord2)
        endif
        if (present(coord3)) then
          if (numDims.le.2) then
            print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                     "numDims not consistent with coords arrays"
            return
          endif
          allocate(coordsUse3(size(coord3)))
          do i = 1,size(coord3)
            coordsUse3(i) = coord3(i)
          enddo
          coords(3) = ESMF_LocalArrayCreate(coordsUse3, ESMF_DATA_COPY, status)
          counts(3) = size(coord3) - 1
          minGlobalCoordPerDimUse(3) = minval(coord3)
          maxGlobalCoordPerDimUse(3) = maxval(coord3)
        endif

!     by deltas (and minCoordPerDim):     TODO: make it a starting value instead of min
      elseif (present(delta1)) then
        counts(1) = size(delta1)
        allocate(coordsUse1(size(delta1)+1))
        coordsUse1(1) = minGlobalCoordPerDim(1)    ! TODO: make sure it's here
        do i = 1,size(delta1)
          coordsUse1(i+1) = coordsUse1(i) + delta1(i)
        enddo
        coords(1) = ESMF_LocalArrayCreate(coordsUse1, ESMF_DATA_COPY, status)
        minGlobalCoordPerDimUse(1) = minGlobalCoordPerDimUse(1)
        maxGlobalCoordPerDimUse(1) = maxval(coordsUse1)
        ! TODO: redefine minGlobalCoordPerDim in case deltas are negative
        if (present(delta2)) then
          if (numDims.le.1) then
            print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                     "numDims not consistent with deltas arrays"
            return
          endif
          counts(2) = size(delta2)
          allocate(coordsUse2(size(delta2)+1))
          coordsUse2(1) = minGlobalCoordPerDim(2)    ! TODO: make sure it's here
          do i = 1,size(delta2)
            coordsUse2(i+1) = coordsUse2(i) + delta2(i)
          enddo
          coords(2) = ESMF_LocalArrayCreate(coordsUse2, ESMF_DATA_COPY, status)
          minGlobalCoordPerDimUse(2) = minGlobalCoordPerDimUse(2)
          maxGlobalCoordPerDimUse(2) = maxval(coordsUse2)
        endif
        if(present(delta3)) then
          if (numDims.le.2) then
            print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                     "numDims not consistent with deltas arrays"
            return
          endif
          counts(3) = size(delta3)
          allocate(coordsUse3(size(delta3)+1))
          coordsUse3(1) = minGlobalCoordPerDim(3)    ! TODO: make sure it's here
          do i = 1,size(delta3)
            coordsUse3(i+1) = coordsUse3(i) + delta3(i)
          enddo
          coords(3) = ESMF_LocalArrayCreate(coordsUse3, ESMF_DATA_COPY, status)
          minGlobalCoordPerDimUse(3) = minGlobalCoordPerDimUse(3)
          maxGlobalCoordPerDimUse(3) = maxval(coordsUse3)
        endif
      else
        print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                 "must have either coords arrays or delta arrays"
        return
      endif

!     Fill in logRectGrid derived type with subroutine arguments
      do i = 1,numDims
        lrgrid%countPerDim(i) = counts(i)
        lrgrid%coords         = coords
      enddo
      grid%gridSpecific%logRectGrid => lrgrid

!     Fill in grid derived type with subroutine arguments
      grid%numDims       = numDims
      grid%gridStructure = ESMF_GridStructure_LogRect
      if(present(horzGridKind   )) grid%horzGridKind    = horzGridKind
      if(present(vertGridKind   )) grid%vertGridKind    = vertGridKind
      if(present(horzStagger    )) grid%horzStagger     = horzStagger
      if(present(vertStagger    )) grid%vertStagger     = vertStagger
      if(present(horzCoordSystem)) grid%horzCoordSystem = horzCoordSystem
      if(present(vertCoordSystem)) grid%vertCoordSystem = vertCoordSystem
      if(present(coordOrder     )) grid%coordOrder      = coordOrder
      if(present(coordIndex     )) grid%coordIndex      = coordIndex

!     Set dimension names and units for each dimension
      if (present(dimNames)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(dimNames)) exit
          grid%dimNames(i) = dimNames(i)
        enddo
      endif
      if (present(dimUnits)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(dimUnits)) exit
          grid%dimUnits(i) = dimUnits(i)
        enddo
      endif

!     Set periodic flags for each dimension
      if (present(periodic)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(periodic)) exit
          grid%periodic(i) = periodic(i)
        enddo
      endif

!     Set global domain limits
      do i=1,size(minGlobalCoordPerDimUse)
        grid%minGlobalCoordPerDim(i) = minGlobalCoordPerDimUse(i)
      enddo
      do i=1,size(maxGlobalCoordPerDimUse)
        grid%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDimUse(i)
      enddo

      grid%gridStatus = ESMF_GridStatus_Init

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridConstructSpecd

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridDistributeUniform - Distribute a uniform Grid

! !INTERFACE:
      subroutine ESMF_LRGridDistributeUniform(grid, layout, decompIds, &
                                              name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in), optional :: decompIds
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_Grid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_GridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_GridCreate}, which calls
!     {\tt ESMF\_GridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid}
!     \item[layout]
!         {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[decompIds]
!          Identifier for which Grid axes are decomposed.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      character(len=ESMF_MAXSTR) :: distGridName, physGridName
      character(len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: dimNames
      character(len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: dimUnits
      integer :: distGridId, physGridId
      integer :: i, numDE1, numDE2, numDims, numDimsGrid
      integer, dimension(ESMF_MAXGRIDDIM) :: decompIdsUse, counts
      integer, dimension(:,:), pointer :: countsPerAxis
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: delta, min, max
      type(ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
      type(ESMF_RelLoc) :: relloc

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Fill in defaults for necessary but optional variables
      do i = 1,ESMF_MAXGRIDDIM
        decompIdsUse(i) = 0
      enddo
      if(present(decompIds)) then
        decompIdsUse = decompIds
      else
        do i = 1, numDims
          decompIdsUse(i) = i
        enddo
      endif

!     Extract some information from the Grid
      numDims = grid%numDims
      do i = 1,numDims
        counts(i)   = grid%gridSpecific%logRectGrid%countPerDim(i)
        min(i)      = grid%minGlobalCoordPerDim(i)
        max(i)      = grid%maxGlobalCoordPerDim(i)
        periodic(i) = grid%periodic(i)
        dimNames(i) = grid%dimNames(i)
        dimUnits(i) = grid%dimUnits(i)
      enddo

!     Create PhysGrid and DistGrid at cell center
      numDimsGrid = numDims
      if (numDims.eq.3) numDimsGrid = 2
      distGridId = 1
      distGridName = 'cell_center'
      physGridId = 1
      physGridName = 'cell_center'
      relloc = ESMF_CELL_CENTER
      call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                  layout, decompIdsUse(1:2), periodic, &
                                  distGridName=distGridName, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistributeUniform: Add DistGrid"
        return
      endif
      call ESMF_LRGridAddPhysGrid(grid, numDimsGrid, counts, physGridId, &
                                  distGridId, relloc, min, max, physGridName, &
                                  status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistributeUniform: Add PhysGrid"
        return
      endif
      distGridId = distGridId + 1 
      physGridId = physGridId + 1 

!     Create any other PhysGrids necessary for horizontal grid stagger
!     TODO: finish filling out, look up D
      select case (grid%horzStagger%stagger)

        ! Arakawa A (centered velocity)
        case (1)

        ! Arakawa B (velocities at grid corner)
        case (2)
          distGridName = 'cell_necorner'
          physGridName = 'cell_necorner'
          relloc = ESMF_CELL_NECORNER
          call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      distGridName=distGridName, rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeUniform: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, numDimsGrid, counts, physGridId, &
                                      distGridId, relloc, min, max, &
                                      physGridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeUniform: Add PhysGrid"
            return
          endif
          distGridId = distGridId + 1
          physGridId = physGridId + 1 

        ! Arakawa C (velocities at cell faces)
        case (3)
          distGridName = 'cell_eface'
          physGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      distGridName=distGridName, rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeUniform: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, numDimsGrid, counts, physGridId, &
                                      distGridId, relloc, min, max, &
                                      physGridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeUniform: Add PhysGrid"
            return
          endif
          distGridId = distGridId + 1
          physGridId = physGridId + 1

        ! Arakawa D
        case (4)
          distGridName = 'cell_nface'
          physGridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      distGridName=distGridName, rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeUniform: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, numDimsGrid, counts, physGridId, &
                                      distGridId, relloc, min, max, &
                                      physGridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeUniform: Add PhysGrid"
            return
          endif
          distGridId = distGridId + 1
          physGridId = physGridId + 1

          distGridName = 'cell_eface'
          physGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      distGridName=distGridName, rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeUniform: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, numDimsGrid, counts, physGridId, &
                                      distGridId, relloc, min, max, &
                                      physGridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeUniform: Add PhysGrid"
            return
          endif
          distGridId = distGridId + 1
          physGridId = physGridId + 1

      end select

!     Create vertical physGrid if requested
      if (numDims.eq.3) then
        distGridName = 'vertical center'
        physGridName = 'vertical center'
        relloc = ESMF_CELL_CELL    ! TODO: right relloc?
        call ESMF_LRGridAddDistGrid(grid, distGridId, 1, counts(3:3), &
                                    layout, decompIdsUse(3:3), &
                                    periodic(3:3), &
                                    distGridName=distGridName, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridDistributeUniform: Add DistGrid"
          return
        endif
        call ESMF_LRGridAddVertPhysGridUni(grid, counts(3), physGridId, &
                                           distGridId, relloc, min(3), &
                                           max(3), physGridName, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridDistributeUniform: Add vert PhysGrid"
          return
        endif
        distGridId = distGridId + 1
        physGridId = physGridId + 1

        select case (grid%vertStagger%stagger)

          ! ESMF_GridStagger_VertCenter - vertical velocity at vertical midpoints
          case (7)

          ! ESMF_GridStagger_VertFace - vertical velocity at vertical face
          case (8)
            distGridName = 'vertical top face'
            physGridName = 'vertical top face'
            relloc = ESMF_CELL_TOPFACE
            call ESMF_LRGridAddDistGrid(grid, distGridId, 1, counts(3:3), &
                                        layout, decompIdsUse(3:3), &
                                        periodic(3:3), &
                                        distGridName=distGridName, rc=status)
            if(status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridDistributeUniform: Add DistGrid"
              return
            endif
            call ESMF_LRGridAddVertPhysGridUni(grid, counts(3), physGridId, &
                                               distGridId, relloc, min(3), &
                                               max(3), physGridName, status)
            if(status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridDistributeUniform: Add vert PhysGrid"
              return
            endif
            distGridId = distGridId + 1
            physGridId = physGridId + 1

! TODO: add default in case vertical stagger is not defined
        end select
      endif

!     Create the BoundingBoxes structure
      do i = 1,numDims
        if (counts(i).ne.0) then
          delta(i) = (max(i)-min(i)) / real(counts(i))
        endif
      enddo
      call ESMF_DELayoutGetSize(layout, numDE1, numDE2, status)
      allocate(countsPerAxis(numDE1*numDE2, ESMF_MAXGRIDDIM), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, countsperaxis(DE1*DE2,maxgrid) =", &
                              numDE1, numDE2, ESMF_MAXGRIDDIM
         return
      endif
      call ESMF_DistGridGetAllCounts(grid%distgrids(1)%ptr, countsPerAxis, &
                                     rc=status)
      call ESMF_LRGridSetBoundingBoxes(grid, numDims, min, delta, &
                                       countsPerAxis, numDE1, numDE2, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistributeUniform: Grid set boxes"
        return
      endif

      ! clean up
      deallocate(countsPerAxis)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridDistributeUniform

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridDistributeSpecd - Distribute a specified Grid

! !INTERFACE:
      subroutine ESMF_LRGridDistributeSpecd(grid, layout, &
                                            countsPerDEDim1, countsPerDEDim2, &
                                            decompIds, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in), optional :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      integer, dimension(:), intent(in), optional :: decompIds
      character (len = *), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_Grid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_GridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_GridCreate}, which calls
!     {\tt ESMF\_GridConstruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid}
!     \item[numDims]
!          Number of grid dimensions.
!     \item[layout]
!         {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[countsPerDEDim1]
!          Array of number of grid increments per DE in the x-direction.
!     \item[countsPerDEDim2]
!          Array of number of grid increments per DE in the y-direction.
!     \item[{[decompIds]}]
!          Identifier for which Grid axes are decomposed.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      character(len=ESMF_MAXSTR) :: distGridName, physGridName
      character(len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: dimNames
      character(len=ESMF_MAXSTR), dimension(ESMF_MAXGRIDDIM) :: dimUnits
      integer :: distGridId, physGridId
      integer :: i, counts(ESMF_MAXGRIDDIM), numDims, numDimsGrid
      integer, dimension(ESMF_MAXGRIDDIM) :: decompIdsUse
      real(ESMF_KIND_R8), dimension(ESMF_MAXGRIDDIM) :: min, max
      real(ESMF_KIND_R8), dimension(:), pointer :: coord1, coord2, coord3
      type(ESMF_Logical), dimension(ESMF_MAXGRIDDIM) :: periodic
      type(ESMF_LocalArray), dimension(:), pointer :: coords
      type(ESMF_RelLoc) :: relloc

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Fill in defaults for necessary but optional variables
!  TODO: fill in countsPerDEDim if not present
      do i = 1,ESMF_MAXGRIDDIM
        decompIdsUse(i) = 0
      enddo
      if(present(decompIds)) then
        decompIdsUse = decompIds
      else
        do i = 1, numDims
          decompIdsUse(i) = i
        enddo
      endif

!     Extract some information from the Grid
      numDims = grid%numDims
      allocate(coords(numDims))
      coords  = grid%gridSpecific%logRectGrid%coords
      do i = 1,numDims
        counts(i)   = grid%gridSpecific%logRectGrid%countPerDim(i)
        min(i)      = grid%minGlobalCoordPerDim(i)
        max(i)      = grid%maxGlobalCoordPerDim(i)
        periodic(i) = grid%periodic(i)
        dimNames(i) = grid%dimNames(i)
        dimUnits(i) = grid%dimUnits(i)
      enddo
      call ESMF_LocalArrayGetData(coords(1), coord1, ESMF_DATA_REF, status)
      if (numDims.ge.2) &
        call ESMF_LocalArrayGetData(coords(2), coord2, ESMF_DATA_REF, status)
      if (numDims.ge.3) &
        call ESMF_LocalArrayGetData(coords(3), coord3, ESMF_DATA_REF, status)

!     Create DistGrid and PhysGrid at cell center
      numDimsGrid = numDims
      if (numDims.eq.3) numDimsGrid = 2
      distGridId = 1
      distGridName = 'cell_center'
      physGridId = 1
      physGridName = 'cell_center'
      relloc = ESMF_CELL_CENTER
      call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                  layout, decompIdsUse(1:2), periodic, &
                                  countsPerDEDim1=countsPerDEDim1, &
                                  countsPerDEDim2=countsPerDEDim2, &
                                  distGridName=distGridName, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistributeSpecd: Add DistGrid"
        return
      endif
      distGridId = distGridId + 1 
      call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, numDims, coord1, &
                                  coord2, countsPerDEDim1, countsPerDEDim2, min, &
                                  dimNames, dimUnits, physGridName, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistributeSpecd: Add PhysGrid"
        return
      endif
      physGridId = physGridId + 1 

!     Create any other DistGrids and PhysGrids necessary for horizontal
!     grid stagger
!     TODO: finish filling out, look up D
      select case (grid%horzStagger%stagger)

        ! Arakawa A (centered velocity)
        case (1)

        ! Arakawa B (velocities at grid corner)
        case (2)
          distGridName = 'cell_necorner'
          physGridName = 'cell_necorner'
          relloc = ESMF_CELL_NECORNER
          call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDEDim1, &
                                      countsPerDEDim2=countsPerDEDim2, &
                                      distGridName=distGridName, rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeSpecd: Add DistGrid"
            return
          endif
          distGridId = distGridId + 1
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, numDims, coord1, &
                                      coord2, countsPerDEDim1, countsPerDEDim2, min, &
                                      dimNames, dimUnits, physGridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeSpecd: Add PhysGrid"
            return
          endif
          physGridId = physGridId + 1 

        ! Arakawa C (velocities at cell faces)
        case (3)
          distGridName = 'cell_eface'
          physGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDEDim1, &
                                      countsPerDEDim2=countsPerDEDim2, &
                                      distGridName=distGridName, rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeSpecd: Add DistGrid"
            return
          endif
          distGridId = distGridId + 1
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, numDims, coord1, &
                                      coord2, countsPerDEDim1, countsPerDEDim2, min, &
                                      dimNames, dimUnits, physGridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeSpecd: Add PhysGrid"
            return
          endif
          physGridId = physGridId + 1 

        ! Arakawa D
        case (4)
          distGridName = 'cell_nface'
          physGridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDEDim1, &
                                      countsPerDEDim2=countsPerDEDim2, &
                                      distGridName=distGridName, rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeSpecd: Add DistGrid"
            return
          endif
          distGridId = distGridId + 1
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, numDims, coord1, &
                                      coord2, countsPerDEDim1, countsPerDEDim2, min, &
                                      dimNames, dimUnits, physGridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeSpecd: Add PhysGrid"
            return
          endif
          physGridId = physGridId + 1 

          distGridName = 'cell_eface'
          physGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, numDimsGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDEDim1, &
                                      countsPerDEDim2=countsPerDEDim2, &
                                      distGridName=distGridName, rc=status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeSpecd: Add DistGrid"
            return
          endif
          distGridId = distGridId + 1
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, numDims, coord1, &
                                      coord2, countsPerDEDim1, countsPerDEDim2, min, &
                                      dimNames, dimUnits, physGridName, status)
          if(status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistributeSpecd: Add PhysGrid"
            return
          endif
          physGridId = physGridId + 1 

      end select

!     Create vertical PhysGrid if requested  TODO

!     Create the BoundingBoxes structure
      call ESMF_LRGridSetBoundingBoxes(grid, numDims, min, coord1, coord2, &
                                       countsPerDEDim1, countsPerDEDim2, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistributeSpecd: Grid set boxes"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridDistributeSpecd

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridDestruct - Free all resources associated with a Grid

! !INTERFACE:
      subroutine ESMF_LRGridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_GridConstruct}, does any additional cleanup before the
!     original {\tt ESMF\_Gri} object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_GridDestroy}, which calls
!     {\tt ESMF\_LRGridDestruct}.
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
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: n

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      !TODO: destruct these
      !  type (ESMF_Base) :: base
      !  type (ESMF_Status) :: gridStatus
      grid%horzGridKind    = ESMF_GridKind_Unknown
      grid%vertGridKind    = ESMF_GridKind_Unknown
      grid%horzStagger     = ESMF_GridStagger_Unknown
      grid%vertStagger     = ESMF_GridStagger_Unknown
      grid%horzCoordSystem = ESMF_CoordSystem_Unknown
      grid%vertCoordSystem = ESMF_CoordSystem_Unknown
      grid%coordOrder      = ESMF_CoordOrder_Unknown
      grid%coordIndex      = ESMF_CoordIndex_Unknown
      grid%periodic        = ESMF_FALSE
      grid%numPhysGrids    = 0
      grid%numDistGrids    = 0

      do n = 1,grid%numPhysGridsAlloc
         call ESMF_PhysGridDestroy(grid%physgrids(n), rc=status)
         if (status /= ESMF_SUCCESS) then
            print *,'ERROR in ESMF_LRGridDestruct: error destroying physgrids'
            return
         endif
      end do
      grid%numPhysGridsAlloc = 0
      !TODO: add status to deallocate calls and trap errors
      deallocate(grid%physgrids)

      deallocate(grid%distGridIndex)

      do n = 1,grid%numDistGridsAlloc
         call ESMF_DistGridDestroy(grid%distgrids(n), rc=status)
         if (status /= ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDestruct: distgrid destroy"
            return
         endif
      end do
      grid%numDistGridsAlloc = 0
      deallocate(grid%distgrids)

      grid%minGlobalCoordPerDim(:) = 0
      grid%maxGlobalCoordPerDim(:) = 0

      call ESMF_LocalArrayDestroy(grid%boundingBoxes, rc=status)
      if (status /= ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDestruct: error destroying local array"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridDestruct

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridAddDistGrid - Add a DistGrid to a LogRectGrid

! !INTERFACE:
      subroutine ESMF_LRGridAddDistGrid(grid, distGridId, numDims, counts, &
                                        layout, decompIds, periodic, &
                                        coversDomain, &
                                        countsPerDEDim1, countsPerDEDim2, &
                                        distGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), target :: grid
      integer, intent(out) :: distGridId
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      type (ESMF_DELayout), intent(in) :: layout
      integer, dimension(numDims), intent(in), optional :: decompIds
      type(ESMF_Logical), dimension(numDims), intent(in), optional :: periodic
      type(ESMF_Logical), dimension(numDims), intent(in), optional :: &
                                                     coversDomain
      integer, dimension(:), intent(in), optional :: countsPerDEDim1
      integer, dimension(:), intent(in), optional :: countsPerDEDim2
      character (len=*), intent(in), optional :: distGridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_DistGrid} to a {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt Grid} to add {\tt DistGrid} to.
!     \item[distGridId]
!          Integer identifier for {\tt ESMF\_DistGrid}.
!     \item[numDims]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of computational cells in each direction.
!     \item[layout]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[decompIDs]
!          Identifier for which Grid axes are decomposed.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[coversDomain]}]
!          Logical specifier (array) to denote if the DistGrid covers the entire
!          physical domain in each direction.
!     \item[{[countsPerDEDim1]}]
!          Array of number of grid increments per DE in the first
!          decomposition direction.
!     \item[{[countsPerDEDim2]}]
!          Array of number of grid increments per DE in the second
!          decomposition direction.
!     \item [{[distGridName]}]
!          {\tt ESMF\_DistGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      type(ESMF_DistGrid) :: distGrid

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Create the DistGrid
      distGrid = ESMF_DistGridCreate(numDims=numDims, counts=counts, &
                                     layout=layout, decompIds=decompIds, &
                                     periodic=periodic, &
                                     coversDomain=coversDomain, &
                                     countsPerDEDim1=countsPerDEDim1, &
                                     countsPerDEDim2=countsPerDEDim2, &
                                     rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddDistGrid: Distgrid create"
        return
      endif

      ! now that it's created, add the distgrid to the grid
      call ESMF_GridAddDistGrid(grid, distGrid, status)
      distGridId = grid%numDistGrids

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridAddDistGrid

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridAddPhysGridUniform - Add a uniform PhysGrid to a LogRectGrid

! !INTERFACE:
      subroutine ESMF_LRGridAddPhysGridUniform(grid, numDims, globalCounts, &
                                               physGridId, distGridId, relloc, &
                                               min, max, physGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: globalCounts
      integer, intent(out) :: physGridId
      integer, intent(in)  :: distGridId
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: min
      real(ESMF_KIND_R8), dimension(numDims), intent(in), optional :: max
      character (len=*), intent(in), optional :: physGridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysGrid} to a {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[numDims]
!          Number of grid dimensions.
!     \item[globalCounts]
!          Array of global number of grid increments in each direction.
!     \item [physGridId]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[relloc]
!          Relative location of data at the centers, faces, and vertices of
!          the {\tt Grid}.
!     \item[{[min]}]
!          Array of minimum physical coordinates in each direction.
!     \item[{[max]}]
!          Array of maximum physical coordinates in each direction.
!     \item [{[physGridName]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, i1, i2, j1, j2, gridBoundWidth
      character(len=ESMF_MAXSTR), dimension(numDims) :: coordNames, coordUnits
      logical, dimension(numDims) :: coordAligned, coordEqualSpaced, coordCyclic
      integer, dimension(numDims) :: countsPlus, localCounts, localStart
      integer, dimension(:), allocatable :: cellType1, cellType2
      real(ESMF_KIND_R8) :: delta(numDims)
      real(ESMF_KIND_R8) :: localMinCoord(numDims)
      real(ESMF_KIND_R8) :: localMaxCoord(numDims)
      type(ESMF_CoordKind), dimension(numDims) :: coordKind
      type(ESMF_DistGrid), pointer :: distGrid
      type(ESMF_PhysGrid) :: physGrid
      type(ESMF_PhysCoord) :: tempCoord
      type(ESMF_CoordSystem) :: coordSystem

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! initialize some values
      gridBoundWidth = 1   ! TODO: move into structure, make input?
      distGrid => grid%distGrids(distGridId)

      do i = 1,numDims
        if (globalCounts(i).ne.0) then
          delta(i) = (max(i) - min(i)) / real(globalCounts(i))
        else
          print *, "ERROR in ESMF_LRGridAddPhysGridUniform: counts=0"
          return
        endif

        localStart(i) = distGrid%ptr%myDEComp%globalAIPerDim(i)%min - 1
        localMinCoord(i) = min(i) + delta(i) * real(localStart(i))
        localMaxCoord(i) = min(i) + delta(i) &
                         * real(distGrid%ptr%myDEComp%globalAIPerDim(i)%max)
        localCounts(i)   = distGrid%ptr%myDEComp%globalAIPerDim(i)%max &
                         - distGrid%ptr%myDEComp%globalAIPerDim(i)%min + 1
      enddo

      ! allocate and load cell type masks
      allocate(cellType1(globalCounts(1)+2*gridBoundWidth), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(1) = ", &
                  globalCounts(1)+2*gridBoundWidth
        return
      endif
      allocate(cellType2(globalCounts(2)+2*gridBoundWidth), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(2) = ", &
                  globalCounts(2)+2*gridBoundWidth
        return
      endif
      cellType1 = 0
      cellType2 = 0
      do i = 1,gridBoundWidth
        cellType1(i) = 1
        cellType2(i) = 1
        cellType1(globalCounts(1)+gridBoundWidth+i) = 1
        cellType2(globalCounts(2)+gridBoundWidth+i) = 1
      enddo
      ! set parameters based on grid type
      select case (grid%horzGridKind%gridKind)

        ! ESMF_GridKind_LatLon
        case (1)
          coordSystem         = ESMF_CoordSystem_Spherical
          coordNames(1)       = 'latitude'
          coordNames(2)       = 'longitude'
          coordKind(1)        = ESMF_CoordKind_Lat
          coordKind(2)        = ESMF_CoordKind_Lon
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .true.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_GridKind_XY
        case (5)
          coordSystem         = ESMF_CoordSystem_Cartesian
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordKind(1)        = ESMF_CoordKind_X
          coordKind(2)        = ESMF_CoordKind_Y
          coordUnits(:)       = ''
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .true.
          coordCyclic(:)      = .false.

        case default
           print *,'Grid type not yet supported in LogRectGridAddPhysGrid'
           status = ESMF_FAILURE

      end select

      ! Create the actual PhysGrid object
      physGrid = ESMF_PhysGridCreate(numDims, relloc, physGridName, &
                                     coordSystem, rc=status)

      do i = 1,numDims
        tempCoord = ESMF_PhysCoordCreate(coordKind=coordKind(i), &
                                         name=coordNames(i), &
                                         units=coordUnits(i), &
                                         aligned=coordAligned(i), &
                                         equalSpaced=coordEqualSpaced(i), &
                                         cyclic=coordCyclic(i), &
                                         minVal=localMinCoord(i), &
                                         maxVal=localMaxCoord(i), rc=status)
        call ESMF_PhysGridSetCoord(physGrid, tempCoord, dimOrder=i, rc=status)
      enddo

      ! now that it's created, add the physgrid to the grid
      call ESMF_GridAddPhysGrid(grid, physGrid, status)
      physGridId = grid%numPhysGrids

      ! set coordinates using total cell count
      countsPlus(1) = localCounts(1) + 2*gridBoundWidth
      countsPlus(2) = localCounts(2) + 2*gridBoundWidth
      call ESMF_LRGridSetCoordUniform(grid, physGridId, numDims, countsPlus, &
                             gridBoundWidth, relloc, delta, localMinCoord, &
                             total=.true., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGridUniform: Grid set coord"
        return
      endif
      ! set coordinates using computational cell count
      call ESMF_LRGridSetCoordUniform(grid, physGridId, numDims, localCounts, &
                             0, relloc, delta, localMinCoord, &
                             total=.false., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGridUniform: Grid set coord"
        return
      endif

      ! set mask using total cell count
      i1 = localStart(1) + 1
      i2 = localStart(1) + countsPlus(1)
      j1 = localStart(2) + 1
      j2 = localStart(2) + countsPlus(2)
      call ESMF_LRGridSetCellMask(grid, physGridId, numDims, countsPlus, &
                                  gridBoundWidth, relloc, cellType1(i1:i2), &
                                  cellType2(j1:j2), status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGridUniform: Grid set cell mask"
        return
      endif

      ! clean up
      deallocate(cellType1)
      deallocate(cellType2)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridAddPhysGridUniform

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridAddPhysGridSpecd - Add a specified PhysGrid to a LogRectGrid

! !INTERFACE:
      subroutine ESMF_LRGridAddPhysGridSpecd(grid, physGridId, relloc, numDims, &
                                             coord1, coord2, &
                                             countsPerDEDim1, countsPerDEDim2, &
                                             min, dimNames, dimUnits, &
                                             physGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), target :: grid
      integer, intent(out) :: physGridId
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in) :: countsPerDEDim2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: min
      character (len=*), dimension(:), intent(in), optional :: dimNames
      character (len=*), dimension(:), intent(in), optional :: dimUnits
      character (len=*), intent(in), optional :: physGridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a {\tt ESMF\_PhysGrid} to a {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          {\tt Grid} to add {\tt PhysGrid} to.
!     \item[physGridId]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[relloc]
!          Relative location of data at the centers, faces, and vertices of
!          the {\tt Grid}.
!     \item[numDims]
!          Number of grid dimensions.
!     \item[coord1]
!          Array of physical coordinates in the first direction.
!     \item[coord2]
!          Array of physical coordinates in the second direction.
!     \item[countsPerDEDim1]
!          Array of number of grid increments per DE in the x-direction.
!     \item[countsPerDEDim2]
!          Array of number of grid increments per DE in the y-direction.
!     \item[{[min]}]
!          Array of minimum physical coordinates in each direction.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item [{[physGridName]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, j, i1, i2, j1, j2, gridBoundWidth, myDE(2)
      integer, dimension(numDims) :: counts, compCount, localStart
      integer, dimension(:), allocatable :: cellType1, cellType2
      character(len=ESMF_MAXSTR), dimension(numDims) :: coordNames, coordUnits
      logical, dimension(numDims) :: coordAligned, coordEqualSpaced, coordCyclic
      real(ESMF_KIND_R8) :: last
      real(ESMF_KIND_R8), dimension(numDims) :: localMin, localMax
      real(ESMF_KIND_R8), dimension(:), allocatable :: coordUse1, coordUse2
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_CoordKind), dimension(numDims) :: coordKind
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: gridp
      type(ESMF_PhysGrid) :: physGrid
      type(ESMF_PhysCoord) :: tempCoord

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! initialize some values
      gridBoundWidth = 1   ! TODO: move into structure, make input?

      ! figure out the position of myDE to get local counts
      gridp%ptr => grid
      call ESMF_GridGetDELayout(gridp, layout, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGridSpecd: get delayout"
        return
      endif
      call ESMF_DELayoutGetDEPosition(layout, myDE(1), myDE(2), status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGridSpecd: delayout get position"
        return
      endif

      localMin(1) = min(1)
      localMin(2) = min(2)
      localStart(1) = 0
      localStart(2) = 0
      if(myDE(1).ge.2) then
        do j = 1,myDE(1)-1
          localStart(1) = localStart(1) + countsPerDEDim1(j)
        enddo
      endif
      j1 = localStart(1) + 1
      j2 = localStart(1) + countsPerDEDim1(myDE(1)) + 1
      localMin(1) = minval(coord1(j1:j2))
      localMax(1) = maxval(coord1(j1:j2))

      if(myDE(2).ge.2) then
        do j = 1,myDE(2)-1
          localStart(2) = localStart(2) + countsPerDEDim2(j)
        enddo
      endif
      j1 = localStart(2) + 1
      j2 = localStart(2) + countsPerDEDim2(myDE(2)) + 1
      localMin(2) = minval(coord2(j1:j2))
      localMax(2) = maxval(coord2(j1:j2))

      ! modify global counts to include ghost region
      compCount(1) = size(coord1)
      compCount(2) = size(coord2)
      counts(1) = compCount(1) + 2*gridBoundWidth
      counts(2) = compCount(2) + 2*gridBoundWidth

      ! allocate and load coords -- plus one for extra vertex
      allocate(coordUse1(counts(1)+1), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(1) =", counts(1)+1
        return
      endif
      allocate(coordUse2(counts(2)+1), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(2) =", counts(2)+1
        return
      endif

      coordUse1(gridBoundWidth+1) = min(1)
      do i = 1,compCount(1)
        coordUse1(i+1+gridBoundWidth) = coord1(i)
      enddo
      do i = gridBoundWidth,1,-1
        coordUse1(i) = coordUse1(i+1) - (coord1(2)-coord1(1))
      enddo
      do i = compCount(1),compCount(1)+gridBoundWidth
        coordUse1(i+1) = coordUse1(i) &
                       + (coord1(compCount(1))-coord1(compCount(1)-1))
      enddo
      coordUse2(gridBoundWidth+1) = min(2)
      do i = 1,compCount(2)
        coordUse2(i+1+gridBoundWidth) = coord2(i)
      enddo
      do i = gridBoundWidth,1,-1
        coordUse2(i) = coordUse2(i+1) - (coord2(2)-coord2(1))
      enddo
      do i = compCount(2),compCount(2)+gridBoundWidth
        coordUse2(i+1) = coordUse2(i) &
                       + (coord2(compCount(2))-coord2(compCount(2)-1))
      enddo

      ! allocate and load cell type masks
      allocate(cellType1(counts(1)), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(1) =", counts(1)
        return
      endif
      allocate(cellType2(counts(2)), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(2) =", counts(2)
        return
      endif
      cellType1 = 0
      cellType2 = 0
      do i = 1,gridBoundWidth
        cellType1(i) = 1
        cellType2(i) = 1
        cellType1(compCount(1)+gridBoundWidth+i) = 1
        cellType2(compCount(2)+gridBoundWidth+i) = 1
      enddo

      ! set parameters based on grid type
      select case (grid%horzGridKind%gridKind)

        ! ESMF_GridKind_LatLon
        case (1)
          coordSystem         = ESMF_CoordSystem_Spherical
          coordNames(1)       = 'latitude'
          coordNames(2)       = 'longitude'
          coordKind(1)        = ESMF_CoordKind_Lat
          coordKind(2)        = ESMF_CoordKind_Lon
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .false.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_GridKind_XY
        case (5)
          coordSystem         = ESMF_CoordSystem_Cartesian
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordKind(1)        = ESMF_CoordKind_X
          coordKind(2)        = ESMF_CoordKind_Y
          coordUnits(:)       = ''
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .false.
          coordCyclic(:)      = .false.

        case default
           print *,'Grid type not yet supported in LRGridAddPhysGrid'
           status = ESMF_FAILURE

      end select

      ! Create the actual PhysGrid object
      physGrid = ESMF_PhysGridCreate(numDims, relloc, physGridName, &
                                     coordSystem, rc=status)

      do i = 1,numDims
        tempCoord = ESMF_PhysCoordCreate(coordKind(i), coordNames(i), &
                                         coordUnits(i), &
                                         coordAligned(i), coordEqualSpaced(i), &
                                         coordCyclic(i), localMin(i), &
                                         localMax(i), rc=status)
        call ESMF_PhysGridSetCoord(physGrid, tempCoord, dimOrder=i, rc=status) 
      enddo

      ! now that it's created, add the physgrid to the grid
      call ESMF_GridAddPhysGrid(grid, physGrid, status)
      physGridId = grid%numPhysGrids

      ! set coordinates using total cell count
      counts(1) = countsPerDEDim1(myDE(1)) + 2*gridBoundWidth
      counts(2) = countsPerDEDim2(myDE(2)) + 2*gridBoundWidth
      i1 = localStart(1) + 1
      i2 = localStart(1) + counts(1) + 1
      j1 = localStart(2) + 1
      j2 = localStart(2) + counts(2) + 1
      call ESMF_LRGridSetCoordSpecd(grid, physGridId, numDims, counts, &
                             gridBoundWidth, relloc, coord1(i1:i2), &
                             coord2(j1:j2), localMin, &
                             total=.true., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGridSpecd: Grid set coord"
        return
      endif
      ! set coordinates using computational cell count
      counts(1) = countsPerDEDim1(myDE(1))
      counts(2) = countsPerDEDim2(myDE(2))
      i1 = localStart(1) + 1 + gridBoundWidth
      i2 = i1 + counts(1)
      j1 = localStart(2) + 1 + gridBoundWidth
      j2 = j1 + counts(2)
      call ESMF_LRGridSetCoordSpecd(grid, physGridId, numDims, counts, 0, relloc, &
                             coord1(i1:i2), coord2(j1:j2), localMin, &
                             total=.false., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGridSpecd: Grid set coord"
        return
      endif

      ! set mask using total cell count
      counts(1) = countsPerDEDim1(myDE(1)) + 2*gridBoundWidth
      counts(2) = countsPerDEDim2(myDE(2)) + 2*gridBoundWidth
      i1 = localStart(1) + 1
      i2 = localStart(1) + counts(1)
      j1 = localStart(2) + 1
      j2 = localStart(2) + counts(2)
      call ESMF_LRGridSetCellMask(grid, physGridId, numDims, counts, &
                                  gridBoundWidth, relloc, cellType1(i1:i2), &
                                  cellType2(j1:j2), status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGridSpecd: Grid set cell mask"
        return
      endif

      deallocate(coordUse1)
      deallocate(coordUse2)
      deallocate(cellType1)
      deallocate(cellType2)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridAddPhysGridSpecd

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridAddVertPhysGridUni - Add a uniform vertical PhysGrid to a LogRectGrid

! !INTERFACE:
      subroutine ESMF_LRGridAddVertPhysGridUni(grid, count, physGridId, &
                                               distGridId, relloc, min, &
                                               max, physGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: count
      integer, intent(out) :: physGridId
      integer, intent(in)  :: distGridId
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), intent(in) :: min
      real(ESMF_KIND_R8), intent(in) :: max
      character (len=*), intent(in), optional :: physGridName
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds a vertical {\tt ESMF\_PhysGrid} to a {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[count]
!          Number of grid increments in the vertical direction.
!     \item [physGridId]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item [distGridId]
!          Integer identifier for the corresponding {\tt ESMF\_DistGrid}.
!     \item[relloc]
!          Relative location of data at the centers, faces, and vertices of
!          the {\tt Grid}.
!     \item[min]
!          Minimum physical coordinate in the vertical direction.
!     \item[max]
!          Maximum physical coordinate in the vertical direction.
!     \item [{[physGridName]}]
!          {\tt ESMF\_PhysGrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, i1, i2, gridBoundWidth
      character(len=ESMF_MAXSTR) :: coordName, coordUnit
      logical :: coordAligned, coordEqualSpaced, coordCyclic
      integer :: localStart
      integer, dimension(1) :: countPlus, localCount
      integer, dimension(:), allocatable :: cellType
      real(ESMF_KIND_R8), dimension(1) :: delta
      real(ESMF_KIND_R8), dimension(1) :: localMinCoord
      real(ESMF_KIND_R8) :: localMaxCoord
      type(ESMF_CoordKind) :: coordKind
      type(ESMF_PhysCoord) :: tempCoord
      type(ESMF_DistGrid), pointer :: distGrid
      type(ESMF_PhysGrid) :: physGrid
      type(ESMF_CoordSystem) :: coordSystem

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! initialize some values
      gridBoundWidth = 1   ! TODO: move into structure, make input?
      distGrid => grid%distgrids(distGridId)

      if (count.ge.1) then
        delta(1) = (max - min) / real(count)
      else
        print *, "ERROR in ESMF_LRGridAddVertPhysGridUni: count < 1"
        return
      endif

      localStart       = distGrid%ptr%myDEComp%globalAIPerDim(3)%min - 1
      localMinCoord(1) = min + delta(1) * real(localStart)
      localMaxCoord    = min + delta(1) &
                       * real(distGrid%ptr%myDEComp%globalAIPerDim(3)%max)
      localCount(1)    = distGrid%ptr%myDEComp%globalAIPerDim(3)%max &
                       - distGrid%ptr%myDEComp%globalAIPerDim(3)%min + 1

      ! allocate and load cell type masks
      allocate(cellType(count+2*gridBoundWidth), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, count =", count+2*gridBoundWidth
        return
      endif
      cellType = 0
      do i = 1,gridBoundWidth
        cellType(i) = 1
        cellType(count+gridBoundWidth+i) = 1
      enddo

      ! set parameters based on vertical coordinate system  TODO: better as a case
      if (grid%vertCoordSystem.eq.ESMF_CoordSystem_Depth) then
        ! ESMF_CoordSystem_Depth
        coordSystem      = ESMF_CoordSystem_Depth
        coordName        = 'depth'
        coordKind        = ESMF_CoordKind_Depth
        coordUnit        = ''
        coordAligned     = .true.
        coordEqualSpaced = .true.
        coordCyclic      = .false.

      elseif (grid%vertCoordSystem.eq.ESMF_CoordSystem_Height) then
        ! ESMF_CoordSystem_Height
        coordSystem      = ESMF_CoordSystem_Height
        coordName        = 'height'
        coordKind        = ESMF_CoordKind_Height
        coordUnit        = ''
        coordAligned     = .true.
        coordEqualSpaced = .true.
        coordCyclic      = .false.

      else
        print *,'Grid type not yet supported in GridAddVertPhysGrid'
        status = ESMF_FAILURE
      endif

      ! Create the actual PhysGrid object
      grid%physGrids(physGridId) = ESMF_PhysGridCreate(1, relloc, physGridName, &
                                                       coordSystem, rc=status)

      tempCoord = ESMF_PhysCoordCreate(coordKind, name=coordName, &
                                       units=coordUnit, &
                                       aligned=coordAligned, &
                                       equalSpaced=coordEqualSpaced, &
                                       cyclic=coordCyclic, &
                                       minVal=localMinCoord(1), &
                                       maxVal=localMaxCoord, rc=status)
      call ESMF_PhysGridSetCoord(grid%physGrids(physGridId), tempCoord, &
                                 dimOrder=3, rc=status)

      ! set coordinates using total cell count
      countPlus(1) = localCount(1) + 2*gridBoundWidth
      call ESMF_LRGridSetCoordUniform(grid, physGridId, 1, countPlus, &
                             gridBoundWidth, relloc, delta, localMinCoord, &
                             total=.true., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddVertPhysGridUni: Grid set coord"
        return
      endif
      ! set coordinates using computational cell count
      call ESMF_LRGridSetCoordUniform(grid, physGridId, 1, localCount, &
                                      0, relloc, delta, localMinCoord, &
                                      total=.false., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddVertPhysGridUni: Grid set coord"
        return
      endif

      ! set mask using total cell count
      i1 = localStart + 1
      i2 = localStart + countPlus(1)
      call ESMF_LRGridSetCellMask(grid, physGridId, 1, countPlus, &
                                  gridBoundWidth, relloc, cellType(i1:i2), &
                                  rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddVertPhysGridUni: Grid set cell mask"
        return
      endif

      ! clean up
      deallocate(cellType)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridAddVertPhysGridUni

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridGetCoord - Get the coordinates of a Grid

! !INTERFACE:
      subroutine ESMF_LRGridGetCoord(grid, physGridId, relloc, centerCoord, &
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
!
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
        print *, "ERROR in ESMF_LRGridGetCoord: need either relloc or PhysGridId"
        return
      endif

      ! If there is a relloc but no physGrid id, then get the id from the relloc
      if (rellocIsValid .and. .not.(physIdIsValid)) then
        call ESMF_GridGetPhysGridId(grid%ptr, relloc, physIdUse, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGetCoord: get PhysGrid id"
          return
        endif
        if (physIdUse.eq.-1) then
          print *, "ERROR in ESMF_LRGridGetCoord: no PhysGrid corresponding", &
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
          print *, "ERROR in ESMF_LRGridGetCoord: PhysGrid get locations"
          return
        endif
      endif
      if (present(cornerCoord)) then
        call ESMF_PhysGridGetRegions(grid%ptr%physGrids(physIdUse), &
                                     vertexArray=cornerCoord, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGetCoord: PhysGrid get regions"
          return
        endif
      endif
      ! TODO: face coords

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGetCoord

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetCoordFromArray - Set the coordinates of a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_LRGridSetCoordFromArray(Grid, array, id, rc)
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
      end subroutine ESMF_LRGridSetCoordFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridGetDE - Get DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_LRGridGetDE(grid, distGridId, physGridId, relloc, MyDE, &
                                  localCellCount, localCellCountPerDim, &
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
      integer :: distGridIdUse

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

! TODO: add code to get distgridId from relloc or physgridId, test for the
!       presence of at least one of these optional arguments
      if (present(distGridId)) distGridIdUse = distGridId

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGetDE(grid%ptr%distgrids(distGridIdUse)%ptr, MyDE, &
                              localCellCount, localCellCountPerDim, &
                              globalStartPerDim, globalAIPerDim, &
                              total=total, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridGetDE: distgrid get de"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGetDE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridGetAllAxisIndex - Get all axis indices for a DistGrid

! !INTERFACE:
      subroutine ESMF_LRGridGetAllAxisIndex(grid, globalAI, distGridId, &
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
      integer :: distGridIdUse

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

! TODO: add code to get distgridId from relloc or physgridId, test for the
!       presence of at least one of these optional arguments
      if (present(distGridId)) distGridIdUse = distGridId

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGetAllAxisIndex(grid%ptr%distgrids(distGridIdUse)%ptr, &
                                        globalAI, total, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridGetAllAxisIndex: ", &
                 "distgrid get all axis index"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGetAllAxisIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_LRGridGlobalToLocalIndex(grid, distGridId, physGridId, &
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
      integer(ESMF_KIND_I4), dimension(:), intent(in),  optional :: global1D
      integer(ESMF_KIND_I4), dimension(:), intent(out), optional :: local1D
      integer(ESMF_KIND_I4), dimension(:,:), intent(in),  optional :: global2D
      integer(ESMF_KIND_I4), dimension(:,:), intent(out), optional :: local2D
      type(ESMF_AxisIndex), dimension(:), intent(in),  optional :: globalAI1D
      type(ESMF_AxisIndex), dimension(:), intent(out), optional :: localAI1D
      type(ESMF_AxisIndex), dimension(:,:), intent(in),  optional :: globalAI2D
      type(ESMF_AxisIndex), dimension(:,:), intent(out), optional :: localAI2D
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
      integer :: distGridIdUse

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

! TODO: add code to get distgridId from relloc or physgridId, test for the
!       presence of at least one of these optional arguments
      if (present(distGridId)) distGridIdUse = distGridId

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridGlobalToLocalIndex(grid%ptr%distgrids(distGridIdUse)%ptr, &
                                           global1D, local1D, &
                                           global2D, local2D, &
                                           globalAI1D, localAI1D, &
                                           globalAI2D, localAI2D, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                 "distgrid global to local"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGlobalToLocalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridLocalToGlobalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_LRGridLocalToGlobalIndex(grid, distGridId, physGridId, &
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
      integer :: distGridIdUse

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

! TODO: add code to get distgridId from relloc or physgridId, test for the
!       presence of at least one of these optional arguments
      if (present(distGridId)) distGridIdUse = distGridId

!     call DistGrid method to retrieve information otherwise not available
!     to the application level
      call ESMF_DistGridLocalToGlobalIndex(grid%ptr%distgrids(distGridIdUse)%ptr, &
                                           local1D, global1D, &
                                           local2D, global2D, &
                                           localAI1D, globalAI1D, &
                                           localAI2D, globalAI2D, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                 "distgrid local to global"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridLocalToGlobalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetCoordFromBuffer - Set the coordinates of a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_LRGridSetCoordFromBuffer(Grid, buffer, id, rc)
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
      end subroutine ESMF_LRGridSetCoordFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetCoordSpecd - Compute coordinates for a specified Grid

! !INTERFACE:
      subroutine ESMF_LRGridSetCoordSpecd(grid, physGridId, numDims, counts, &
                                          gridBoundWidth, relloc, coord1, coord2, &
                                          min, total, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: physGridId
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      integer, intent(in) :: gridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes coordinates for a {\tt ESMF\_Grid}
!     via a prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[physGridId]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[numDims]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[gridBoundWidth]
!          Number of extra cell layers in the internal coordinate representation
!          for halo and ghost cells.  Used by {\tt ESMF\_Regrid}.
!     \item[relloc]
!          Relative location in grid cell for which this PhysGrid.
!     \item[coord1]
!          Array of specified grid coordinates in the first dimension.
!     \item[coord2]
!          Array of specified grid coordinates in the second dimension.
!     \item[min]
!          Array of minimum local physical coordinates in each dimension.
!     \item[{[total]}]
!          Logical flag to optionally set physical coordinate arrays of total cells.
!          Default is to set computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, j, i1, j1
      real(ESMF_KIND_R8) :: coordUse1, coordUse2
      real(ESMF_KIND_R8), dimension(:,:), pointer :: temp1, temp2
      type(ESMF_Array), dimension(:), pointer :: arrayTemp
      type(ESMF_DataKind) :: kind
      type(ESMF_DataType) :: type

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: different subroutines for different numDims?  or case?
      ! TODO: could be a 1-D array for each coord axis later, but that
      !       would have to be supported by Regrid first

      ! allocate arrays
      allocate(arrayTemp(numDims))

      ! create ESMF_Arrays
      kind = ESMF_R8
      type = ESMF_DATA_REAL
      arrayTemp(1) = ESMF_ArrayCreate(numDims, type, kind, counts, &
                                      halo_width=gridBoundWidth, rc=status)
      arrayTemp(2) = ESMF_ArrayCreate(numDims, type, kind, counts, &
                                      halo_width=gridBoundWidth, rc=status)
      call ESMF_ArrayGetData(arrayTemp(1), temp1, ESMF_DATA_REF, status)
      call ESMF_ArrayGetData(arrayTemp(2), temp2, ESMF_DATA_REF, status)

!     For now, an if construct for the different relative locations
!     TODO: also set corners and faces
      if (relloc .eq. ESMF_CELL_UNDEFINED) then
        status = ESMF_FAILURE

      elseif (relloc .eq. ESMF_CELL_CENTER) then
        do i = 1,counts(1)
          coordUse1 = 0.5d0*(coord1(i)+coord1(i+1))
          do j = 1,counts(2)
            coordUse2 = 0.5d0*(coord2(j)+coord2(j+1))
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_NFACE) then
        do i = 1,counts(1)
          coordUse1 = 0.5d0*(coord1(i)+coord1(i+1))
          do j = 1,counts(2)
            coordUse2 = coord2(j+1)
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_SFACE) then
        do i = 1,counts(1)
          coordUse1 = 0.5d0*(coord1(i)+coord1(i+1))
          do j = 1,counts(2)
            coordUse2 = coord2(j)
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_EFACE) then
        do i = 1,counts(1)
          coordUse1 = coord1(i+1)
          do j = 1,counts(2)
            coordUse2 = 0.5d0*(coord2(j)+coord2(j+1))
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_WFACE) then
        do i = 1,counts(1)
          coordUse1 = coord1(i)
          do j = 1,counts(2)
            coordUse2 = 0.5d0*(coord2(j)+coord2(j+1))
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

      elseif (relloc .eq. ESMF_CELL_NECORNER) then
        do i = 1,counts(1)
          coordUse1 = coord1(i+1)
          do j = 1,counts(2)
            coordUse2 = coord2(j+1)
            temp1(i,j) = coordUse1 
            temp2(i,j) = coordUse2
          enddo
        enddo

        ! TODO: rest of the corners

      else
        print *, "This relative location not yet supported in ", &
                 "LRGridSetCoordSpecd"
        return
      endif

      ! now set the location array in PhysGrid
      call ESMF_PhysGridSetLocations(grid%physGrids(physGridId), &
                                     locationArray=arrayTemp, total=total, &
                                     rc=status)
            ! TODO: add name to set call
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridSetCoordSpecd: PhysGrid set locations"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridSetCoordSpecd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetCoordUniform - Compute coordinates for a uniform Grid

! !INTERFACE:
      subroutine ESMF_LRGridSetCoordUniform(grid, physGridId, numDims, counts, &
                                            gridBoundWidth, relloc, delta, min, &
                                            total, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: physGridId
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      integer, intent(in) :: gridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: delta
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes coordinates for a {\tt ESMF\_Grid} via a
!     prescribed method.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[physGridId]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[numDims]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[gridBoundWidth]
!          Number of extra cell layers in the internal coordinate representation
!          for halo and ghost cells.  Used by {\tt ESMF\_Regrid}.
!     \item[relloc]
!          Relative location in grid cell for which this PhysGrid.
!     \item[delta]
!          Array of uniform grid increments in each dimension.
!     \item[min]
!          Array of minimum physical coordinates in each dimension.
!     \item[{[total]}]
!          Logical flag to optionally set physical coordinate arrays of total cells.
!          Default is to set computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOP

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, j, i1, j1
      real(ESMF_KIND_R8) :: coord1, coord2
      real(ESMF_KIND_R8), dimension(:), pointer :: temp
      real(ESMF_KIND_R8), dimension(:,:), pointer :: temp1, temp2
      type(ESMF_Array), dimension(:), pointer :: arrayTemp
      type(ESMF_DataKind) :: kind
      type(ESMF_DataType) :: type

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: could be a 1-D array for each coord axis later, but that
      !       would have to be supported by Regrid first

      ! allocate arrays
      allocate(arrayTemp(numDims))

      ! create ESMF_Arrays
      kind = ESMF_R8
      type = ESMF_DATA_REAL
      do i = 1,numDims
        arrayTemp(i) = ESMF_ArrayCreate(numDims, type, kind, counts, &
                                        halo_width=gridBoundWidth, rc=status)
      enddo

      select case (numDims)
      case(1)   ! 1D coordinates, assumed mostly for vertical grids

        ! get data
        call ESMF_ArrayGetData(arrayTemp(1), temp, ESMF_DATA_REF, status)

!       For now, an if construct for the different relative locations
        if (relloc .eq. ESMF_CELL_UNDEFINED) then
          status = ESMF_FAILURE

        elseif (relloc.eq.ESMF_CELL_CENTER .or. relloc.eq.ESMF_CELL_CELL) then  ! TODO:?
          do i = 1,counts(1)
            i1 = i - gridBoundWidth
            temp(i) = delta(1)*0.5*real(i1+i1-1) + min(1)
          enddo

        elseif (relloc .eq. ESMF_CELL_TOPFACE) then   ! TODO: check bottom or top
          do i = 1,counts(1)
            i1 = i - gridBoundWidth
            temp(i) = delta(1)*real(i1) + min(1)
          enddo

        else
          print *, "This relative location not supported for 1D Grids in ", &
                   "LRGridSetCoordUnifrom"
          return
        endif


      case(2)   ! 2D coordinates
 
        ! get data
        call ESMF_ArrayGetData(arrayTemp(1), temp1, ESMF_DATA_REF, status)
        call ESMF_ArrayGetData(arrayTemp(2), temp2, ESMF_DATA_REF, status)

!       For now, an if construct for the different relative locations
!       TODO: also set corners and faces
        if (relloc .eq. ESMF_CELL_UNDEFINED) then
          status = ESMF_FAILURE

        elseif (relloc .eq. ESMF_CELL_CENTER) then
          do i = 1,counts(1)
            i1 = i - gridBoundWidth
            coord1 = delta(1)*0.5*real(i1+i1-1) + min(1)
            do j = 1,counts(2)
              j1 = j - gridBoundWidth
              coord2 = delta(2)*0.5*real(j1+j1-1) + min(2)
              temp1(i,j) = coord1 
              temp2(i,j) = coord2 
            enddo
          enddo

        elseif (relloc .eq. ESMF_CELL_NFACE) then
          do i = 1,counts(1)
            i1 = i - gridBoundWidth
            coord1 = delta(1)*0.5*real(i1+i1-1) + min(1)
            do j = 1,counts(2)
              j1 = j - gridBoundWidth
              coord2 = delta(2)*real(j1) + min(2)
              temp1(i,j) = coord1 
              temp2(i,j) = coord2 
            enddo
          enddo

        elseif (relloc .eq. ESMF_CELL_SFACE) then
          do i = 1,counts(1)
            i1 = i - gridBoundWidth
            coord1 = delta(1)*0.5*real(i1+i1-1) + min(1)
            do j = 1,counts(2)
              j1 = j - gridBoundWidth
              coord2 = delta(2)*real(j1-1) + min(2)
              temp1(i,j) = coord1 
              temp2(i,j) = coord2 
            enddo
          enddo

        elseif (relloc .eq. ESMF_CELL_EFACE) then
          do i = 1,counts(1)
            i1 = i - gridBoundWidth
            coord1 = delta(1)*real(i1) + min(1)
            do j = 1,counts(2)
              j1 = j - gridBoundWidth
              coord2 = delta(2)*0.5*real(j1+j1-1) + min(2)
              temp1(i,j) = coord1 
              temp2(i,j) = coord2 
            enddo
          enddo

        elseif (relloc .eq. ESMF_CELL_WFACE) then
          do i = 1,counts(1)
            i1 = i - gridBoundWidth
            coord1 = delta(1)*real(i1-1) + min(1)
            do j = 1,counts(2)
              j1 = j - gridBoundWidth
              coord2 = delta(2)*0.5*real(j1+j1-1) + min(2)
              temp1(i,j) = coord1 
              temp2(i,j) = coord2 
            enddo
          enddo

        elseif (relloc .eq. ESMF_CELL_NECORNER) then
          do i = 1,counts(1)
            i1 = i - gridBoundWidth
            coord1 = delta(1)*real(i1) + min(1)
            do j = 1,counts(2)
              j1 = j - gridBoundWidth
              coord2 = delta(2)*real(j1) + min(2)
              temp1(i,j) = coord1 
              temp2(i,j) = coord2 
            enddo
          enddo

        ! TODO: rest of the corners

        else
          print *, "This relative location not yet supported in ", &
                   "LRGridSetCoordUnifrom"
          return
        endif


      case default
         print *,"ERROR in LRGridSetCoordUniform: ", &
                 "grids of this dimension are not yet supported"
         status = ESMF_Failure
      end select

      ! now set the location array in PhysGrid
      call ESMF_PhysGridSetLocations(grid%physGrids(physGridId), &
                                     locationArray=arrayTemp, total=total, &
                                     rc=status)
            ! TODO: add name to set call
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridSetCoordUniform: PhysGrid set locations"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridSetCoordUniform

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetCoordCopy - Copies coordinates from one grid to another

! !INTERFACE:
      subroutine ESMF_LRGridSetCoordCopy(grid, gridIn, id, rc)
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
      end subroutine ESMF_LRGridSetCoordCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridGet - Gets a variety of information about the grid

! !INTERFACE:
      subroutine ESMF_LRGridGet(grid, distGridId, physGridId, relloc, &
                                horzGridKind, vertGridKind, &
                                horzStagger, vertStagger, &
                                horzCoordSystem, vertCoordSystem, &
                                coordOrder, minGlobalCoordPerDim, &
                                maxGlobalCoordPerDim, globalCellCountPerDim, &
                                globalStartPerDEPerDim, maxLocalCellCountPerDim, &
                                cellCountPerDEPerDim, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in), optional :: distGridId
      integer, intent(in), optional :: physGridId
      type(ESMF_RelLoc), intent(in), optional :: relloc
      type(ESMF_GridKind), intent(out), optional :: horzGridKind
      type(ESMF_GridKind), intent(out), optional :: vertGridKind
      type(ESMF_GridStagger), intent(out), optional :: horzStagger
      type(ESMF_GridStagger), intent(out), optional :: vertStagger
      type(ESMF_CoordSystem), intent(out), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(out), optional :: vertCoordSystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordOrder
      real(ESMF_KIND_R8), intent(out), dimension(ESMF_MAXGRIDDIM), &
                            optional :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(ESMF_MAXGRIDDIM), &
                            optional :: maxGlobalCoordPerDim
      integer, intent(out), dimension(ESMF_MAXGRIDDIM), &
                            optional :: globalCellCountPerDim
      integer, intent(out), dimension(:,:), optional :: globalStartPerDEPerDim
      integer, intent(out), dimension(ESMF_MAXGRIDDIM), &
                            optional :: maxLocalCellCountPerDim
      integer,              dimension(:,:), pointer, &
                            optional :: cellCountPerDEPerDim
      type (ESMF_Logical), intent(out), optional :: periodic(:)
      character(len = *), intent(out), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version gets a variety of information about a {\tt ESMF\_Grid}, depending
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
      integer :: i, distGridIdUse
      type(ESMF_GridType), pointer :: gridp

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
 
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_LRGridGet called with invalid grid object"
        return
      endif

      gridp => grid%ptr

      ! if present, gets information from the grid derived type
      if(present(horzGridKind   )) horzGridKind    = gridp%horzGridKind
      if(present(vertGridKind   )) vertGridKind    = gridp%vertGridKind
      if(present(horzStagger    )) horzStagger     = gridp%horzStagger
      if(present(vertStagger    )) vertStagger     = gridp%vertStagger
      if(present(horzCoordSystem)) horzCoordSystem = gridp%horzCoordSystem
      if(present(vertCoordSystem)) vertCoordSystem = gridp%vertCoordSystem
      if(present(coordOrder     )) coordOrder      = gridp%coordOrder

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
      if(present(globalCellCountPerDim) .or. present(globalStartPerDEPerDim) &
                                     .or. present(maxLocalCellCountPerDim)) then
        call ESMF_DistGridGet(gridp%distgrids(distGridIdUse), &
                              globalCellCountPerDim=globalCellCountPerDim, &
                              globalStartPerDEPerDim=globalStartPerDEPerDim, &
                              maxLocalCellCountPerDim=maxLocalCellCountPerDim, &
                              rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGet: DistGrid get"
          return
        endif
      endif
      if(present(cellCountPerDEPerDim)) then
        ! TODO: check size of cellCountPerDEPerDim
        call ESMF_DistGridGetAllCounts(gridp%distgrids(distGridIdUse)%ptr, &
                                       cellCountPerDEPerDim, rc=status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGet: DistGrid get all counts"
          return
        endif
      endif

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
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(minGlobalCoordPerDim)) exit
          minGlobalCoordPerDim(i) = gridp%minGlobalCoordPerDim(i)
        enddo
      endif
      if(present(maxGlobalCoordPerDim)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(maxGlobalCoordPerDim)) exit
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

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSet - Sets a variety of information about the grid

! !INTERFACE:
      subroutine ESMF_LRGridSet(grid, horzGridKind, vertGridKind, &
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
      type(ESMF_CoordOrder),  intent(in), optional :: coordOrder
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: maxGlobalCoordPerDim
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
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

      end subroutine ESMF_LRGridSet

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridSetCellMask - Compute cell identifier mask for a Grid

! !INTERFACE:
      subroutine ESMF_LRGridSetCellMask(grid, physGridId, numDims, counts, &
                                        gridBoundWidth, relloc, cellType1, &
                                        cellType2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType) :: grid
      integer, intent(in) :: physGridId
      integer, intent(in) :: numDims
      integer, dimension(numDims), intent(in) :: counts
      integer, intent(in) :: gridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, dimension(:), intent(in) :: cellType1
      integer, dimension(:), intent(in), optional :: cellType2
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set internally computes cell types for an {\tt ESMF\_Grid}
!     and sets them as an integer mask in a corresponding {\tt ESMF\_PhysGrid}.
!     This mask is intended for internal use to indicate which cells are in
!     the computational regime (cellType=0), a ghost region (cellType=1), or a
!     halo region (cellType=2).
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[physGridId]
!          Identifier of the {\tt ESMF\_PhysGrid} to be modified.
!     \item[numDims]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[gridBoundWidth]
!          Width, in cells, of the ficticious boundary around the grid for
!          halo and ghost regions.
!     \item[relloc]
!          Relative location in grid cell for which this PhysGrid.
!     \item[cellType1]
!          Array of cell type identifiers in the first dimension.
!     \item[{[cellType2]}]
!          Array of cell type identifiers in the second dimension.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      character(len=ESMF_MAXSTR) :: name
      integer :: i, j, iMax1, jMax1, iType, jType
      integer, dimension(:,:), pointer :: temp
      type(ESMF_Array) :: arrayTemp
      type(ESMF_DataKind) :: kind
      type(ESMF_DataType) :: type

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: different subroutines for different numDims?  or case?

      ! create ESMF_Array
      kind = ESMF_I4
      type = ESMF_DATA_INTEGER
      arrayTemp = ESMF_ArrayCreate(numDims, type, kind, counts, &
                                   halo_width=gridBoundWidth, rc=status)
      call ESMF_ArrayGetData(arrayTemp, temp, ESMF_DATA_REF, status)

!     TODO: should this be different for different relative locations?
      iMax1 = counts(1) - gridBoundWidth + 1
      jMax1 = counts(2) - gridBoundWidth + 1
      do i = 1,counts(1)
        iType = cellType1(i)
        do j = 1,counts(2)
          jType = cellType2(j)
          ! default is computational
          temp(i,j) = 0
          ! identify ghost cells
          if(iType.eq.1 .or. jType.eq.1) then
            temp(i,j) = 1
          ! identify halo cells
          elseif (i.le.gridBoundWidth .or. j.le.gridBoundWidth .or. &
                  i.ge.iMax1          .or. j.ge.jMax1) then
            temp(i,j) = 2
          endif
        enddo
      enddo

      ! now set the mask array in PhysGrid
      name = 'cell type total'
      call ESMF_PhysGridSetMask(grid%physGrids(physGridId), &
                                maskArray=arrayTemp, &
                                maskType=ESMF_GridMaskKind_RegionId, &
                                name=name, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridSetCellMask: PhysGrid set mask"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridSetCellMask

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridSetMaskFromArray - Set a mask in a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_LRGridSetMaskFromArray(grid, array, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_LocalArray), intent(in) :: array
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the mask data exists already and is
!     being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[array]
!          ESMF LocalArray of data.
!     \item [{[name]}]
!           {\tt Mask} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetMaskFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetMaskFromBuffer - Set a mask in a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_LRGridSetMaskFromBuffer(grid, buffer, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      real, dimension (:), pointer :: buffer
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the mask data exists already and is
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
      end subroutine ESMF_LRGridSetMaskFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetMaskFromMask - Set a mask in a Grid from an existing  mask of different type

! !INTERFACE:
      subroutine ESMF_LRGridSetMaskFromMask(grid, mask, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(in) :: mask
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the mask data will be created from an existing
!     mask of a different type.  If the types are the same, call MaskCopy.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[mask]
!          Mask identifier.
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
      end subroutine ESMF_LRGridSetMaskFromMask

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetMaskCopy - Copies a mask from one grid to another.

! !INTERFACE:
      subroutine ESMF_LRGridSetMaskCopy(grid, gridIn, name, nameIn, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_Grid), intent(in) :: gridIn
      character (len=*), intent(in), optional :: name
      character (len=*), intent(in), optional :: nameIn
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set copies a mask for a {\tt ESMF\_Grid} from another {\tt ESMF\_Grid}.
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
      end subroutine ESMF_LRGridSetMaskCopy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetMetricFromArray - Set a metric for a Grid from an existing ESMF array

! !INTERFACE:
      subroutine ESMF_LRGridSetMetricFromArray(grid, array, name, rc)
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
      end subroutine ESMF_LRGridSetMetricFromArray

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetMetricFromBuffer - Set a metric for a Grid from an existing data buffer

! !INTERFACE:
      subroutine ESMF_LRGridSetMetricFromBuffer(grid, buffer, name, rc)
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
      end subroutine ESMF_LRGridSetMetricFromBuffer

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetMetricCompute - Compute a metric for a Grid

! !INTERFACE:
      subroutine ESMF_LRGridSetMetricCompute(grid, name, id, rc)
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
      end subroutine ESMF_LRGridSetMetricCompute

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridSetMetricCopy - Copies a metric from one grid to another

! !INTERFACE:
      subroutine ESMF_LRGridSetMetricCopy(grid, name, gridIn, nameIn, rc)
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
      end subroutine ESMF_LRGridSetMetricCopy

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridSetBBoxesUni - Set the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_LRGridSetBBoxesUni(grid, numDims, min, delta, &
                                                countsPerAxis, numDE1, numDE2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), intent(inout) :: grid
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: delta
      integer, dimension(:,:), intent(in) :: countsPerAxis
      integer, intent(in) :: numDE1
      integer, intent(in) :: numDE2
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the region identifier data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[numDims]
!          Number of grid dimensions (directions).
!     \item[min]
!          Array of minimum physical coordinates in each direction.
!     \item[delta]
!          Array of uniform physical increments between nodes in each direction.
!     \item[countsPerAxis]
!          Array of number of grid increments per DE in each direction.
!     \item[numDE1]
!          Number of DEs in the first direction of the decomposition.
!     \item[numDE2]
!          Number of DEs in the second direction of the decomposition.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: DE, numDEs, rank, npts
      integer :: i, j, jDE
      real(ESMF_KIND_R8) :: start, stop
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      numDEs = numDE1*numDE2
      rank   = 2   ! TODO: hard-coded for now
      npts   = 2**rank

!     TODO: break out by rank?
!     Assume the following starage for bounding boxes:
!       number of DEs * npts * rank
!       where npts is the number of points necessary to describe a bounding box
!       and rank is the number of dimensions.  For the time being, the points
!       are stored in the following order:
!                     1. (Xmin,Ymin,Zmin)
!                     2. (Xmax,Ymin,Zmin)
!                     3. (Xmax,Ymax,Zmin)
!                     4. (Xmin,Ymax,Zmin)
!                     5. (Xmin,Ymin,Zmax)
!                     6. (Xmax,Ymin,Zmax)
!                     7. (Xmax,Ymax,Zmax)
!                     8. (Xmin,Ymax,Zmax)
      allocate(boxes(numDEs,npts,rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, boxes(nDE,npt,rank) = ", numDEs,npts,rank
         return
      endif
      

!     Calculate box for each DE
!     Direction 1 first
      start = min(1)
      stop  = min(1)
      do j = 1,numDE1
        stop = stop + delta(1)*real(countsPerAxis(j,1))
        do i = 1,numDE2
          DE = (i-1)*numDE1 + j
          boxes(DE,1,1) = start
          boxes(DE,2,1) = stop
          boxes(DE,3,1) = stop
          boxes(DE,4,1) = start
        enddo
        start = stop
      enddo

!     Direction 2 next
      start = min(2)
      stop  = min(2)
      do j = 1,numDE2
        jDE  = (j-1)*numDE1 + 1
        stop = stop + delta(2)*real(countsPerAxis(jDE,2))
        do i = 1,numDE1
          DE = (j-1)*numDE1 + i
          boxes(DE,1,2) = start
          boxes(DE,2,2) = stop
          boxes(DE,3,2) = stop
          boxes(DE,4,2) = start
        enddo
        start = stop
      enddo

      grid%boundingBoxes = ESMF_LocalArrayCreate(boxes, ESMF_DATA_REF, status)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridSetBBoxesUni

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridSetBBoxesSpecd - Set the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_LRGridSetBBoxesSpecd(grid, numDims, min, delta1, &
                                                  delta2, countsPerDEDim1, &
                                                  countsPerDEDim2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridType), intent(inout) :: grid
      integer, intent(in) :: numDims
      real(ESMF_KIND_R8), dimension(numDims), intent(in) :: min
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: delta2
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in) :: countsPerDEDim2
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     This version of set assumes the region identifier data exists already
!     and is being passed in through an {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Pointer to a {\tt ESMF\_Grid} to be modified.
!     \item[numDims]
!          Number of grid dimensions (directions).
!     \item[min]
!          Array of minimum physical coordinate in each direction.
!     \item[delta1]
!          Array of physical increments between nodes in the first direction.
!     \item[delta2]
!          Array of physical increments between nodes in the second direction.
!     \item[countsPerDEDim1]
!          Array of number of grid increments per DE in the x-direction.
!     \item[countsPerDEDim2]
!          Array of number of grid increments per DE in the y-direction.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: DE, numDE1, numDE2, numDEs, npts
      integer :: i, i1, j
      real(ESMF_KIND_R8) :: start, stop
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      numDE1 = size(countsPerDEDim1)
      numDE2 = size(countsPerDEDim2)
      numDEs = numDE1*numDE2
      npts   = 2**numDims

!     TODO: break out by rank?
!     Assume the following starage for bounding boxes:
!       number of DEs * npts * numDims
!       where npts is the number of points necessary to describe a bounding box
!       and rank is the number of dimensions.  For the time being, the points
!       are stored in the following order:
!                     1. (Xmin,Ymin,Zmin)
!                     2. (Xmax,Ymin,Zmin)
!                     3. (Xmax,Ymax,Zmin)
!                     4. (Xmin,Ymax,Zmin)
!                     5. (Xmin,Ymin,Zmax)
!                     6. (Xmax,Ymin,Zmax)
!                     7. (Xmax,Ymax,Zmax)
!                     8. (Xmin,Ymax,Zmax)
      allocate(boxes(numDEs,npts,numDims), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, boxes(nDE,npt,numDims) = ", numDEs,npts, &
                   numDims
         return
      endif

!     Calculate box for each DE
!     Direction 1 first
      start = min(1)
      stop  = min(1)
      i1    = 0
      do j = 1,numDE1
        do i = i1+1,i1+countsPerDEDim1(j)
          stop = stop + delta1(i)
        enddo
        do i = 1,numDE2
          DE = (i-1)*numDE1 + j
          boxes(DE,1,1) = start
          boxes(DE,2,1) = stop
          boxes(DE,3,1) = stop
          boxes(DE,4,1) = start
        enddo
        start = stop
        i1    = i1 + countsPerDEDim1(j)
      enddo
!     Direction 2 next
      start = min(2)
      stop  = min(2)
      i1    = 0
      do j = 1,numDE2
        do i = i1+1,i1+countsPerDEDim2(j)
          stop = stop + delta2(i)
        enddo
        do i = 1,numDE1
          DE = (j-1)*numDE1 + i
          boxes(DE,1,2) = start
          boxes(DE,2,2) = stop
          boxes(DE,3,2) = stop
          boxes(DE,4,2) = start
        enddo
        start = stop
        i1    = i1 + countsPerDEDim2(j)
      enddo

      grid%boundingBoxes = ESMF_LocalArrayCreate(boxes, ESMF_DATA_REF, status)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridSetBBoxesSpecd

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LRGridValidate - Check internal consistency of a Grid

! !INTERFACE:
      subroutine ESMF_LRGridValidate(grid, opt, rc)
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

      character(len=ESMF_MAXSTR) :: name, str
      type(ESMF_GridType), pointer :: gp
      integer :: status

      if (present(rc)) rc = ESMF_FAILURE

      if (.not. associated(grid%ptr)) then
        print *, "Empty or Uninitialized Grid"
        return
      endif

      gp => grid%ptr
      if (gp%gridstatus /= ESMF_GridStatus_Ready) then
        return
      endif

      call ESMF_GetName(gp%base, name, status)
      if(status .NE. ESMF_SUCCESS) then
        return
      endif

      ! TODO: add calls to PhysGrid and distgrid validates

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridValidate

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridBoxIntersectRecv - Determine a DomainList covering a box
!
! !INTERFACE:
      subroutine ESMF_LRGridBoxIntersectRecv(grid, &
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i, j, rank, nDEs, numDomains
      integer :: size, totalPoints
      integer :: counts(ESMF_MAXDIM)
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes
      type(ESMF_AxisIndex), dimension(:,:), pointer :: grid_ai, localAI
      type(ESMF_Domain) :: domain
      type(ESMF_LocalArray) :: array

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! get set of bounding boxes from the grid
      call ESMF_GridGetBoundingBoxes(grid%ptr, array, status)

      ! get rank and counts from the bounding boxes array
      call ESMF_LocalArrayGet(array, counts=counts, rc=status)
      nDEs = counts(1)
      rank = counts(3)

      ! allocate arrays now
      allocate(grid_ai(nDEs,rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, grid_ai(nDE,rank) =", nDEs, rank
         return
      endif
      allocate(localAI(nDEs,rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error"
         return
      endif
    !  allocate(boxes(nDEs,2**rank,rank), stat=status)
    !  if (status .ne. 0) then
    !     print *, "allocation error, boxes(nDE,2^rank,rank) =", nDEs,2**rank,rank
    !     return
    !  endif

      ! get pointer to the actual bounding boxes data
      call ESMF_LocalArrayGetData(array, boxes, rc=status)

      ! get set of axis indices from grid
      call ESMF_LRGridGetAllAxisIndex(grid, grid_ai, distGridId=1, &
                                      total=total, rc=status)

      ! translate the AIs from global to local
      call ESMF_LRGridGlobalToLocalIndex(grid, distGridId=1, &
                                         globalAI2D=grid_ai, &
                                         localAI2D=localAI, rc=status)

      ! loop through bounding boxes, looking for overlap with our "box"
      ! TODO: a better algorithm

      ! go through list of DEs to calculate the number of domains
      ! TODO: use David's DomainList routines, but they are untested
      numDomains = 0
      do i = 1,nDEs
        if ((localMinPerDim(1).gt.max(boxes(i,2,1),boxes(i,3,1))) .or. &
            (localMaxPerDim(1).lt.min(boxes(i,1,1),boxes(i,4,1))) .or. &
            (localMinPerDim(2).gt.max(boxes(i,3,2),boxes(i,4,2))) .or. &
            (localMaxPerDim(2).lt.min(boxes(i,1,2),boxes(i,2,2)))) cycle
        numDomains = numDomains + 1
      enddo

      domainList = ESMF_DomainListCreate(numDomains)

      ! now fill in the domain list
      !  TODO: only one loop instead of two, one that figures the number
      !        of domains and one that fills it in
      ! TODO: move some of this code to Base and add a DomainList method?
      numDomains = 0
      totalPoints  = 0
      do j = 1,nDEs
        if ((localMinPerDim(1).gt.max(boxes(j,2,1),boxes(j,3,1))) .or. &
            (localMaxPerDim(1).lt.min(boxes(j,1,1),boxes(j,4,1))) .or. &
            (localMinPerDim(2).gt.max(boxes(j,3,2),boxes(j,4,2))) .or. &
            (localMaxPerDim(2).lt.min(boxes(j,1,2),boxes(j,2,2)))) cycle
        numDomains = numDomains + 1
        domainList%domains(numDomains)%DE   = j - 1  ! DEs start with 0
        domainList%domains(numDomains)%rank = rank
        size = 1
        do i = 1,rank
          domainList%domains(numDomains)%ai(i) = localAI(j,i)
          size = size * (localAI(j,i)%max - localAI(j,i)%min + 1)
        enddo
        totalPoints = totalPoints + size
      enddo
      domainList%total_points = totalPoints

      ! TODO:  the code below is taken from Phil's regrid routines and needs
      !        to be incorporated at some point
      !
      ! if spherical coordinates, set up constants for longitude branch cut
      !

      !if (dstPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !   if (units = 'degrees') then
      !      lon_thresh = 270.0
      !      lon_cycle  = 360.0
      !   else if (units = 'radians') then
      !      lon_thresh = 1.5*pi
      !      lon_cycle  = 2.0*pi
      !   endif
      !endif
      !
      ! correct for longitude crossings if spherical coords
      ! assume degrees and x is longitude
      !
      !if (dstPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) - lon_cycle
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) + lon_cycle
      !endif
      !
      ! make sure src bbox is in same longitude range as dst bbox
      ! assume degrees and x is longitude
      !
      !   if (srcPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) - lon_cycle
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) + lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) - lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) + lon_cycle
      !   endif ! Spherical coords

      deallocate(grid_ai)
      deallocate(localAI)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridBoxIntersectRecv

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridBoxIntersectSend - Determine a DomainList covering a box
!
! !INTERFACE:
      subroutine ESMF_LRGridBoxIntersectSend(dstGrid, srcGrid, localMinPerDim, &
                                             localMaxPerDim, myAI, domainList, rc)
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i, j, rank, nDEs, numDomains
      integer :: size, totalPoints
      integer :: counts(ESMF_MAXDIM)
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: boxes
      type(ESMF_AxisIndex), dimension(:), pointer :: myLocalAI
      type(ESMF_Domain) :: domain
      type(ESMF_LocalArray) :: array

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! get set of bounding boxes from the grid
      call ESMF_GridGetBoundingBoxes(dstGrid%ptr, array, status)

      ! get rank and counts from the bounding boxes array
      call ESMF_LocalArrayGet(array, counts=counts, rc=status)
      nDEs = counts(1)
      rank = counts(3)

      ! allocate arrays now
      allocate(myLocalAI(rank), stat=status)
      if (status .ne. 0) then
         print *, "allocation error"
         return
      endif

      ! translate myAI to local index
      call ESMF_LRGridGlobalToLocalIndex(srcGrid, distGridId=1, &
                                         globalAI1D=myAI, &
                                         localAI1D=myLocalAI, rc=status)

      ! get pointer to the actual bounding boxes data
      call ESMF_LocalArrayGetData(array, boxes, rc=status)

      ! loop through bounding boxes, looking for overlap with our "box"
      ! TODO: a better algorithm

      ! go through list of DEs to calculate the number of domains
      ! TODO: use David's DomainList routines, but they are untested
      numDomains = 0
      do i = 1,nDEs
        if ((localMinPerDim(1).gt.max(boxes(i,2,1),boxes(i,3,1))) .or. &
            (localMaxPerDim(1).lt.min(boxes(i,1,1),boxes(i,4,1))) .or. &
            (localMinPerDim(2).gt.max(boxes(i,3,2),boxes(i,4,2))) .or. &
            (localMaxPerDim(2).lt.min(boxes(i,1,2),boxes(i,2,2)))) cycle
        numDomains = numDomains + 1
      enddo

      domainList = ESMF_DomainListCreate(numDomains)

      ! now fill in the domain list  TODO: only one loop instead of two, one that
      ! figures the number of domains and one that fills it in
      ! TODO: move some of this code to Base and add a DomainList method
      numDomains  = 0
      totalPoints = 0
      do j = 1,nDEs
        if ((localMinPerDim(1).gt.max(boxes(j,2,1),boxes(j,3,1))) .or. &
            (localMaxPerDim(1).lt.min(boxes(j,1,1),boxes(j,4,1))) .or. &
            (localMinPerDim(2).gt.max(boxes(j,3,2),boxes(j,4,2))) .or. &
            (localMaxPerDim(2).lt.min(boxes(j,1,2),boxes(j,2,2)))) cycle
        numDomains = numDomains + 1
        domainList%domains(numDomains)%DE   = j - 1  ! DEs start with 0
        domainList%domains(numDomains)%rank = rank
        size = 1
        do i = 1,rank
          domainList%domains(numDomains)%ai(i) = myLocalAI(i)
          size = size * (myLocalAI(i)%max - myLocalAI(i)%min + 1)
        enddo
        totalPoints = totalPoints + size
      enddo
      domainList%total_points = totalPoints

      ! TODO:  the code below is taken from Phil's regrid routines and needs
      !        to be incorporated at some point
      !
      ! if spherical coordinates, set up constants for longitude branch cut
      !

      !if (dstPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !   if (units = 'degrees') then
      !      lon_thresh = 270.0
      !      lon_cycle  = 360.0
      !   else if (units = 'radians') then
      !      lon_thresh = 1.5*pi
      !      lon_cycle  = 2.0*pi
      !   endif
      !endif
      !
      ! correct for longitude crossings if spherical coords
      ! assume degrees and x is longitude
      !
      !if (dstPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) - lon_cycle
      !   if (dst_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !      dst_DE_bbox(2) = dst_DE_bbox(2) + lon_cycle
      !endif
      !
      ! make sure src bbox is in same longitude range as dst bbox
      ! assume degrees and x is longitude
      !
      !   if (srcPhysGrid%coordSystem == ESMF_CoordSystem_Spherical) then
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) - lon_cycle
      !      if (src_DE_bbox(1) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(1) = src_DE_bbox(1) + lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) >  lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) - lon_cycle
      !      if (src_DE_bbox(2) - dst_DE_bbox(1) < -lon_thresh) &
      !         src_DE_bbox(2) = src_DE_bbox(2) + lon_cycle
      !   endif ! Spherical coords

      deallocate(myLocalAI)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridBoxIntersectSend

!------------------------------------------------------------------------------
!!BOP
!! !IROUTINE: ESMF_LRGridSearchPoint - Search the grid for a cell containing point
!
! !INTERFACE:
!      subroutine ESMF_LRGridSearchPoint(dstAdd, x, y, DEId, searchGrid, &
!                                        physGridId, rc)
!!
!! !ARGUMENTS:
!
!      integer, dimension(?) :: dstAdd       ! location in grid of grid cell
!                                            ! containing search point
!      real (kind=?), intent(in) :: x        ! x coordinates of search point 
!      real (kind=?), intent(in) :: y        ! y coordinates of search point 
!      integer, intent(in) :: DEId           ! DE which owns the search point
!      type(ESMF_Grid), intent(in) :: searchGrid
!                                            ! grid to search for location of
!                                            ! point
!      integer, intent(in), optional :: physGridId
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
!!     \item[DEId]
!!          id of {\tt ESMF\_DE} that owns search point.
!!     \item[searchGrid]
!!          ESMF {\tt ESMF\_Grid} to search for location.
!!     \item[{[physGridId]}]
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
!!
!!     extract appropriate PhysGrid and DistGrid to search
!!     extract various other grid properties
!!
!      if (.not. present(physGridId)) physGridId = 1 (or whatever default)
!      ! combine these queries?  format of query functions?
!      call ESMF_LRGridGet(searchGrid, physGrid=physGridId) ??? 
!      call ESMF_LRGridGet(searchGrid, distGrid = ??)
!      call ESMF_LRGridGet(searchGrid, horzGridKind = searchGridKind)
!!
!!     Call appropriate search routine based on coordinate system and
!!     grid type.
!!
! 
!      select case (srchGridKind)
!      case(ESMF_GridKind_LatLon,      ESMF_GridKind_LatLonMercator, &
!           ESMF_GridKind_LatLonGauss, ESMF_GridKind_Reduced)
!         !*** simple search adequate for these cases
!         call ESMF_PhysGridSearchBboxSpherical(dstAdd, x, y, DEId, physGrid, &
!                                               distGrid, status)
!
!      case(ESMF_GridKind_Dipole,   ESMF_GridKind_Tripole, &
!           ESMF_GridKind_Geodesic, ESMF_GridKind_CubedSphere)
!         !*** must use more general algorithm for these cases
!         call ESMF_PhysGridSearchGeneralSpherical(dstAdd, x, y, DEId, physGrid, &
!                                                  distGrid, status)
!
!      case(ESMF_GridKind_XY, ESMF_GridKind_XYVar)
!         call ESMF_PhysGridSearchBboxCartesian(dstAdd, x, y, DEId, physGrid, &
!                                               distGrid, status)
!
!      case default
!         print *,'GridSearchPoint: search of this grid type not supported'
!         status = ESMF_Failure
!      end select
!
!      end subroutine ESMF_LRGridSearchPoint
!
!------------------------------------------------------------------------------

      end module ESMF_LogRectGridMod
