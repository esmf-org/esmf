! $id: ESMF_LogRectGrid.F90,v 1.42 2004/03/20 00:08:40 cdeluca Exp $
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
      use ESMF_IOSpecMod      ! ESMF I/O class
      use ESMF_LocalArrayMod  ! ESMF local array class
      use ESMF_DataMapMod     ! ESMF data map class
      use ESMF_DELayoutMod    ! ESMF layout class
      use ESMF_ArrayMod
      use ESMF_ArrayCreateMod
      use ESMF_ArrayGetMod
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

    public ESMF_GridCreateLogRectUniform
    public ESMF_GridCreateLogRect
    public ESMF_LRGridDistribute
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
    public ESMF_LRGridGetCellMask
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
      '$Id: ESMF_LogRectGrid.F90,v 1.55 2004/04/05 17:29:22 jwolfe Exp $'

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
!BOPI
! !INTERFACE:
      interface ESMF_LRGridSetCoord

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_LRGridSetCoordFromArray
         module procedure ESMF_LRGridSetCoordFromBuffer
         module procedure ESMF_LRGridSetCoordCompute
         module procedure ESMF_LRGridSetCoordCopy

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     coordinates as part of a {\tt ESMF\_Grid}.

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
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

!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
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

!EOPI
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
          module procedure ESMF_LRGridAddPhysGrid
          module procedure ESMF_LRGridAddVertPhysGrid

! !DESCRIPTION:
!     This interface provides a single entry point for methods that
!     search a {\tt ESMF\_Grid} for point(s).

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
      function ESMF_GridCreateLogRectUniform(dimCount, counts, &
                                             minGlobalCoordPerDim, &
                                             maxGlobalCoordPerDim, deltaPerDim, &
                                             horzGridType, vertGridType, &
                                             horzStagger, vertStagger, &
                                             horzCoordSystem, vertCoordSystem, &
                                             dimNames, dimUnits, &
                                             coordOrder, coordIndex, periodic, &
                                             layout, decompIds, &
                                             countsPerDEDecomp1, countsPerDEDecomp2, &
                                             name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateLogRectUniform
!
! !ARGUMENTS:
      integer, intent(in) :: dimCount
      integer, dimension(dimCount), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(dimCount), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(dimCount), intent(in), optional :: &
                                                            maxGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(dimCount), intent(in), optional :: deltaPerDim
      type(ESMF_GridType), intent(in), optional :: horzGridType
      type(ESMF_GridType), intent(in), optional :: vertGridType
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
      integer, dimension(:), intent(in), optional :: countsPerDEDecomp1
      integer, dimension(:), intent(in), optional :: countsPerDEDecomp2
      character(len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and internally generates the {\tt ESMF\_Grid}.  Return a pointer
!     to the new {\tt ESMF\_Grid}.  This routine is specific for uniformly spaced
!     Grids, and can create Grids from two different sets of arguments:
!        (1). given min, max, and count (variables minGlobalCoordPerDim, 
!             maxGlobalCoordPerDim, and counts);
!        (2). given min, delta, and count (variables minGlobalCoordPerDim, 
!             deltaPerDim, and counts).
!
!     The arguments are:
!     \begin{description}
!     \item[dimCount]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[minGlobalCoordPerDim]
!          Array of minimum physical coordinates in each dimension.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum physical coordinates in each direction.
!     \item[{[deltaPerDim]}]
!          Array of constant physical increments in each direction.
!     \item[{[horzGridType]}]
!          {\tt ESMF\_GridType} specifier to denote horizontal grid type.
!     \item[{[vertGridType]}]
!          {\tt ESMF\_GridType} specifier to denote vertical grid type.
!     \item[{[horzStagger]}]
!          {\tt ESMF\_GridStagger} specifier to denote horizontal grid stagger.
!     \item[{[vertStagger]}]
!          {\tt ESMF\_GridStagger} specifier to denote vertical grid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[coordOrder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote coordinate ordering.
!     \item[{[coordIndex]}]
!          {\tt ESMF\_CoordIndex} specifier to denote global or local indexing.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[layout]}]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[decompIds]}]
!          Identifier for which Grid axes are decomposed.
!     \item[{[countsPerDEDecomp1]}]
!          Array of number of grid increments per DE in the first decomposition
!          direction
!     \item[{[countsPerDEDecomp2]}]
!          Array of number of grid increments per DE in the second decomposition
!          direction
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      type(ESMF_GridClass), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize pointers
    !  nullify(grid)
    !  nullify(ESMF_GridCreateLogRectUniform%ptr)

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateLogRectUniform: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize grid internals.
      call ESMF_LRGridConstructUniform(grid, dimCount, counts, &
                                       minGlobalCoordPerDim, &
                                       maxGlobalCoordPerDim, deltaPerDim, &
                                       horzGridType, vertGridType, &
                                       horzStagger, vertStagger, &
                                       horzCoordSystem, vertCoordSystem, &
                                       dimNames, dimUnits, &
                                       coordOrder, coordIndex, periodic, &
                                       name, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateLogRectUniform: Grid construct"
        return
      endif

      ! if layout information available, call grid distribute method
      if (present(layout)) then
        call ESMF_LRGridDistribute(grid, layout, &
                                   countsPerDEDecomp1=countsPerDEDecomp1, &
                                   countsPerDEDecomp2=countsPerDEDecomp2, &
                                   decompIds=decompIds, name=name, &
                                   rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridCreateLogRectUniform: Grid distribute"
          return
        endif
      endif

      ! Set return values.
      ESMF_GridCreateLogRectUniform%ptr => grid
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateLogRectUniform

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_GridCreateLogRect - Create a new specified LogRect Grid internally

! !INTERFACE:
      function ESMF_GridCreateLogRect(dimCount, counts, minGlobalCoordPerDim, &
                                      delta1, delta2, delta3, &
                                      coord1, coord2, coord3, &
                                      horzGridType, vertGridType, &
                                      horzStagger, vertStagger, &
                                      horzCoordSystem, vertCoordSystem, &
                                      dimNames, dimUnits, &
                                      coordOrder, coordIndex, periodic, &
                                      layout, decompIds, &
                                      countsPerDEDecomp1, countsPerDEDecomp2, &
                                      name, rc)
!
! !RETURN VALUE:
      type(ESMF_Grid) :: ESMF_GridCreateLogRect
!
! !ARGUMENTS:
      integer, intent(in) :: dimCount
      integer, dimension(dimCount), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                 minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta3
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord3
      type(ESMF_GridType), intent(in), optional :: horzGridType
      type(ESMF_GridType), intent(in), optional :: vertGridType
      type(ESMF_GridStagger), intent(in), optional :: horzStagger
      type(ESMF_GridStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      character (len=*), dimension(:), intent(in), optional :: dimNames
      character (len=*), dimension(:), intent(in), optional :: dimUnits
      type(ESMF_CoordOrder), intent(in), optional :: coordOrder
      type(ESMF_CoordIndex), intent(in), optional :: coordIndex
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      type(ESMF_DELayout), intent(in), optional :: layout
      integer, dimension(dimCount), intent(in), optional :: decompIds
      integer, dimension(:), intent(in), optional :: countsPerDEDecomp1
      integer, dimension(:), intent(in), optional :: countsPerDEDecomp2
      character (len=*), intent(in), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_Grid} object, constructs its
!     internals, and internally generates the {\tt ESMF\_Grid}.  Return a pointer
!     to the new {\tt ESMF\_Grid}.  This routine is for Grids with user-specified
!     spacing, and can create Grids from two different sets of arguments:
!        (1). given min and arrays of deltas (variables minGlobalCoordPerDim and
!             delta1, delta2, and delta3, if applicable);
!        (2). given arrays of coordinates (variables coords1, coords2, and
!             coords3, if applicable).
!
!     The arguments are:
!     \begin{description}
!     \item[dimCount]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[{[minGlobalCoordsPerDim]}]
!          Array of minimum physical coordinate in each direction.
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
!     \item[{[horzGridType]}]
!          {\tt ESMF\_GridType} specifier to denote horizontal grid type.
!     \item[{[vertGridType]}]
!          {\tt ESMF\_GridType} specifier to denote vertical grid type.
!     \item[{[horzStagger]}]
!          {\tt ESMF\_GridStagger} specifier to denote horizontal grid stagger.
!     \item[{[vertStagger]}]
!          {\tt ESMF\_GridStagger} specifier to denote vertical grid stagger.
!     \item[{[horzCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the horizontal grid.
!     \item[{[vertCoordSystem]}]
!          {\tt ESMF\_CoordSystem} which identifies an ESMF standard
!          coordinate system (e.g. spherical, cartesian, pressure, etc.) for
!          the vertical grid.
!     \item[{[dimNames]}]
!          Array of dimension names.
!     \item[{[dimUnits]}]
!          Array of dimension units.
!     \item[{[coordOrder]}]
!          {\tt ESMF\_CoordOrder} specifier to denote coordinate ordering.
!     \item[{[coordIndex]}]
!          {\tt ESMF\_CoordIndex} specifier to denote global or local indexing.
!     \item[{[periodic]}]
!          Logical specifier (array) to denote periodicity along the coordinate
!          axes.
!     \item[{[layout]}]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[{[decompIds]}]
!          Identifier for which Grid axes are decomposed.
!     \item[{[countsPerDEDecomp1]}]
!          Array of number of grid increments per DE in the first decomposition
!          direction.
!     \item[{[countsPerDEDecomp2]}]
!          Array of number of grid increments per DE in the second decomposition
!          direction.
!     \item[{[name]}]
!          {\tt ESMF\_Grid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      type(ESMF_GridClass), pointer :: grid    ! Pointer to new grid
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

      ! Initialize pointers
      nullify(grid)
      nullify(ESMF_GridCreateLogRect%ptr)

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (status .NE. 0) then
        print *, "ERROR in ESMF_GridCreateLogRect: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize grid internals.
      call ESMF_LRGridConstructSpecd(grid, dimCount, coord1, coord2, coord3, &
                                     minGlobalCoordPerDim=minGlobalCoordPerDim, &
                                     delta1=delta1, delta2=delta2, delta3=delta3, &
                                     horzGridType=horzGridType, &
                                     vertGridType=vertGridType, &
                                     horzStagger=horzStagger, &
                                     vertStagger=vertStagger, &
                                     horzCoordSystem=horzCoordSystem, &
                                     vertCoordSystem=vertCoordSystem, &
                                     dimNames=dimNames, dimunits=dimUnits, &
                                     coordOrder=coordOrder, coordIndex=coordIndex, &
                                     periodic=periodic, name=name, rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_GridCreateLogRect: Grid construct"
        return
      endif

      ! if layout information available, call grid distribute method
      if (present(layout)) then
        call ESMF_LRGridDistribute(grid, layout, &
                                   countsPerDEDecomp1=countsPerDEDecomp1, &
                                   countsPerDEDecomp2=countsPerDEDecomp2, &
                                   decompIds=decompIds, name=name, &
                                   rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_GridCreateLogRect: Grid distribute"
          return
        endif
      endif

      ! Set return values.
      ESMF_GridCreateLogRect%ptr => grid
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_GridCreateLogRect

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

      type(ESMF_GridClass), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateRead%ptr)

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (status .NE. 0) then
        print *, "ERROR in ESMF_LRGridCreateRead: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateRead: Grid construct"
        return
      endif

      ! Set return values.
      ESMF_LRGridCreateRead%ptr => grid
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateRead

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

      type(ESMF_GridClass), pointer :: grid        ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateCopy%ptr)

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (status .NE. 0) then
        print *, "ERROR in ESMF_LRGridCreateCopy: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateCopy: Grid construct"
        return
      endif

      ! Set return values.
      ESMF_LRGridCreateCopy%ptr => grid
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateCopy

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

      type(ESMF_GridClass), pointer :: grid       ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateCutout%ptr)

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (status .NE. 0) then
        print *, "ERROR in ESMF_LRGridCreateCutout: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateCutout: Grid construct"
        return
      endif

      ! Set return values.
      ESMF_LRGridCreateCutout%ptr => grid
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateCutout

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

      type(ESMF_GridClass), pointer :: grid       ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateDiffRes%ptr)

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (status .NE. 0) then
        print *, "ERROR in ESMF_LRGridCreateDiffRes: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateDiffRes: Grid construct"
        return
      endif

      ! Set return values.
      ESMF_LRGridCreateDiffRes%ptr => grid
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateDiffRes

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

      type(ESMF_GridClass), pointer :: grid       ! Pointer to new grid
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize pointers
      nullify(grid)
      nullify(ESMF_LRGridCreateExchange%ptr)

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(grid, stat=status)
      ! If error write message and return.
      ! Formal error handling will be added asap.
      if (status .NE. 0) then
        print *, "ERROR in ESMF_LRGridCreateExchange: Allocate"
        return
      endif

      ! Call construction method to allocate and initialize grid internals.
      call ESMF_GridConstructNew(grid, name, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridCreateExchange: Grid construct"
        return
      endif

      ! Set return values.
      ESMF_LRGridCreateExchange%ptr => grid
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_LRGridCreateExchange

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridConstructSpecificNew - Construct a new empty logRectGrid specific type

! !INTERFACE:
      subroutine ESMF_LRGridConstructSpecificNew(lrgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_LogRectGrid) :: lrgrid
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already allocated
!     {\tt ESMF\_LogRectGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_LogRectGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only.
!
!     The arguments are:
!     \begin{description}
!     \item[lrgrid]
!          The {\tt ESMF\_LogRectGrid} object to be constructed.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: i

      ! Initialize return code
      rc     = ESMF_FAILURE

      ! Initialize lrgrid contents to default values
      do i = 1,ESMF_MAXGRIDDIM
        lrgrid%countPerDim(i) = 0
        lrgrid%deltaPerDim(i) = 0.0
      enddo
      nullify(lrgrid%coords)

      ! Set return values.
      rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridConstructSpecificNew

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridConstructUniform - Construct a uniform Grid

! !INTERFACE:
      subroutine ESMF_LRGridConstructUniform(grid, dimCount, counts, &
                                             minGlobalCoordPerDim, &
                                             maxGlobalCoordPerDim, &
                                             deltaPerDim, &
                                             horzGridType, vertGridType, &
                                             horzStagger, vertStagger, &
                                             horzCoordSystem, vertCoordSystem, &
                                             dimNames, dimUnits, &
                                             coordOrder, coordIndex, &
                                             periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
      integer, intent(in) :: dimCount
      integer, dimension(dimCount), intent(in) :: counts
      real(ESMF_KIND_R8), dimension(dimCount), intent(in) :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(dimCount), intent(in), optional :: &
                                                            maxGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(dimCount), intent(in), optional :: &
                                                            deltaPerDim
      type(ESMF_GridType), intent(in), optional :: horzGridType
      type(ESMF_GridType), intent(in), optional :: vertGridType
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
!     \item[dimCount]
!          Number of grid dimensions.
!     \item[counts]
!          Array of number of grid increments in each dimension.
!     \item[minGlobalCoordPerDim]
!          Array of minimum physical coordinates in each dimension.
!     \item[{[maxGlobalCoordPerDim]}]
!          Array of maximum physical coordinates in each direction.
!     \item[{[horzGridType]}]
!          Integer specifier to denote horizontal grid type.
!     \item[{[vertGridType]}]
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
      type (ESMF_LogRectGrid), pointer :: lrgrid
      real :: recheck
      real, dimension(dimCount) :: useMaxes, useDeltas

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initialize the derived type contents, including setting name
      call ESMF_GridConstructNew(grid, name, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridConstructUniform: Grid construct"
        return
      endif

      ! sanity check for bad values
      do i=1,dimCount
        if (counts(i) .le. 0) then
           print *, "bad value for count, ", counts(i), "for dimension ", i
           return
        endif
      enddo

      ! Fill in default values for optional arguments which weren't specified
      ! and check for an over-specified system which isn't consistent.  (in which
      ! case, deltas are overwritten and a warning printed.)
      if (present(deltaPerDim)) then
         do i=1,dimCount
           useDeltas(i) = deltaPerDim(i)
         enddo
      else
         useDeltas(:) = 1.0   ! default values
      endif

      if (present(maxGlobalCoordPerDim)) then
         do i=1,dimCount
           useMaxes(i) = maxGlobalCoordPerDim(i)
         enddo
      else
         do i=1,dimCount
           useMaxes(i) = minGlobalCoordPerDim(i) + (counts(i) * useDeltas(i))
         enddo
      endif

      if (present(deltaPerDim) .and. present(maxGlobalCoordPerDim)) then
         do i=1,dimCount
            recheck = (useMaxes(i) - minGlobalCoordPerDim(i)) / real(counts(i))
            if (recheck-useDeltas(i) .gt. 0.00001) then
              print *, "WARNING: Inconsistent set of min, max, deltas, and ", &
                       "counts specified"
              print *, "delta for dimension", i, "reset from", useDeltas(i), &
                       "to ", recheck
              useDeltas(i) = recheck
            endif
         enddo
      endif

      ! Fill in logRectGrid derived type with subroutine arguments
      allocate(grid%gridSpecific%logRectGrid, stat=status)  ! todo: check ec
      lrgrid => grid%gridSpecific%logRectGrid
      call ESMF_LRGridConstructSpecificNew(lrgrid, status)
      if (status .ne. ESMF_SUCCESS) then
         print *, "error from LRGridConstructSpecificNew"
         return
      endif
      do i = 1,dimCount
        lrgrid%countPerDim(i) = counts(i)
        lrgrid%deltaPerDim(i) = useDeltas(i) 
      enddo

      ! Fill in grid derived type with subroutine arguments
      grid%dimCount      = dimCount
      grid%gridStructure = ESMF_GridStructure_LogRect
      if (present(horzGridType   )) grid%horzGridType    = horzGridType
      if (present(vertGridType   )) grid%vertGridType    = vertGridType
      if (present(horzStagger    )) grid%horzStagger     = horzStagger
      if (present(vertStagger    )) grid%vertStagger     = vertStagger
      if (present(horzCoordSystem)) grid%horzCoordSystem = horzCoordSystem
      if (present(vertCoordSystem)) grid%vertCoordSystem = vertCoordSystem
      if (present(coordOrder     )) grid%coordOrder      = coordOrder
      if (present(coordIndex     )) grid%coordIndex      = coordIndex

      ! Set dimension names and units for each dimension
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

      ! Set periodic flags for each dimension
      if (present(periodic)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(periodic)) exit
          grid%periodic(i) = periodic(i)
        enddo
      endif

      ! Set global domain limits
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
      else
        ! default values computed above
        do i=1,size(maxGlobalCoordPerDim)
          grid%maxGlobalCoordPerDim(i) = useMaxes(i)
        enddo
      endif

      grid%gridStatus = ESMF_GridStatus_Init

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridConstructUniform

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridConstructSpecd - Construct a specified Grid

! !INTERFACE:
      subroutine ESMF_LRGridConstructSpecd(grid, dimCount, &
                                           coord1, coord2, coord3, &
                                           minGlobalCoordPerDim, &
                                           delta1, delta2, delta3, &
                                           horzGridType, vertGridType, &
                                           horzStagger, vertStagger, &
                                           horzCoordSystem, vertCoordSystem, &
                                           dimNames, dimUnits, &
                                           coordOrder, coordIndex, &
                                           periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
      integer, intent(in) :: dimCount
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord3
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: &
                                                         minGlobalCoordPerDim
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta2
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: delta3
      type(ESMF_GridType), intent(in), optional :: horzGridType
      type(ESMF_GridType), intent(in), optional :: vertGridType
      type(ESMF_GridStagger), intent(in), optional :: horzStagger
      type(ESMF_GridStagger), intent(in), optional :: vertStagger
      type(ESMF_CoordSystem), intent(in), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(in), optional :: vertCoordSystem
      character (len=*), dimension(:), intent(in), optional :: dimNames
      character (len=*), dimension(:), intent(in), optional :: dimUnits
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
!     \item[dimCount]
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
!     \item[{[horzGridType]}]
!          Integer specifier to denote horizontal grid type.
!     \item[{[vertGridType]}]
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
      integer, dimension(dimCount) :: counts
      real(ESMF_KIND_R8), dimension(dimCount) :: minGlobalCoordPerDimUse, &
                                                 maxGlobalCoordPerDimUse
      real(ESMF_KIND_R8), dimension(:), pointer :: coordsUse1, coordsUse2, &
                                                   coordsUse3
      type(ESMF_LocalArray), dimension(:), pointer :: coords
      type(ESMF_LogRectGrid), pointer :: lrgrid

      ! Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initialize the derived type contents, including setting name
      call ESMF_GridConstructNew(grid, name, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridConstructSpecd: Grid construct"
        return
      endif

      ! Fill in logRectGrid derived type with subroutine arguments
      ! TODO: check stat return code against 0 (not ESMF_FAILURE)
      allocate(grid%gridSpecific%logRectGrid, stat=status) 
      lrgrid => grid%gridSpecific%logRectGrid
      call ESMF_LRGridConstructSpecificNew(lrgrid, status)
      if (status .ne. ESMF_SUCCESS) then
         print *, "error from LRGridConstructSpecificNew"
         return
      endif
      allocate(grid%gridSpecific%logRectGrid%coords(dimCount), stat=status)
   
      coords => grid%gridSpecific%logRectGrid%coords

      ! Two ways to make a grid here: by coordinates or by minima and deltas
      ! by coordinates:
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
          if (dimCount.le.1) then
            print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                     "dimCount not consistent with coords arrays"
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
          if (dimCount.le.2) then
            print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                     "dimCount not consistent with coords arrays"
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

      ! by deltas (and minCoordPerDim):     TODO: make it a starting value instead of min
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
          if (dimCount.le.1) then
            print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                     "dimCount not consistent with deltas arrays"
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
        if (present(delta3)) then
          if (dimCount.le.2) then
            print *, "ERROR in ESMF_LRGridConstructSpecd: ", &
                     "dimCount not consistent with deltas arrays"
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

      ! either way, we have the counts now
      do i = 1,dimCount
        lrgrid%countPerDim(i) = counts(i)
      enddo

      ! Fill in grid derived type with subroutine arguments
      grid%dimCount      = dimCount
      grid%gridStructure = ESMF_GridStructure_LogRect
      if (present(horzGridType   )) grid%horzGridType    = horzGridType
      if (present(vertGridType   )) grid%vertGridType    = vertGridType
      if (present(horzStagger    )) grid%horzStagger     = horzStagger
      if (present(vertStagger    )) grid%vertStagger     = vertStagger
      if (present(horzCoordSystem)) grid%horzCoordSystem = horzCoordSystem
      if (present(vertCoordSystem)) grid%vertCoordSystem = vertCoordSystem
      if (present(coordOrder     )) grid%coordOrder      = coordOrder
      if (present(coordIndex     )) grid%coordIndex      = coordIndex

      ! Set dimension names and units for each dimension
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

      ! Set periodic flags for each dimension
      if (present(periodic)) then
        do i=1,ESMF_MAXGRIDDIM
          if (i > size(periodic)) exit
          grid%periodic(i) = periodic(i)
        enddo
      endif

      ! Set global domain limits
      do i=1,size(minGlobalCoordPerDimUse)
        grid%minGlobalCoordPerDim(i) = minGlobalCoordPerDimUse(i)
      enddo
      do i=1,size(maxGlobalCoordPerDimUse)
        grid%maxGlobalCoordPerDim(i) = maxGlobalCoordPerDimUse(i)
      enddo

      grid%gridStatus = ESMF_GridStatus_Init

      ! Clean up
      deallocate(coordsUse1)
      deallocate(coordsUse2)
      if (dimCount.ge.3) deallocate(coordsUse3)

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridConstructSpecd

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridDistribute - Distribute a Grid

! !INTERFACE:
      subroutine ESMF_LRGridDistribute(grid, layout, &
                                       countsPerDEDecomp1, countsPerDEDecomp2, &
                                       decompIds, name, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in), optional :: countsPerDEDecomp1
      integer, dimension(:), intent(in), optional :: countsPerDEDecomp2
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
!     \item[{[countsPerDEDecomp1]}]
!          Array of number of grid increments per DE in the first decomposition axis.
!     \item[{[countsPerDEDecomp2]}]
!          Array of number of grid increments per DE in the second decomposition axis.
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
      character(len=ESMF_MAXSTR), dimension(:), allocatable :: dimNames, dimUnits
      integer :: distGridId, physGridId, nDEs(0:2)
      integer :: i, dimCount, dimCountGrid, size
      integer, dimension(:), allocatable :: decompIdsUse, counts
      integer, dimension(:), allocatable :: countsPerDEDecomp1Use, &
                                            countsPerDEDecomp2Use
      integer, dimension(:), allocatable :: countsPerDE1, countsPerDE2, &
                                            countsPerDE3
      real(ESMF_KIND_R8) :: delta
      real(ESMF_KIND_R8), dimension(:), allocatable :: min, max
      real(ESMF_KIND_R8), dimension(:), pointer :: coord1, coord2, coord3
      type(ESMF_Logical), dimension(:), allocatable :: periodic
      type(ESMF_LocalArray), dimension(:), pointer :: coords
      type(ESMF_RelLoc) :: relloc

      ! Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! validate the layout before going any furthe
      call ESMF_DELayoutValidate(layout, rc=status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_LogRectGridDistribute: bad layout, cannot distribute grid"
          return
      endif
      
      ! Extract some information from the Grid
      dimCount = grid%dimCount
      allocate(counts(dimCount))
      allocate(min(dimCount))
      allocate(max(dimCount))
      allocate(dimNames(dimCount))
      allocate(dimUnits(dimCount))
      allocate(periodic(dimCount))

      nullify(coords)
      if (associated(grid%gridSpecific%logRectGrid%coords)) then
        coords => grid%gridSpecific%logRectGrid%coords
      endif

      do i = 1,dimCount
        counts(i)   = grid%gridSpecific%logRectGrid%countPerDim(i)
        min(i)      = grid%minGlobalCoordPerDim(i)
        max(i)      = grid%maxGlobalCoordPerDim(i)
        periodic(i) = grid%periodic(i)
        dimNames(i) = grid%dimNames(i)
        dimUnits(i) = grid%dimUnits(i)
      enddo
      if (associated(coords)) then
        call ESMF_LocalArrayGetData(coords(1), coord1, ESMF_DATA_COPY, status)
        if (dimCount.ge.2) &
          call ESMF_LocalArrayGetData(coords(2), coord2, ESMF_DATA_COPY, status)
        if (dimCount.ge.3) &
          call ESMF_LocalArrayGetData(coords(3), coord3, ESMF_DATA_COPY, status)
      else              ! assume uniform grid, so no coords stored
        allocate(coord1(counts(1)+1))
        coord1(1) = min(1)
        delta     = (max(1) - min(1)) / real(counts(1))
        do i = 1,counts(1)
          coord1(i+1) = coord1(i) + delta
        enddo
        if (dimCount.ge.2) then
          allocate(coord2(counts(2)+1))
          coord2(1) = min(2)
          delta     = (max(2) - min(2)) / real(counts(2))
          do i = 1,counts(2)
            coord2(i+1) = coord2(i) + delta
          enddo
        endif
        if (dimCount.ge.3) then
          allocate(coord3(counts(3)+1))
          coord3(1) = min(3)
          delta     = (max(3) - min(3)) / real(counts(3))
          do i = 1,counts(3)
            coord3(i+1) = coord3(i) + delta
          enddo
        endif
      endif

      ! Fill in defaults for necessary but optional variables
      allocate(decompIdsUse(dimCount))
      decompIdsUse = 0
      if (present(decompIds)) then
        decompIdsUse = decompIds
      else
        do i = 1, 2
          decompIdsUse(i) = i
        enddo
      endif

      nDEs(0) = 1
      call ESMF_DELayoutGetSize(layout, nDEs(1), nDEs(2), status)

      ! if there is an axis to decompose, either grab the specfied countsPerDE
      ! or parse the global count
      do i = 1,dimCount
        if (decompIdsUse(i).eq.1) then
          size = nDEs(1)
          allocate(countsPerDEDecomp1Use(size))
          if (present(countsPerDEDecomp1)) then
            countsPerDEDecomp1Use = countsPerDEDecomp1
          else
            call ESMF_DELayoutParse(layout, 1, counts(i), &
                                    countsPerDEDecomp1Use, rc)
          endif
        endif
        if (decompIdsUse(i).eq.2) then
          size = nDEs(2)
          allocate(countsPerDEDecomp2Use(size))
          if (present(countsPerDEDecomp2)) then
            countsPerDEDecomp2Use = countsPerDEDecomp2
          else
            call ESMF_DELayoutParse(layout, 2, counts(i), &
                                    countsPerDEDecomp2Use, rc)
          endif
        endif
      enddo

      ! Determine if the axis are decomposed and load counts arrays
      size = nDEs(decompIdsUse(1))
      allocate(countsPerDE1(size))
      if     (decompIdsUse(1).eq.0) then
        countsPerDE1(:) = counts(1)
      elseif (decompIdsUse(1).eq.1) then
        countsPerDE1(:) = countsPerDEDecomp1Use(:)
      elseif (decompIdsUse(1).eq.2) then
        countsPerDE1(:) = countsPerDEDecomp2Use(:)
      endif

      size = nDEs(decompIdsUse(2))
      allocate(countsPerDE2(size))
      if     (decompIdsUse(2).eq.0) then
        countsPerDE2(:) = counts(2)
      elseif (decompIdsUse(2).eq.1) then
        countsPerDE2(:) = countsPerDEDecomp1Use(:)
      elseif (decompIdsUse(2).eq.2) then
        countsPerDE2(:) = countsPerDEDecomp2Use(:)
      endif

      if (dimCount.eq.3) then
        size = nDEs(decompIdsUse(3))
        allocate(countsPerDE3(size))
        if     (decompIdsUse(3).eq.0) then
          countsPerDE3(:) = counts(3)
        elseif (decompIdsUse(3).eq.1) then
          countsPerDE3(:) = countsPerDEDecomp1Use(:)
        elseif (decompIdsUse(3).eq.2) then
          countsPerDE3(:) = countsPerDEDecomp2Use(:)
        endif
      endif

      ! Create DistGrid and PhysGrid at cell center
      dimCountGrid = dimCount
      if (dimCount.eq.3) dimCountGrid = 2
      distGridId = 1
      distGridName = 'cell_center'
      physGridId = 1
      physGridName = 'cell_center'
      relloc = ESMF_CELL_CENTER
      call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                  layout, decompIdsUse(1:2), periodic, &
                                  countsPerDEDim1=countsPerDE1, &
                                  countsPerDEDim2=countsPerDE2, &
                                  distGridName=distGridName, rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
        return
      endif
      call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                  decompIdsUse(1:2), coord1, coord2, &
                                  countsPerDE1, countsPerDE2, &
                                  dimNames, dimUnits, physGridName, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
        return
      endif
      grid%distGridIndex(physGridId) = distGridId
      distGridId = distGridId + 1 
      physGridId = physGridId + 1 

      ! Create any other DistGrids and PhysGrids necessary for horizontal
      ! grid stagger
      ! TODO: finish filling out, look up D
      select case (grid%horzStagger%stagger)

        ! Arakawa A (centered velocity)
        case (1)

        ! Arakawa B_NE (velocities at NE grid corner)
        case (2)
          distGridName = 'cell_necorner'
          physGridName = 'cell_necorner'
          relloc = ESMF_CELL_NECORNER
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1 
          physGridId = physGridId + 1 

        ! Arakawa B_SW (velocities at SW grid corner)
        case (3)
          distGridName = 'cell_swcorner'
          physGridName = 'cell_swcorner'
          relloc = ESMF_CELL_SWCORNER
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1

        ! Arakawa B_SE (velocities at SE grid corner)
        case (4)
          distGridName = 'cell_secorner'
          physGridName = 'cell_secorner'
          relloc = ESMF_CELL_SECORNER
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1

        ! Arakawa B_NW (velocities at NW grid corner)
        case (5)
          distGridName = 'cell_nwcorner'
          physGridName = 'cell_nwcorner'
          relloc = ESMF_CELL_NWCORNER
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1

        ! Arakawa C_NE (U at E face, V at N face) and
        ! Arakawa D_NE (V at E face, U at N face)
        case (6,10)
          distGridName = 'cell_eface'
          physGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1
          distGridName = 'cell_nface'
          physGridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1

        ! Arakawa C_SW (U at W face, V at S face) and
        ! Arakawa D_SW (V at W face, U at S face)
        case (7,11)
          distGridName = 'cell_wface'
          physGridName = 'cell_wface'
          relloc = ESMF_CELL_WFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1
          distGridName = 'cell_sface'
          physGridName = 'cell_sface'
          relloc = ESMF_CELL_SFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1

        ! Arakawa C_SE (U at E face, V at S face) and
        ! Arakawa D_SE (V at E face, U at S face)
        case (8,12)
          distGridName = 'cell_eface'
          physGridName = 'cell_eface'
          relloc = ESMF_CELL_EFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1
          distGridName = 'cell_sface'
          physGridName = 'cell_sface'
          relloc = ESMF_CELL_SFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1

        ! Arakawa C_NW (U at W face, V at N face) and
        ! Arakawa D_NW (V at W face, U at N face)
        case (9,13)
          distGridName = 'cell_wface'
          physGridName = 'cell_wface'
          relloc = ESMF_CELL_WFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1
          distGridName = 'cell_nface'
          physGridName = 'cell_nface'
          relloc = ESMF_CELL_NFACE
          call ESMF_LRGridAddDistGrid(grid, distGridId, dimCountGrid, counts, &
                                      layout, decompIdsUse(1:2), periodic, &
                                      countsPerDEDim1=countsPerDE1, &
                                      countsPerDEDim2=countsPerDE2, &
                                      distGridName=distGridName, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
            return
          endif
          call ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCountGrid, &
                                      decompIdsUse(1:2), coord1, coord2, &
                                      countsPerDE1, countsPerDE2, &
                                      dimNames, dimUnits, physGridName, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridDistribute: Add PhysGrid"
            return
          endif
          grid%distGridIndex(physGridId) = distGridId
          distGridId = distGridId + 1
          physGridId = physGridId + 1

      end select

      ! Create vertical PhysGrid if requested
      if (dimCount.eq.3) then
        distGridName = 'vertical center'
        physGridName = 'vertical center'
        relloc = ESMF_CELL_CELL    ! TODO: right relloc?
        call ESMF_LRGridAddDistGrid(grid, distGridId, 1, counts(3:3), &
                                    layout, decompIdsUse(3:3), &
                                    periodic(3:3), &
                                    countsPerDEDim1=countsPerDE3, &
                                    distGridName=distGridName, rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridDistributeUniform: Add DistGrid"
          return
        endif
        call ESMF_LRGridAddVertPhysGrid(grid, physGridId, relloc, coord3, &
                                        countsPerDE3, physGridName, status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridDistribute: Add vert PhysGrid"
          return
        endif
        grid%distGridIndex(physGridId) = distGridId
        distGridId = distGridId + 1
        physGridId = physGridId + 1

        select case (grid%vertStagger%stagger)

          ! ESMF_GridStagger_VertCenter - vertical velocity at vertical midpoints
          case (16)

          ! ESMF_GridStagger_VertFace - vertical velocity at top vertical face
          case (17)
            distGridName = 'vertical top face'
            physGridName = 'vertical top face'
            relloc = ESMF_CELL_TOPFACE
            call ESMF_LRGridAddDistGrid(grid, distGridId, 1, counts(3:3), &
                                        layout, decompIdsUse(3:3), &
                                        periodic(3:3), &
                                        countsPerDEDim1=countsPerDE3, &
                                        distGridName=distGridName, rc=status)
            if (status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridDistribute: Add DistGrid"
              return
            endif
            call ESMF_LRGridAddVertPhysGrid(grid, physGridId, relloc, coord3, &
                                            countsPerDE3, physGridName, status)
            if (status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridDistribute: Add vert PhysGrid"
              return
            endif
            grid%distGridIndex(physGridId) = distGridId
            distGridId = distGridId + 1
            physGridId = physGridId + 1

        ! TODO: add default in case vertical stagger is not defined
        end select
      endif

      ! Create the BoundingBoxes structure
      call ESMF_LRGridSetBoundingBoxes(grid, dimCount, coord1, coord2, &
                                       countsPerDE1, countsPerDE2, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridDistribute: Grid set boxes"
        return
      endif

      ! Clean up
      deallocate(counts)
      deallocate(min)
      deallocate(max)
      deallocate(dimNames)
      deallocate(dimUnits)
      deallocate(periodic)
      if (associated(coord1)) deallocate(coord1)
      if (dimCount.ge.2 .and. associated(coord2)) deallocate(coord2)
      if (dimCount.ge.3 .and. associated(coord3)) deallocate(coord3)
      deallocate(decompIdsUse)
      deallocate(countsPerDEDecomp1Use)
      deallocate(countsPerDEDecomp2Use)
      deallocate(countsPerDE1)
      deallocate(countsPerDE2)
      if (dimCount.ge.3) deallocate(countsPerDE3)

      grid%gridStatus = ESMF_GridStatus_Ready
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridDistribute

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridDestruct - Free all resources associated with a Grid

! !INTERFACE:
      subroutine ESMF_LRGridDestruct(grid, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
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

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      !TODO: destruct these
      !  type (ESMF_Base) :: base
      !  type (ESMF_Status) :: gridStatus
      grid%horzGridType    = ESMF_GridType_Unknown
      grid%vertGridType    = ESMF_GridType_Unknown
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

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridDestruct

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridAddDistGrid - Add a DistGrid to a LogRectGrid

! !INTERFACE:
      subroutine ESMF_LRGridAddDistGrid(grid, distGridId, dimCount, counts, &
                                        layout, decompIds, periodic, &
                                        coversDomain, &
                                        countsPerDEDim1, countsPerDEDim2, &
                                        distGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), target :: grid
      integer, intent(out) :: distGridId
      integer, intent(in) :: dimCount 
      integer, dimension(dimCount), intent(in) :: counts
      type (ESMF_DELayout), intent(in) :: layout
      integer, dimension(dimCount), intent(in), optional :: decompIds
      type(ESMF_Logical), dimension(dimCount), intent(in), optional :: periodic
      type(ESMF_Logical), dimension(dimCount), intent(in), optional :: &
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
!     \item[dimCount]
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
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Create the DistGrid
      distGrid = ESMF_DistGridCreate(dimCount=dimCount, counts=counts, &
                                     layout=layout, decompIds=decompIds, &
                                     periodic=periodic, &
                                     coversDomain=coversDomain, &
                                     countsPerDEDim1=countsPerDEDim1, &
                                     countsPerDEDim2=countsPerDEDim2, &
                                     rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddDistGrid: Distgrid create"
        return
      endif

      ! now that it's created, add the distgrid to the grid
      call ESMF_GridAddDistGrid(grid, distGrid, status)
      distGridId = grid%numDistGrids

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridAddDistGrid

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridAddPhysGrid - Add a PhysGrid to a LogRectGrid

! !INTERFACE:
      subroutine ESMF_LRGridAddPhysGrid(grid, physGridId, relloc, dimCount, &
                                        decompIds, coord1, coord2, &
                                        countsPerDEDim1, countsPerDEDim2, &
                                        dimNames, dimUnits, &
                                        physGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), target :: grid
      integer, intent(out) :: physGridId
      type(ESMF_RelLoc), intent(in) :: relloc
      integer, intent(in) :: dimCount 
      integer, dimension(:), intent(in) :: decompIds
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
      integer, dimension(:), intent(in) :: countsPerDEDim1
      integer, dimension(:), intent(in) :: countsPerDEDim2
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
!     \item[dimCount]
!          Number of grid dimensions.
!     \item[decompIds]
!          Identifier for which Grid axes are decomposed.
!     \item[coord1]
!          Array of physical coordinates in the first direction.
!     \item[coord2]
!          Array of physical coordinates in the second direction.
!     \item[countsPerDEDim1]
!          Array of number of grid increments per DE in the x-direction.
!     \item[countsPerDEDim2]
!          Array of number of grid increments per DE in the y-direction.
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
      integer :: i, j, i1, i2, j1, j2, gridBoundWidth, myDE(2), myDEDecomp(0:2)
      integer, dimension(dimCount) :: counts, compCount, localStart
      integer, dimension(:), allocatable :: cellType1, cellType2
      character(len=ESMF_MAXSTR), dimension(dimCount) :: coordNames, coordUnits
      logical, dimension(dimCount) :: coordAligned, coordEqualSpaced, coordCyclic
      real(ESMF_KIND_R8) :: last
      real(ESMF_KIND_R8), dimension(dimCount) :: localMin, localMax
      real(ESMF_KIND_R8), dimension(:), allocatable :: coordUse1, coordUse2
      type(ESMF_CoordSystem) :: coordSystem
      type(ESMF_CoordType), dimension(dimCount) :: coordType
      type(ESMF_DELayout) :: layout
      type(ESMF_Grid) :: gridp
      type(ESMF_PhysGrid) :: physGrid
      type(ESMF_PhysCoord) :: tempCoord

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! initialize some values
      gridBoundWidth = 1   ! TODO: move into structure, make input?

      ! figure out the position of myDE to get local counts
      gridp%ptr => grid
      call ESMF_GridGetDELayout(gridp, layout, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGrid: get delayout"
        return
      endif
      myDEDecomp(0) = 1
      call ESMF_DELayoutGetDEPosition(layout, myDEDecomp(1), myDEDecomp(2), &
                                      status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGrid: delayout get position"
        return
      endif
     
      ! modify myDE array by decompIds
      do i = 1,dimCount
        myDE(i) = myDEDecomp(decompIds(i))
      enddo

      localMin(1) = 0.0
      localMin(2) = 0.0
      localStart(1) = 0
      localStart(2) = 0
      if (myDE(1).ge.2) then
        do j = 1,myDE(1)-1
          localStart(1) = localStart(1) + countsPerDEDim1(j)
        enddo
      endif
      j1 = localStart(1) + 1
      j2 = localStart(1) + countsPerDEDim1(myDE(1))
      localMin(1) = minval(coord1(j1:j2))
      localMax(1) = maxval(coord1(j1:j2))

      if (myDE(2).ge.2) then
        do j = 1,myDE(2)-1
          localStart(2) = localStart(2) + countsPerDEDim2(j)
        enddo
      endif
      j1 = localStart(2) + 1
      j2 = localStart(2) + countsPerDEDim2(myDE(2))
      localMin(2) = minval(coord2(j1:j2))
      localMax(2) = maxval(coord2(j1:j2))

      ! modify global counts to include ghost region
      compCount(1) = size(coord1)
      compCount(2) = size(coord2)
      counts(1) = compCount(1) + 2*gridBoundWidth
      counts(2) = compCount(2) + 2*gridBoundWidth

      ! allocate and load coords
      allocate(coordUse1(counts(1)), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(1) =", counts(1)
        return
      endif
      allocate(coordUse2(counts(2)), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(2) =", counts(2)
        return
      endif

      do i = 1,compCount(1)
        coordUse1(i+gridBoundWidth) = coord1(i)
      enddo
      do i = gridBoundWidth,1,-1
        coordUse1(i) = coordUse1(i+1) - (coord1(2)-coord1(1))
      enddo
      do i = compCount(1)+gridBoundWidth,compCount(1)+2*gridBoundWidth-1
        coordUse1(i+1) = coordUse1(i) &
                       + (coord1(compCount(1))-coord1(compCount(1)-1))
      enddo
      do i = 1,compCount(2)
        coordUse2(i+gridBoundWidth) = coord2(i)
      enddo
      do i = gridBoundWidth,1,-1
        coordUse2(i) = coordUse2(i+1) - (coord2(2)-coord2(1))
      enddo
      do i = compCount(2)+gridBoundWidth,compCount(2)+2*gridBoundWidth-1
        coordUse2(i+1) = coordUse2(i) &
                       + (coord2(compCount(2))-coord2(compCount(2)-1))
      enddo

      ! allocate and load cell type masks -- these are by cell and not vertex,
      ! so the counts are all one less
      allocate(cellType1(counts(1)-1), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(1) =", counts(1)-1
        return
      endif
      allocate(cellType2(counts(2)-1), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, counts(2) =", counts(2)-1
        return
      endif
      cellType1 = 0
      cellType2 = 0
      do i = 1,gridBoundWidth
        cellType1(i) = 1
        cellType2(i) = 1
        cellType1(compCount(1)-1+gridBoundWidth+i) = 1
        cellType2(compCount(2)-1+gridBoundWidth+i) = 1
      enddo

      ! set parameters based on grid type
      select case (grid%horzGridType%gridType)

        ! ESMF_GridType_LatLon
        case (1)
          coordSystem         = ESMF_CoordSystem_Spherical
          coordNames(1)       = 'latitude'
          coordNames(2)       = 'longitude'
          coordType(1)        = ESMF_CoordType_Lat
          coordType(2)        = ESMF_CoordType_Lon
          coordUnits(:)       = 'degrees'
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .false.
          coordCyclic(1)      = .true.
          coordCyclic(2)      = .false.

        ! ESMF_GridType_XY
        case (7)
          coordSystem         = ESMF_CoordSystem_Cartesian
          coordNames(1)       = 'x'
          coordNames(2)       = 'y'
          coordType(1)        = ESMF_CoordType_X
          coordType(2)        = ESMF_CoordType_Y
          coordUnits(:)       = ''
          coordAligned(:)     = .true.
          coordEqualSpaced(:) = .false.
          coordCyclic(:)      = .false.

        case default
           print *,'Grid type not yet supported in LRGridAddPhysGrid'
           status = ESMF_FAILURE

      end select

      ! Create the actual PhysGrid object
      physGrid = ESMF_PhysGridCreate(dimCount, relloc, physGridName, &
                                     coordSystem, rc=status)

      do i = 1,dimCount
        tempCoord = ESMF_PhysCoordCreate(coordType(i), coordNames(i), &
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
      call ESMF_LRGridSetCoord(grid, physGridId, dimCount, counts, &
                               gridBoundWidth, relloc, coordUse1(i1:i2), &
                               coordUse2(j1:j2), total=.true., rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGrid: Grid set coord"
        return
      endif
      ! set coordinates using computational cell count
      counts(1) = countsPerDEDim1(myDE(1))
      counts(2) = countsPerDEDim2(myDE(2))
      i1 = localStart(1) + 1 + gridBoundWidth
      i2 = i1 + counts(1)
      j1 = localStart(2) + 1 + gridBoundWidth
      j2 = j1 + counts(2)
      call ESMF_LRGridSetCoord(grid, physGridId, dimCount, counts, 0, relloc, &
                               coordUse1(i1:i2), coordUse2(j1:j2), total=.false., &
                               rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGrid: Grid set coord"
        return
      endif

      ! set mask using total cell count
      counts(1) = countsPerDEDim1(myDE(1)) + 2*gridBoundWidth
      counts(2) = countsPerDEDim2(myDE(2)) + 2*gridBoundWidth
      i1 = localStart(1) + 1
      i2 = localStart(1) + counts(1)
      j1 = localStart(2) + 1
      j2 = localStart(2) + counts(2)
      call ESMF_LRGridSetCellMask(grid, physGridId, dimCount, counts, &
                                  gridBoundWidth, relloc, cellType1(i1:i2), &
                                  cellType2(j1:j2), status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGrid: Grid set cell mask"
        return
      endif

      deallocate(coordUse1)
      deallocate(coordUse2)
      deallocate(cellType1)
      deallocate(cellType2)

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridAddPhysGrid

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridAddVertPhysGrid - Add a vertical PhysGrid to a LogRectGrid

! !INTERFACE:
      subroutine ESMF_LRGridAddVertPhysGrid(grid, physGridId, relloc, coord, &
                                            countsPerDEDim, physGridName, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), target :: grid
      integer, intent(out) :: physGridId
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord
      integer, dimension(:), intent(in) :: countsPerDEDim
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
!     \item [physGridId]
!          Integer identifier for {\tt ESMF\_PhysGrid}.
!     \item[relloc]
!          Relative location of data at the centers, faces, and vertices of
!          the {\tt Grid}.
!     \item[coord]
!          Array of physical coordinates in the vertical direction.
!     \item[countsPerDEDim]
!          Array of number of grid increments per DE in the vertical direction.
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
      integer :: i, i1, i2, gridBoundWidth, myDE(2)
      character(len=ESMF_MAXSTR) :: coordName, coordUnit
      logical :: coordAligned, coordEqualSpaced, coordCyclic
      integer :: count, compCount, localStart
      integer, dimension(1) :: localCount
      integer, dimension(:), allocatable :: cellType
      real(ESMF_KIND_R8) :: localMinCoord, localMaxCoord
      real(ESMF_KIND_R8), dimension(:), allocatable :: coordUse
      type(ESMF_CoordType) :: coordType
      type(ESMF_DELayout) :: layout
      type(ESMF_PhysCoord) :: tempCoord
      type(ESMF_Grid) :: gridp
      type(ESMF_PhysGrid) :: physGrid
      type(ESMF_CoordSystem) :: coordSystem

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! initialize some values
      gridBoundWidth = 1   ! TODO: move into structure, make input?

      ! figure out the position of myDE to get local counts
      gridp%ptr => grid
      call ESMF_GridGetDELayout(gridp, layout, status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGrid: get delayout"
        return
      endif
      call ESMF_DELayoutGetDEPosition(layout, myDE(1), myDE(2), status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddPhysGrid: delayout get position"
        return
      endif

      localMinCoord = 0.0
      localStart    = 0
  !    if (myDE(1).ge.2) then    TODO: modify to use decompId to indirect address
  !      do j = 1,myDE(1)-1
  !        localStart = localStart + countsPerDEDim(j)
  !      enddo
  !    endif
      i1 = localStart + 1
  !    i2 = localStart + countsPerDEDim(myDE(1))
      i2 = localStart + countsPerDEDim(1)      ! for now
      localMinCoord = minval(coord(i1:i2))
      localMaxCoord = maxval(coord(i1:i2))

      ! modify global counts to include ghost region
      compCount = size(coord)
      count     = compCount + 2*gridBoundWidth

      ! allocate and load coords
      allocate(coordUse(count), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, count =", count
        return
      endif

      do i = 1,compCount
        coordUse(i+gridBoundWidth) = coord(i)
      enddo
      do i = gridBoundWidth,1,-1
        coordUse(i) = coordUse(i+1) - (coord(2)-coord(1))
      enddo
      do i = compCount+gridBoundWidth,compCount+2*gridBoundWidth-1
        coordUse(i+1) = coordUse(i) &
                      + (coord(compCount)-coord(compCount-1))
      enddo

      ! allocate and load cell type masks
      allocate(cellType(count-1), stat=status)
      if (status .ne. 0) then
        print *, "allocation error, count =", count-1
        return
      endif
      cellType = 0
      do i = 1,gridBoundWidth
        cellType(i) = 1
        cellType(compCount-1+gridBoundWidth+i) = 1
      enddo

      ! set parameters based on vertical coordinate system  TODO: better as a case
      if (grid%vertCoordSystem.eq.ESMF_CoordSystem_Depth) then
        ! ESMF_CoordSystem_Depth
        coordSystem      = ESMF_CoordSystem_Depth
        coordName        = 'depth'
        coordType        = ESMF_CoordType_Depth
        coordUnit        = ''
        coordAligned     = .true.
        coordEqualSpaced = .true.
        coordCyclic      = .false.

      elseif (grid%vertCoordSystem.eq.ESMF_CoordSystem_Height) then
        ! ESMF_CoordSystem_Height
        coordSystem      = ESMF_CoordSystem_Height
        coordName        = 'height'
        coordType        = ESMF_CoordType_Height
        coordUnit        = ''
        coordAligned     = .true.
        coordEqualSpaced = .true.
        coordCyclic      = .false.

      else
        print *,'Grid type not yet supported in GridAddVertPhysGrid'
        status = ESMF_FAILURE
      endif

      ! Create the actual PhysGrid object
      physGrid = ESMF_PhysGridCreate(1, relloc, physGridName, coordSystem, &
                                     rc=status)

      tempCoord = ESMF_PhysCoordCreate(coordType, name=coordName, &
                                       units=coordUnit, &
                                       aligned=coordAligned, &
                                       equalSpaced=coordEqualSpaced, &
                                       cyclic=coordCyclic, &
                                       minVal=localMinCoord, &
                                       maxVal=localMaxCoord, rc=status)
      call ESMF_PhysGridSetCoord(physGrid, tempCoord, dimOrder=1, rc=status)

      ! now that it's created, add the physgrid to the grid
      call ESMF_GridAddPhysGrid(grid, physGrid, status)
      physGridId = grid%numPhysGrids

      ! set coordinates using total cell count
      localCount(1) = countsPerDEDim(1) + 2*gridBoundWidth  ! TODO: indirect address for countPer
      i1 = localStart + 1
      i2 = localStart + localCount(1) + 1
      call ESMF_LRGridSetCoord(grid, physGridId, 1, localCount, &
                               gridBoundWidth, relloc, coordUse(i1:i2), &
                               total=.true., rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddVertPhysGrid: Grid set coord"
        return
      endif
      ! set coordinates using computational cell count
      localCount(1) = countsPerDEDim(1)  ! TODO: indirect address for countPer
      i1 = localStart + 1 + gridBoundWidth
      i2 = i1 + localCount(1)
      call ESMF_LRGridSetCoord(grid, physGridId, 1, localCount, &
                               0, relloc, coordUse(i1:i2), &
                               total=.false., rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddVertPhysGrid: Grid set coord"
        return
      endif

      ! set mask using total cell count
      localCount(1) = countsPerDEDim(1) + 2*gridBoundWidth  ! TODO: indirect address for countPer
      i1 = localStart + 1
      i2 = localStart + localCount(1) + 1
      ! TODO: fix setcellmask to work 1d
      ! call ESMF_LRGridSetCellMask(grid, physGridId, 1, localCount, &
      !                             gridBoundWidth, relloc, cellType(i1:i2), &
      !                             rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridAddVertPhysGrid: Grid set cell mask"
        return
      endif

      ! clean up
      deallocate(coordUse)
      deallocate(cellType)

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridAddVertPhysGrid

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridGetCoord - Get the coordinates of a Grid

! !INTERFACE:
      subroutine ESMF_LRGridGetCoord(grid, horzRelLoc, vertRelLoc, centerCoord, &
                                     cornerCoord, faceCoord, reorder, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      type(ESMF_Array), intent(out), dimension(:), optional :: centerCoord
      type(ESMF_Array), intent(out), dimension(:), optional :: cornerCoord
      type(ESMF_Array), intent(out), optional :: faceCoord
      logical, intent(in), optional :: reorder
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
!     \item[{[horzRelLoc]}]
!          Horizontal relative location of the {\tt ESMF\_PhysGrid} to be
!          queried.
!     \item[{[vertRelLoc]}]
!          Vertical relative location of the {\tt ESMF\_PhysGrid} to be
!          queried.
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
!     \item[{[reorder]}]
!          Logical.  If TRUE, reorder any results using the GridOrder before
!          returning.  If FALSE do not reorder.  The default value is TRUE
!          and users should not need to reset this for most applications.
!     \item[{[total]}]
!          Logical. If TRUE, return the total coordinates including internally
!          generated boundary cells. If FALSE return the
!          computational cells (which is what the user will be expecting.)
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i
      integer :: horzPhysIdUse, vertPhysIdUse
      integer :: aSize, gridRank, index
      integer, dimension(3) :: order
      logical :: reorderUse
      type(ESMF_Array) :: tempArray
      type(ESMF_Array), dimension(:), pointer :: coord, coord2
      type(ESMF_Array), dimension(:,:), pointer :: tempArray2

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! some basic error checking    TODO: more
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_LRGridGet called with invalid grid object"
        return
      endif

      ! Initialize other variables
      horzPhysIdUse = -1
      vertPhysIdUse = -1
      reorderUse    = .TRUE.
      if (present(reorder)) reorderUse = reorder

      ! Get the grid rank -- to check if there is a vertical grid available
      gridRank = grid%ptr%dimCount

      ! get physgrid identifiers from relative locations
      if (present(horzRelLoc)) then
        if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
          call ESMF_GridGetPhysGridId(grid%ptr, horzRelLoc, horzPhysIdUse, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetCoord: get PhysGrid id"
            return
          endif
        else
          print *, "ERROR in ESMF_LRGridGetCoord: undefined horizontal relloc"
          return
        endif
      endif

      if (present(vertRelLoc)) then
   !     if (gridRank.le.2) then
   !       print *, "ERROR in ESMF_LRGridGetCoord: ", &
   !                "defined vertical relloc but only a 2D grid"
   !       return
   !     endif
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. gridRank.eq.3) then
          call ESMF_GridGetPhysGridId(grid%ptr, vertRelLoc, vertPhysIdUse, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetCoord: get PhysGrid id"
            return
          endif
 !       else
 !         print *, "ERROR in ESMF_LRGridGetCoord: undefined vertical relloc"
 !         return
        endif
      endif

      ! Call PhysGridGet with valid PhysGrid
      if (present(centerCoord)) then
        index = 1
        aSize = min(gridRank, size(centerCoord))
        allocate(coord(aSize))
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index = 3
          call ESMF_PhysGridGetLocations(grid%ptr%physGrids(horzPhysIdUse), &
                                         locationArray=coord(1:2), &
                                         total=total, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetCoord: PhysGrid get locations"
            return
          endif
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          call ESMF_PhysGridGetLocations(grid%ptr%physGrids(vertPhysIdUse), &
                                         locationArray=coord(index:index), &
                                         total=total, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetCoord: PhysGrid get locations"
            return
          endif
        endif
        if (reorderUse) then
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            ! if i and j are reordered, then the coordinate data arrays need to
            ! be shuffled as well
            if (gridOrder(1,grid%ptr%coordOrder%order,2).eq.1) then
              centerCoord(order(i)) = coord(i)
            else
              call ESMF_LRGridReshape(coord(i), tempArray, status)
              centerCoord(order(i)) = tempArray
            endif
          enddo
        else
          do i = 1,aSize
            centerCoord(i) = coord(i)
          enddo
        endif
        deallocate(coord)
      endif

      if (present(cornerCoord)) then
        index = 1
        aSize = min(gridRank, size(cornerCoord))
        allocate(coord2(aSize))
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index = 3
          call ESMF_PhysGridGetRegions(grid%ptr%physGrids(horzPhysIdUse), &
                                       vertexArray=coord2(1:2), rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetCoord: PhysGrid get regions"
            return
          endif
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          call ESMF_PhysGridGetRegions(grid%ptr%physGrids(horzPhysIdUse), &
                                       vertexArray=coord2(index:index), &
                                       rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetCoord: PhysGrid get regions"
            return
          endif
        endif
        if (reorderUse) then
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            cornerCoord(order(i)) = coord2(i)
          enddo
        else
          do i = 1,aSize
            cornerCoord(i) = coord2(i)
          enddo
        endif
        deallocate(coord2)
      endif

      ! TODO: face coords

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGetCoord

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetCoordFromArray

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridGetDE - Get DE (local) information for a Grid

! !INTERFACE:
      subroutine ESMF_LRGridGetDE(grid, horzRelLoc, vertRelLoc, &
                                  myDE, localCellCount, localCellCountPerDim, &
                                  minLocalCoordPerDim, maxLocalCoordPerDim, &
                                  globalStartPerDim, globalAIPerDim, reorder, &
                                  total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer, intent(inout), optional :: myDE
      integer, intent(inout), optional :: localCellCount
      integer, dimension(:), intent(inout), optional :: localCellCountPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            minLocalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), optional :: &
                            maxLocalCoordPerDim
      integer, dimension(:), intent(inout), optional :: globalStartPerDim
      type(ESMF_AxisIndex), dimension(:), intent(inout), &
                                            optional :: globalAIPerDim
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.  Since a single
!     {\tt ESMF\_Grid} can have many {\tt ESMF\_DistGrids}, the correct
!     {\tt ESMF\_DistGrid} must be identified by this calling routine.  For a 3D
!     {\tt ESMF\_Grid}, the user must supply identifiers for both the horizontal
!     and vertical grids if querying for an array of values, like
!     localCellCountPerDim.  The {\tt ESMF\_DistGrid(s)} are identified
!     using the set of input variables:  horzRelLoc and/or vertRelLoc.
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be queried.
!     \item[{[horzRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
!     \item[{[myDE]}]
!          Identifier for this {\tt ESMF\_DE}, zero-based.
!     \item[{[localCellCount]}]
!          Local (on this {\tt ESMF\_DE}) number of cells.
!     \item[{[localCellCountPerDim]}]
!          Local (on this {\tt ESMF\_DE}) number of cells per axis.
!     \item[{[minLocalCoordPerDim]}]
!          Array of minimum local physical coordinates in each direction.
!     \item[{[maxLocalCoordPerDim]}]
!          Array of maximum local physical coordinates in each direction.
!     \item[{[globalStartPerDim]}]
!          Global index of starting counts for each dimension.
!     \item[{[globalAIPerDim]}]
!          Global axis indices for each dimension.
!     \item[{[reorder]}]
!          Logical.  If TRUE, reorder any results using the GridOrder before
!          returning.  If FALSE do not reorder.  The default value is TRUE
!          and users should not need to reset this for most applications.
!     \item[{[total]}]
!          Logical flag to indicate getting DistGrid information for total cells.
!          The default is the computational regime.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer :: aSize, gridRank, index
      integer :: horzCellCount, vertCellCount
      integer, dimension(3) :: localCellCountPerDimUse, &
                               globalStartPerDimUse, order
      logical :: reorderUse
      real(ESMF_KIND_R8), dimension(3) :: minLCPDUse, maxLCPDUse
      type(ESMF_AxisIndex), dimension(3) :: globalAIPerDimUse
      type(ESMF_PhysCoord) :: coord

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! some basic error checking    TODO: more
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_LRGridGet called with invalid grid object"
        return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1
      reorderUse    = .TRUE.
      if (present(reorder)) reorderUse = reorder

      ! Get the grid rank -- to check if there is a vertical grid available
      gridRank = grid%ptr%dimCount

      ! get physgrid and distgrid identifiers from relative locations
      if (present(horzRelLoc)) then
        if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
          call ESMF_GridGetPhysGridId(grid%ptr, horzRelLoc, horzPhysIdUse, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetCoord: get PhysGrid id"
            return
          endif
          horzDistIdUse = grid%ptr%distGridIndex(horzPhysIdUse)
        else
          print *, "ERROR in ESMF_LRGridGetCoord: undefined horizontal relloc"
          return
        endif
      endif

      if (present(vertRelLoc)) then
  !      if (gridRank.le.2) then
  !        print *, "ERROR in ESMF_LRGridGetCoord: ", &
  !                 "defined vertical relloc but only a 2D grid"
  !        return
  !      endif
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. gridRank.eq.3) then
          call ESMF_GridGetPhysGridId(grid%ptr, vertRelLoc, vertPhysIdUse, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetCoord: get PhysGrid id"
            return
          endif
          vertDistIdUse = grid%ptr%distGridIndex(vertPhysIdUse)
  !      else
  !        print *, "ERROR in ESMF_LRGridGetCoord: undefined vertical relloc"
  !        return
        endif
      endif

      ! TODO: make sure the horzDistIdUse points to a horizontal distgrid,
      !       same for vert

      ! use DELayout call instead of DistGrid to get myDE to avoid zero-based
      ! vs. 1-based issues.  note: layout the same for all distgrids, so use 1
      if (present(myDE)) &
        call ESMF_DELayoutGetDEid(grid%ptr%distgrids(1)%ptr%layout, myDE, status) 

      ! make DistGrid calls first
      ! check maximum size of array variables
      aSize = 0
      if (present(localCellCountPerDim)) &
        aSize = max(aSize, size(localCellCountPerDim))
      if (present(   globalStartPerDim)) &
        aSize = max(aSize, size(   globalStartPerDim))
      if (present(      globalAIPerDim)) &
        aSize = max(aSize, size(      globalAIPerDim))
      aSize = min(gridRank, aSize)

      ! call DistGrid method to retrieve information otherwise not available
      ! to the application level
      if (present(localCellCount)) then
        horzCellCount = 1
        vertCellCount = 1
        if (horzPhysIdUse.ne.-1) then
          call ESMF_DistGridGetDE(grid%ptr%distgrids(horzDistIdUse)%ptr, &
                                  horzCellCount, &
                                  total=total, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetDE: distgrid get de"
            return
          endif
        endif
        if (vertPhysIdUse.ne.-1) then
          call ESMF_DistGridGetDE(grid%ptr%distgrids(vertDistIdUse)%ptr, &
                                  vertCellCount, &
                                  total=total, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetDE: distgrid get de"
            return
          endif
        endif
        localCellCount = horzCellCount*vertCellCount
      endif
      if (aSize.ge.1) then
        index = 1
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index         = 3
          call ESMF_DistGridGetDE(grid%ptr%distgrids(horzDistIdUse)%ptr, &
                    localCellCountPerDim=localCellCountPerDimUse(1:2), &
                    globalStartPerDim=globalStartPerDimUse(1:2), &
                    globalAIPerDim=globalAIPerDimUse(1:2), &
                    total=total, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetDE: distgrid get de"
            return
          endif
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          call ESMF_DistGridGetDE(grid%ptr%distgrids(vertDistIdUse)%ptr, &
                    localCellCountPerDim=localCellCountPerDimUse(index:index), &
                    globalStartPerDim=globalStartPerDimUse(index:index), &
                    globalAIPerDim=globalAIPerDimUse(index:index), &
                    total=total, rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetDE: distgrid get de"
            return
          endif
        endif
        ! load local values into return arguments
        if (reorderUse) then
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            if (present(localCellCountPerDim)) &
                localCellCountPerDim(order(i)) = localCellCountPerDimUse(i)
            if (present(   globalStartPerDim)) &
                globalStartPerDim(order(i)) =    globalStartPerDimUse(i)
            if (present(      globalAIPerDim)) &
                globalAIPerDim(order(i)) =       globalAIPerDimUse(i)
          enddo
        else
          do i = 1,aSize
            if (present(localCellCountPerDim)) &
                localCellCountPerDim(i) = localCellCountPerDimUse(i)
            if (present(   globalStartPerDim)) &
                globalStartPerDim(i) =    globalStartPerDimUse(i)
            if (present(      globalAIPerDim)) &
                globalAIPerDim(i) =       globalAIPerDimUse(i)
          enddo
        endif
      endif

      ! now make PhysGrid calls
      if (present(minLocalCoordPerDim)) then
        index = 1
        aSize = min(gridRank, size(minLocalCoordPerDim))
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index = 3
          do i = 1,2
            coord = grid%ptr%physgrids(horzPhysIdUse)%ptr%coords(i)
            call ESMF_PhysCoordGetExtents(coord, minVal=minLCPDUse(i), &
                                          rc=status)
            if (status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridGetDE: physcoord get extents"
              return
            endif
          enddo
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          coord = grid%ptr%physgrids(vertPhysIdUse)%ptr%coords(1)
          call ESMF_PhysCoordGetExtents(coord, minVal=minLCPDUse(index), &
                                        rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetDE: physcoord get extents"
            return
          endif
        endif
        if (reorderUse) then
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            minLocalCoordPerDim(order(i)) = minLCPDUse(i)
          enddo
        else
          do i = 1,aSize
            minLocalCoordPerDim(i) = minLCPDUse(i)
          enddo
        endif
      endif

      if (present(maxLocalCoordPerDim)) then
        index = 1
        aSize = min(gridRank, size(maxLocalCoordPerDim))
        if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
          index = 3
          do i = 1,2
            coord = grid%ptr%physgrids(horzPhysIdUse)%ptr%coords(i)
            call ESMF_PhysCoordGetExtents(coord, maxVal=maxLCPDUse(i), &
                                          rc=status)
            if (status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridGetDE: physcoord get extents"
              return
            endif
          enddo
        endif
        if (aSize.ge.index .AND. vertPhysIdUse.ne.-1) then
          coord = grid%ptr%physgrids(vertPhysIdUse)%ptr%coords(1)
          call ESMF_PhysCoordGetExtents(coord, maxVal=maxLCPDUse(index), &
                                        rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetDE: physcoord get extents"
            return
          endif
        endif
        if (reorderUse) then
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            maxLocalCoordPerDim(order(i)) = maxLCPDUse(i)
          enddo
        else
          do i = 1,aSize
            maxLocalCoordPerDim(i) = maxLCPDUse(i)
          enddo
        endif
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGetDE

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridGetAllAxisIndex - Get all axis indices for a DistGrid

! !INTERFACE:
      subroutine ESMF_LRGridGetAllAxisIndex(grid, globalAI, horzRelLoc, &
                                            vertRelLoc, total, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: globalAI
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
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
!     \item[{[horzRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
!     \item[{[total]}]
!          Logical flag for whether the axis indices should be for total
!          cells or not.  Default is false, which infers computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i
      integer :: aSize, gridRank, index, sizeAI
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer, dimension(ESMF_MAXGRIDDIM) :: order
      type(ESMF_AxisIndex), dimension(:,:), pointer :: horzAI, vertAI

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! some basic error checking    TODO: more
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_LRGridGet called with invalid grid object"
        return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the grid rank and check against size of globalAI
      gridRank = grid%ptr%dimCount
      if (size(globalAI,2).lt.gridRank) then
        print *, "WARNING in ESMF_LRGridGetDE: ", &
                 "globalAI array size smaller than grid rank"
      endif

      ! Get the size of the AI array and allocate horz and vert temp AI arrays
      sizeAI = size(globalAI,1)
      aSize  = min(gridRank, size(globalAI,2))
      allocate(horzAI(sizeAI,2))
      allocate(vertAI(sizeAI,1))

      ! get distgrid identifiers from relative locations
      if (present(horzRelLoc)) then
        if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
          call ESMF_GridGetPhysGridId(grid%ptr, horzRelLoc, horzPhysIdUse, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetAllAxisIndex: get PhysGrid id"
            return
          endif
          horzDistIdUse = grid%ptr%distGridIndex(horzPhysIdUse)
        else
          print *, "ERROR in ESMF_LRGridGetAllAxisIndex: ", &
                   "undefined horizontal relloc"
          return
        endif
      endif

      if (present(vertRelLoc)) then
   !     if (gridRank.le.2) then
   !       print *, "ERROR in ESMF_LRGridGetAllAxisIndex: ", &
   !                "defined vertical relloc but only a 2D grid"
   !       return
   !     endif
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. gridRank.eq.3) then
          call ESMF_GridGetPhysGridId(grid%ptr, vertRelLoc, vertPhysIdUse, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetAllAxisIndex: get PhysGrid id"
            return
          endif
          vertDistIdUse = grid%ptr%distGridIndex(vertPhysIdUse)
   !     else
   !       print *, "ERROR in ESMF_LRGridGetAllAxisIndex: ", &
   !                "undefined vertical relloc"
   !       return
        endif
      endif

      ! call DistGrid method to retrieve information otherwise not available
      ! to the application level
      index = 1
      if (aSize.ge.2 .AND. horzPhysIdUse.ne.-1) then
        index = 3
        call ESMF_DistGridGetAllAxisIndex(grid%ptr%distgrids(horzDistIdUse)%ptr, &
                                          horzAI, total, rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGetAllAxisIndex: ", &
                   "distgrid get all axis index"
          return
        endif
      endif
      if (aSize.ge.index .AND. vertDistIdUse.ne.-1) then
        call ESMF_DistGridGetAllAxisIndex(grid%ptr%distgrids(vertDistIdUse)%ptr, &
                                          vertAI, total, rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGetAllAxisIndex: ", &
                   "distgrid get all axis index"
          return
        endif
      else
        if (gridRank.eq.3) then
          print *, "ERROR in ESMF_LRGridGetAllAxisIndex: ", &
                   "no valid vertRelLoc when one is needed"
          return
        endif
      endif

      ! Load temp values into input array and clean up
      order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
      if (aSize.ge.2) then
        globalAI(:,order(1)) = horzAI(:,1)
        globalAI(:,order(2)) = horzAI(:,2)
      endif
      if (aSize.ge.index) globalAI(:,order(3)) = vertAI(:,1)

      deallocate(horzAI)
      deallocate(vertAI)

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGetAllAxisIndex

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_LRGridGlobalToLocalIndex(grid, horzRelLoc, vertRelLoc, &
                                               global1D, local1D, &
                                               global2D, local2D, &
                                               globalAI1D, localAI1D, &
                                               globalAI2D, localAI2D, &
                                               dimOrder, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      integer(ESMF_KIND_I4), dimension(:), intent(in),  optional :: global1D
      integer(ESMF_KIND_I4), dimension(:), intent(out), optional :: local1D
      integer(ESMF_KIND_I4), dimension(:,:), intent(in),  optional :: global2D
      integer(ESMF_KIND_I4), dimension(:,:), intent(out), optional :: local2D
      type(ESMF_AxisIndex), dimension(:), intent(in),  optional :: globalAI1D
      type(ESMF_AxisIndex), dimension(:), intent(out), optional :: localAI1D
      type(ESMF_AxisIndex), dimension(:,:), intent(in),  optional :: globalAI2D
      type(ESMF_AxisIndex), dimension(:,:), intent(out), optional :: localAI2D
      integer, dimension(:), intent(in), optional :: dimOrder
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!     Provides access to a {\tt ESMF\_DistGrid} routine that translates an array of
!     integer cell identifiers from global indexing to local indexing
!
!     The arguments are:
!     \begin{description}
!     \item[grid]
!          Class to be used.
!     \item[horzRelLoc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
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
!EOPI

      integer :: status                              ! Error status
      logical :: rcpresent                           ! Return code present
      integer :: i
      integer :: order(3)
      integer :: gridRank, aSize, tempSize, tempSize2
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer, dimension(:), allocatable :: dimOrderUse
      integer(ESMF_KIND_I4), dimension(:,:), allocatable :: gTemp2D,   lTemp2D
      type(ESMF_AxisIndex),  dimension(:,:), allocatable :: gTempAI2D, lTempAI2D
      type(ESMF_DistGridType), pointer :: hdgtype, vdgtype

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! some basic error checking    TODO: more
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_LRGridGet called with invalid grid object"
        return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the grid rank -- to check if there is a vertical grid available
      gridRank = grid%ptr%dimCount

      ! determine the largest input array size and allocate temp arrays
      aSize = 0
      if (present(global1D)) then
        tempSize = size(global1D)
        aSize = max(aSize, tempSize)
      endif
      if (present(global2D)) then
        tempSize  = size(global2D,1)
        tempSize2 = size(global2D,2)
        aSize = max(aSize, tempSize)
        allocate(gtemp2D(tempSize,tempSize2))
        allocate(ltemp2D(tempSize,tempSize2))
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          gtemp2D(:,i) = global2D(:,order(i))
        enddo
      endif
      if (present(globalAI1D)) then
        tempSize = size(globalAI1D)
        aSize = max(aSize, tempSize)
      endif
      if (present(globalAI2D)) then
        tempSize  = size(globalAI2D,1)
        tempSize2 = size(globalAI2D,2)
        aSize = max(aSize, tempSize)
        allocate(gtempAI2D(tempSize,tempSize2))
        allocate(ltempAI2D(tempSize,tempSize2))
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          gtempAI2D(:,i) = globalAI2D(:,order(i))
        enddo
      endif
      aSize = min(gridRank, aSize)

      ! calculate default if dimOrder is not present
      allocate(dimOrderUse(aSize))
      if (present(dimOrder)) then
        dimOrderUse(:) = dimOrder(:)
      else
        do i = 1,size(dimOrderUse)
          dimOrderUse(i) = i
        enddo
      endif

      ! get distgrid identifier from relative locations
      if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
        call ESMF_GridGetPhysGridId(grid%ptr, horzRelLoc, horzPhysIdUse, status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: get PhysGrid id"
          return
        endif
        horzDistIdUse = grid%ptr%distGridIndex(horzPhysIdUse)
      else
        print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                 "undefined horizontal relloc"
        return
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. gridRank.eq.3) then
          call ESMF_GridGetPhysGridId(grid%ptr, vertRelLoc, vertPhysIdUse, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: get PhysGrid id"
            return
          endif
          vertDistIdUse = grid%ptr%distGridIndex(vertPhysIdUse)
        endif
  !    else
  !      print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
  !               "undefined vertical relloc"
  !      return
      endif

      hdgtype => grid%ptr%distgrids(horzDistIdUse)%ptr
      if (vertDistIdUse.ne.-1) then
        vdgtype => grid%ptr%distgrids(vertDistIdUse)%ptr
      else
        if (aSize.ge.3 .and. gridRank.eq.3) then
          print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                   "no valid vertRelLoc when one is needed"
          return
        endif
      endif

      ! call DistGrid method to retrieve information otherwise not available
      ! to the application level
      ! can't send parts of optional arguments, so for now break out
      if (present(global1D)) then
        if (gridRank.le.2) then
          call ESMF_DistGridGlobalToLocalIndex(hdgtype, &
                                               global1D=global1D, &
                                               local1D=local1D, &
                                               dimOrder=dimOrderUse(1:2), &
                                               rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                     "distgrid global to local"
            return
          endif
        else
          print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                   "1D operation not yet defined for 3d grids"
          return
        endif
      endif

      if (present(global2D)) then
        call ESMF_DistGridGlobalToLocalIndex(hdgtype, &
                                             global2D=gTemp2D(:,1:2), &
                                             local2D=lTemp2D(:,1:2), &
                                             dimOrder=dimOrderUse(1:2), &
                                             rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                   "distgrid global to local"
          return
        endif
        if (vertDistIdUse.ne.-1) then
          call ESMF_DistGridGlobalToLocalIndex(vdgtype, &
                                               global2D=gTemp2D(:,3:3), &
                                               local2D=lTemp2D(:,3:3), &
                                               dimOrder=dimOrderUse(3:3), &
                                               rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                     "distgrid global to local"
            return
          endif
        endif
        tempSize2 = size(global2D,2)
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          local2D(:,order(i)) = ltemp2D(:,i)
        enddo
        deallocate(gtemp2D)
        deallocate(ltemp2D)
      endif

      if (present(globalAI1D)) then
        if (gridRank.le.2) then
          call ESMF_DistGridGlobalToLocalIndex(hdgtype, &
                                               globalAI1D=globalAI1D, &
                                               localAI1D=localAI1D, &
                                               dimOrder=dimOrderUse(1:2), &
                                               rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                     "distgrid global to local"
            return
          endif
        else
          print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                   "1D operation not yet defined for 3d grids"
          return
        endif
      endif

      if (present(globalAI2D)) then
        call ESMF_DistGridGlobalToLocalIndex(hdgtype, &
                                             globalAI2D=gTempAI2D(:,1:2), &
                                             localAI2D=lTempAI2D(:,1:2), &
                                             dimOrder=dimOrderUse(1:2), &
                                             rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                   "distgrid global to local"
          return
        endif
        if (vertDistIdUse.ne.-1) then
          call ESMF_DistGridGlobalToLocalIndex(vdgtype, &
                                               globalAI2D=gTempAI2D(:,3:3), &
                                               localAI2D=lTempAI2D(:,3:3), &
                                               dimOrder=dimOrderUse(3:3), &
                                               rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                     "distgrid global to local"
            return
          endif
        endif
        tempSize2 = size(globalAI2D,2)
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          localAI2D(:,order(i)) = ltempAI2D(:,i)
        enddo
        deallocate(gtempAI2D)
        deallocate(ltempAI2D)
      endif

      deallocate(dimOrderUse)

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGlobalToLocalIndex

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridLocalToGlobalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_LRGridLocalToGlobalIndex(grid, horzRelLoc, vertRelLoc, &
                                               local1D, global1D, &
                                               local2D, global2D, &
                                               localAI1D, globalAI1D, &
                                               localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_RelLoc), intent(in) :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
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
!     \item[horzRelLoc]
!          {\tt ESMF\_RelLoc} identifier corresponding to the horizontal
!          grid.
!     \item[{[vertRelLoc]}]
!          {\tt ESMF\_RelLoc} identifier corresponding to the vertical
!          grid.
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
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i
      integer :: order(3)
      integer :: gridRank, aSize, tempSize, tempSize2
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer(ESMF_KIND_I4), dimension(:,:), allocatable :: gTemp2D,   lTemp2D
      type(ESMF_AxisIndex),  dimension(:,:), allocatable :: gTempAI2D, lTempAI2D
      type(ESMF_DistGridType), pointer :: hdgtype, vdgtype

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      ! some basic error checking    TODO: more
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_LRGridGet called with invalid grid object"
        return
      endif

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the grid rank -- to check if there is a vertical grid available
      gridRank = grid%ptr%dimCount

      ! determine the largest input array size and allocate temp arrays
      aSize = 0
      if (present(local1D)) then
        tempSize = size(local1D)
        aSize = max(aSize, tempSize)
      endif
      if (present(local2D)) then
        tempSize  = size(local2D,1)
        tempSize2 = size(local2D,2)
        aSize = max(aSize, tempSize)
        allocate(gtemp2D(tempSize,tempSize2))
        allocate(ltemp2D(tempSize,tempSize2))
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          ltemp2D(:,i) = local2D(:,order(i))
        enddo
      endif
      if (present(localAI1D)) then
        tempSize = size(localAI1D)
        aSize = max(aSize, tempSize)
      endif
      if (present(localAI2D)) then
        tempSize  = size(localAI2D,1)
        tempSize2 = size(localAI2D,2)
        aSize = max(aSize, tempSize)
        allocate(gtempAI2D(tempSize,tempSize2))
        allocate(ltempAI2D(tempSize,tempSize2))
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          ltempAI2D(:,i) = localAI2D(:,order(i))
        enddo
      endif
      aSize = min(gridRank, aSize)

      ! get distgrid identifiers from relative locations
      if (horzRelLoc.ne.ESMF_CELL_UNDEFINED) then
        call ESMF_GridGetPhysGridId(grid%ptr, horzRelLoc, horzPhysIdUse, status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: get PhysGrid id"
          return
        endif
        horzDistIdUse = grid%ptr%distGridIndex(horzPhysIdUse)
      else
        print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                 "undefined horizontal relloc"
        return
      endif

      if (present(vertRelLoc)) then
        if (vertRelLoc.ne.ESMF_CELL_UNDEFINED .AND. gridRank.eq.3) then
          call ESMF_GridGetPhysGridId(grid%ptr, vertRelLoc, vertPhysIdUse, status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: get PhysGrid id"
            return
          endif
          vertDistIdUse = grid%ptr%distGridIndex(vertPhysIdUse)
        endif
      endif

      hdgtype => grid%ptr%distgrids(horzDistIdUse)%ptr
      if (vertDistIdUse.ne.-1) then
        vdgtype => grid%ptr%distgrids(vertDistIdUse)%ptr
      else
        if (aSize.ge.3 .and. gridRank.eq.3) then
          print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                   "no valid vertRelLoc when one is needed"
          return
        endif
      endif

      ! call DistGrid method to retrieve information otherwise not available
      ! to the application level
      ! can't send parts of optional arguments, so for now break out  TODO: fix
      if (present(local1D)) then
        if (gridRank.le.2) then
          call ESMF_DistGridLocalToGlobalIndex(hdgtype, &
                                               local1D=local1D, &
                                               global1D=global1D, &
                                               rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                     "distgrid global to local"
            return
          endif
        else
          print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                   "1D operation not yet defined for 3d grids"
          return
        endif
      endif

      if (present(local2D)) then
        call ESMF_DistGridLocalToGlobalIndex(hdgtype, &
                                             local2D=lTemp2D(:,1:2), &
                                             global2D=gTemp2D(:,1:2), &
                                             rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                   "distgrid global to local"
          return
        endif
        if (vertDistIdUse.ne.-1) then
          call ESMF_DistGridLocalToGlobalIndex(vdgtype, &
                                               local2D=lTemp2D(:,3:3), &
                                               global2D=gTemp2D(:,3:3), &
                                               rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                     "distgrid global to local"
            return
          endif
        endif
        tempSize2 = size(local2D,2)
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          global2D(:,order(i)) = gtemp2D(:,i)
        enddo
        deallocate(gtemp2D)
        deallocate(ltemp2D)
      endif

      if (present(localAI1D)) then
        if (gridRank.le.2) then
          call ESMF_DistGridLocalToGlobalIndex(hdgtype, &
                                               localAI1D=localAI1D, &
                                               globalAI1D=globalAI1D, &
                                               rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                     "distgrid global to local"
            return
          endif
        else
          print *, "ERROR in ESMF_LRGridGlobalToLocalIndex: ", &
                   "1D operation not yet defined for 3d grids"
          return
        endif
      endif

      if (present(localAI2D)) then
        call ESMF_DistGridLocalToGlobalIndex(hdgtype, &
                                             localAI2D=lTempAI2D(:,1:2), &
                                             globalAI2D=gTempAI2D(:,1:2), &
                                             rc=status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                   "distgrid global to local"
          return
        endif
        if (vertDistIdUse.ne.-1) then
          call ESMF_DistGridLocalToGlobalIndex(vdgtype, &
                                               localAI2D=lTempAI2D(:,3:3), &
                                               globalAI2D=gTempAI2D(:,3:3), &
                                               rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridLocalToGlobalIndex: ", &
                     "distgrid global to local"
            return
          endif
        endif
        tempSize2 = size(localAI2D,2)
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,tempSize2)
        do i = 1,tempSize2
          globalAI2D(:,order(i)) = gtempAI2D(:,i)
        enddo
        deallocate(gtempAI2D)
        deallocate(ltempAI2D)
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridLocalToGlobalIndex

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetCoordFromBuffer

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridSetCoordCompute - Compute coordinates for a Grid

! !INTERFACE:
      subroutine ESMF_LRGridSetCoordCompute(grid, physGridId, dimCount, counts, &
                                      gridBoundWidth, relloc, coord1, coord2, &
                                      total, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
      integer, intent(in) :: physGridId
      integer, intent(in) :: dimCount
      integer, dimension(dimCount), intent(in) :: counts
      integer, intent(in) :: gridBoundWidth
      type(ESMF_RelLoc), intent(in) :: relloc
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in), optional :: coord2
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
!     \item[dimCount]
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
!     \item[{[coord2]}]
!          Array of specified grid coordinates in the second dimension.
!     \item[{[total]}]
!          Logical flag to optionally set physical coordinate arrays of total cells.
!          Default is to set computational cells.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI

      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present
      integer :: i, j, i1, j1
      real(ESMF_KIND_R8) :: coordUse1, coordUse2
      real(ESMF_KIND_R8), dimension(:), pointer :: temp
      real(ESMF_KIND_R8), dimension(:,:), pointer :: temp1, temp2
      type(ESMF_Array), dimension(:), pointer :: arrayTemp
      type(ESMF_DataKind) :: kind
      type(ESMF_DataType) :: type

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: could be a 1-D array for each coord axis later, but that
      !       would have to be supported by Regrid first

      ! allocate arrays
      allocate(arrayTemp(dimCount))

      ! create ESMF_Arrays
      kind = ESMF_R8
      type = ESMF_DATA_REAL
      do i = 1,dimCount
        arrayTemp(i) = ESMF_ArrayCreate(dimCount, type, kind, counts, &
                                        haloWidth=gridBoundWidth, rc=status)
      enddo

      select case (dimCount)
      case(1)   ! 1D coordinates, assumed mostly for vertical grids

        ! get data
        call ESMF_ArrayGetData(arrayTemp(1), temp, ESMF_DATA_REF, status)

        ! For now, an if construct for the different relative locations
        if (relloc .eq. ESMF_CELL_UNDEFINED) then
          status = ESMF_FAILURE

        elseif (relloc.eq.ESMF_CELL_CENTER .or. relloc.eq.ESMF_CELL_CELL) then  ! TODO:?
          do i = 1,counts(1)
            temp(i) = 0.5d0*(coord1(i)+coord1(i+1))
          enddo

        elseif (relloc .eq. ESMF_CELL_TOPFACE) then   ! TODO: check bottom or top
          do i = 1,counts(1)
            temp(i) = coord1(i+1)
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

        ! For now, an if construct for the different relative locations
        ! TODO: also set corners and faces
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

        elseif (relloc .eq. ESMF_CELL_SWCORNER) then
          do i = 1,counts(1)
            coordUse1 = coord1(i)
            do j = 1,counts(2)
              coordUse2 = coord2(j)
              temp1(i,j) = coordUse1 
              temp2(i,j) = coordUse2
            enddo
          enddo

        elseif (relloc .eq. ESMF_CELL_SECORNER) then
          do i = 1,counts(1)
            coordUse1 = coord1(i+1)
            do j = 1,counts(2)
              coordUse2 = coord2(j)
              temp1(i,j) = coordUse1 
              temp2(i,j) = coordUse2
            enddo
          enddo

        elseif (relloc .eq. ESMF_CELL_NWCORNER) then
          do i = 1,counts(1)
            coordUse1 = coord1(i)
            do j = 1,counts(2)
              coordUse2 = coord2(j+1)
              temp1(i,j) = coordUse1 
              temp2(i,j) = coordUse2
            enddo
          enddo

        else
          print *, "This relative location not yet supported in ", &
                   "LRGridSetCoordCompute"
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
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridSetCoordCompute: PhysGrid set locations"
        return
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridSetCoordCompute

!------------------------------------------------------------------------------
!BOPI
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
!     This version of set copies the coordinates of a {\tt ESMF\_Grid} from
!     another Grid.
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetCoordCopy

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridGet - Gets a variety of information about the grid

! !INTERFACE:
      subroutine ESMF_LRGridGet(grid, horzRelLoc, vertRelLoc, &
                                horzGridType, vertGridType, &
                                horzStagger, vertStagger, &
                                horzCoordSystem, vertCoordSystem, &
                                coordOrder, dimCount, minGlobalCoordPerDim, &
                                maxGlobalCoordPerDim, globalCellCountPerDim, &
                                globalStartPerDEPerDim, maxLocalCellCountPerDim, &
                                cellCountPerDEPerDim, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_RelLoc), intent(in), optional :: horzRelLoc
      type(ESMF_RelLoc), intent(in), optional :: vertRelLoc
      type(ESMF_GridType), intent(out), optional :: horzGridType
      type(ESMF_GridType), intent(out), optional :: vertGridType
      type(ESMF_GridStagger), intent(out), optional :: horzStagger
      type(ESMF_GridStagger), intent(out), optional :: vertStagger
      type(ESMF_CoordSystem), intent(out), optional :: horzCoordSystem
      type(ESMF_CoordSystem), intent(out), optional :: vertCoordSystem
      type(ESMF_CoordOrder),  intent(out), optional :: coordOrder
      integer, intent(out), optional :: dimCount
      real(ESMF_KIND_R8), intent(out), dimension(:), &
                            optional :: minGlobalCoordPerDim
      real(ESMF_KIND_R8), intent(out), dimension(:), &
                            optional :: maxGlobalCoordPerDim
      integer, intent(out), dimension(:), optional :: globalCellCountPerDim
      integer, intent(out), dimension(:,:), optional :: globalStartPerDEPerDim
      integer, intent(out), dimension(:), optional :: maxLocalCellCountPerDim
      integer, intent(out), dimension(:,:), optional :: cellCountPerDEPerDim
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
!     \item[{[horzGridType]}]
!          Integer specifier to denote horizontal grid type.
!     \item[{[vertGridType]}]
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
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i, aSize, gridRank
      integer :: horzDistIdUse, vertDistIdUse
      integer :: horzPhysIdUse, vertPhysIdUse
      integer, dimension(ESMF_MAXGRIDDIM) :: order
      integer, dimension(:), allocatable :: gCCPDUse, mLCCPDUse
      integer, dimension(:,:), allocatable :: gSPDEPDUse, cCPDEPDUse
      type(ESMF_GridClass), pointer :: gridp

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
 
      ! some basic error checking    TODO: more
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_LRGridGet called with invalid grid object"
        return
      endif

      gridp => grid%ptr

      ! Initialize other variables
      horzDistIdUse = -1
      vertDistIdUse = -1
      horzPhysIdUse = -1
      vertPhysIdUse = -1

      ! Get the grid rank -- to check if there is a vertical grid available
      gridRank = gridp%dimCount

      ! if present, gets information from the grid derived type
      if (present(horzGridType   )) horzGridType    = gridp%horzGridType
      if (present(vertGridType   )) vertGridType    = gridp%vertGridType
      if (present(horzStagger    )) horzStagger     = gridp%horzStagger
      if (present(vertStagger    )) vertStagger     = gridp%vertStagger
      if (present(horzCoordSystem)) horzCoordSystem = gridp%horzCoordSystem
      if (present(vertCoordSystem)) vertCoordSystem = gridp%vertCoordSystem
      if (present(coordOrder     )) coordOrder      = gridp%coordOrder
      if (present(dimCount       )) dimCount        = gridp%dimCount

      ! get name from base obj
      if (present(name)) then
        call ESMF_GetName(gridp%base, name, status)
        if (status .ne. ESMF_SUCCESS) then
           print *, "ERROR in ESMF_GridGetName"
           return
        endif
      endif

      ! Get global coordinate extents
      if (present(minGlobalCoordPerDim)) then
        aSize = min(gridRank, size(minGlobalCoordPerDim))
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
        do i=1,aSize
          minGlobalCoordPerDim(order(i)) = gridp%minGlobalCoordPerDim(i)
        enddo
      endif
      if (present(maxGlobalCoordPerDim)) then
        aSize = min(gridRank, size(maxGlobalCoordPerDim))
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
        do i=1,aSize
          maxGlobalCoordPerDim(order(i)) = gridp%maxGlobalCoordPerDim(i)
        enddo
      endif

      ! get the periodicity
      if (present(periodic)) then
        aSize = min(gridRank, size(periodic))
        order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
        do i=1,aSize
          periodic(order(i)) = gridp%periodic(i)
        enddo
      endif

      ! if DistGrid info is being queried, make sure there is a valid distGridId
      if (present(globalCellCountPerDim)   .or. &
          present(globalStartPerDEPerDim)  .or. &
          present(maxLocalCellCountPerDim) .or. &
          present(cellCountPerDEPerDim)) then

        ! determine the largest input array size
        aSize = 0
        if (present(globalCellCountPerDim)) &
            aSize = max(aSize, size(globalCellCountPerDim))
        if (present(maxLocalCellCountPerDim)) &
            aSize = max(aSize, size(maxLocalCellCountPerDim))
        if (present(globalStartPerDEPerDim)) &
            aSize = max(aSize, size(globalStartPerDEPerDim,2))
        if (present(cellCountPerDEPerDim)) &
            aSize = max(aSize, size(cellCountPerDEPerDim,2))
        aSize = min(gridRank, aSize)

        ! get distgrid identifiers from relative locations
        if (.not.(present(horzRelLoc))) then
          print *, "ERROR in ESMF_LRGridGetDE: ", &
                   "no valid horizontal relative location"
          return
        endif
        call ESMF_GridGetPhysGridId(grid%ptr, horzRelLoc, horzPhysIdUse, &
                                    status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGetDE: get horizontal PhysGrid id"
          return
        endif
        horzDistIdUse = grid%ptr%distGridIndex(horzPhysIdUse)

        if (aSize.ge.3) then
          if (.not.(present(vertRelLoc))) then
            print *, "ERROR in ESMF_LRGridGetDE: ", &
                     "no valid vertical DistGrid identifier"
            return
          endif
          call ESMF_GridGetPhysGridId(grid%ptr, vertRelLoc, vertPhysIdUse, &
                                      status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGetDE: get vertical PhysGrid id"
            return
          endif
          vertDistIdUse = grid%ptr%distGridIndex(vertPhysIdUse)
        endif

        ! Get distgrid info with global coordinate counts
        if (present(globalCellCountPerDim)) then
          aSize = min(gridRank, size(globalCellCountPerDim))
          allocate(gCCPDUse(aSize))
          call ESMF_DistGridGet(gridp%distgrids(horzDistIdUse), &
                                globalCellCountPerDim=gCCPDUse(1:2), &
                                rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGet: DistGrid get"
            return
          endif
          if (aSize.ge.3) then
            call ESMF_DistGridGet(gridp%distgrids(vertDistIdUse), &
                                  globalCellCountPerDim=gCCPDUse(3:3), &
                                  rc=status)
            if (status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridGet: DistGrid get"
              return
            endif
          endif
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            globalCellCountPerDim(order(i)) = gCCPDUse(i)
          enddo
          deallocate(gCCPDUse)
        endif

        if (present(globalStartPerDEPerDim)) then
          aSize = min(gridRank, size(globalStartPerDEPerDim,2))
          allocate(gSPDEPDUse(size(globalStartPerDEPerDim,1), aSize))
          call ESMF_DistGridGet(gridp%distgrids(horzDistIdUse), &
                                globalStartPerDEPerDim=gSPDEPDUse(:,1:2), &
                                rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGet: DistGrid get"
            return
          endif
          if (aSize.ge.3) then
            call ESMF_DistGridGet(gridp%distgrids(vertDistIdUse), &
                                  globalStartPerDEPerDim=gSPDEPDUse(:,3:3), &
                                  rc=status)
            if (status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridGet: DistGrid get"
              return
            endif
          endif
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            globalStartPerDEPerDim(:,order(i)) = gSPDEPDUse(:,i)
          enddo
          deallocate(gSPDEPDUse)
        endif

        if (present(maxLocalCellCountPerDim)) then
          aSize = min(gridRank, size(maxLocalCellCountPerDim))
          allocate(mLCCPDUse(aSize))
          call ESMF_DistGridGet(gridp%distgrids(horzDistIdUse), &
                                maxLocalCellCountPerDim=mLCCPDUse(1:2), &
                                rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGet: DistGrid get"
            return
          endif
          if (aSize.ge.3) then
            call ESMF_DistGridGet(gridp%distgrids(vertDistIdUse), &
                                  maxLocalCellCountPerDim=mLCCPDUse(3:3), &
                                  rc=status)
            if (status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridGet: DistGrid get"
              return
            endif
          endif
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            maxLocalCellCountPerDim(order(i)) = mLCCPDUse(i)
          enddo
          deallocate(mLCCPDUse)
        endif

        if (present(cellCountPerDEPerDim)) then
          aSize = min(gridRank, size(cellCountPerDEPerDim,2))
          allocate(cCPDEPDUse(size(cellCountPerDEPerDim,1), aSize))
          call ESMF_DistGridGetAllCounts(gridp%distgrids(horzDistIdUse)%ptr, &
                                         cCPDEPDUse(:,1:2), rc=status)
          if (status .NE. ESMF_SUCCESS) then
            print *, "ERROR in ESMF_LRGridGet: DistGrid get all counts"
            return
          endif
          if (aSize.ge.3) then
            call ESMF_DistGridGetAllCounts(gridp%distgrids(vertDistIdUse)%ptr, &
                                           cCPDEPDUse(:,3:3), rc=status)
            if (status .NE. ESMF_SUCCESS) then
              print *, "ERROR in ESMF_LRGridGet: DistGrid get all counts"
              return
            endif
          endif
          order(:) = gridOrder(:,grid%ptr%coordOrder%order,aSize)
          do i = 1,aSize
            cellCountPerDEPerDim(:,order(i)) = cCPDEPDUse(:,i)
          enddo
          deallocate(cCPDEPDUse)
        endif
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGet

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridSet - Sets a variety of information about the grid

! !INTERFACE:
      subroutine ESMF_LRGridSet(grid, horzGridType, vertGridType, &
                              horzStagger, vertStagger, &
                              horzCoordSystem, vertCoordSystem, &
                              coordOrder, minGlobalCoordPerDim, &
                              maxGlobalCoordPerDim, periodic, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
      type(ESMF_GridType), intent(in), optional :: horzGridType
      type(ESMF_GridType), intent(in), optional :: vertGridType
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
!     \item[{[horzGridType]}]
!          Integer specifier to denote horizontal grid type.
!     \item[{[vertGridType]}]
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
!EOPI

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! loop index

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! if present, set information filling in grid derived type
      if (present(horzGridType)) grid%horzGridType = horzGridType
      if (present(vertGridType)) grid%vertGridType = vertGridType
      if (present(horzStagger)) grid%horzStagger = horzStagger
      if (present(vertStagger)) grid%vertStagger = vertStagger
      if (present(horzCoordSystem)) grid%horzCoordSystem = horzCoordSystem
      if (present(vertCoordSystem)) grid%vertCoordSystem = vertCoordSystem
      if (present(coordOrder)) grid%coordOrder = coordOrder
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

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridSet

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridGetCellMask - Retrieves cell identifier mask for a Grid

! !INTERFACE:
      subroutine ESMF_LRGridGetCellMask(grid, maskArray, relloc, rc)
!
! !ARGUMENTS:
      type(ESMF_Grid) :: grid
      type(ESMF_Array), intent(inout) :: maskArray
      type(ESMF_RelLoc), intent(in) :: relloc
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
!     \item[relloc]
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

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! some basic error checking    TODO: more
      if (.not.associated(grid%ptr)) then
        print *, "ERROR: ESMF_LRGridGetCellMask called with invalid grid object"
        return
      endif

      ! Initialize other variables
      physIdUse = -1

      if (relloc.ne.ESMF_CELL_UNDEFINED) then
        call ESMF_GridGetPhysGridId(grid%ptr, relloc, physIdUse, status)
        if (status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_LRGridGetCellMask: get PhysGrid id"
          return
        endif
      else
        print *, "ERROR in ESMF_LRGridGetCellMask: undefined horizontal relloc"
        return
      endif
      if (physIdUse.eq.-1) then
        print *, "ERROR in ESMF_LRGridGetCellMask: ", &
                 "no PhysGrid corresponding to relloc"
        return
      endif

      ! call PhysGrid with the valid Id
      call ESMF_PhysGridGetMask(grid%ptr%physGrids(physIdUse), maskArray, id=1, &
                                rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridGetCellMask: PhysGrid get mask"
        return
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridGetCellMask

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridSetCellMask - Compute cell identifier mask for a Grid

! !INTERFACE:
      subroutine ESMF_LRGridSetCellMask(grid, physGridId, dimCount, counts, &
                                        gridBoundWidth, relloc, cellType1, &
                                        cellType2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass) :: grid
      integer, intent(in) :: physGridId
      integer, intent(in) :: dimCount
      integer, dimension(dimCount), intent(in) :: counts
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
!     \item[dimCount]
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

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: different subroutines for different dimCount?  or case?

      ! create ESMF_Array
      kind = ESMF_I4
      type = ESMF_DATA_INTEGER
      arrayTemp = ESMF_ArrayCreate(dimCount, type, kind, counts, &
                                   haloWidth=gridBoundWidth, rc=status)
      call ESMF_ArrayGetData(arrayTemp, temp, ESMF_DATA_REF, status)

      ! TODO: should this be different for different relative locations?
      iMax1 = counts(1) - gridBoundWidth + 1
      jMax1 = counts(2) - gridBoundWidth + 1
      do i = 1,counts(1)
        iType = cellType1(i)
        do j = 1,counts(2)
          jType = cellType2(j)
          ! default is computational
          temp(i,j) = 0
          ! identify ghost cells
          if (iType.eq.1 .or. jType.eq.1) then
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
                                maskType=ESMF_GridMaskType_RegionId, &
                                name=name, rc=status)
      if (status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_LRGridSetCellMask: PhysGrid set mask"
        return
      endif

      if (rcpresent) rc = ESMF_SUCCESS

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
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetMaskFromBuffer

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetMaskFromMask

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetMaskCopy

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetMetricFromArray

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetMetricFromBuffer

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetMetricCompute

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

!
!  code goes here
!
      end subroutine ESMF_LRGridSetMetricCopy

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridSetBoundingBoxes - Set the array of bounding boxes per DE

! !INTERFACE:
      subroutine ESMF_LRGridSetBoundingBoxes(grid, dimCount, coord1, &
                                             coord2, countsPerDEDim1, &
                                             countsPerDEDim2, rc)
!
! !ARGUMENTS:
      type(ESMF_GridClass), intent(inout) :: grid
      integer, intent(in) :: dimCount
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord1
      real(ESMF_KIND_R8), dimension(:), intent(in) :: coord2
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
!     \item[dimCount]
!          Number of grid dimensions (directions).
!     \item[coord1]
!          Array of physical coordinates in the first direction.
!     \item[coord2]
!          Array of physical coordinates in the second direction.
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

      ! Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      numDE1 = size(countsPerDEDim1)
      numDE2 = size(countsPerDEDim2)
      numDEs = numDE1*numDE2
      npts   = 2**dimCount

      ! TODO: break out by rank?
      ! Assume the following starage for bounding boxes:
      !   number of DEs * npts * dimCount
      !   where npts is the number of points necessary to describe a bounding box
      !   and rank is the number of dimensions.  For the time being, the points
      !   are stored in the following order:
      !                 1. (Xmin,Ymin,Zmin)
      !                 2. (Xmax,Ymin,Zmin)
      !                 3. (Xmax,Ymax,Zmin)
      !                 4. (Xmin,Ymax,Zmin)
      !                 5. (Xmin,Ymin,Zmax)
      !                 6. (Xmax,Ymin,Zmax)
      !                 7. (Xmax,Ymax,Zmax)
      !                 8. (Xmin,Ymax,Zmax)
      allocate(boxes(numDEs,npts,dimCount), stat=status)
      if (status .ne. 0) then
         print *, "allocation error, boxes(nDE,npt,dimCount) = ", numDEs,npts, &
                   dimCount
         return
      endif

      ! Calculate box for each DE
      ! Direction 1 first
      start = 0.0
      stop  = 0.0
      i1    = 0
      do j = 1,numDE1
        start = minval(coord1(i1+1:i1+countsPerDEDim1(j)))
        stop  = maxval(coord1(i1+1:i1+countsPerDEDim1(j)))
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

      ! Direction 2 next
      start = 0.0
      stop  = 0.0
      i1    = 0
      do j = 1,numDE2
        start = minval(coord2(i1+1:i1+countsPerDEDim2(j)))
        stop  = maxval(coord2(i1+1:i1+countsPerDEDim2(j)))
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

      grid%boundingBoxes = ESMF_LocalArrayCreate(boxes, ESMF_DATA_COPY, status)

      ! Clean up
      deallocate(boxes)

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridSetBoundingBoxes

!------------------------------------------------------------------------------
!BOPI
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
!EOPI

      character(len=ESMF_MAXSTR) :: name, str
      type(ESMF_GridClass), pointer :: gp
      integer :: status

      if (present(rc)) rc = ESMF_FAILURE

      if (.not. associated(grid%ptr)) then
        print *, "Empty or Uninitialized Grid"
        return
      endif

      gp => grid%ptr
      if (gp%gridStatus /= ESMF_GridStatus_Ready) then
        return
      endif

      call ESMF_GetName(gp%base, name, status)
      if (status .NE. ESMF_SUCCESS) then
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
      if (present(rc)) then
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
      call ESMF_LRGridGetAllAxisIndex(grid, grid_ai, &
                                      horzRelLoc=ESMF_CELL_CENTER, total=total, &
                                      rc=status)

      ! translate the AIs from global to local
      call ESMF_LRGridGlobalToLocalIndex(grid, horzRelLoc=ESMF_CELL_CENTER, &
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

      if (rcpresent) rc = ESMF_SUCCESS

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
      if (present(rc)) then
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
      call ESMF_LRGridGlobalToLocalIndex(srcGrid, horzRelLoc=ESMF_CELL_CENTER, &
                                         globalAI1D=myAI, localAI1D=myLocalAI, &
                                         rc=status)

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

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridBoxIntersectSend

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LRGridReshape - Switch the dimensions of the data of an Array

 !INTERFACE:
      subroutine ESMF_LRGridReshape(array1, array2, rc)
!
! !ARGUMENTS:

      type(ESMF_Array), intent(in)  :: array1   ! source array
      type(ESMF_Array), intent(out) :: array2   ! dest array
      integer, intent(out), optional :: rc  ! return code

! !DESCRIPTION:
!     This routine takes the data from one {\tt ESMF\_Array} and reorders it,
!     switching ranks, to create a destination Array.
!
!     The arguments are:
!     \begin{description}
!     \item[array1]
!          Source {\tt ESMF\_Array}.
!     \item[array2]
!          Destination {\tt ESMF\_Array}.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i, i1, j, j1
      real(ESMF_KIND_R8), dimension(:,:), pointer :: temp1, temp2

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! get data in source array
      call ESMF_ArrayGetData(array1, temp1, rc=status)
      i1 = size(temp1,1)
      j1 = size(temp1,2)

      ! allocate data for destination array
      allocate(temp2(j1,i1))

      ! transfer data
      do j = 1,j1
        do i = 1,i1
          temp2(j,i) = temp1(i,j)
        enddo
      enddo
 
      ! make destination array from data
      array2 = ESMF_ArrayCreate(temp2, ESMF_DATA_COPY, rc=status)

      deallocate(temp2)

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LRGridReshape

!------------------------------------------------------------------------------
!!BOPI
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
!!EOPI
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
!      call ESMF_LRGridGet(searchGrid, horzGridType = searchGridType)
!!
!!     Call appropriate search routine based on coordinate system and
!!     grid type.
!!
! 
!      select case (srchGridType)
!      case(ESMF_GridType_LatLon,      ESMF_GridType_LatLonMercator, &
!           ESMF_GridType_LatLonGauss, ESMF_GridType_Reduced)
!         !*** simple search adequate for these cases
!         call ESMF_PhysGridSearchBboxSpherical(dstAdd, x, y, DEId, physGrid, &
!                                               distGrid, status)
!
!      case(ESMF_GridType_Dipole,   ESMF_GridType_Tripole, &
!           ESMF_GridType_Geodesic, ESMF_GridType_CubedSphere)
!         !*** must use more general algorithm for these cases
!         call ESMF_PhysGridSearchGeneralSpherical(dstAdd, x, y, DEId, physGrid, &
!                                                  distGrid, status)
!
!      case(ESMF_GridType_XY, ESMF_GridType_XYVar)
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
