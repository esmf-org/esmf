! $Id: ESMF_ArrayComm.F90,v 1.26 2004/03/18 22:39:38 cdeluca Exp $
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
!     ESMF Array Comm module
      module ESMF_ArrayCommMod
!
!==============================================================================
!
! This file contains the Array methods which do communication, and so
! must be compiled after the comm routines.  These are logically part of
! the Array class.
!
!------------------------------------------------------------------------------
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_ArrayMod - Manage data arrays uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt Array} class and 
!  associated functions and subroutines.  
!
! C and C++ arrays are simple pointers to memory.
! Fortran arrays contain shape and stride definitions and are strongly
! typed.  To enable interoperability between the languages the C++ code
! must be able to obtain this information from the Fortran description
! (which is called the "dope vector" in Fortran), either through a priori
! knowledge or through query.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_LocalArrayMod
      use ESMF_DataMapMod
      use ESMF_DELayoutMod
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_RHandleMod
      use ESMF_RouteMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArrayGetAllAxisIndices

      public ESMF_ArrayHaloStore, ESMF_ArrayHalo, ESMF_ArrayHaloRelease
      public ESMF_ArrayRedistStore, ESMF_ArrayRedist, ESMF_ArrayRedistRelease
      ! Regrid methods are in ESMF_Regrid.F90

      public ESMF_ArrayAllGather, ESMF_ArrayGather, ESMF_ArrayScatter
      !!public ESMF_ArrayReduce, ESMF_ArrayAllReduce
      !!public ESMF_ArrayBroadcast, ESMF_ArrayAlltoAll

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_ArrayComm.F90,v 1.26 2004/03/18 22:39:38 cdeluca Exp $'
!
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_ArrayHalo - halo a distributed array
!
! !INTERFACE:
      interface ESMF_ArrayHalo

! !PRIVATE MEMBER FUNCTIONS:
          module procedure ESMF_ArrayHaloNew
          module procedure ESMF_ArrayHaloDeprecated

! !DESCRIPTION:
!     This interface provides both the revised entry point for
!      calling Halo on an {\tt ESMF\_Array} object, and temporarily
!      for backwards compatibility an older interface into the same code.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayRedist - redistribute a distributed array
!
! !INTERFACE:
      interface ESMF_ArrayRedist

! !PRIVATE MEMBER FUNCTIONS:
          module procedure ESMF_ArrayRedistNew
          module procedure ESMF_ArrayRedistDeprecated

! !DESCRIPTION:
!     This interface provides both the revised entry point for
!      calling Redistribute on an {\tt ESMF\_Array} object, and temporarily
!      for backwards compatibility an older interface into the same code.

!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayAllGather - gather a distributed array to all DEs
!
! !INTERFACE:
      interface ESMF_ArrayAllGather

! !PRIVATE MEMBER FUNCTIONS:
          module procedure ESMF_ArrayAllGatherGrid
          module procedure ESMF_ArrayAllGatherList

! !DESCRIPTION:
!     This interface provides both the revised entry point for
!      calling Redistribute on an {\tt ESMF\_Array} object, and temporarily
!      for backwards compatibility an older interface into the same code.

!EOPI
      end interface

!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayAllGatherGrid - gather a distributed Array
!
! !INTERFACE:
      subroutine ESMF_ArrayAllGatherGrid(array, grid, datamap, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_DataMap), intent(in) :: datamap
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed Array into a global Array on all DEs.
!
!
!EOP
! !REQUIREMENTS:
      integer :: status         ! local error status
      logical :: rcpresent      ! did user specify rc?
      integer :: gridrank, datarank
      integer :: i, j, nDEs
      type(ESMF_DELayout) :: layout
      integer, dimension(ESMF_MAXDIM) :: decompids
      integer, dimension(:,:), pointer :: localAxisLengths, tempCCPDEPD
      integer, dimension(ESMF_MAXDIM) :: dimorder, dimlengths
      integer, dimension(ESMF_MAXGRIDDIM) :: decomps
      integer, dimension(ESMF_MAXDIM) :: localMaxDimCount, globalCellDim
      integer, dimension(:), allocatable :: tempMLCCPD, tempGCCPD

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif
 
! extract necessary information from the grid
      call ESMF_GridGet(grid, numDims=gridrank, rc=status)
      call ESMF_GridGetDELayout(grid, layout, status)
      call ESMF_DELayoutGetNumDEs(layout, nDEs, status)
      allocate(localAxisLengths(nDEs,ESMF_MAXDIM), stat=status)
      allocate( tempMLCCPD(     gridrank), stat=status)
      allocate(  tempGCCPD(     gridrank), stat=status)
      allocate(tempCCPDEPD(nDEs,gridrank), stat=status)

      call ESMF_GridGet(grid, globalCellCountPerDim=tempGCCPD, &
                        cellCountPerDEPerDim=tempCCPDEPD, &
                        maxLocalCellCountPerDim=tempMLCCPD, rc=rc)
!     call ESMF_GridGet(grid, decomps, rc=status)   !TODO: add decomps
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayAllGatherGrid: GridGet returned failure"
        return
      endif
      decomps(1) = 1    ! TODO: remove this once the grid call is created
      decomps(2) = 2

! get the Array sizes
      call ESMF_ArrayGet(array, rank=datarank, counts=dimlengths, &
                         rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayAllGatherGrid: ArrayGet returned failure"
        return
      endif

! Query the datamap and set info for grid so it knows how to match up the
! array indices and the grid indices.
      call ESMF_DataMapGet(datamap, dataIorder=dimorder, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayAllGatherGrid: DataMapGet returned failure"
        return
      endif

! calculate decompids and dimlengths, modified if necessary by dimorders
      do i=1, datarank
        decompids(i) = dimorder(i)
        globalCellDim(i) = dimlengths(i)
        localMaxDimCount(i) = dimlengths(i)
        do j=1, nDEs
          localAxisLengths(j,i) = dimlengths(i)
        enddo
        if(dimorder(i).ne.0) then
          decompids(i) = decomps(dimorder(i))
          globalCellDim(i) = tempGCCPD(dimorder(i))
          localMaxDimCount(i) = tempMLCCPD(dimorder(i))
          do j=1, nDEs
            localAxisLengths(j,i) = tempCCPDEPD(j,dimorder(i))
          enddo
        endif
      enddo


! call c routine to allgather
        call c_ESMC_ArrayAllGather(array, layout, decompids, datarank, &
                                   localAxisLengths, globalCellDim, &
                                   localMaxDimCount, array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayAllGather returned error"
          return
        endif

! Clean up
        deallocate(localAxisLengths)
        deallocate(      tempMLCCPD)
        deallocate(       tempGCCPD)
        deallocate(     tempCCPDEPD)

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayAllGatherGrid

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayAllGatherList - gather a distributed Array
!
! !INTERFACE:
      subroutine ESMF_ArrayAllGatherList(array, layout, decompids, &
                                         localAxisLengths, globalDimLengths, &
                                         local_maxlengths, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      integer, dimension(:,:), intent(in) :: localAxisLengths
      integer, dimension(:), intent(in) :: globalDimLengths
      integer, dimension(:), intent(in) :: local_maxlengths
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed Array into a global Array on all DEs.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
        integer :: size_decomp, size_axislengths, i

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif
 
! call c routine to allgather
        size_decomp = size(decompids)
        size_axislengths = size(localAxisLengths,1) * size(localAxisLengths,2)
        call c_ESMC_ArrayAllGather(array, layout, decompids, size_decomp, &
                                   localAxisLengths, &
                                   globalDimLengths, local_maxlengths, &
                                   array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayAllGather returned error"
          return
        endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayAllGatherList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayGather - Gather a distributed Array
!
! !INTERFACE:
      subroutine ESMF_ArrayGather(array, layout, decompids, &
                                  global_dimlengths, local_maxlengths, deid, &
                                  array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      integer, dimension(:), intent(in) :: global_dimlengths
      integer, dimension(:), intent(in) :: local_maxlengths
      integer, intent(in) :: deid
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed Array into a global Array on all DEs.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
        integer :: size_decomp, size_AI
        integer :: i

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif
 
! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayGather(array, layout, decompids, size_decomp, &
                                global_dimlengths, local_maxlengths, deid, &
                                array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayGather returned error"
          return
        endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayGather

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayGetAllAxisIndices - Get all AIs associated with a Grid
!
! !INTERFACE:
      subroutine ESMF_ArrayGetAllAxisIndices(array, grid, datamap, totalindex, &
                                             compindex, exclindex, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_DataMap), intent(in) :: datamap
      type(ESMF_AxisIndex), dimension(:,:), pointer, optional :: totalindex
      type(ESMF_AxisIndex), dimension(:,:), pointer, optional :: compindex
      type(ESMF_AxisIndex), dimension(:,:), pointer, optional :: exclindex
      integer, intent(out), optional :: rc
! 
! !DESCRIPTION:
!   Used to retrieve the index annotations from all {\tt ESMF\_Array}s         
!    associated with a {\tt ESMF\_Grid}.  This computes the values
!    instead of broadcasting them.
!
!EOP
! !REQUIREMENTS:

      integer :: status, nDEs, i, j, gridrank, datarank
      integer, dimension(:), allocatable :: dimOrder
      type(ESMF_AxisIndex), dimension(:), pointer :: arrayindex
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gridindex, globalindex
      type(ESMF_DELayout) :: layout
      type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc

      ! get layout from the grid in order to get the number of DEs
      call ESMF_ArrayGet(array, rank=datarank, rc=status)
      call ESMF_GridGet(grid, numDims=gridrank, rc=status)
      call ESMF_GridGetDELayout(grid, layout, status)
      call ESMF_DELayoutGetNumDEs(layout, nDEs, status)

      ! allocate dimOrder array and get from datamap
      allocate(dimOrder(datarank), stat=status)
      call ESMF_DataMapGet(datamap, dataIorder=dimOrder, &
                           horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                           rc=status)

      ! allocate arrayindex array and get all of them from the array
      allocate(arrayindex(datarank), stat=status)
      call ESMF_ArrayGetAxisIndex(array, arrayindex, rc=status)

      ! allocate gridindex array and get all of them from the grid
      allocate(gridindex(nDEs,gridrank), stat=status)
      call ESMF_GridGetAllAxisIndex(grid, gridindex, horzRelLoc=horzRelLoc, &
                                    vertRelLoc=vertRelLoc, rc=status)

      ! load globalindex with arrayindex and gridindex
      allocate(globalindex(nDEs,datarank), stat=status)
      do i = 1,datarank
        if (dimOrder(i).eq.0) then
          globalindex(:,i) = arrayindex(i)
        else
          globalindex(:,i) = gridindex(:,dimOrder(i))
        endif
      enddo

      if (present(totalindex)) then
          call c_ESMC_ArrayGetAllAxisIndex(array, ESMF_DOMAIN_TOTAL, &
                                           globalindex, nDEs, datarank, &
                                           totalindex, status)
          if (status .ne. ESMF_SUCCESS) goto 10
          ! translate from C++ to F90
          do j=1,size(totalindex, 2)
            do i=1, nDEs
              totalindex(i,j)%min = totalindex(i,j)%min + 1
              totalindex(i,j)%max = totalindex(i,j)%max + 1
            enddo
          enddo
      endif

      if (present(compindex)) then
          call c_ESMC_ArrayGetAllAxisIndex(array, ESMF_DOMAIN_COMPUTATIONAL, &
                                           globalindex, nDEs, datarank, &
                                           compindex, status)
          if (status .ne. ESMF_SUCCESS) goto 10
          ! translate from C++ to F90
          do j=1,size(compindex, 2)
            do i=1, nDEs
              compindex(i,j)%min = compindex(i,j)%min + 1
              compindex(i,j)%max = compindex(i,j)%max + 1
            enddo
          enddo
      endif

      if (present(exclindex)) then
          call c_ESMC_ArrayGetAllAxisIndex(array, ESMF_DOMAIN_EXCLUSIVE, &
                                           globalindex, nDEs, datarank, &
                                           exclindex, status)
          if (status .ne. ESMF_SUCCESS) goto 10
          ! translate from C++ to F90
          do j=1,size(exclindex, 2)
            do i=1, nDEs
              exclindex(i,j)%min = exclindex(i,j)%min + 1
              exclindex(i,j)%max = exclindex(i,j)%max + 1
            enddo
          enddo
      endif

      status = ESMF_SUCCESS

 10   continue

      ! Clean up
      deallocate(dimOrder,    stat=status)
      deallocate(arrayindex,  stat=status)
      deallocate(gridindex,   stat=status)
      deallocate(globalindex, stat=status)

      if (present(rc)) rc = status 

      end subroutine ESMF_ArrayGetAllAxisIndices

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayHalo - Halo an Array
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayHalo()
      subroutine ESMF_ArrayHaloDeprecated(array, layout, &
                                          AI_global, global_dimlens, &
                                          decompids, periodic, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      type(ESMF_DELayout) :: layout
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_global
      integer, dimension(:), intent(in) :: global_dimlens
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_Logical), dimension(:), intent(in) :: periodic
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to halo an Array.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
        integer :: size_decomp

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif
 
! call c routine to halo
        size_decomp = size(decompids)
        call c_ESMC_ArrayHalo(array, layout, AI_global, global_dimlens, &
                              decompids, size_decomp, periodic, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayHalo returned error"
          return
        endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayHaloDeprecated

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayHalo - Perform a halo operation
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayHalo()
      subroutine ESMF_ArrayHaloNew(array, routehandle, blocking, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_RouteHandle), intent(in) :: routehandle
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt Halo} operation over the data in an {\tt ESMF\_Array}.
!     This routine updates the data inside the {\tt ESMF\_Array} in place.
!     It uses a precomputed {\tt ESMF\_Route} for the communications
!     pattern.
!
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing data to be halo'd.
!     \item [route]
!           {\tt ESMF\_RouteHandle} has been precomputed.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS:
      integer :: status         ! local error status
      logical :: rcpresent      ! did user specify rc?
      type(ESMF_LocalArray) :: local_array
      type(ESMF_Route) :: route

      ! initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
 
      call ESMF_RouteHandleGet(routehandle, route1=route, rc=status)

      ! Execute the communications call.
      local_array = array
      call ESMF_RouteRun(route, local_array, local_array, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayHalo: RouteRun returned failure"
        return
      endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayHaloNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayHaloRelease - release the information stored about this Halo operation
!
! !INTERFACE:
      subroutine ESMF_ArrayHaloRelease(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Release the information stored about this Halo operation.
!
!     \begin{description}
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} associated with Halo that is no longer
!           needed.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc=rc)

      end subroutine ESMF_ArrayHaloRelease

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayHaloStore - Store a halo operation
!
! !INTERFACE:
      subroutine ESMF_ArrayHaloStore(array, grid, datamap, routehandle, &
                                     halodirection, blocking, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_DataMap), intent(in) :: datamap
      type(ESMF_RouteHandle), intent(out) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt Halo} operation over the data
!     in an {\tt ESMF\_Array}.  This routine updates the data
!     inside the {\tt ESMF\_Array} in place.  It uses the {\tt ESMF\_Grid}
!     and {\tt ESMF\_DataMap} as a template to understand how this
!     {\tt ESMF\_Array} relates to {\tt ESMF\_Array}s on other {\tt DE}s.
!
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing data to be halo'd.
!     \item [grid]
!           {\tt ESMF\_Grid} which matches how this array was decomposed.
!     \item [datamap]
!           {\tt ESMF\_DataMap} which matches how this array relates to the
!           given grid.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} is returned to be used during the
!           execution of the Halo.
!     \item [{[halodirection]}]
!           {\tt ESMF\_HaloDirection} to indicate which of the boundaries
!           should be updated.  If not specified, all boundaries are updated.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:
      integer :: status         ! local error status
      logical :: rcpresent      ! did user specify rc?
      type(ESMF_DELayout) :: layout
      type(ESMF_Logical), dimension(:), allocatable :: periodic
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI, dst_AI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI, gl_dst_AI
      type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc
      type(ESMF_Route) :: route
      integer, dimension(:), allocatable :: globalCellCountPerDim, decompids
      integer, dimension(ESMF_MAXDIM) :: dimorder, dimlengths
      integer, dimension(:,:), allocatable :: globalStartPerDEPerDim
      integer :: size_decomp, size_AI
      integer :: nDEs, my_DE
      integer :: gridrank, datarank
      integer :: i, numDims
      logical :: hascachedroute    ! can we reuse an existing route?

      ! initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif
 
      ! TODO: all this code could be moved to the C++ side once Grid has
      !       an interface

      ! create the routehandle
      routehandle = ESMF_RouteHandleCreate(status)
    
      ! Extract layout information from the Grid
      call ESMF_GridGetDELayout(grid, layout, status)
      call ESMF_GridGet(grid, numDims=gridrank, rc=status)

      ! Our DE number in the layout and the total number of DEs
      call ESMF_DELayoutGetDEid(layout, my_DE, status)
      call ESMF_DElayoutGetNumDEs(layout, nDEs, rc=status)

      ! Allocate temporary arrays
      allocate(              periodic(      gridrank), stat=status)
      allocate(             decompids(      gridrank), stat=status)
      allocate( globalCellCountPerDim(      gridrank), stat=status)
      allocate(globalStartPerDEPerDim(nDEs, gridrank), stat=status)
      allocate(                src_AI(nDEs, gridrank), stat=status)
      allocate(                dst_AI(nDEs, gridrank), stat=status)
      allocate(             gl_src_AI(nDEs, gridrank), stat=status)
      allocate(             gl_dst_AI(nDEs, gridrank), stat=status)
     
      ! Extract more information from the Grid
      call ESMF_GridGet(grid, globalCellCountPerDim=globalCellCountPerDim, &
                        globalStartPerDEPerDim=globalStartPerDEPerDim, &
                        periodic=periodic, rc=status)
      ! TODO: get decompids, get grid rank here?
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayHalo: GridGet returned failure"
         return
      endif

      ! Query the datamap and set info for grid so it knows how to
      ! match up the array indicies and the grid indicies.
      call ESMF_DataMapGet(datamap, horzRelLoc=horzRelLoc, &
                           vertRelLoc=vertRelLoc, &
                           dataIorder=dimorder, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayHalo: DataMapGet returned failure"
        return
      endif

      ! And get the Array sizes
      call ESMF_ArrayGet(array, rank=datarank, counts=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayHalo: ArrayGet returned failure"
         return
      endif

      ! TODO: apply dimorder and decompids to get mapping of array to data

      ! set up things we need to find a cached route or precompute one
      call ESMF_ArrayGetAllAxisIndices(array, grid, datamap, totalindex=dst_AI, &
                                       compindex=src_AI, rc=status)

      ! translate AI's into global numbering
      call ESMF_GridLocalToGlobalIndex(grid, horzRelLoc=horzRelLoc, &
                                       vertRelLoc=vertRelloc, localAI2D=dst_AI, &
                                       globalAI2D=gl_dst_AI, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayHalo: GridLocalToGlobalIndex returned failure"
         return
      endif
      call ESMF_GridLocalToGlobalIndex(grid, horzRelLoc=horzRelLoc, &
                                       vertRelLoc=vertRelloc, localAI2D=src_AI, &
                                       globalAI2D=gl_src_AI, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayHalo: GridLocalToGlobalIndex returned failure"
         return
      endif

      ! Does this same route already exist?  If so, then we can drop
      ! down immediately to RouteRun.
      call ESMF_RouteGetCached(datarank, my_DE, gl_dst_AI, gl_dst_AI, &
                               nDEs, layout, my_DE, gl_src_AI, gl_src_AI, &
                               nDEs, layout, periodic, hascachedroute, &
                               route, status)

      if (.not. hascachedroute) then
          ! Create the route object.
          route = ESMF_RouteCreate(layout, rc)

          call ESMF_RoutePrecomputeHalo(route, datarank, my_DE, gl_src_AI, &
                                        gl_dst_AI, nDEs, &
                                        globalStartPerDEPerDim, &
                                        globalCellCountPerDim, layout, &
                                        periodic, status)
      endif

      ! and set route into routehandle object
      call ESMF_RouteHandleSet(routehandle, route1=route, rc=status)

      ! get rid of temporary arrays
      if (allocated(periodic))    deallocate(periodic)
      if (allocated(decompids))   deallocate(decompids)
      if (allocated(globalCellCountPerDim)) deallocate(globalCellCountPerDim)
      if (allocated(globalStartPerDEPerDim)) &
         deallocate(globalStartPerDEPerDim, stat=status)
      if (associated(    src_AI)) deallocate(src_AI, stat=status)
      if (associated(    dst_AI)) deallocate(dst_AI, stat=status)
      if (associated( gl_src_AI)) deallocate(gl_src_AI, stat=status)
      if (associated( gl_dst_AI)) deallocate(gl_dst_AI, stat=status)

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayHaloStore

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayRedist - Redistribute an Array
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayRedist()
      subroutine ESMF_ArrayRedistDeprecated(array, layout, globalStart, &
                                  global_dimlengths, rank_trans, olddecompids, &
                                  decompids, redistarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: globalStart
      integer, dimension(:), intent(in) :: global_dimlengths
      integer, dimension(:), intent(in) :: rank_trans
      integer, dimension(:), intent(in) :: olddecompids
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_Array), intent(in) :: redistarray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to redistribute an Array.
!
!
!EOPI
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
        integer :: size_rank_trans
        integer :: size_decomp

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! call c routine to query index
        size_rank_trans = size(rank_trans)
        size_decomp = size(decompids)
        call c_ESMC_ArrayRedist(array, layout, globalStart, global_dimlengths, &
                                rank_trans, size_rank_trans, olddecompids, &
                                decompids, size_decomp, redistarray, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayRedist returned error"
          return
        endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayRedistDeprecated

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayRedist - Redistribute an Array
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayRedist
      subroutine ESMF_ArrayRedistNew(srcArray, dstArray, routehandle, &
                                     blocking, rc) 
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcArray
      type(ESMF_Array), intent(inout) :: dstArray
      type(ESMF_RouteHandle), intent(in) :: routehandle
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to redistribute an Array.
!
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing results.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} has been precomputed.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!
!EOP
! !REQUIREMENTS:
      integer :: status         ! local error status
      logical :: rcpresent      ! did user specify rc?
      type(ESMF_LocalArray) :: dstLocalArray, srcLocalArray
      type(ESMF_Route) :: route

      ! initialize return code; assume failure until success certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_RouteHandleGet(routehandle, route1=route, rc=status)

      ! Execute the communications call.
      dstLocalArray = dstArray
      srcLocalArray = srcArray
      call ESMF_RouteRun(route, srcLocalArray, dstLocalArray, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayRedist: RouteRun returned failure"
        return
      endif

! set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayRedistNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayRedistRelease - Release the information stored about this Redist operation
!
! !INTERFACE:
      subroutine ESMF_ArrayRedistRelease(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Release the information stored about this Redist operation.
!
!     \begin{description}
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} associated with Redist that is no longer
!           needed.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc=rc)

      end subroutine ESMF_ArrayRedistRelease

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Compute information about a data Redistribution
!
! !INTERFACE:
      subroutine ESMF_ArrayRedistStore(srcArray, srcGrid, srcDataMap, &
                                       dstArray, dstGrid, dstDataMap, &
                                       parentLayout, routehandle, blocking, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcArray
      type(ESMF_Grid), intent(in) :: srcGrid
      type(ESMF_DataMap), intent(in) :: srcDataMap
      type(ESMF_Array), intent(inout) :: dstArray
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_DataMap), intent(in) :: dstDataMap
      type(ESMF_DELayout), intent(in) :: parentLayout
      type(ESMF_RouteHandle), intent(out) :: routehandle
      type(ESMF_Async), intent(inout), optional :: blocking
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Compute and store information about this redistribute operation.
!
!  The arguments are:
!   \begin{description}
!   \item[srcArray]
!    {\tt ESMF\_Array} containing the data source.
!   \item[srcGrid]
!    {\tt ESMF\_Grid} describing the grid on which the source data is arranged.
!   \item[srcDataMap]
!    {\tt ESMF\_DataMap} describing how the source data maps onto the grid.
!   \item[dstArray]
!    {\tt ESMF\_Array} where the destination data will be put.
!   \item[dstGrid]
!    {\tt ESMF\_Grid} describing the grid on which the destination data is arranged.
!   \item[dstDataMap]
!    {\tt ESMF\_DataMap} describing how the destination data maps onto the grid.
!   \item[parentLayout]
!    {\tt ESMF\_DELayout} object which includes all {\tt DE}s in both the
!    source and destination grids.
!   \item [routehandle]
!    Returned {\tt ESMF\_RouteHandle} which identifies this communication pattern.
!   \item [{[blocking]}]
!    Optional argument to indicate synchronous or asychronous communications.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!
!EOP
! !REQUIREMENTS:
      integer :: status         ! local error status
      logical :: rcpresent      ! did user specify rc?
      type(ESMF_DELayout) :: dstLayout, srcLayout
      type(ESMF_Logical), dimension(:), allocatable :: periodic
      type(ESMF_AxisIndex), dimension(:,:), pointer :: dstCompAI, srcCompAI, &
                                                     dstTotalAI, srcTotalAI, &
                                                   dstCLocalAI, srcCLocalAI, &
                                                   dstTLocalAI, srcTLocalAI
      type(ESMF_RelLoc) :: dstHorzRelLoc, srcHorzRelLoc, &
                           dstVertRelLoc, srcVertRelLoc
      type(ESMF_Route) :: route
      integer, dimension(:), allocatable :: dstCellCountPerDim, decompids, &
                                            srcCellCountPerDim
      integer, dimension(ESMF_MAXDIM) :: dstDimOrder, srcDimOrder, dimlengths
      integer, dimension(:,:), allocatable :: dstStartPerDEPerDim, &
                                              srcStartPerDEPerDim
      integer :: nDEs, dstMyDE, srcMyDE
      integer :: gridrank, datarank
      integer :: i, numDims
      logical :: hascachedroute     ! can we reuse an existing route?

      ! initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! create the routehandle
      routehandle = ESMF_RouteHandleCreate(status)

      ! Extract layout information from the Grids
      call ESMF_GridGetDELayout(dstGrid, dstLayout, status)
      call ESMF_GridGetDELayout(srcGrid, srcLayout, status)
      call ESMF_GridGet(srcGrid, numDims=gridrank, rc=status)

      ! Our DE number in the layout and the total number of DEs
      call ESMF_DELayoutGetDEid(dstLayout, dstMyDE, status)
      call ESMF_DELayoutGetDEid(srcLayout, srcMyDE, status)
      call ESMF_DElayoutGetNumDEs(srcLayout, nDEs, rc=status)

      ! Allocate temporary arrays
      allocate(           periodic(      gridrank), stat=status)
      allocate(          decompids(      gridrank), stat=status)
      allocate( dstCellCountPerDim(      gridrank), stat=status)
      allocate(dstStartPerDEPerDim(nDEs, gridrank), stat=status)
      allocate( srcCellCountPerDim(      gridrank), stat=status)
      allocate(srcStartPerDEPerDim(nDEs, gridrank), stat=status)
      allocate(          dstCompAI(nDEs, gridrank), stat=status)
      allocate(          srcCompAI(nDEs, gridrank), stat=status)
      allocate(         dstTotalAI(nDEs, gridrank), stat=status)
      allocate(         srcTotalAI(nDEs, gridrank), stat=status)
      allocate(        dstCLocalAI(nDEs, gridrank), stat=status)
      allocate(        srcCLocalAI(nDEs, gridrank), stat=status)
      allocate(        dstTLocalAI(nDEs, gridrank), stat=status)
      allocate(        srcTLocalAI(nDEs, gridrank), stat=status)
     
      ! Extract more information from the Grids
      ! TODO: get decompids?
      call ESMF_GridGet(dstGrid, globalCellCountPerDim=dstCellCountPerDim, &
                        globalStartPerDEPerDim=dstStartPerDEPerDim, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayRedist: GridGet returned failure"
         return
      endif
      call ESMF_GridGet(srcGrid, globalCellCountPerDim=srcCellCountPerDim, &
                        globalStartPerDEPerDim=srcStartPerDEPerDim, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayRedist: GridGet returned failure"
         return
      endif

      ! Query the datamap and set info for grid so it knows how to
      ! match up the array indicies and the grid indicies.
      call ESMF_DataMapGet(dstDataMap, horzRelLoc=dstHorzRelLoc, &
                           vertRelLoc=dstVertRelLoc, &
                           dataIorder=dstDimOrder, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayRedist: DataMapGet returned failure"
        return
      endif
      call ESMF_DataMapGet(srcDataMap, horzRelLoc=srcHorzRelLoc, &
                           vertRelLoc=srcVertRelLoc, &
                           dataIorder=srcDimOrder, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ArrayRedist: DataMapGet returned failure"
        return
      endif

      ! And get the Array sizes
      call ESMF_ArrayGet(srcArray, rank=datarank, counts=dimlengths, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayRedist: ArrayGet returned failure"
         return
      endif

      ! TODO: apply dimorder and decompids to get mapping of array to data

      ! set up things we need to find a cached route or precompute one
      call ESMF_ArrayGetAllAxisIndices(dstArray, dstGrid, dstDataMap, &
                                       compindex =dstCLocalAI, &
                                       totalindex=dstTLocalAI, rc=status)
      call ESMF_ArrayGetAllAxisIndices(srcArray, srcGrid, srcDataMap, &
                                       compindex =srcCLocalAI, &
                                       totalindex=srcTLocalAI, rc=status)

      ! translate AI's into global numbering
      call ESMF_GridLocalToGlobalIndex(dstGrid, &
                                       horzRelLoc=dstHorzRelLoc, &
                                       vertRelLoc=dstVertRelLoc, &
                                       localAI2D=dstCLocalAI, &
                                       globalAI2D=dstCompAI, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayRedist: GridLocalToGlobalIndex returned failure"
         return
      endif
      call ESMF_GridLocalToGlobalIndex(dstGrid, &
                                       horzRelLoc=dstHorzRelLoc, &
                                       vertRelLoc=dstVertRelLoc, &
                                       localAI2D=dstTLocalAI, &
                                       globalAI2D=dstTotalAI, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayRedist: GridLocalToGlobalIndex returned failure"
         return
      endif
      call ESMF_GridLocalToGlobalIndex(srcGrid, &
                                       horzRelLoc=srcHorzRelLoc, &
                                       vertRelLoc=srcVertRelLoc, &
                                       localAI2D=srcCLocalAI, &
                                       globalAI2D=srcCompAI, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayRedist: GridLocalToGlobalIndex returned failure"
         return
      endif
      call ESMF_GridLocalToGlobalIndex(srcGrid, &
                                       horzRelLoc=srcHorzRelLoc, &
                                       vertRelLoc=srcVertRelLoc, &
                                       localAI2D=srcTLocalAI, &
                                       globalAI2D=srcTotalAI, rc=status)
      if(status .NE. ESMF_SUCCESS) then
         print *, "ERROR in ArrayRedist: GridLocalToGlobalIndex returned failure"
         return
      endif

      ! Does this same route already exist?  If so, then we can drop
      ! down immediately to RouteRun.
      call ESMF_RouteGetCached(datarank, &
                               dstMyDE, dstCompAI, dstTotalAI, nDEs, dstLayout, &
                               srcMyDE, srcCompAI, srcTotalAI, nDEs, srcLayout, &
                               periodic, hascachedroute, route, status)

      if (.not. hascachedroute) then
          ! Create the route object.
          route = ESMF_RouteCreate(parentLayout, rc)

          call ESMF_RoutePrecomputeRedist(route, datarank, dstMyDE, dstCompAI, &
                                          dstTotalAI, dstStartPerDEPerDim, &
                                          dstCellCountPerDim, dstLayout, &
                                          srcMyDE, srcCompAI, srcTotalAI, &
                                          srcStartPerDEPerDim, &
                                          srcCellCountPerDim, srcLayout, status)
      endif

      ! and set route into routehandle object
      call ESMF_RouteHandleSet(routehandle, route1=route, rc=status)

      ! get rid of temporary arrays
      if (allocated(           periodic)) deallocate(periodic)
      if (allocated(          decompids)) deallocate(decompids)
      if (allocated( dstCellCountPerDim)) deallocate(dstCellCountPerDim)
      if (allocated( srcCellCountPerDim)) deallocate(srcCellCountPerDim)
      if (allocated(dstStartPerDEPerDim)) deallocate(dstStartPerDEPerDim)
      if (allocated(srcStartPerDEPerDim)) deallocate(srcStartPerDEPerDim)
      if (associated(         dstCompAI)) deallocate(dstCompAI)
      if (associated(         srcCompAI)) deallocate(srcCompAI)
      if (associated(        dstTotalAI)) deallocate(dstTotalAI)
      if (associated(        srcTotalAI)) deallocate(srcTotalAI)
      if (associated(       dstCLocalAI)) deallocate(dstCLocalAI)
      if (associated(       srcCLocalAI)) deallocate(srcCLocalAI)
      if (associated(       dstTLocalAI)) deallocate(dstTLocalAI)
      if (associated(       srcTLocalAI)) deallocate(srcTLocalAI)

! set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayRedistStore

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayScatter - Scatter a single Array
!
! !INTERFACE:
      subroutine ESMF_ArrayScatter(array, layout, decompids, deid, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      integer, intent(in) :: deid
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to scatter a single Array into a distributed Array across all DEs.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
        integer :: size_decomp
        integer :: i

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif
 
! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayScatter(array, layout, decompids, size_decomp, &
                                 deid, array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayScatter returned error"
          return
        endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayScatter

       end module ESMF_ArrayCommMod

