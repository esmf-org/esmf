! $Id: ESMF_ArrayComm.F90,v 1.59 2004/12/03 20:47:44 nscollins Exp $
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
#define ESMF_FILENAME "ESMF_ArrayComm.F90"
!
!     ESMF Array Comm module
      module ESMF_ArrayCommMod
!
!==============================================================================
!
! This file contains the Array methods which do communication, and so
! must be compiled after the comm routines.  These are logically part 
! of the Array class.
!
!------------------------------------------------------------------------------
#include "ESMF.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_ArrayCommMod - Data communication routines at the Array level
!
! !DESCRIPTION:
!
! The code in this file implements the distributed {\tt ESMF\_Array} class 
! communication routines.  The {\tt ESMF\_Array} class definitions and basic
! methods are in the Array module.  These routines are broken out into a
! separate module so that the Route and Grid methods can be compiled 
! after the basic Array definitions, and before the ArrayComm routines.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_IOSpecMod
      use ESMF_LogErrMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayDataMapMod
      use ESMF_VMTypesMod
      use ESMF_VMBaseMod
      use ESMF_VMCommMod
      use ESMF_DELayoutMod  
      use ESMF_ArrayMod
      use ESMF_ArrayGetMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_RHandleMod
      use ESMF_RouteMod
      use ESMF_FieldDataMapMod
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

      public ESMF_ArrayGather, ESMF_ArrayScatter
      !!public ESMF_ArrayAllGather
      !!public ESMF_ArrayReduce, ESMF_ArrayAllReduce
      !!public ESMF_ArrayBroadcast, ESMF_ArrayAlltoAll

!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_ArrayComm.F90,v 1.59 2004/12/03 20:47:44 nscollins Exp $'
!
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !IROUTINE: ESMF_ArrayHalo - Halo a distributed array
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
! !IROUTINE: ESMF_ArrayRedist - Redistribute an Array
!
! !INTERFACE:
      interface ESMF_ArrayRedist

! !PRIVATE MEMBER FUNCTIONS:
          module procedure ESMF_ArrayRedistNew
          module procedure ESMF_ArrayRedistDeprecated

! !DESCRIPTION:
!     This interface provides both the revised entry point for
!      calling redistribute on an {\tt ESMF\_Array} object, and temporarily
!      for backwards compatibility an older interface into the same code.

!EOPI
      end interface

#if 0
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayAllGather - Gather a distributed array to all DEs
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
#endif

!==============================================================================

      contains

!==============================================================================



#if 0
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayAllGather - Gather an Array and put results on all DEs
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayAllGather
      subroutine ESMF_ArrayAllGatherList(array, delayout, decompids, &
                                         localAxisLengths, globalDimLengths, &
                                         local_maxlengths, gatheredArray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in) :: decompids
      integer, dimension(:,:), intent(in) :: localAxisLengths
      integer, dimension(:), intent(in) :: globalDimLengths
      integer, dimension(:), intent(in) :: local_maxlengths
      type(ESMF_Array), intent(out) :: gatheredArray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed Array into a global Array on all DEs.
!
!EOPI

        integer :: localrc         ! local error status
        integer :: size_decomp, size_axislengths, i

        ! initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE
 
        ! call c routine to allgather
        size_decomp = size(decompids)
        size_axislengths = size(localAxisLengths,1) * size(localAxisLengths,2)
        call c_ESMC_ArrayAllGather(array, delayout, decompids, size_decomp, &
                                   localAxisLengths, &
                                   globalDimLengths, local_maxlengths, &
                                   gatheredArray, localrc)

        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayAllGatherList

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayGather - Gather a distributed Array
!
! !INTERFACE:
      subroutine ESMF_ArrayGather(array, delayout, decompids, &
                                  global_dimlengths, local_maxlengths, deid, &
                                  gatheredArray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in) :: decompids
      integer, dimension(:), intent(in) :: global_dimlengths
      integer, dimension(:), intent(in) :: local_maxlengths
      integer, intent(in) :: deid
      type(ESMF_Array), intent(out) :: gatheredArray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed Array into a global Array on all DEs.
!
!
!EOPI

        integer :: localrc         ! local error status
        logical :: rcpresent      ! did user specify rc?
        integer :: size_decomp, size_AI
        integer :: i

        ! initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE
 
        ! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayGather(array, delayout, decompids, size_decomp, &
                                global_dimlengths, local_maxlengths, deid, &
                                gatheredArray, localrc)

        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayGather
#endif

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGather"
!BOP
! !IROUTINE: ESMF_ArrayGather - Gather an Array onto one DE
!
! !INTERFACE:
    subroutine ESMF_ArrayGather(array, grid, datamap, rootDE, gatheredArray, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(in) :: array
    type(ESMF_Grid), intent(in) :: grid
    type(ESMF_FieldDataMap), intent(in) :: datamap
    integer, intent(in) :: rootDE
    type(ESMF_Array), intent(out) :: gatheredArray
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Gather a distributed {\tt ESMF\_Array} over multiple DEs into 
!  a single {\tt ESMF\_Array} on one DE.
!
!  The arguments are:
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing distributed data to be gathered.
!     \item [grid]
!           {\tt ESMF\_Grid} which corresponds to the distributed data.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which describes the mapping of the
!           data onto the cells in the {\tt ESMF\_Grid}.
!     \item [rootDE]
!           The DE number on which the resulting gathered {\tt ESMF\_Array}
!           will be created.  
!     \item [gatheredArray]
!           On the {\tt rootDE}, the resulting gathered {\tt ESMF\_Array}.
!           On all other DEs, an invalid {\tt ESMF\_Array}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

    integer :: status         ! local error status
    integer :: gridrank, datarank
    integer :: i, j, nDEs
    type(ESMF_DELayout) :: delayout
    type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc
    integer, dimension(ESMF_MAXDIM) :: decompids
    integer, dimension(:,:), pointer :: localAxisLengths, tempCCPDEPD
    integer, dimension(ESMF_MAXDIM) :: dimOrder, dimlengths
    integer, dimension(ESMF_MAXGRIDDIM) :: decomps
    integer:: size_decomp
    integer, dimension(ESMF_MAXDIM) :: localMaxDimCount, globalCellDim
    integer, dimension(:), allocatable :: tempMLCCPD, tempGCCPD

    ! initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_FAILURE
 
    ! extract necessary information from the grid
    call ESMF_GridGet(grid, dimCount=gridrank, delayout=delayout, rc=status)
    call ESMF_DELayoutGet(delayout, deCount=nDEs, dimCount=size_decomp, &
      rc=status)
    allocate(localAxisLengths(nDEs,ESMF_MAXDIM), stat=status)
    allocate( tempMLCCPD(     gridrank), stat=status)
    allocate(  tempGCCPD(     gridrank), stat=status)
    allocate(tempCCPDEPD(nDEs,gridrank), stat=status)

    ! Query the datamap and set info for grid so it knows how to match up the
    ! array indices and the grid indices.
    call ESMF_FieldDataMapGet(datamap, dataIndexList=dimOrder, &
                              horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                              rc=status)
    if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

    call ESMF_GridGet(grid, &
                        horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                        globalCellCountPerDim=tempGCCPD, &
                        cellCountPerDEPerDim=tempCCPDEPD, &
                        maxLocalCellCountPerDim=tempMLCCPD, rc=rc)
    ! call ESMF_GridGet(grid, decomps, rc=status)   !TODO: add decomps
    if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
    ! size_decomp = size(decompids) ! already have this value
    decomps(1) = 1    ! TODO: remove this once the grid call is created
    decomps(2) = 2

    ! get the Array sizes
    call ESMF_ArrayGet(array, rank=datarank, counts=dimlengths, rc=status)
    if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

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


    ! call c routine to gather
    call c_ESMC_ArrayGather(array, delayout, decompids, size_decomp, &
                            localAxisLengths, globalCellDim, localMaxDimCount, &
                            rootDE, gatheredArray, status)
#if 0
        call c_ESMC_ArrayAllGather(array, delayout, decompids, datarank, &
                                   localAxisLengths, globalCellDim, &
                                   localMaxDimCount, gatheredArray, status)
#endif

    if (ESMF_LogMsgFoundError(status, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return

    ! Clean up
    deallocate(localAxisLengths)
    deallocate(      tempMLCCPD)
    deallocate(       tempGCCPD)
    deallocate(     tempCCPDEPD)

    ! set return code if user specified it
    if (present(rc)) rc = ESMF_SUCCESS

    end subroutine ESMF_ArrayGather

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayGetAllAxisIndices"
!BOPI
! !IROUTINE: ESMF_ArrayGetAllAxisIndices - Get all AIs associated with a Grid
!
! !INTERFACE:
      subroutine ESMF_ArrayGetAllAxisIndices(array, grid, datamap, totalindex, &
                                             compindex, exclindex, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_FieldDataMap), intent(in) :: datamap
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
! %TODO: add missing description section here
!
!EOPI

      integer :: status, nDEs, i, j, gridrank, datarank
      integer, dimension(:), allocatable :: dimOrder
      type(ESMF_AxisIndex), dimension(:), pointer :: arrayindex
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gridindex, globalindex
      type (ESMF_DELayout) :: delayout
      type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc

      ! get layout from the grid in order to get the number of DEs
      call ESMF_ArrayGet(array, rank=datarank, rc=status)
      call ESMF_GridGet(grid, dimCount=gridrank, rc=status)
      call ESMF_GridGet(grid, delayout=delayout, rc=status)
      call ESMF_DELayoutGet(delayout, nDEs, rc=status)

      ! allocate dimOrder array and get from datamap
      allocate(dimOrder(datarank), stat=status)
      call ESMF_FieldDataMapGet(datamap, dataIndexList=dimOrder, &
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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloDeprecated"
!BOPI
! !IROUTINE: ESMF_ArrayHalo - Halo an Array
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayHalo()
      subroutine ESMF_ArrayHaloDeprecated(array, delayout, &
                                          AI_global, global_dimlens, &
                                          decompids, periodic, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      type(ESMF_DELayout) :: delayout
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_global
      integer, dimension(:), intent(in) :: global_dimlens
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_Logical), dimension(:), intent(in) :: periodic
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to halo an Array.
!
!  % OLD interface - replaced by ArrayHaloRun, but may still be used
!  %  by some code or tests.  Verify (also with users) before removing.
!
!EOPI

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
        call c_ESMC_ArrayHalo(array, delayout, AI_global, global_dimlens, &
                              decompids, size_decomp, periodic, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayHaloDeprecated

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloNew"
!BOP
! !IROUTINE: ESMF_ArrayHalo - Halo an Array
!
! !INTERFACE:
    ! Private name; call using ESMF_ArrayHalo()
    subroutine ESMF_ArrayHaloNew(array, routehandle, blocking, commhandle, rc)
!
! !ARGUMENTS:
    type(ESMF_Array), intent(inout) :: array
    type(ESMF_RouteHandle), intent(in) :: routehandle
    type(ESMF_BlockingFlag), intent(in), optional :: blocking
    type(ESMF_CommHandle), intent(inout), optional :: commhandle
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Perform a halo operation over the data in an {\tt ESMF\_Array}.
!   This routine updates the data inside the {\tt ESMF\_Array} in place.
!   It uses a precomputed {\tt ESMF\_Route} for the communications pattern.
!   (See {\tt ESMF\_ArrayHaloPrecompute()} for how to precompute and 
!   associate an {\tt ESMF\_Route} with an {\tt ESMF\_RouteHandle}).
!
!   \begin{description}
!   \item [array]
!         {\tt ESMF\_Array} containing data to be haloed.
!   \item [routehandle]
!         {\tt ESMF\_RouteHandle} which was returned from an
!         {\tt ESMF\_ArrayHaloPrecompute()} call.
!   \item [{[blocking]}]
!         Optional argument which specifies whether the operation should
!         wait until complete before returning or return as soon
!         as the communication between DEs has been scheduled.
!         If not present, default is to do synchronous communications.
!         Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!         {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!   \item [{[commhandle]}]
!         If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!         argument is required.  Information about the pending operation
!         will be stored in the {\tt ESMF\_CommHandle} and can be queried
!         or waited for later.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

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

      ! fortran equivalent of a cast - routerun wants a local array 
      local_array%this%ptr = array%this%ptr

      ! Execute the communications call.
      call ESMF_RouteRun(route, local_array, local_array, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayHaloNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloRelease"
!BOP
! !IROUTINE: ESMF_ArrayHaloRelease - Release resources stored for halo operation
!
! !INTERFACE:
      subroutine ESMF_ArrayHaloRelease(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     When the precomputed information about a halo operation is no longer
!     needed, this routine releases the associated resources.
!
!     \begin{description}
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} associated with halo operation which
!           should be released.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc=rc)

      end subroutine ESMF_ArrayHaloRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayHaloStore"
!BOP
! !IROUTINE: ESMF_ArrayHaloStore - Store resources for a halo operation
!
! !INTERFACE:
      subroutine ESMF_ArrayHaloStore(array, grid, datamap, routehandle, &
                                     halodirection, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_Grid), intent(in) :: grid
      type(ESMF_FieldDataMap), intent(in) :: datamap
      type(ESMF_RouteHandle), intent(out) :: routehandle
      type(ESMF_HaloDirection), intent(in), optional :: halodirection
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Precompute the data movements needed to 
!     perform a halo operation over the data in an {\tt ESMF\_Array}.  
!     It associates this information with the {\tt routehandle}, which 
!     should then provided to {\tt ESMF\_ArrayHalo()} at execution time.
!     The {\tt ESMF\_Grid} and {\tt ESMF\_FieldDataMap} are used as
!     templates to understand how this {\tt ESMF\_Array} relates 
!     to {\tt ESMF\_Array}s on other DEs.
!
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing data to be haloed.
!     \item [grid]
!           {\tt ESMF\_Grid} which matches how this data was decomposed.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which matches how the data in the
!           {\tt ESMF\_Array} relates to the given {\tt ESMF\_Grid}.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} is returned to be used during the
!           execution of the halo operation.
!     \item [{[halodirection]}]
!           {\tt ESMF\_HaloDirection} to indicate which of the boundaries
!           should be updated.  If not specified, all boundaries are updated.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: status         ! local error status
      type(ESMF_DELayout) :: delayout
      type(ESMF_VM) :: vm
      type(ESMF_Logical), dimension(:), allocatable :: periodic
      type(ESMF_AxisIndex), dimension(:,:), pointer :: src_AI, dst_AI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: gl_src_AI, gl_dst_AI
      type(ESMF_RelLoc) :: horzRelLoc, vertRelLoc
      type(ESMF_Route) :: route
      integer, dimension(:), allocatable :: globalCellCountPerDim, decompids
      integer, dimension(ESMF_MAXDIM) :: dimorder, dimlengths
      integer, dimension(:,:), allocatable :: globalStartPerDEPerDim
      integer :: nDEs, my_DE
      integer :: gridrank, datarank
      logical :: hascachedroute    ! can we reuse an existing route?

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
 
      ! TODO: all this code could be moved to the C++ side once Grid has
      !       an interface

      ! create the routehandle
      routehandle = ESMF_RouteHandleCreate(status)
    
      ! Extract layout information from the Grid
      call ESMF_GridGet(grid, delayout=delayout, rc=status)
      call ESMF_GridGet(grid, dimCount=gridrank, rc=status)
      
      ! Get the associated VM
      call ESMF_DELayoutGetVM(delayout, vm, rc=status)

      ! Our DE number in the layout and the total number of DEs
      call ESMF_DELayoutGet(delayout, deCount=nDEs, localDE=my_DE, rc=status)

      ! Allocate temporary arrays
      allocate(              periodic(      gridrank), stat=status)
      allocate(             decompids(      gridrank), stat=status)
      allocate( globalCellCountPerDim(      gridrank), stat=status)
      allocate(globalStartPerDEPerDim(nDEs, gridrank), stat=status)
      allocate(                src_AI(nDEs, gridrank), stat=status)
      allocate(                dst_AI(nDEs, gridrank), stat=status)
      allocate(             gl_src_AI(nDEs, gridrank), stat=status)
      allocate(             gl_dst_AI(nDEs, gridrank), stat=status)

      ! Query the datamap and set info for grid so it knows how to
      ! match up the array indicies and the grid indicies.
      call ESMF_FieldDataMapGet(datamap, horzRelLoc=horzRelLoc, &
                           vertRelLoc=vertRelLoc, &
                           dataIndexList=dimorder, rc=status)
       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
     
      ! Extract more information from the Grid
      call ESMF_GridGet(grid, &
                        horzRelLoc=horzRelLoc, vertRelLoc=vertRelLoc, &
                        globalCellCountPerDim=globalCellCountPerDim, &
                        globalStartPerDEPerDim=globalStartPerDEPerDim, &
                        periodic=periodic, rc=status)
      ! TODO: get decompids, get grid rank here?
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! And get the Array sizes
      call ESMF_ArrayGet(array, rank=datarank, counts=dimlengths, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! TODO: apply dimorder and decompids to get mapping of array to data

      ! set up things we need to find a cached route or precompute one
      call ESMF_ArrayGetAllAxisIndices(array, grid, datamap, totalindex=dst_AI, &
                                       compindex=src_AI, rc=status)

      ! translate AI's into global numbering
      call ESMF_GridDELocalToGlobalAI(grid, horzRelLoc=horzRelLoc, &
                                      vertRelLoc=vertRelloc, &
                                      localAI2D=dst_AI, &
                                      globalAI2D=gl_dst_AI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      call ESMF_GridDELocalToGlobalAI(grid, horzRelLoc=horzRelLoc, &
                                      vertRelLoc=vertRelloc, &
                                      localAI2D=src_AI, &
                                      globalAI2D=gl_src_AI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Does this same route already exist?  If so, then we can drop
      ! down immediately to RouteRun.
      call ESMF_RouteGetCached(datarank, my_DE, gl_dst_AI, gl_dst_AI, &
                               nDEs, delayout, my_DE, gl_src_AI, gl_src_AI, &
                               nDEs, delayout, periodic, hascachedroute, &
                               route, status)

      if (.not. hascachedroute) then
          ! Create the route object.
          route = ESMF_RouteCreate(vm, rc)

          call ESMF_RoutePrecomputeHalo(route, datarank, my_DE, gl_src_AI, &
                                        gl_dst_AI, nDEs, &
                                        globalStartPerDEPerDim, &
                                        globalCellCountPerDim, delayout, &
                                        periodic, status)
      endif

      ! and set route into routehandle object
      call ESMF_RouteHandleSet(routehandle, route1=route, &
                               htype=ESMF_HALOHANDLE, rc=status)

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
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayHaloStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistDeprecated"
!BOPI
! !IROUTINE: ESMF_ArrayRedist - Redistribute an Array
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayRedist()
      subroutine ESMF_ArrayRedistDeprecated(array, delayout, globalStart, &
                                  global_dimlengths, rank_trans, olddecompids, &
                                  decompids, redistarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      type(ESMF_DELayout) :: delayout
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
!  % TODO: verify that all calls to this version have been removed
!  % and are not being used by user code, and remove this routine.
!
!EOPI

        integer :: status         ! local error status
        integer :: size_rank_trans
        integer :: size_decomp

        ! initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE

        ! call c routine to query index
        size_rank_trans = size(rank_trans)
        size_decomp = size(decompids)
        call c_ESMC_ArrayRedist(array, delayout, globalStart, global_dimlengths, &
                                rank_trans, size_rank_trans, olddecompids, &
                                decompids, size_decomp, redistarray, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayRedistDeprecated

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistNew"
!BOP
! !IROUTINE: ESMF_ArrayRedist - Redistribute an Array
!
! !INTERFACE:
      ! Private name; call using ESMF_ArrayRedist
      subroutine ESMF_ArrayRedistNew(srcArray, dstArray, routehandle, &
                                     blocking, commhandle, rc) 
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcArray
      type(ESMF_Array), intent(inout) :: dstArray
      type(ESMF_RouteHandle), intent(in) :: routehandle
      type(ESMF_BlockingFlag), intent(in), optional :: blocking
      type(ESMF_CommHandle), intent(inout), optional :: commhandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Redistribute the data in one set of {\tt ESMF\_Array}s to another
!  set of {\tt ESMF\_Array}s.  Data redistribution does no interpolation,
!  so during the {\tt ESMF\_ArrayRedistPrecompute()} call the 
!  {\tt ESMF\_Grid}s must have identical coordinates.  
!  The distribution of the {\tt ESMF\_Grid} can be over different
!  {\tt ESMF\_DELayout}s, or the {\tt ESMF\_FieldDataMaps} can differ.
!  The {\tt routehandle} argument must be the one which was associated
!  with the precomputed data movements during the precompute operation, and
!  if the data movement is identical for different collections of
!  {\tt ESMF\_Array}s, the same {\tt routehandle} can be supplied during
!  multiple calls to this execution routine, specifying a different set of
!  source and destination {\tt ESMF\_Array}s each time.
!
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing results.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} precomputed by 
!           {\tt ESMF\_ArrayRedistPrecompute()}.
!     \item [{[blocking]}]
!           Optional argument which specifies whether the operation should
!           wait until complete before returning or return as soon
!           as the communication between {\tt DE}s has been scheduled.
!           If not present, default is to do synchronous communications.
!           Valid values for this flag are {\tt ESMF\_BLOCKING} and 
!           {\tt ESMF\_NONBLOCKING}.
!      (This feature is not yet supported.  All operations are synchronous.)
!     \item [{[commhandle]}]
!           If the blocking flag is set to {\tt ESMF\_NONBLOCKING} this 
!           argument is required.  Information about the pending operation
!           will be stored in the {\tt ESMF\_CommHandle} and can be queried
!           or waited for later.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status         ! local error status
      type(ESMF_LocalArray) :: dstLocalArray, srcLocalArray
      type(ESMF_Route) :: route

      ! initialize return code; assume failure until success certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_RouteHandleGet(routehandle, route1=route, rc=status)

      ! Execute the communications call.
      dstLocalArray = dstArray
      srcLocalArray = srcArray
      call ESMF_RouteRun(route, srcLocalArray, dstLocalArray, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! set return code if user specified it
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayRedistNew

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistRelease"
!BOP
! !IROUTINE: ESMF_ArrayRedistRelease - Release resources stored for redist operation
!
! !INTERFACE:
      subroutine ESMF_ArrayRedistRelease(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     When the precomputed information about a 
!     redistribution operation is no longer
!     needed, this routine releases the associated resources.
!
!     \begin{description}
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} associated with redist operation which
!           should be released.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      call ESMF_RouteHandleDestroy(routehandle, rc=rc)

      end subroutine ESMF_ArrayRedistRelease

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRedistStore"
!BOP
! !IROUTINE: ESMF_ArrayRedistStore - Store resources for a redist operation
!
! !INTERFACE:
      subroutine ESMF_ArrayRedistStore(srcArray, srcGrid, srcDataMap, &
                                       dstArray, dstGrid, dstDataMap, &
                                       parentDElayout, routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: srcArray
      type(ESMF_Grid), intent(in) :: srcGrid
      type(ESMF_FieldDataMap), intent(in) :: srcDataMap
      type(ESMF_Array), intent(inout) :: dstArray
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_FieldDataMap), intent(in) :: dstDataMap
      type(ESMF_DELayout), intent(in) :: parentDElayout
      type(ESMF_RouteHandle), intent(out) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Precompute and associate the required data movements to redistribute
!  data over one set of {\tt ESMF\_Array}s to another
!  set of {\tt ESMF\_Array}s.  Data redistribution does no interpolation,
!  so both {\tt ESMF\_Grid}s must have identical coordinates.
!  The distribution of the {\tt ESMF\_Grid}s can be over different
!  {\tt ESMF\_DELayout}s, or the {\tt ESMF\_FieldDataMap}s can differ.
!  The {\tt routehandle} argument is associated with the stored information
!  and must be supplied to {\tt ESMF\_ArrayRedist()} to execute the
!  operation.  Call {\tt ESMF\_ArrayRedistRelease()} when this information
!  is no longer required.
!
!  The arguments are:
!   \begin{description}
!   \item[srcArray]
!    {\tt ESMF\_Array} containing the data source.
!   \item[srcGrid]
!    {\tt ESMF\_Grid} describing the grid on which the source data is arranged.
!   \item[srcDataMap]
!    {\tt ESMF\_FieldDataMap} describing how the source data maps onto the grid.
!   \item[dstArray]
!    {\tt ESMF\_Array} where the destination data will be put.
!   \item[dstGrid]
!    {\tt ESMF\_Grid} describing the grid on which the destination data is 
!    arranged.
!   \item[dstDataMap]
!    {\tt ESMF\_FieldDataMap} describing how the destination data maps 
!    onto the grid.
!   \item[parentDElayout]
!    {\tt ESMF\_DELayout} object which includes all DEs in both the
!    source and destination grids.
!   \item [routehandle]
!    Returned {\tt ESMF\_RouteHandle} which identifies this 
!    communication pattern.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

      integer :: status         ! local error status
      type(ESMF_DELayout) :: dstDElayout, srcDElayout
      type(ESMF_VM) :: vm
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
      logical :: hascachedroute     ! can we reuse an existing route?

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! create the routehandle
      routehandle = ESMF_RouteHandleCreate(status)

      ! Extract layout information from the Grids
      call ESMF_GridGet(dstGrid, delayout=dstDElayout, rc=status)
      call ESMF_GridGet(srcGrid, delayout=srcDElayout, rc=status)
      call ESMF_GridGet(srcGrid, dimCount=gridrank, rc=status)

      ! Our DE number in the layout and the total number of DEs
      call ESMF_DELayoutGet(dstDElayout, deCount=nDEs, localDe=dstMyDE, &
                               rc=status)
      call ESMF_DELayoutGet(srcDElayout, localDE=srcMyDE, rc=status)

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

      ! Query the datamap and set info for grid so it knows how to
      ! match up the array indicies and the grid indicies.
      call ESMF_FieldDataMapGet(dstDataMap, horzRelLoc=dstHorzRelLoc, &
                           vertRelLoc=dstVertRelLoc, &
                           dataIndexList=dstDimOrder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      call ESMF_FieldDataMapGet(srcDataMap, horzRelLoc=srcHorzRelLoc, &
                           vertRelLoc=srcVertRelLoc, &
                           dataIndexList=srcDimOrder, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Extract more information from the Grids
      ! TODO: get decompids?
      call ESMF_GridGet(dstGrid, &
                        horzRelLoc=dstHorzRelLoc, vertRelLoc=dstVertRelLoc, &
                        globalCellCountPerDim=dstCellCountPerDim, &
                        globalStartPerDEPerDim=dstStartPerDEPerDim, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      call ESMF_GridGet(srcGrid, &
                        horzRelLoc=srcHorzRelLoc, vertRelLoc=srcVertRelLoc, &
                        globalCellCountPerDim=srcCellCountPerDim, &
                        globalStartPerDEPerDim=srcStartPerDEPerDim, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! And get the Array sizes
      call ESMF_ArrayGet(srcArray, rank=datarank, counts=dimlengths, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! TODO: apply dimorder and decompids to get mapping of array to data

      ! set up things we need to find a cached route or precompute one
      call ESMF_ArrayGetAllAxisIndices(dstArray, dstGrid, dstDataMap, &
                                       compindex =dstCLocalAI, &
                                       totalindex=dstTLocalAI, rc=status)
      call ESMF_ArrayGetAllAxisIndices(srcArray, srcGrid, srcDataMap, &
                                       compindex =srcCLocalAI, &
                                       totalindex=srcTLocalAI, rc=status)

      ! translate AI's into global numbering
      call ESMF_GridDELocalToGlobalAI(dstGrid, &
                                      horzRelLoc=dstHorzRelLoc, &
                                      vertRelLoc=dstVertRelLoc, &
                                      localAI2D=dstCLocalAI, &
                                      globalAI2D=dstCompAI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_GridDELocalToGlobalAI(dstGrid, &
                                      horzRelLoc=dstHorzRelLoc, &
                                      vertRelLoc=dstVertRelLoc, &
                                      localAI2D=dstTLocalAI, &
                                      globalAI2D=dstTotalAI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_GridDELocalToGlobalAI(srcGrid, &
                                      horzRelLoc=srcHorzRelLoc, &
                                      vertRelLoc=srcVertRelLoc, &
                                      localAI2D=srcCLocalAI, &
                                      globalAI2D=srcCompAI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      call ESMF_GridDELocalToGlobalAI(srcGrid, &
                                      horzRelLoc=srcHorzRelLoc, &
                                      vertRelLoc=srcVertRelLoc, &
                                      localAI2D=srcTLocalAI, &
                                      globalAI2D=srcTotalAI, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Does this same route already exist?  If so, then we can drop
      ! down immediately to RouteRun.
      call ESMF_RouteGetCached(datarank, &
                            dstMyDE, dstCompAI, dstTotalAI, nDEs, dstDElayout, &
                            srcMyDE, srcCompAI, srcTotalAI, nDEs, srcDElayout, &
                            periodic, hascachedroute, route, status)

      if (.not. hascachedroute) then
          ! Get the associated VM
          call ESMF_DELayoutGetVM(parentDElayout, vm, rc=status)

          ! Create the route object.
          route = ESMF_RouteCreate(vm, rc)

          call ESMF_RoutePrecomputeRedist(route, datarank, dstMyDE, dstCompAI, &
                                       dstTotalAI, dstStartPerDEPerDim, &
                                       dstCellCountPerDim, dstDElayout, &
                                       srcMyDE, srcCompAI, srcTotalAI, &
                                       srcStartPerDEPerDim, &
                                       srcCellCountPerDim, srcDElayout, status)
      endif

      ! and set route into routehandle object
      call ESMF_RouteHandleSet(routehandle, route1=route, &
                               htype=ESMF_REDISTHANDLE, rc=status)

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
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayRedistStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayScatter"
!BOPI
! !IROUTINE: ESMF_ArrayScatter - Scatter a single Array across multiple DEs
!
! !INTERFACE:
      subroutine ESMF_ArrayScatter(array, delayout, decompids, rootDE, &
                                   scatteredArray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_DELayout), intent(in) :: delayout
      integer, dimension(:), intent(in) :: decompids
      integer, intent(in) :: rootDE
      type(ESMF_Array), intent(out) :: scatteredArray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Scatter a single {\tt ESMF\_Array} on one DE into
!  a distributed {\tt ESMF\_Array} over multiple DEs.
!
!  The arguments are:
!     \begin{description}
!     \item [array]
!           {\tt ESMF\_Array} containing undistributed data to be scattered.
!     \item [grid]
!           {\tt ESMF\_Grid} which will correspond to the distributed data.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap} which describes the mapping of the
!           data onto the cells in the {\tt ESMF\_Grid}.
!     \item [rootDE]
!           The DE number on which the source {\tt ESMF\_Array} is located.
!     \item [scatteredArray]
!           The resulting distributed {\tt ESMF\_Array} after scattering.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

        integer :: localrc         ! local error status
        integer :: size_decomp

        ! initialize return code; assume failure until success is certain
        if (present(rc)) rc = ESMF_FAILURE
 
        ! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayScatter(array, delayout, decompids, size_decomp, &
                                 rootDE, scatteredArray, localrc)

        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! set return code if user specified it
        if (present(rc)) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayScatter

       end module ESMF_ArrayCommMod









