! $Id: ESMF_RegridTypes.F90,v 1.38 2004/04/13 22:58:41 jwolfe Exp $
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
!     ESMF Regrid Types Module
      module ESMF_RegridTypesMod
!
!==============================================================================
!
! This file contains the Regrid class definition and a few simple Regrid class
! methods.  The remaining class methods are included in another module in
! order to facilitate easy branching of Regrid creation based on regrid method.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!BOPI
! !MODULE: ESMF_RegridTypesMod - Regridding and interpolation data types
!
! !DESCRIPTION:
!
! The code in this file implements parts of the Regrid class.  Regrid is 
! responsible for any regridding and interpolation required for ESMF 
! applications.
! Regridding includes any process that transforms a field from one ESMF
! grid to another, including:
! \begin{itemize}
! \item bilinear or bicubic interpolation
! \item conservative remapping
! \item spectral or other functional transforms
! \item sub-sampling, super-sampling or shifting.
! \end{itemize}
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod       ! ESMF base   class
      use ESMF_newDELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_DataMapMod
      use ESMF_ArrayMod      ! ESMF array  class
      use ESMF_DistGridMod   ! ESMF distributed grid class
      use ESMF_PhysGridMod   ! ESMF physical grid class
      use ESMF_GridTypesMod  ! ESMF grid   class
      use ESMF_GridMod       ! ESMF grid   class
      use ESMF_RHandleMod    ! ESMF route handle class
      use ESMF_RouteMod      ! ESMF route  class
      use ESMF_ArrayCommMod

      implicit none

!------------------------------------------------------------------------------
!     !  ESMF_RegridType
!
!     ! Description of ESMF_RegridType.

      type ESMF_RegridType
      sequence
      private
        type (ESMF_Base) :: base

        type (ESMF_Array) :: & 
           srcArray,   &! source array
           dstArray     ! destination array

        type (ESMF_Grid) :: &
           srcGrid,    &! source grid
           dstGrid      ! destination grid

        type (ESMF_DataMap) :: &
           srcDatamap,    &! source datamap
           dstDatamap      ! destination datamap

        integer ::       &
           method,       &! method used for this regridding
           numLinks       ! number of unique links between grids

        type (ESMF_Array), pointer :: &
           srcAdd,       &! addresses of source field for regrid operation
           dstAdd         ! addresses of destination field for regrid op

        type (ESMF_Array), pointer :: &
           weights        ! array of weights for performing the
                          ! regridding transformation

        integer ::       &
           redistrbOption ! option for redistributing data for regrid

        type (ESMF_Route), pointer :: & ! pre-stored redistribution patterns
           gather,       &! not sure, just trying to get it to compile jw
           srcRoute,     &! route for redistribution on source side
           dstRoute       ! route for redistribution on destination side

      end type


!------------------------------------------------------------------------------
! !PUBLIC DATA MEMBERS:
!
  
      integer, parameter, public ::       &! supported regrid methods
         ESMF_RegridMethod_none     =  0, &! no regridding or undefined regrid
         ESMF_RegridMethod_FieldCopy=  1, &! same Grid so copy field
         ESMF_RegridMethod_Redist   =  2, &! same PhysGrid, just redistribute
         ESMF_RegridMethod_Bilinear =  3, &! bilinear (logically-rect grids)
         ESMF_RegridMethod_Bicubic  =  4, &! bicubic  (logically-rect grids)
         ESMF_RegridMethod_Conserv1 =  5, &! 1st-order conservative
         ESMF_RegridMethod_Conserv2 =  6, &! 2nd-order conservative
         ESMF_RegridMethod_Raster   =  7, &! regrid by rasterizing domain
         ESMF_RegridMethod_NearNbr  =  8, &! nearest-neighbor dist-weighted avg
         ESMF_RegridMethod_Fourier  =  9, &! Fourier transform
         ESMF_RegridMethod_Legendre = 10, &! Legendre transform
         ESMF_RegridMethod_Index    = 11, &! index-space regrid (shift, stencil)
         ESMF_RegridMethod_Linear   = 12, &! linear for 1-d regridding
         ESMF_RegridMethod_Spline   = 13, &! cubic spline for 1-d regridding
         ESMF_RegridMethod_RegridCopy=51, &! copy existing regrid
         ESMF_RegridMethod_Shift    = 52, &! shift addresses of existing regrid
         ESMF_RegridMethod_Adjoint  = 53, &! create adjoint of existing regrid
         ESMF_RegridMethod_File     = 89, &! read a regrid from a file
         ESMF_RegridMethod_User     = 90   ! user-supplied method

      integer, parameter, public ::     &! options for field data motion
         ESMF_RegridDistrb_None   =  0, &! no data motion required or undefined
         ESMF_RegridDistrb_Source =  1, &! redistribute source field
         ESMF_RegridDistrb_Dest   =  2, &! redistribute destination field
         ESMF_RegridDistrb_Both   =  3   ! redistribute both 

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_RegridType

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
    public ESMF_RegridAddLink        ! Adds address pair and weight to regrid
    public ESMF_RegridRouteConstruct ! Constructs a Route used by Regrid to
                                     ! gather data
    public ESMF_RegridTypeGet        ! returns a regrid attribute
    public ESMF_RegridTypeSet        ! sets    a regrid attribute
    public ESMF_RegridConstructEmpty ! creates an empty regrid structure
    public ESMF_RegridDestruct       ! deallocate memory associated with a regrid

!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridTypes.F90,v 1.38 2004/04/13 22:58:41 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOPI
! !INTERFACE:
      interface ESMF_RegridAddLink

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridAddLink1D
         module procedure ESMF_RegridAddLink2D

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_Regrid} add
!     link methods.

!EOPI
      end interface
!==============================================================================

      contains

!==============================================================================
!
! This section includes utility routines for performing actions on the
! main Regrid type.  All other create methods are implemented in another
! module to facilitate branching based on type of regridding required.
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridAddLink1D - Adds address pair and regrid weight to regrid

! !INTERFACE:
      subroutine ESMF_RegridAddLink1D(tv, srcAdd, dstAdd, weight, rc)
!
! !ARGUMENTS:

      type(ESMF_TransformValues), intent(inout) :: tv
      integer, intent(in) :: srcAdd
      integer, intent(in) :: dstAdd
      real(kind=ESMF_KIND_R8), intent(in) :: weight
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Adds an address pair and regrid weight to an existing regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[transformvalues]
!          Stored information related to the actual data transformation
!          needed when moving data from one grid to another.
!     \item[srcAdd]
!          Address in source field array for this link.
!     \item[dstAdd]
!          Address in destination field array for this link.
!     \item[weights]
!          Regrid weight for this link.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!  TODO
      logical :: rcpresent
      integer :: status
      integer, dimension(:), pointer :: srcPtr, dstPtr
      integer :: numList
      real(kind=ESMF_KIND_R8), dimension(:), pointer :: wgtPtr
      type(ESMF_LocalArray) :: srcIndex, dstIndex, weights
      type (ESMF_Array) :: &! temps for use when re-sizing arrays
         srcAddTmp, dstAddTmp, weightsTmp

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_TransformValuesGet(tv, numList=numList, srcIndex=srcIndex, &
                                   dstIndex=dstIndex, weights=weights, rc=rc)
      call ESMF_LocalArrayGetData(srcIndex, srcPtr, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(dstIndex, dstPtr, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(weights , wgtPtr, ESMF_DATA_REF, rc)

      ! increment number of links for this regrid
      numList = numList + 1

      !  if new number of links exceeds array sizes, re-size arrays
      ! TODO: resize check

      ! Add addresses and weights to regrid arrays
      srcPtr(numList) = srcAdd
      dstPtr(numList) = dstAdd
      wgtPtr(numList) = weight
      call ESMF_TransformValuesSet(tv, numList=numList, srcIndex=srcIndex, &
                                   dstIndex=dstIndex, weights=weights, rc=rc)

      rc = ESMF_SUCCESS

      end subroutine ESMF_RegridAddLink1D

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridAddLink2D - Adds address pair and regrid weight to regrid

! !INTERFACE:
      subroutine ESMF_RegridAddLink2D(tv, srcAdd, dstAdd, weight, rc)
!
! !ARGUMENTS:

      type(ESMF_TransformValues), intent(inout) :: tv
      integer, intent(in) :: srcAdd
      integer, dimension(2), intent(in) :: dstAdd
      real(kind=ESMF_KIND_R8), intent(in) :: weight
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Adds an address pair and regrid weight to an existing regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[tv]
!          Stored information related to the actual data transformation
!          needed when moving data from one grid to another.
!     \item[srcAdd]
!          Address in source field array for this link.
!     \item[dstAdd]
!          Address in destination field array for this link.
!     \item[weights]
!          Regrid weight for this link.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!  TODO
      logical :: rcpresent
      integer :: status
      integer, dimension(:), pointer :: srcPtr, dstPtr
      integer :: numList
      real(kind=ESMF_KIND_R8), dimension(:), pointer :: wgtPtr
      type(ESMF_LocalArray) :: srcIndex, dstIndex, weights
      type (ESMF_Array) :: &! temps for use when re-sizing arrays
         srcAddTmp, dstAddTmp, weightsTmp

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_TransformValuesGet(tv, numList=numList, srcIndex=srcIndex, &
                                   dstIndex=dstIndex, weights=weights, rc=rc)
      call ESMF_LocalArrayGetData(srcIndex, srcPtr, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(dstIndex, dstPtr, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(weights , wgtPtr, ESMF_DATA_REF, rc)

      ! increment number of links for this regrid
      numList = numList + 1

      !  if new number of links exceeds array sizes, re-size arrays
      ! TODO: resize check

      ! Add addresses and weights to regrid arrays
      dstPtr(2*(numList-1)+1) = dstAdd(1)
      dstPtr(2*(numList-1)+2) = dstAdd(2)
      srcPtr(numList) = srcAdd
      wgtPtr(numList) = weight
      call ESMF_TransformValuesSet(tv, numList=numList, srcIndex=srcIndex, &
                                   dstIndex=dstIndex, weights=weights, rc=rc)

      rc = ESMF_SUCCESS

      end subroutine ESMF_RegridAddLink2D

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridRouteConstruct - Constructs a Route used to gather data

! !INTERFACE:
      function ESMF_RegridRouteConstruct(dimCount, srcGrid, dstGrid, &
                                         recvDomainList, srcDatamap, srcArray, &
                                         dstDatamap, dstArray, reorder, total, &
                                         rc)
!
! !RETURN VALUE:
      type(ESMF_Route) :: ESMF_RegridRouteConstruct
!
! !ARGUMENTS:

      integer, intent(in) :: dimCount
      type(ESMF_Grid), intent(in) :: srcGrid
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_DomainList), intent(inout) :: recvDomainList
      type(ESMF_DataMap), intent(in) :: srcDatamap
      type(ESMF_Array), intent(in), optional :: srcArray
      type(ESMF_DataMap), intent(in), optional :: dstDatamap
      type(ESMF_Array), intent(in), optional :: dstArray
      logical, intent(in), optional :: reorder
      logical, intent(in), optional :: total
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Adds an address pair and regrid weight to an existing regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:
!EOPI
!TODO: Leave here or move to Route?
      logical :: rcpresent
      integer :: status
      integer :: myDE, gridrank, nDEs, theirDE, i, j
      integer, dimension(:), allocatable :: dimOrder
      logical :: totalUse
      real(ESMF_KIND_R8) :: count
      real(ESMF_KIND_R8), dimension(:), allocatable :: dstMin, dstMax
      real(ESMF_KIND_R8), dimension(:), allocatable :: srcMin, srcMax
      type(ESMF_AxisIndex), dimension(:), allocatable :: myAI, myArrayAI, &
                                                         myArrayLocalAI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: allAI, allLocalAI
      type(ESMF_newDELayout) :: srcDELayout
      type(ESMF_DomainList) :: sendDomainList
      type(ESMF_RelLoc) :: horzRelLoc
      type(ESMF_Route) :: route

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     use optional arguments if present
      totalUse = .false.
      if(present(total)) totalUse = total

      call ESMF_GridGetDELayout(srcGrid, srcDELayout, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridRouteConstruct: GridGetDELayout ", &
                 "returned failure"
        return
      endif
      call ESMF_newDELayoutGet(srcDELayout, localDE=myDE, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridRouteConstruct: DELayoutGet ", &
                 "returned failure"
        return
      endif

      ! Extract some layout information for use in this regrid.
      call ESMF_GridGet(srcGrid, dimCount=gridrank, rc=status)
      allocate (myAI(gridrank))
      call ESMF_DataMapGet(srcDataMap, horzRelloc=horzRelLoc, rc=status)
      call ESMF_GridGetDE(srcGrid, horzRelLoc=horzRelLoc, &
                          globalAIPerDim=myAI, reorder=reorder, &
                          total=totalUse, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridRouteConstruct: GridGetDE ", &
                 "returned failure"
        return
      endif

      ! From each grid get the bounding box information on this DE
      call ESMF_GridGet(srcGrid, dimCount=gridrank, rc=status)
      allocate (srcMin(gridrank))
      allocate (srcMax(gridrank))
      call ESMF_GridGet(dstGrid, dimCount=gridrank, rc=status)
      allocate (dstMin(gridrank))
      allocate (dstMax(gridrank))
      call ESMF_DataMapGet(srcDataMap, horzRelloc=horzRelLoc, rc=status)
      call ESMF_GridGetDE(srcGrid, horzRelLoc=horzRelLoc, &
                          minLocalCoordPerDim=srcMin, &
                          maxLocalCoordPerDim=srcMax, &
                          reorder=.false., rc=status)
      call ESMF_DataMapGet(dstDataMap, horzRelloc=horzRelLoc, rc=status)
      call ESMF_GridGetDE(dstGrid, horzRelLoc=horzRelLoc, &
                          minLocalCoordPerDim=dstMin, &
                          maxLocalCoordPerDim=dstMax, &
                          reorder=.false., rc=status)

      ! calculate intersections
      call ESMF_GridBoxIntersectSend(dstGrid, srcGrid, srcMin, srcMax, &
                                     myAI, sendDomainList, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridRouteConstruct: GridBoxIntersectSend ", &
                 "returned failure"
        return
      endif
      call ESMF_GridBoxIntersectRecv(srcGrid, dstMin, dstMax, &
                                     recvDomainList, total=totalUse, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridRouteConstruct: GridBoxIntersectRecv ", &
                 "returned failure"
        return
      endif

      ! Modify DomainLists for Array dimensions larger than Grid dimensions
      ! TODO: move this to its own subroutine?
      if ((dimCount.gt.gridrank) .and. (present(srcArray))) then ! TODO: fill in
      ! sendDomainList first
        allocate(myArrayAI(dimCount))
        allocate(myArrayLocalAI(dimCount))
        allocate(dimOrder(dimCount))
        if (totalUse) then
          call ESMF_ArrayGetAxisIndex(srcArray, totalindex=myArrayAI, rc=status)
        else
          call ESMF_ArrayGetAxisIndex(srcArray, compindex=myArrayAI, rc=status)
        endif
        call ESMF_DataMapGet(srcDataMap, dataIorder=dimOrder, rc=status)
        do i = 1,sendDomainList%num_domains
          do j = 1,sendDomainList%domains(i)%rank
            myAI(j) = sendDomainList%domains(i)%ai(j)
          enddo
          sendDomainList%domains(i)%rank = dimCount
          do j = 1,dimCount
            if (dimOrder(j).eq.1) then
              sendDomainList%domains(i)%ai(j) = myAI(1)
            elseif (dimOrder(j).eq.2) then
              sendDomainList%domains(i)%ai(j) = myAI(2)
            elseif (dimOrder(j).eq.0) then
              sendDomainList%domains(i)%ai(j) = myArrayAI(j)
            else
              !TODO: add error
            endif
          enddo
        enddo
        do j = 1,dimCount
          if (dimOrder(j).eq.0) then
            count = myArrayAI(j)%max - myArrayAI(j)%min + 1
            sendDomainList%total_points = sendDomainList%total_points*count
          endif
        enddo

      ! recvDomainList next
        call ESMF_newDELayoutGet(srcDELayout, deCount=nDEs, status)
        allocate(allAI(nDEs,dimCount))
        allocate(allLocalAI(nDEs,dimCount))
        if (totalUse) then
          call ESMF_ArrayGetAllAxisIndices(srcArray, srcGrid, srcDataMap, &
                                           totalindex=allAI, rc=status)
        else
          call ESMF_ArrayGetAllAxisIndices(srcArray, srcGrid, srcDataMap, &
                                           compindex=allAI, rc=status)
        endif
        do i = 1,recvDomainList%num_domains
          theirDE = recvDomainList%domains(i)%DE + 1
          do j = 1,recvDomainList%domains(i)%rank
            myAI(j) = recvDomainList%domains(i)%ai(j)
          enddo
          recvDomainList%domains(i)%rank = dimCount
          do j = 1,dimCount
            if (dimOrder(j).eq.1) then
              recvDomainList%domains(i)%ai(j) = myAI(1)
            elseif (dimOrder(j).eq.2) then
              recvDomainList%domains(i)%ai(j) = myAI(2)
            elseif (dimOrder(j).eq.0) then
              recvDomainList%domains(i)%ai(j) = allAI(theirDE,j)
            else
              !TODO: add error
            endif
          enddo
        enddo
        do j = 1,dimCount
          if (dimOrder(j).eq.0) then
            count = allAI(theirDE,j)%max - allAI(theirDE,j)%min + 1
            recvDomainList%total_points = recvDomainList%total_points*count
          endif
        enddo

        deallocate(dimOrder)
        deallocate(myArrayAI)
        deallocate(myArrayLocalAI)
        deallocate(allAI)
        deallocate(allLocalAI)
      endif

      ! Create Route
      ! TODO: this must be either a parent layout, or the src and dst layouts
      !  must be identical.
      route = ESMF_RouteCreate(srcDELayout, status)
      call ESMF_RoutePrecomputeDomList(route, dimCount, myDE, sendDomainList, &
                                       recvDomainList, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in RegridRouteConstruct: ", &
                 "RoutePrecomputeDomList returned failure"
        return
      endif
      ! set size of recv items in Route
      call ESMF_RouteSetRecvItems(route, recvDomainList%total_points, status)

      ! Clean up
      deallocate(myAI)
      deallocate(srcMin)
      deallocate(srcMax)
      deallocate(dstMin)
      deallocate(dstMax)

      ! Set return values
      ESMF_RegridRouteConstruct = route
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_RegridRouteConstruct

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridTypeGet - Get attribute of a Regrid type

! !INTERFACE:
      subroutine ESMF_RegridTypeGet(regrid, name, srcArray, dstArray, &
                                    srcGrid, dstGrid, srcDatamap, dstDatamap, &
                                    method, numLinks, gather, rc)
!
! !ARGUMENTS:

      type(ESMF_RegridType), intent(in) :: regrid
      character (*), intent(out), optional :: name
      type (ESMF_Array), intent(out), optional :: srcArray, dstArray
      type (ESMF_Grid), intent(out), optional :: srcGrid,  dstGrid
      type (ESMF_DataMap), intent(out), optional :: srcDatamap,  dstDatamap
      integer, intent(out), optional :: method
      integer, intent(out), optional :: numLinks
      type (ESMF_Route), intent(out), optional :: gather
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns the value of a Regrid attribute.  The
!     attribute is specified through optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Regrid type to be queried.
!     \item[{[name]}]
!          Name for this regrid.
!     \item[{[srcArray]}]
!          
!     \item[{[dstArray]}]
!          
!     \item[{[srcGrid]}]
!          
!     \item[{[dstGrid]}]
!          
!     \item[{[method]}]
!          Integer enum of method used in this regrid.
!     \item[{[numLinks]}]
!          Number of unique links between grids for this regrid.
!     \item[{[gather]}]
!          Route used to gather non-local information to perform regrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      logical :: rcpresent
      integer :: stat, status

      status = ESMF_FAILURE
      rcpresent=.FALSE.

!     Initialize return code
      status = ESMF_SUCCESS
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
      
      ! Get name if requested
      if (present(name)) then
         call ESMF_GetName(regrid%base, name, stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      ! Get arrays if requested
      if (present(srcArray)) then
         srcArray = regrid%srcArray
      endif
      if (present(dstArray)) then
         dstArray = regrid%dstArray
      endif

      ! Get grids if requested
      if (present(srcGrid)) then
         srcGrid = regrid%srcGrid
      endif
      if (present(dstGrid)) then
         dstGrid = regrid%dstGrid
      endif

      ! Get datamaps if requested
      if (present(srcDatamap)) then
         srcDatamap = regrid%srcDatamap
      endif
      if (present(dstDatamap)) then
         dstDatamap = regrid%dstDatamap
      endif

      ! get method or number of links
      if (present(method))   method   = regrid%method
      if (present(numLinks)) numLinks = regrid%numLinks
      if (present(gather))   gather   = regrid%gather

      if (rcpresent) rc = status

      end subroutine ESMF_RegridTypeGet

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridTypeSet - Set attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridTypeSet(regrid, name,              &
                                    srcArray, dstArray,        &
                                    srcGrid,  dstGrid,         &
                                    srcDatamap,  dstDatamap,   &
                                    method, numLinks, gather, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(inout) :: regrid

      character (*),       intent(in ), optional :: name
      type (ESMF_Array),   intent(in ), optional :: srcArray,   dstArray
      type (ESMF_Grid),    intent(in ), optional :: srcGrid,    dstGrid
      type (ESMF_DataMap), intent(in ), optional :: srcDatamap, dstDatamap
      integer,             intent(in ), optional :: method
      integer,             intent(in ), optional :: numLinks
      type (ESMF_Route),   intent(in ), optional :: gather
      integer,             intent(out), optional :: rc

!
! !DESCRIPTION:
!     Sets a Regrid attribute with the given value.
!     The attribute is determined by optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be modified.
!     \item[{[name]}]
!          Name for this regrid.
!     \item[{[srcArray]}]
!          
!     \item[{[dstArray]}]
!          
!     \item[{[srcGrid]}]
!          
!     \item[{[dstGrid]}]
!         
!     \item[{[method]}]
!          Integer enum of method used in this regrid.
!     \item[{[numLinks]}]
!          Number of unique links between grids for this regrid.
!     \item[{[gather]}]
!          Route used to gather non-local information to perform regrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:
!  TODO

      logical :: rcpresent
      integer :: stat, status

      status = ESMF_SUCCESS
      rcpresent=.FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
      
      ! Set name if requested
      if (present(name)) then
         call ESMF_SetName(regrid%base, name, "Regrid", stat)
         if (stat /= ESMF_SUCCESS) status = ESMF_FAILURE
      endif

      ! Set arrays if requested
      if (present(srcArray)) regrid%srcArray = srcArray
      if (present(dstArray)) regrid%dstArray = dstArray
      
      ! Set grids if requested
      if (present(srcGrid)) regrid%srcGrid = srcGrid
      if (present(dstGrid)) regrid%dstGrid = dstGrid
      
      ! Set datamaps if requested
      if (present(srcDatamap)) regrid%srcDatamap = srcDatamap
      if (present(dstDatamap)) regrid%dstDatamap = dstDatamap
      
      ! get method or number of links
      if (present(method))   regrid%method   = method
      if (present(numLinks)) regrid%numLinks = numLinks
      if (present(gather))   regrid%gather   = gather

      if (rcpresent) rc = status

      end subroutine ESMF_RegridTypeSet

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridConstructEmpty - Create empty regrid structure

! !INTERFACE:
      subroutine ESMF_RegridConstructEmpty(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(inout) :: regrid
      integer, intent(out) :: rc
!
! !DESCRIPTION:
!     ESMF routine which creates and initializes a regrid structure.
!     The structure is later filled with appropriate data using the
!     set function. Intended for internal ESMF use only; end-users 
!     use {\tt ESMF\_Create} functions.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          The regrid object to be initialized.
!     \item[rc]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

        ! initialize the base object
        call ESMF_BaseCreate(regrid%base, "Regrid", rc=rc)

        ! nullify pointers
      
        nullify(regrid%srcAdd)
        nullify(regrid%dstAdd)
        nullify(regrid%weights)
        nullify(regrid%gather)

        ! initialize scalars
        
        regrid%method   = ESMF_RegridMethod_none
        regrid%numLinks = 0

        rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructEmpty

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_RegridDestruct - Free any Regrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_RegridDestruct(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(inout) :: regrid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_RegridConstruct}, does any additional cleanup before the
!     original Regrid object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_RegridDestroy}, which calls
!     {\tt ESMF_RegridDestruct}.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          The regrid object to be destructed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      regrid%method   = ESMF_RegridMethod_none
      regrid%numLinks = 0

!     deallocate regrid transform
      !TODO
      !destroy srcAdd, dstAdd, weights ESMF arrays
      nullify(regrid%srcAdd)
      nullify(regrid%dstAdd)
      nullify(regrid%weights)

!     destroy communication structures
      !TODO
      !destroy route regrid%gather
      nullify(regrid%gather)

      ! and free anything associated with the base object
      call ESMF_BaseDestroy(regrid%base, status)

      if(rcpresent) rc = status

      end subroutine ESMF_RegridDestruct

!------------------------------------------------------------------------------

      end module ESMF_RegridTypesMod
