! $Id: ESMF_RegridTypes.F90,v 1.52 2004/06/08 09:27:20 nscollins Exp $
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
#define ESMF_FILENAME "ESMF_RegridTypes.F90"
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
      use ESMF_BaseTypesMod
      use ESMF_BaseMod       ! ESMF base   class
      use ESMF_LogErrMod
      use ESMF_DELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayDataMapMod
      use ESMF_ArrayMod      ! ESMF array  class
      use ESMF_DistGridMod   ! ESMF distributed grid class
      use ESMF_PhysGridMod   ! ESMF physical grid class
      use ESMF_GridTypesMod  ! ESMF grid   class
      use ESMF_GridMod       ! ESMF grid   class
      use ESMF_RHandleMod    ! ESMF route handle class
      use ESMF_RouteMod      ! ESMF route  class
      use ESMF_FieldDataMapMod
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

        type (ESMF_FieldDataMap) :: &
           srcDatamap,    &! source datamap
           dstDatamap      ! destination datamap

        integer ::       &
           method         ! method used for this regridding

        integer ::       &
           redistrbOption ! option for redistributing data for regrid

      end type

!------------------------------------------------------------------------------
!     !  ESMF_Regrid
!
!     !  The Regrid data structure that is passed between languages.

      type ESMF_Regrid
      sequence
      private
        type (ESMF_RegridType), pointer :: ptr     ! pointer to a regrid type
      end type

!------------------------------------------------------------------------------
! !PUBLIC DATA MEMBERS:
!
  
      integer, parameter, public ::          &! supported regrid methods
         ESMF_REGRID_METHOD_NONE       =  0, &! no regridding or undefined regrid
         ESMF_REGRID_METHOD_BILINEAR   =  1, &! bilinear (logically-rect grids)
         ESMF_REGRID_METHOD_BICUBIC    =  2, &! bicubic  (logically-rect grids)
         ESMF_REGRID_METHOD_CONSERV1   =  3, &! 1st-order conservative
         ESMF_REGRID_METHOD_CONSERV2   =  4, &! 2nd-order conservative
         ESMF_REGRID_METHOD_RASTER     =  5, &! regrid by rasterizing domain
         ESMF_REGRID_METHOD_NEAR_NBR   =  6, &! nearest-neighbor dist-weighted avg
         ESMF_REGRID_METHOD_FOURIER    =  7, &! Fourier transform
         ESMF_REGRID_METHOD_LEGENDRE   =  8, &! Legendre transform
         ESMF_REGRID_METHOD_INDEX      =  9, &! index-space regrid (shift, stencil)
         ESMF_REGRID_METHOD_LINEAR     = 10, &! linear for 1-d regridding
         ESMF_REGRID_METHOD_SPLINE     = 11, &! cubic spline for 1-d regridding
         ESMF_REGRID_METHOD_REGRIDCOPY = 51, &! copy existing regrid
         ESMF_REGRID_METHOD_SHIFT      = 52, &! shift addresses of existing regrid
         ESMF_REGRID_METHOD_ADJOINT    = 53, &! create adjoint of existing regrid
         ESMF_REGRID_METHOD_FILE       = 89, &! read a regrid from a file
         ESMF_REGRID_METHOD_USER       = 90   ! user-supplied method

      integer, parameter, public ::      &! options for field data motion
         ESMF_REGRID_DISTRB_NONE   =  0, &! no data motion required or undefined
         ESMF_REGRID_DISTRB_SOURCE =  1, &! redistribute source field
         ESMF_REGRID_DISTRB_DEST   =  2, &! redistribute destination field
         ESMF_REGRID_DISTRB_BOTH   =  3   ! redistribute both 

      integer, parameter, public ::     &! options for normalization
         ESMF_REGRID_NORM_UNKNOWN  = 0, &! unknown or undefined normalization
         ESMF_REGRID_NORM_NONE     = 1, &! no normalization
         ESMF_REGRID_NORM_DSTAREA  = 2, &
         ESMF_REGRID_NORM_FRACAREA = 3

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Regrid

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
    public ESMF_RegridAddLink        ! Adds address pair and weight to regrid
    public ESMF_RegridRouteConstruct ! Constructs a Route used by Regrid to
                                     ! gather data
    public ESMF_RegridGet            ! returns a regrid attribute
    public ESMF_RegridSet            ! sets    a regrid attribute
    public ESMF_RegridCreateEmpty    ! creates an empty regrid structure
    public ESMF_RegridConstructEmpty ! constructs an empty regrid structure
    public ESMF_RegridDestruct       ! deallocate memory associated with a regrid

!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridTypes.F90,v 1.52 2004/06/08 09:27:20 nscollins Exp $'

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
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridAddLink1D"
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
! !REQUIREMENTS:  TODO

      integer :: localrc
      integer, dimension(:), pointer :: srcPtr, dstPtr
      integer :: numList
      real(kind=ESMF_KIND_R8), dimension(:), pointer :: wgtPtr
      type(ESMF_LocalArray) :: srcIndex, dstIndex, weights
      type (ESMF_Array) :: &! temps for use when re-sizing arrays
         srcAddTmp, dstAddTmp, weightsTmp

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_TransformValuesGet(tv, numList=numList, srcIndex=srcIndex, &
                                   dstIndex=dstIndex, weights=weights, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_LocalArrayGetData(srcIndex, srcPtr, ESMF_DATA_REF, localrc)
      call ESMF_LocalArrayGetData(dstIndex, dstPtr, ESMF_DATA_REF, localrc)
      call ESMF_LocalArrayGetData(weights , wgtPtr, ESMF_DATA_REF, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! increment number of links for this regrid
      numList = numList + 1

      !  if new number of links exceeds array sizes, re-size arrays
      ! TODO: resize check

      ! Add addresses and weights to regrid arrays
      srcPtr(numList) = srcAdd
      dstPtr(numList) = dstAdd
      wgtPtr(numList) = weight
      call ESMF_TransformValuesSet(tv, numList=numList, srcIndex=srcIndex, &
                                   dstIndex=dstIndex, weights=weights, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridAddLink1D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridAddLink2D"
!BOPI
! !IROUTINE: ESMF_RegridAddLink2D - Adds address pair and regrid weight to regrid

! !INTERFACE:
      subroutine ESMF_RegridAddLink2D(tv, srcAdd, dstAdd, weight, aggregate, rc)
!
! !ARGUMENTS:

      type(ESMF_TransformValues), intent(inout) :: tv
      integer, intent(in) :: srcAdd
      integer, dimension(2), intent(in) :: dstAdd
      real(kind=ESMF_KIND_R8), intent(in) :: weight
      logical, intent(in), optional :: aggregate
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
!     \item[{[aggregate]}]
!          Logical flag to indicate whether the current list of links should
!          be searched for an already occurring weight corresponding to the
!          current address pair and the current weight added on instead of
!          making a new link.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: !  TODO

      integer :: localrc
      integer, dimension(:), pointer :: srcPtr, dstPtr
      integer :: numList, i, i1
      logical :: aggregateUse, newLink
      real(kind=ESMF_KIND_R8), dimension(:), pointer :: wgtPtr
      type(ESMF_LocalArray) :: srcIndex, dstIndex, weights
      type (ESMF_Array) :: &! temps for use when re-sizing arrays
         srcAddTmp, dstAddTmp, weightsTmp

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      newLink = .true.
      aggregateUse = .false.
      if (present(aggregate)) aggregateUse=aggregate

      call ESMF_TransformValuesGet(tv, numList=numList, srcIndex=srcIndex, &
                                   dstIndex=dstIndex, weights=weights, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_LocalArrayGetData(srcIndex, srcPtr, ESMF_DATA_REF, localrc)
      call ESMF_LocalArrayGetData(dstIndex, dstPtr, ESMF_DATA_REF, localrc)
      call ESMF_LocalArrayGetData(weights , wgtPtr, ESMF_DATA_REF, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! if the aggregation flag is set, search through the already existing
      ! links for the current address pair
      if (aggregateUse .and. numList.ne.0) then
        do i = 1,numList
          i1 = 2*(i-1)
          if ((dstPtr(i1+1) .eq. dstAdd(1)) .AND. &
              (dstPtr(i1+2) .eq. dstAdd(2)) .AND. &
              (srcPtr(i)    .eq. srcAdd   )) then
            newLink = .false.
            wgtPtr(i) = wgtPtr(i) + weight
            go to 10
          endif
        enddo
   10   continue
      endif

      ! if this is a new link, increment number of links for this regrid
      if (newLink) then
        numList = numList + 1

        !  if new number of links exceeds array sizes, re-size arrays
        ! TODO: resize check

        ! Add addresses and weights to regrid arrays
        dstPtr(2*(numList-1)+1) = dstAdd(1)
        dstPtr(2*(numList-1)+2) = dstAdd(2)
        srcPtr(numList) = srcAdd
        wgtPtr(numList) = weight
      endif

      call ESMF_TransformValuesSet(tv, numList=numList, srcIndex=srcIndex, &
                                   dstIndex=dstIndex, weights=weights, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridAddLink2D

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridRouteConstruct"
!BOPI
! !IROUTINE: ESMF_RegridRouteConstruct - Constructs a Route used to gather data

! !INTERFACE:
      function ESMF_RegridRouteConstruct(dimCount, srcGrid, dstGrid, &
                                         recvDomainList, parentDELayout, &
                                         srcDatamap, srcArray, &
                                         dstDatamap, dstArray, &
                                         reorder, total, rc)
!
! !RETURN VALUE:
      type(ESMF_Route) :: ESMF_RegridRouteConstruct
!
! !ARGUMENTS:

      integer, intent(in) :: dimCount
      type(ESMF_Grid), intent(in) :: srcGrid
      type(ESMF_Grid), intent(in) :: dstGrid
      type(ESMF_DomainList), intent(inout) :: recvDomainList
      type(ESMF_DELayout), intent(in) :: parentDELayout
      type(ESMF_FieldDataMap), intent(in) :: srcDatamap
      type(ESMF_Array), intent(in), optional :: srcArray
      type(ESMF_FieldDataMap), intent(in), optional :: dstDatamap
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

      integer :: localrc
      integer :: myDE, gridrank, nDEs, theirDE, i, j
      integer, dimension(:), allocatable :: dimOrder
      logical :: totalUse
      real(ESMF_KIND_R8) :: count
      real(ESMF_KIND_R8), dimension(:), allocatable :: dstMin, dstMax
      real(ESMF_KIND_R8), dimension(:), allocatable :: srcMin, srcMax
      type(ESMF_AxisIndex), dimension(:), allocatable :: myAI, myArrayAI, &
                                                         myArrayLocalAI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: allAI, allLocalAI
      type(ESMF_DELayout) :: srcDELayout
      type(ESMF_DomainList) :: sendDomainList
      type(ESMF_RelLoc) :: horzRelLoc
      type(ESMF_Route) :: route

!     Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

!     use optional arguments if present
      totalUse = .false.
      if (present(total)) totalUse = total

      call ESMF_GridGet(srcGrid, delayout=srcDELayout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_DELayoutGet(srcDELayout, localDE=myDE, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Extract some layout information for use in this regrid.
      call ESMF_GridGet(srcGrid, dimCount=gridrank, rc=localrc)
      allocate (myAI(gridrank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "myAI", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_FieldDataMapGet(srcDataMap, horzRelloc=horzRelLoc, rc=localrc)
      call ESMF_GridGetDE(srcGrid, horzRelLoc=horzRelLoc, &
                          globalAIPerDim=myAI, reorder=reorder, &
                          total=totalUse, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! From each grid get the bounding box information on this DE
      call ESMF_GridGet(srcGrid, dimCount=gridrank, rc=localrc)
      allocate(srcMin(gridrank), &
               srcMax(gridrank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "src arrays", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_GridGet(dstGrid, dimCount=gridrank, rc=localrc)
      allocate(dstMin(gridrank), &
               dstMax(gridrank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dst arrays", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_FieldDataMapGet(srcDataMap, horzRelloc=horzRelLoc, rc=localrc)
      call ESMF_GridGetDE(srcGrid, horzRelLoc=horzRelLoc, &
                          minLocalCoordPerDim=srcMin, &
                          maxLocalCoordPerDim=srcMax, &
                          reorder=.false., rc=localrc)
      call ESMF_FieldDataMapGet(dstDataMap, horzRelloc=horzRelLoc, rc=localrc)
      call ESMF_GridGetDE(dstGrid, horzRelLoc=horzRelLoc, &
                          minLocalCoordPerDim=dstMin, &
                          maxLocalCoordPerDim=dstMax, &
                          reorder=.false., rc=localrc)

      ! calculate intersections
      call ESMF_GridBoxIntersectSend(dstGrid, srcGrid, srcMin, srcMax, &
                                     myAI, sendDomainList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_GridBoxIntersectRecv(srcGrid, dstMin, dstMax, &
                                     recvDomainList, total=totalUse, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Modify DomainLists for Array dimensions larger than Grid dimensions
      ! TODO: move this to its own subroutine?
      if ((dimCount.gt.gridrank) .and. (present(srcArray))) then ! TODO: fill in
      ! sendDomainList first
        allocate(     myArrayAI(dimCount), &
                 myArrayLocalAI(dimCount), &
                       dimOrder(dimCount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "dimCount arrays", &
                                       ESMF_CONTEXT, rc)) return

        if (totalUse) then
          call ESMF_ArrayGetAxisIndex(srcArray, totalindex=myArrayAI, rc=localrc)
        else
          call ESMF_ArrayGetAxisIndex(srcArray, compindex=myArrayAI, rc=localrc)
        endif
        call ESMF_FieldDataMapGet(srcDataMap, dataIndices=dimOrder, rc=localrc)
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
        call ESMF_DELayoutGet(srcDELayout, deCount=nDEs, rc=localrc)
        allocate(     allAI(nDEs,dimCount), &
                 allLocalAI(nDEs,dimCount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "allAI arrays", &
                                       ESMF_CONTEXT, rc)) return

        if (totalUse) then
          call ESMF_ArrayGetAllAxisIndices(srcArray, srcGrid, srcDataMap, &
                                           totalindex=allAI, rc=localrc)
        else
          call ESMF_ArrayGetAllAxisIndices(srcArray, srcGrid, srcDataMap, &
                                           compindex=allAI, rc=localrc)
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

        deallocate(      dimOrder, &
                        myArrayAI, &
                   myArrayLocalAI, &
                            allAI, &
                       allLocalAI, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                       ESMF_CONTEXT, rc)) return
      endif

      ! Create Route
      route = ESMF_RouteCreate(parentDELayout, localrc)
      call ESMF_RoutePrecomputeDomList(route, dimCount, myDE, sendDomainList, &
                                       recvDomainList, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set size of recv items in Route
      call ESMF_RouteSetRecvItems(route, recvDomainList%total_points, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Clean up
      deallocate(  myAI, &
                 srcMin, &
                 srcMax, &
                 dstMin, &
                 dstMax, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate", &
                                     ESMF_CONTEXT, rc)) return

      ! Set return values
      ESMF_RegridRouteConstruct = route
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_RegridRouteConstruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridGet"
!BOP
! !IROUTINE: ESMF_RegridGet - Get an attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGet(regrid, name, srcArray, dstArray, &
                                srcGrid,  dstGrid, srcDatamap,  dstDatamap, &
                                method, rc)
!
! !ARGUMENTS:

      type(ESMF_Regrid),        intent(inout) :: regrid
      character (*),            intent(out), optional :: name
      type (ESMF_Array),        intent(out), optional :: srcArray
      type (ESMF_Array),        intent(out), optional :: dstArray
      type (ESMF_Grid),         intent(out), optional :: srcGrid
      type (ESMF_Grid),         intent(out), optional :: dstGrid
      type (ESMF_FieldDataMap), intent(out), optional :: srcDatamap
      type (ESMF_FieldDataMap), intent(out), optional :: dstDatamap
      integer,                  intent(out), optional :: method
      integer,                  intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns the value of a Regrid attribute.  The
!     attribute is specified through optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Regrid to be queried.
!     \item[TODO:] fix this doc - make it match arg list and fix double
!          brackets to be bracket, curley brace, bracket.
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
!     \item[{[srcDatamap]}]
!          
!     \item[{[dstDatamap]}]
!          
!     \item[{[method]}]
!          Integer enum of method used in this regrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      integer :: localrc
      type(ESMF_RegridType), pointer :: rtype

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
 
      rtype => regrid%ptr
      ! Get name if requested
      if (present(name)) then
        call ESMF_GetName(rtype%base, name, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! Get arrays if requested
      if (present(srcArray)) then
         srcArray = rtype%srcArray
      endif
      if (present(dstArray)) then
         dstArray = rtype%dstArray
      endif

      ! Get grids if requested
      if (present(srcGrid)) then
         srcGrid = rtype%srcGrid
      endif
      if (present(dstGrid)) then
         dstGrid = rtype%dstGrid
      endif

      ! Get datamaps if requested
      if (present(srcDatamap)) then
         srcDatamap = rtype%srcDatamap
      endif
      if (present(dstDatamap)) then
         dstDatamap = rtype%dstDatamap
      endif

      ! get method
      if (present(method)) method = rtype%method

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridGet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridSet"
!BOPI
! !IROUTINE: ESMF_RegridSet - Set attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridSet(regrid, srcArray, dstArray, &
                                srcGrid,  dstGrid,          &
                                srcDatamap,  dstDatamap,    &
                                method, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid),        intent(inout) :: regrid
      type (ESMF_Array),        intent(in   ), optional :: srcArray
      type (ESMF_Array),        intent(in   ), optional :: dstArray
      type (ESMF_Grid),         intent(in   ), optional :: srcGrid
      type (ESMF_Grid),         intent(in   ), optional :: dstGrid
      type (ESMF_FieldDataMap), intent(in   ), optional :: srcDatamap
      type (ESMF_FieldDataMap), intent(in   ), optional :: dstDatamap
      integer,                  intent(in ), optional :: method
      character (*),            intent(in ), optional :: name
      integer,                  intent(out), optional :: rc

!
! !DESCRIPTION:
!     Sets a Regrid attribute with the given value.
!     The attribute is determined by optional arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be modified.
!     \item[{[srcArray]}]
!
!     \item[{[dstArray]}]
!
!     \item[{[srcGrid]}]
!
!     \item[{[dstGrid]}]
!
!     \item[{[srcDataMap]}]
!
!     \item[{[dstDataMap]}]
!
!     \item[{[method]}]
!          Integer enum of method used in this regrid.
!     \item[{[name]}]
!          Name for this regrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc
      type(ESMF_RegridType), pointer :: rtype

      ! Initalize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
 
      rtype => regrid%ptr
      ! Set name if requested
      if (present(name)) then
        call ESMF_SetName(rtype%base, name, "Regrid", localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      ! Set arrays if requested
      if (present(srcArray)) rtype%srcArray = srcArray
      if (present(dstArray)) rtype%dstArray = dstArray
           
      ! Set grids if requested
      if (present(srcGrid)) rtype%srcGrid = srcGrid
      if (present(dstGrid)) rtype%dstGrid = dstGrid
 
      ! Set datamaps if requested
      if (present(srcDatamap)) rtype%srcDatamap = srcDatamap
      if (present(dstDatamap)) rtype%dstDatamap = dstDatamap
 
      ! get method 
      if (present(method)) rtype%method = method

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridSet

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridCreateEmpty"
!BOPI
! !IROUTINE: ESMF_RegridCreateEmpty - Create empty regrid structure

! !INTERFACE:
      function ESMF_RegridCreateEmpty(rc)
!
! !RETURN VALUE:
      type(ESMF_Regrid) :: ESMF_RegridCreateEmpty
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which creates and initializes a regrid structure.
!     The structure is later filled with appropriate data using the
!     set function. Intended for internal ESMF use only.
!
!     The arguments are:
!     \begin{description}
!     \item[{[name]}]
!          {\tt Regrid} name.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc
      type(ESMF_RegridType), pointer :: rgtype    ! Pointer to new regrid

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

!     Initialize pointers
      nullify(rgtype)
      nullify(ESMF_RegridCreateEmpty%ptr)

      allocate(rgtype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Regrid type", &
                                     ESMF_CONTEXT, rc)) return

!     Call construction method to allocate and initialize regrid internals.
      call ESMF_RegridConstructEmpty(rgtype, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

!     Set return values.
      ESMF_RegridCreateEmpty%ptr => rgtype

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_RegridCreateEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridConstructEmpty"
!BOPI
! !IROUTINE: ESMF_RegridConstructEmpty - Create empty regrid structure

! !INTERFACE:
      subroutine ESMF_RegridConstructEmpty(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridType), intent(inout) :: regrid
      integer, intent(out), optional :: rc
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
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! initialize the base object
      call ESMF_BaseCreate(regrid%base, "Regrid", rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      ! TODO: add error handling

      ! initialize scalars
      regrid%method         = ESMF_REGRID_METHOD_NONE
      regrid%redistrbOption = ESMF_REGRID_DISTRB_NONE

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridConstructEmpty

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridDestruct"
!BOPI
! !IROUTINE: ESMF_RegridDestruct - Free any Regrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_RegridDestruct(regrid, rc)
!
! !ARGUMENTS:
      type(ESMF_Regrid), intent(inout) :: regrid
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

      integer :: localrc                          ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      regrid%ptr%method         = ESMF_REGRID_METHOD_NONE
      regrid%ptr%redistrbOption = ESMF_REGRID_DISTRB_NONE

      ! and free anything associated with the base object
      call ESMF_BaseDestroy(regrid%ptr%base, localrc)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDestruct

!------------------------------------------------------------------------------

      end module ESMF_RegridTypesMod
