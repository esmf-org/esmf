! $Id: ESMF_Regrid.F90,v 1.90 2004/12/22 20:54:48 jwolfe Exp $
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
#define ESMF_FILENAME "ESMF_Regrid.F90"
!
!     ESMF Regrid Module
      module ESMF_RegridMod
!
!==============================================================================
!
! This file contains most of the Regrid class methods.  The Regrid data type
! and some utility functions are contained in RegridTypesMod.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
!==============================================================================
!BOPI
! !MODULE: ESMF_RegridMod - Regridding and interpolation
!
! !DESCRIPTION:
!
! The code in this file implements most of the Regrid class methods.  Regrid 
! is responsible for any regridding and interpolation required for ESMF 
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
      use ESMF_BaseMod         ! ESMF base   class
      use ESMF_ArrayDataMapMod ! ESMF array datamap class
      use ESMF_VMTypesMod
      use ESMF_VMBaseMod
      use ESMF_VMCommMod
      use ESMF_DELayoutMod     ! ESMF DE layout class
      use ESMF_ArrayMod        ! ESMF array  class
      use ESMF_ArrayGetMod     ! ESMF array  class
      use ESMF_DistGridMod     ! ESMF distributed grid class
      use ESMF_PhysGridMod     ! ESMF physical grid class
      use ESMF_GridMod         ! ESMF grid   class
      use ESMF_RHandleMod      ! ESMF route handle class
      use ESMF_RouteMod        ! ESMF route  class
      use ESMF_ArrayCommMod    ! ESMF array comm class
      use ESMF_FieldDataMapMod ! ESMF field datamap class
      use ESMF_FieldMod        ! ESMF field  class
      use ESMF_BundleMod       ! ESMF bundle class
      use ESMF_RegridTypesMod  ! ESMF regrid data types and utilities
      use ESMF_RegridBilinearMod ! ESMF rg methods related to bilinear regrid
      use ESMF_RegridNearNbrMod  ! ESMF rg methods related to nearest-nbr regrid
      use ESMF_RegridConservMod  ! ESMF rg methods related to conservative regrid
      use ESMF_RegridLinearMod   ! ESMF rg methods related to linear regrid

      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

    ! These are wrapper routines which call RegridCreate to do the
    !  actual work.  Since all our routines are data centric methods
    !  and we are not exposing an externally visible "regrid" object, 
    !  these routines must exist to be consistent with the other interfaces.  
    ! 
    public ESMF_ArrayRegridStore, ESMF_ArrayRegrid, ESMF_ArrayRegridRelease
    public ESMF_RegridDestroy    ! deallocate memory associated with a regrid
    public ESMF_RegridValidate   ! Error checking and validation
    public ESMF_RegridPrint      ! Prints various regrid info

!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
         '$Id: ESMF_Regrid.F90,v 1.90 2004/12/22 20:54:48 jwolfe Exp $'

!==============================================================================

      contains

!==============================================================================
!
! This section includes some of the Regrid Create methods.
!
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridCreate"
!BOPI
! !IROUTINE: ESMF_RegridCreate - Precomputes Regrid data

! !INTERFACE:
      subroutine ESMF_RegridCreate(srcArray, srcGrid, srcDatamap, hasSrcData, &
                                   dstArray, dstGrid, dstDatamap, hasDstData, &
                                   parentVM, routehandle, regridmethod, &
                                   regridNorm, srcMask, dstMask, rc)
!
! !ARGUMENTS:
      type(ESMF_Array),         intent(in   ) :: srcArray
      type(ESMF_Grid),          intent(in   ) :: srcGrid
      type(ESMF_FieldDataMap),  intent(in   ) :: srcDatamap
      logical,                  intent(in   ) :: hasSrcData
      type(ESMF_Array),         intent(in   ) :: dstArray
      type(ESMF_Grid),          intent(in   ) :: dstGrid
      type(ESMF_FieldDataMap),  intent(in   ) :: dstDatamap
      logical,                  intent(in   ) :: hasDstData
      type(ESMF_VM),            intent(in   ) :: parentVM
      type(ESMF_RouteHandle),   intent(inout) :: routehandle
      type(ESMF_RegridMethod),  intent(in   ) :: regridmethod
      type(ESMF_RegridNormOpt), intent(in   ), optional :: regridNorm
      type(ESMF_Mask),          intent(in   ), optional :: srcMask
      type(ESMF_Mask),          intent(in   ), optional :: dstMask
      integer,                  intent(  out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array, grid, and datamap, this routine precomputes
!     both the communication pattern needed to move data to the proper
!     processors, and the sparse matrix of weights needed to compute the data
!     interpolation for moving data from one grid to another.
!     This routine returns a handle to the precomputed information in the
!     {\tt routehandle} argument.  This same value should be supplied
!     at run time, along with the actual data pointers.  The same precomputed
!     handle can be used on any data which matches the data arrays, grid,
!     and datamaps supplied here, so one does not have to generate multiple
!     routehandles for similar data values.
!
!     The arguments are:
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data.
!     \item [srcGrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           source array has been decomposed.
!     \item [srcDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array maps to
!           the specified source grid.
!     \item [hasSrcData]
!           Logical specifier for whether or not this DE has source data.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing destination data.
!     \item [dstGrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           destination array should be decomposed.
!     \item [dstDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array should map to
!           the specified destination grid.
!     \item [hasDstData]
!           Logical specifier for whether or not this DE has destination data.
!     \item [routehandle]
!           Returned value which identifies the precomputed Route and other
!           necessary information.
!     \item [regridmethod]
!           Type of regridding to do.  A set of predefined methods are
!           supplied.
!     \item [{[regridNorm]}]
!           Normalization option, only for specific regrid types.
!     \item [{[srcMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstMask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!   The supported regridding methods for this create function are currently:
!   \begin{description}
!   \item[ESMF\_REGRID\_METHOD\_BILINEAR  ] bilinear (logically-rectangular grids)
!   \item[ESMF\_REGRID\_METHOD\_CONSERV1  ] first-order conservative
!   \item[ESMF\_REGRID\_METHOD\_LINEAR    ] linear for 1-d regridding
!   \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      ! TODO: the interfaces have changed - this will no longer be called
      !  with fields, but with the grid, datamap, and array which are either
      !  contained in a field or are specified by the user separately in an
      !  array method call.  i believe the code at this level needs to 
      !  take an already declared routehandle and fill it in with the
      !  route and weights that it computes.  we have no "Regrid" object
      !  at this point - it is a subtype of a routehandle object.  so this
      !  could either compute the route and weights and return them, or
      !  it seems maybe better to fill them in here in case we end up with
      !  more than just a single route and weight array.
      ! TODO: so this code needs to be overhauled...   

      integer :: localrc            ! Error status
      !character(len=ESMF_MAXSTR) :: regridName
      logical :: dummy

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! TODO: error check for regridnormOpt only for conservative methods
      
      ! Call the appropriate create routine based on method choice

      select case(regridmethod%regridMethod)

      !-------------
      ! ESMF_REGRID_METHOD_BILINEAR
      case(1)
        routehandle = ESMF_RegridConstructBilinear( &
                                 srcArray, srcGrid, srcDatamap, hasSrcData, &
                                 dstArray, dstGrid, dstDatamap, hasDstData, &
                                 parentVM, srcMask, dstMask, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_BICUBIC
      case(2)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Bicubic not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_CONSERV1
      case(3)
        routehandle = ESMF_RegridConstructConserv( &
                                 srcArray, srcGrid, srcDatamap, hasSrcData, &
                                 dstArray, dstGrid, dstDatamap, hasDstData, &
                                 parentVM, srcMask, dstMask, &
                                 regridnorm=regridNorm, order=1, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_CONSERV2
      case(4)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Second-order conservative method not yet supported", &
                   ESMF_CONTEXT, rc)
        return
      !   routehandle = ESMF_RegridConstructConserv(srcArray, dstArray, &
      !                                        regridName, order=2, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_RASTER
      case(5)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Raster method not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_NEAR_NBR
      case(6)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Nearest neighbor method not yet supported", &
                   ESMF_CONTEXT, rc)
        return
      !   routehandle = ESMF_RegridConstructNearNbr(srcArray, dstArray, &
      !                                        regridName, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_FOURIER
      case(7)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Fourier transforms not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_LEGENDRE
      case(8)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Legendre transforms not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_INDEX
      case(9)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Index-space methods not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_LINEAR
      case(10)
        routehandle = ESMF_RegridConstructLinear( &
                                 srcArray, srcGrid, srcDatamap, hasSrcData, &
                                 dstArray, dstGrid, dstDatamap, hasDstData, &
                                 parentVM, srcMask, dstMask, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_SPLINE
      case(11)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "1-d cubic splines not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_REGRIDCOPY
      case(51)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Regrid copied from another regrid not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_SHIFT
      case(52)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Regrid shifted from another regrid not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_ADJOINT
      case(53)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Adjoint from existing regrid not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_FILE
      case(89)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "Regrid read from file not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_USER
      case(90)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                   "User-defined regridding not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      case default
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                    "Invalid method", &
                                    ESMF_CONTEXT, rc)
        return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridRunR4"
!BOPI
! !IROUTINE: ESMF_RegridRunR4 - Performs a regridding between two arrays of type R4

! !INTERFACE:
      subroutine ESMF_RegridRunR4(srcarray, dstarray, srcDataMap, dstDataMap, &
                                  routehandle, rc)
!
! !ARGUMENTS:

      type(ESMF_Array), intent(in) :: srcarray    ! array to be regridded
      type(ESMF_Array), intent(inout) :: dstarray   ! resulting regridded array
      type (ESMF_FieldDataMap), intent(in) :: srcDatamap
      type (ESMF_FieldDataMap), intent(in) :: dstDatamap
      type(ESMF_RouteHandle), intent(in) :: routehandle 
                                                  ! precomputed regrid structure
                                                  ! with regridding info
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array and precomputed regridding information, this 
!     routine regrids the source array to a new array on the destination
!     grid.  
!
!EOPI

      integer :: localrc
      integer :: i, i2, n, d1, d2, s1, asize, rank, counts(ESMF_MAXDIM)
      integer :: dstUndecomp, srcUndecomp, srcUndecompSize, srcStride
      integer :: di1, di2, dj1, dj2, dk1, dk2
      integer :: si1, si2, sj1, sj2, sk1, sk2
      integer, dimension(:), allocatable :: dstDimOrder, srcDimOrder
      integer, dimension(:,:), allocatable :: dindex, sindex
      integer(ESMF_KIND_I4), dimension(:), pointer :: dstIndex, srcIndex
      logical :: dummy
      real(ESMF_KIND_R4) :: zero
      real(ESMF_KIND_R8), dimension(:), pointer :: weights
      real(ESMF_KIND_R4), dimension(:), pointer :: gatheredData
      real(ESMF_KIND_R4), dimension(:,:), pointer :: dstData2D
      real(ESMF_KIND_R4), dimension(:,:,:), pointer :: dstData3D
      !real(ESMF_KIND_R4), dimension(:,:,:,:), pointer :: dstData4D
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      type(ESMF_Route) :: rh
      type(ESMF_LocalArray) :: srcindexarr, dstindexarr, weightsarr
      integer :: numlinks
      type(ESMF_LocalArray) :: gatheredArray, srcLocalArray
      type(ESMF_TransformValues) :: tv

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      zero = 0.0
      ! get the first route from the table and run it to gather the
      ! data values which overlap this bounding box.
 
      call ESMF_RouteHandleGet(routehandle, route1=rh, rc=localrc)

      ! get the indirect indices and weights from the routehandle
      call ESMF_RouteHandleGet(routehandle, tdata=tv, rc=localrc)

      ! get a real f90 pointer from all the arrays
      ! srcIndex, dstIndex and weights TKR can be fixed, but unfortunately the
      ! gatheredData and dstData can be whatever the user wants - so this code
      ! might need to move into another file and be macroized heavily for TKR.
      call ESMF_TransformValuesGet(tv, numlist=numlinks, srcindex=srcindexarr, &
                                   dstindex=dstindexarr, weights=weightsarr, rc=rc)
      call ESMF_LocalArrayGetData(srcindexarr, srcIndex, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(dstindexarr, dstIndex, ESMF_DATA_REF, rc)
      call ESMF_LocalArrayGetData(weightsarr, weights, ESMF_DATA_REF, rc)

      ! from the domain or from someplace, get the counts of how many data points
      ! we are expecting from other DEs.  we might also need to know what
      ! data type is coming since this is user data and not coordinates at 
      ! execution time.  or does the incoming data have to match the type
      ! of the outgoing array?  so we can get the data type and shape from
      ! the dstarray argument to this function.  and what about halo widths?

      call ESMF_RouteGetRecvItems(rh, asize, localrc)

      call ESMF_ArrayGet(srcarray, rank=rank, type=type, kind=kind, &
                         counts=counts, rc=localrc)
      gatheredArray = ESMF_LocalArrayCreate(1, type, kind, asize, rc=localrc)
      srcLocalArray = srcarray
      call ESMF_RouteRun(rh, srcLocalArray, gatheredArray, rc=localrc)

      allocate(dstDimOrder(rank), &
               srcDimOrder(rank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dim orders", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_FieldDataMapGet(dstDataMap, dataIndexList=dstDimOrder, rc=localrc)
      call ESMF_FieldDataMapGet(srcDataMap, dataIndexList=srcDimOrder, rc=localrc)

      ! break out here by rank   TODO: compare datarank to regridrank (or
      !                                compare datamaps)
      select case(rank)

      ! TODO: apply the weights from src to destination
      !  does this need a nested loop and an array of ESMF_Arrays, one
      !  for each DE which sends data?  i think the answer is not for now
      !  because all data has been pushed into a single array.  but eventually
      !  if we want to start supporting vectors or other complicated data 
      !  shapes, we may have to start preserving the array and datamaps
      !  from the original locations.

      !-------------
      case(2) ! 2D data for regrid

        call ESMF_LocalArrayGetData(gatheredArray, gatheredData, &
                                    ESMF_DATA_REF, rc)
        call ESMF_ArrayGetData(dstarray, dstData2D, ESMF_DATA_REF, rc)

        !*** initialize dest field to zero
   
        dstData2D = zero

        !*** do the regrid

        do n = 1,numlinks
          d1 = dstIndex((n-1)*2 + 1)
          d2 = dstIndex((n-1)*2 + 2)
          s1 = srcIndex(n)
          dstData2D(d1,d2) = dstData2D(d1,d2) &
                           + (gatheredData(s1) * weights(n))
        enddo

      !-------------
      case(3) ! 3D data for regrid

        call ESMF_LocalArrayGetData(gatheredArray, gatheredData, &
                                    ESMF_DATA_REF, rc)
        call ESMF_ArrayGetData(dstarray, dstData3D, ESMF_DATA_REF, rc)

        !*** initialize dest field to zero
   
        dstData3D = zero

        !*** for cases where datarank>gridrank, figure out the undecomposed
        dstUndecomp     = 0   !TODO: should be arrays to be general
        srcUndecomp     = 0
        srcUndecompSize = 0
        srcStride       = 1
        do i = 1,rank
          if (dstDimOrder(i).eq.0) then
            dstUndecomp = i
          endif
          if (srcDimOrder(i).eq.0) then
            srcUndecomp = i
            srcUndecompSize = counts(i)
          else
            srcStride = srcStride * counts(i)
          endif
        enddo

        !*** special code if Undecomp = 1 or rank
        if (srcUndecomp.eq.1 .and. dstUndecomp.eq.1) then

          !*** do the regrid
          i2 = size(dstData3D,1)
          do n = 1,numlinks
            d1 = dstIndex((n-1)*2 + 1)
            d2 = dstIndex((n-1)*2 + 2)
            s1 = (srcIndex(n)-1)*i2        ! assumes i2 = srcUndecompSize
            do i = 1,i2
              dstData3D(i,d1,d2) = dstData3D(i,d1,d2) &
                                 + (gatheredData(s1+i) * weights(n))
            enddo
          enddo

        elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.rank) then

          !*** do the regrid
          i2 = size(dstData3D,rank)
          do i = 1,i2
            do n = 1,numlinks
              d1 = dstIndex((n-1)*2 + 1)
              d2 = dstIndex((n-1)*2 + 2)
              s1 = (i-1)*srcStride + srcIndex(n)
              dstData3D(d1,d2,i) = dstData3D(d1,d2,i) &
                                 + (gatheredData(s1) * weights(n))
            enddo
          enddo

        elseif (srcUndecomp.eq.1 .and. dstUndecomp.eq.rank) then

          !*** do the regrid
          i2 = size(dstData3D,rank)
          do i = 1,i2
            do n = 1,numlinks
              d1 = dstIndex((n-1)*2 + 1)
              d2 = dstIndex((n-1)*2 + 2)
              s1 = (srcIndex(n)-1)*i2 + i    ! assumes i2 = srcUndecompSize
              dstData3D(d1,d2,i) = dstData3D(d1,d2,i) &
                                 + (gatheredData(s1) * weights(n))
            enddo
          enddo

        elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.1) then

          !*** do the regrid
          i2 = size(dstData3D,1)
          do n = 1,numlinks
            d1 = dstIndex((n-1)*2 + 1)
            d2 = dstIndex((n-1)*2 + 2)
            do i = 1,i2
              s1 = (i-1)*srcStride + srcIndex(n)
              dstData3D(i,d1,d2) = dstData3D(i,d1,d2) &
                                 + (gatheredData(s1) * weights(n))
            enddo
          enddo

        else
          allocate(dindex(rank,2), &
                   sindex(rank,2), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "Indexes", &
                                         ESMF_CONTEXT, rc)) return
          do n = 1,numlinks
            dindex(1,1) = 1
            dindex(1,2) = size(dstData3D,dstUndecomp)
            dindex(2,1) = dstIndex((n-1)*2 + 1)
            dindex(2,2) = dstIndex((n-1)*2 + 1)
            dindex(3,1) = dstIndex((n-1)*2 + 2)
            dindex(3,2) = dstIndex((n-1)*2 + 2)
            di1 = dindex(dstDimOrder(1)+1,1)
            di2 = dindex(dstDimOrder(1)+1,2)
            dj1 = dindex(dstDimOrder(2)+1,1)
            dj2 = dindex(dstDimOrder(2)+1,2)
            dk1 = dindex(dstDimOrder(3)+1,1)
            dk2 = dindex(dstDimOrder(3)+1,2)
            sindex(1,1) = 1
            sindex(1,2) = size(gatheredData,srcUndecomp)
            sindex(2,1) = srcIndex((n-1)*2 + 1)
            sindex(2,2) = srcIndex((n-1)*2 + 1)
            sindex(3,1) = srcIndex((n-1)*2 + 2)
            sindex(3,2) = srcIndex((n-1)*2 + 2)
            si1 = sindex(srcDimOrder(1)+1,1)
            si2 = sindex(srcDimOrder(1)+1,2)
            sj1 = sindex(srcDimOrder(2)+1,1)
            sj2 = sindex(srcDimOrder(2)+1,2)
            sk1 = sindex(srcDimOrder(3)+1,1)
            sk2 = sindex(srcDimOrder(3)+1,2)
   !         dstData3D(di1:di2,dj1:dj2,dk1:dk2) = &
   !                  dstData3D(di1:di2,dj1:dj2,dk1:dk2) &
   !               + (gatheredData(si1:si2,sj1:sj2) * weights(n))

          enddo
          deallocate(dindex, &
                     sindex, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocate indexes", &
                                         ESMF_CONTEXT, rc)) return
        endif

      case default
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                    "Invalid rank", &
                                    ESMF_CONTEXT, rc)
        return
      end select

      ! nuke temp arrays
      deallocate(dstDimOrder, &
                 srcDimOrder, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate dim orders", &
                                     ESMF_CONTEXT, rc)) return

      ! set return codes
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridRunR4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridRunR8"
!BOPI
! !IROUTINE: ESMF_RegridRunR8 - Performs a regridding between two arrays of type R8

! !INTERFACE:
      subroutine ESMF_RegridRunR8(srcArray, srcDataMap, hasSrcData, &
                                  dstArray, dstDataMap, hasDstData, &
                                  routehandle, rc)
!
! !ARGUMENTS:

      type(ESMF_Array),        intent(in   ) :: srcArray
      type(ESMF_FieldDataMap), intent(in   ) :: srcDatamap
      logical,                 intent(in   ) :: hasSrcData
      type(ESMF_Array),        intent(inout) :: dstArray
      type(ESMF_FieldDataMap), intent(in   ) :: dstDatamap
      logical,                 intent(in   ) :: hasDstData
      type(ESMF_RouteHandle),  intent(in   ) :: routehandle 
                                                  ! precomputed regrid structure
                                                  ! with regridding info
      integer,                 intent(  out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array and precomputed regridding information, this 
!     routine regrids the source array to a new array on the destination
!     grid.  
!
!EOPI

      integer :: localrc
      integer :: i, i2, n, d1, d2, s1, asize, rank, counts(ESMF_MAXDIM)
      integer :: dstUndecomp, srcUndecomp, srcUndecompSize, srcStride
      integer :: di1, di2, dj1, dj2, dk1, dk2
      integer :: si1, si2, sj1, sj2, sk1, sk2
      integer, dimension(:), allocatable :: dstDimOrder, srcDimOrder
      integer, dimension(:,:), allocatable :: dindex, sindex
      integer(ESMF_KIND_I4), dimension(:), pointer :: dstIndex, srcIndex
      logical :: dummy
      real(ESMF_KIND_R8) :: zero
      real(ESMF_KIND_R8), dimension(:), pointer :: weights
      real(ESMF_KIND_R8), dimension(:), pointer :: gatheredData
      real(ESMF_KIND_R8), dimension(:,:), pointer :: dstData2D
      real(ESMF_KIND_R8), dimension(:,:,:), pointer :: dstData3D
      !real(ESMF_KIND_R8), dimension(:,:,:,:), pointer :: dstData4D
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      type(ESMF_Route) :: rh
      type(ESMF_LocalArray) :: srcindexarr, dstindexarr, weightsarr
      integer :: numlinks
      type(ESMF_LocalArray) :: gatheredArray, srcLocalArray
      type(ESMF_TransformValues) :: tv

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      zero = 0.0
      nullify(gatheredData)

      ! get the first route from the table and run it to gather the
      ! data values which overlap this bounding box.
      call ESMF_RouteHandleGet(routehandle, route1=rh, rc=localrc)

      call ESMF_ArrayGet(srcArray, rank=rank, type=type, kind=kind, &
                         counts=counts, rc=localrc)

      ! get the indirect indices and weights from the routehandle
      asize = 1
      if (hasDstData) then
        call ESMF_RouteHandleGet(routehandle, tdata=tv, rc=localrc)

        ! get a real f90 pointer from all the arrays
        ! srcIndex, dstIndex and weights TKR can be fixed, but unfortunately the
        ! gatheredData and dstData can be whatever the user wants - so this code
        ! might need to move into another file and be macroized heavily for TKR.
        call ESMF_TransformValuesGet(tv, numlist=numlinks, srcindex=srcindexarr, &
                                     dstindex=dstindexarr, weights=weightsarr, rc=rc)
        call ESMF_LocalArrayGetData(srcindexarr, srcIndex, ESMF_DATA_REF, rc)
        call ESMF_LocalArrayGetData(dstindexarr, dstIndex, ESMF_DATA_REF, rc)
        call ESMF_LocalArrayGetData(weightsarr, weights, ESMF_DATA_REF, rc)

        ! from the domain or from someplace, get the counts of how many data points
        ! we are expecting from other DEs.  we might also need to know what
        ! data type is coming since this is user data and not coordinates at 
        ! execution time.  or does the incoming data have to match the type
        ! of the outgoing array?  so we can get the data type and shape from
        ! the dstarray argument to this function.  and what about halo widths?

        call ESMF_RouteGetRecvItems(rh, asize, localrc)
      endif

      gatheredArray = ESMF_LocalArrayCreate(1, type, kind, asize, rc=localrc)
      srcLocalArray = srcArray

      call ESMF_RouteRun(rh, srcLocalArray, gatheredArray, rc=localrc)

      allocate(dstDimOrder(rank), &
               srcDimOrder(rank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dim orders", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_FieldDataMapGet(dstDataMap, dataIndexList=dstDimOrder, rc=localrc)
      call ESMF_FieldDataMapGet(srcDataMap, dataIndexList=srcDimOrder, rc=localrc)

      ! break out here by rank   TODO: compare datarank to regridrank (or
      !                                compare datamaps)
      if (hasDstData) then
      select case(rank)

      ! TODO: apply the weights from src to destination
      !  does this need a nested loop and an array of ESMF_Arrays, one
      !  for each DE which sends data?  i think the answer is not for now
      !  because all data has been pushed into a single array.  but eventually
      !  if we want to start supporting vectors or other complicated data 
      !  shapes, we may have to start preserving the array and datamaps
      !  from the original locations.

      !-------------
      case(2) ! 2D data for regrid

        call ESMF_LocalArrayGetData(gatheredArray, gatheredData, &
                                    ESMF_DATA_REF, rc)
        call ESMF_ArrayGetData(dstarray, dstData2D, ESMF_DATA_REF, rc)

        !*** initialize dest field to zero
   
        dstData2D = zero

        !*** do the regrid

        do n = 1,numlinks
          d1 = dstIndex((n-1)*2 + 1)
          d2 = dstIndex((n-1)*2 + 2)
          s1 = srcIndex(n)
          dstData2D(d1,d2) = dstData2D(d1,d2) &
                           + (gatheredData(s1) * weights(n))
        enddo

      !-------------
      case(3) ! 3D data for regrid

        call ESMF_LocalArrayGetData(gatheredArray, gatheredData, &
                                    ESMF_DATA_REF, rc)
        call ESMF_ArrayGetData(dstarray, dstData3D, ESMF_DATA_REF, rc)

        !*** initialize dest field to zero
   
        dstData3D = zero

        !*** for cases where datarank>gridrank, figure out the undecomposed
        dstUndecomp     = 0   !TODO: should be arrays to be general
        srcUndecomp     = 0
        srcUndecompSize = 0
        srcStride       = 1
        do i = 1,rank
          if (dstDimOrder(i).eq.0) then
            dstUndecomp = i
          endif
          if (srcDimOrder(i).eq.0) then
            srcUndecomp = i
            srcUndecompSize = counts(i)
          else
            srcStride = srcStride * counts(i)
          endif
        enddo

        !*** special code if Undecomp = 1 or rank
        if (srcUndecomp.eq.1 .and. dstUndecomp.eq.1) then

          !*** do the regrid
          i2 = size(dstData3D,1)
          do n = 1,numlinks
            d1 = dstIndex((n-1)*2 + 1)
            d2 = dstIndex((n-1)*2 + 2)
            s1 = (srcIndex(n)-1)*i2        ! assumes i2 = srcUndecompSize
            do i = 1,i2
              dstData3D(i,d1,d2) = dstData3D(i,d1,d2) &
                                 + (gatheredData(s1+i) * weights(n))
            enddo
          enddo

        elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.rank) then

          !*** do the regrid
          i2 = size(dstData3D,rank)
          do i = 1,i2
            do n = 1,numlinks
              d1 = dstIndex((n-1)*2 + 1)
              d2 = dstIndex((n-1)*2 + 2)
              s1 = (i-1)*srcStride + srcIndex(n)
              dstData3D(d1,d2,i) = dstData3D(d1,d2,i) &
                                 + (gatheredData(s1) * weights(n))
            enddo
          enddo

        elseif (srcUndecomp.eq.1 .and. dstUndecomp.eq.rank) then

          !*** do the regrid
          i2 = size(dstData3D,rank)
          do i = 1,i2
            do n = 1,numlinks
              d1 = dstIndex((n-1)*2 + 1)
              d2 = dstIndex((n-1)*2 + 2)
              s1 = (srcIndex(n)-1)*i2 + i    ! assumes i2 = srcUndecompSize
              dstData3D(d1,d2,i) = dstData3D(d1,d2,i) &
                                 + (gatheredData(s1) * weights(n))
            enddo
          enddo

        elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.1) then

          !*** do the regrid
          i2 = size(dstData3D,1)
          do n = 1,numlinks
            d1 = dstIndex((n-1)*2 + 1)
            d2 = dstIndex((n-1)*2 + 2)
            do i = 1,i2
              s1 = (i-1)*srcStride + srcIndex(n)
              dstData3D(i,d1,d2) = dstData3D(i,d1,d2) &
                                 + (gatheredData(s1) * weights(n))
            enddo
          enddo

        else
          allocate(dindex(rank,2), &
                   sindex(rank,2), stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "Indexes", &
                                         ESMF_CONTEXT, rc)) return
          do n = 1,numlinks
            dindex(1,1) = 1
            dindex(1,2) = size(dstData3D,dstUndecomp)
            dindex(2,1) = dstIndex((n-1)*2 + 1)
            dindex(2,2) = dstIndex((n-1)*2 + 1)
            dindex(3,1) = dstIndex((n-1)*2 + 2)
            dindex(3,2) = dstIndex((n-1)*2 + 2)
            di1 = dindex(dstDimOrder(1)+1,1)
            di2 = dindex(dstDimOrder(1)+1,2)
            dj1 = dindex(dstDimOrder(2)+1,1)
            dj2 = dindex(dstDimOrder(2)+1,2)
            dk1 = dindex(dstDimOrder(3)+1,1)
            dk2 = dindex(dstDimOrder(3)+1,2)
            sindex(1,1) = 1
            sindex(1,2) = size(gatheredData,srcUndecomp)
            sindex(2,1) = srcIndex((n-1)*2 + 1)
            sindex(2,2) = srcIndex((n-1)*2 + 1)
            sindex(3,1) = srcIndex((n-1)*2 + 2)
            sindex(3,2) = srcIndex((n-1)*2 + 2)
            si1 = sindex(srcDimOrder(1)+1,1)
            si2 = sindex(srcDimOrder(1)+1,2)
            sj1 = sindex(srcDimOrder(2)+1,1)
            sj2 = sindex(srcDimOrder(2)+1,2)
            sk1 = sindex(srcDimOrder(3)+1,1)
            sk2 = sindex(srcDimOrder(3)+1,2)
   !         dstData3D(di1:di2,dj1:dj2,dk1:dk2) = &
   !                  dstData3D(di1:di2,dj1:dj2,dk1:dk2) &
   !               + (gatheredData(si1:si2,sj1:sj2) * weights(n))

          enddo
          deallocate(dindex, &
                     sindex, stat=localrc)
          if (ESMF_LogMsgFoundAllocError(localrc, "deallocate indexes", &
                                         ESMF_CONTEXT, rc)) return
        endif

      case default
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_RANK, &
                                    "Invalid rank", &
                                    ESMF_CONTEXT, rc)
        return
      end select

      endif

      ! nuke temp arrays
      call ESMF_LocalArrayDestroy(gatheredArray, rc=localrc)
      deallocate(dstDimOrder, &
                 srcDimOrder, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate dim orders", &
                                     ESMF_CONTEXT, rc)) return

      ! set return codes
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridRunR8

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridDestroy"
!BOPI
! !IROUTINE: ESMF_RegridDestroy - Free all resources associated with a Regrid

! !INTERFACE:
      subroutine ESMF_RegridDestroy(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Destroys all a {\tt Regrid} object previously allocated
!     via an {\tt ESMF\_RegridCreate routine}.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          The regrid object to be destroyed.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS: TODO
!EOPI

      integer :: localrc            ! Error status
      !type(ESMF_RouteHandle) :: rhandle1
      type(ESMF_Route) :: route
      type(ESMF_TransformValues) :: tv

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      ! Does this destroy each of the routes?  it certainly needs to
      !  destroy the arrays in the TransformValues object.  (i think the
      !  answer is yes - this code should call route delete here.)

      call ESMF_RouteHandleGet(routehandle, route1=route, tdata=tv, &
                               rc=localrc)
      call ESMF_RouteDestroy(route, localrc)
      call ESMF_TransformValuesDestroy(tv, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridValidate"
!BOP
! !IROUTINE: ESMF_RegridValidate - Check internal consistency of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridValidate(routehandle, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: routehandle
      character (len=*), intent(in), optional :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that a Regrid is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[routehandle]
!          Class to be queried.
!     \item[[opt]]
!          Validation options.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      ! 
      ! code goes here
      ! 

      end subroutine ESMF_RegridValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridPrint"
! TODO: this routine should be BOP once it is filled in
!BOPI
! !IROUTINE: ESMF_RegridPrint - Print the contents of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridPrint(routehandle, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(in) :: routehandle
      character (len=*), intent(in) :: opt
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Print information about a Regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[regrid]
!          Class to be queried.
!     \item[[opt]]
!          Print ptions that control the type of information and level of
!          detail.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ! TODO: does this even need to be here?  it seems the print and
      ! validate routines should be in the routehandle code only.

      ! 
      ! code goes here
      ! 

      end subroutine ESMF_RegridPrint

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Non-Regrid methods which must be here to be able to call RegridCreate,
! RegridRun, etc.
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRegridStore"
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRegridStore(srcArray, srcGrid, srcDatamap, hasSrcData, &
                                       dstArray, dstGrid, dstDatamap, hasDstData, &
                                       parentVM, routehandle, &
                                       regridmethod, regridnorm, &
                                       srcmask, dstmask, rc) 
!
! !ARGUMENTS:
      type(ESMF_Array),         intent(in   ) :: srcArray
      type(ESMF_Grid),          intent(in   ) :: srcGrid
      type(ESMF_FieldDataMap),  intent(in   ) :: srcDatamap
      logical,                  intent(in   ) :: hasSrcData
      type(ESMF_Array),         intent(in   ) :: dstArray
      type(ESMF_Grid),          intent(in   ) :: dstGrid
      type(ESMF_FieldDataMap),  intent(in   ) :: dstDatamap
      logical,                  intent(in   ) :: hasDstData
      type(ESMF_VM),            intent(in   ) :: parentVM
      type(ESMF_RouteHandle),   intent(inout) :: routehandle
      type(ESMF_RegridMethod),  intent(in   ) :: regridmethod
      type(ESMF_RegridNormOpt), intent(in   ), optional :: regridnorm
      type(ESMF_Mask),          intent(in   ), optional :: srcmask
      type(ESMF_Mask),          intent(in   ), optional :: dstmask
      integer,                  intent(  out), optional :: rc
!
! !DESCRIPTION:
! Used to Regrid data in an Array.
!
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data.
!     \item [srcGrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           source array has been decomposed.  
!     \item [srcDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array maps to
!           the specified source grid.
!     \item [hasSrcData]
!           Logical specifier for whether or not this DE has source data.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing destination data.
!     \item [dstGrid]
!           {\tt ESMF\_Grid} which corresponds to how the data in the
!           destination array should be decomposed.  
!     \item [dstDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array should map to
!           the specified destination grid.
!     \item [hasDstData]
!           Logical specifier for whether or not this DE has destination data.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Field}s,
!           most commonly the vm of the Coupler if the regridding is
!           inter-component, but could also be the individual vm for a
!           component if the regridding is intra-component.
!     \item [routehandle]
!           Returned value which identifies the precomputed Route and other
!           necessary information.
!     \item [regridmethod]
!           Type of regridding to do.  A set of predefined methods are
!           supplied.
!     \item [{[regridnorm]}]
!           Normalization option, only for specific regrid types.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:

      integer :: localrc        ! local error status

      ! assume failure until success certain
      if (present(rc)) rc = ESMF_FAILURE

      ! TODO: add code here
      !  The form of this code depends on how we organize the interfaces
      !  between the Regrid code and this code.  This is the lowest level
      !  public interface to the Regrid code, so anything we do below
      !  here will be internal interfaces and not public.  The interfaces
      !  need to be as useful to the regrid code as possible.

      call ESMF_RegridCreate(srcArray, srcGrid, srcDatamap, hasSrcData, &   
                             dstArray, dstGrid, dstDatamap, hasDstData, &
                             parentVM, routehandle, regridmethod, &
                             regridnorm=regridnorm, &
                             srcmask=srcmask, dstmask=dstmask, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set successful routehandle to type regrid
      call ESMF_RouteHandleSet(routehandle, htype=ESMF_REGRIDHANDLE, rc=localrc)

      ! set return code if user specified it
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayRegridStore

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRegrid"
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRegrid(srcArray, srcDataMap, hasSrcData, &
                                  dstArray, dstDataMap, hasDstData, &
                                  routehandle, srcmask, dstmask, &
                                  blocking, commhandle, rc)
!
! !ARGUMENTS:
      type(ESMF_Array),        intent(inout) :: srcarray
      type(ESMF_FieldDataMap), intent(inout) :: srcDataMap
      logical,                 intent(in   ) :: hasSrcData
      type(ESMF_Array),        intent(inout) :: dstarray
      type(ESMF_FieldDataMap), intent(inout) :: dstDataMap
      logical,                 intent(in   ) :: hasDstData
      type(ESMF_RouteHandle),  intent(in   ) :: routehandle
      type(ESMF_Mask),         intent(in   ), optional :: srcmask
      type(ESMF_Mask),         intent(in   ), optional :: dstmask
      type(ESMF_BlockingFlag), intent(in   ), optional :: blocking
      type(ESMF_CommHandle),   intent(inout), optional :: commhandle
      integer,                 intent(  out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt Regrid} operation over the data in an {\tt ESMF\_Array}.
!     This routine updates the data inside the {\tt ESMF\_Array} in place.
!     It uses a precomputed {\tt ESMF\_Route} for the communications
!     pattern.
!
!     \begin{description}
!     \item [srcarray]
!           {\tt ESMF\_Array} containing source data to be regridded.
!     \item [dstarray]
!           {\tt ESMF\_Array} containing locations to put regridded data.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} has been precomputed.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
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
!EOP
! !REQUIREMENTS:

      integer :: localrc        ! local error status
      logical :: dummy
      type(ESMF_DataKind) :: srcKind, dstKind

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE
 
      ! get datakinds from the two arrays
      call ESMF_ArrayGet(srcArray, kind=srcKind, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_ArrayGet(dstArray, kind=dstKind, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Execute the communications call based on datakinds
      if (srcKind.eq.ESMF_R4 .AND. dstKind.eq.ESMF_R4) then
        call ESMF_RegridRunR4(srcarray, dstarray, srcDataMap, dstDataMap, &
                              routehandle, localrc)
      elseif (srcKind.eq.ESMF_R8 .AND. dstKind.eq.ESMF_R8) then
        call ESMF_RegridRunR8(srcArray, srcDataMap, hasSrcData, &
                              dstArray, dstDataMap, hasDstData, &
                              routehandle, localrc)
      else
        dummy = ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                "arrays of differing types or this type not yet supported", &
                ESMF_CONTEXT, rc)
        return
      endif
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return code if user specified it
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayRegrid

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ArrayRegridRelease"
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRegridRelease(routehandle, rc)
!
! !ARGUMENTS:
      type(ESMF_RouteHandle), intent(inout) :: routehandle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Release the information stored about this Regrid operation.
!
!     \begin{description}
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} associated with Regrid that is no longer
!           needed.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!
!EOP

      integer :: localrc        ! local error status

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_FAILURE

      call ESMF_RouteHandleDestroy(routehandle, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayRegridRelease


   end module ESMF_RegridMod
