! $Id: ESMF_Regrid.F90,v 1.120.2.3 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
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
! ***THIS CODE IS CURRENTLY NON-FUNCTIONAL WHILE WE BRING IN A NEW,
! ***MORE GENERAL REGRIDDING ENGINE.  It REMAINS HERE FOR THE TIME BEING
! ***BECAUSE WE ANTICIPATE REUSING PARTS OF THE INTERFACE.
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
! igrid to another, including:
! \begin{itemize}
! \item bilinear or bicubic interpolation
! \item conservative remapping
! \item spectral or other functional transforms
! \item sub-sampling, super-sampling or shifting.
! \end{itemize}
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_UtilTypesMod      ! general utility methods and parameters
      use ESMF_BaseMod           ! base class
      use ESMF_InternArrayDataMapMod   ! array datamap class
      use ESMF_VMMod             ! virtual machine class
      use ESMF_DELayoutMod       ! DE layout class
      use ESMF_InternArrayMod    ! internal array class
      use ESMF_InternArrayGetMod       ! array class
      use ESMF_PhysGridMod       ! physical igrid class
      use ESMF_IGridMod           ! igrid class
      use ESMF_RHandleMod        ! route handle class
      use ESMF_RouteMod          ! route class
      use ESMF_InternArrayCommMod      ! array communications class
      use ESMF_FieldDataMapMod   ! field datamap class
      use ESMF_FieldMod          ! field class
      use ESMF_FieldBundleMod         ! bundle class
      use ESMF_RegridTypesMod    ! regrid data types and utilities
      use ESMF_RegridBilinearMod ! methods related to bilinear regrid
      use ESMF_RegridNearNbrMod  ! methods related to nearest-nbr regrid
      use ESMF_RegridConservMod  ! methods related to conservative regrid
      use ESMF_RegridLinearMod   ! methods related to linear regrid
      use ESMF_InitMacrosMod

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
    public ESMF_IArrayRegridStore, ESMF_IArrayRegrid, ESMF_IArrayRegridRelease
    public ESMF_RegridDestroy    ! deallocate memory associated with a regrid
    public ESMF_RegridValidate   ! Error checking and validation
    public ESMF_RegridHasData    ! framework internal use only

!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
         '$Id: ESMF_Regrid.F90,v 1.120.2.3 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IArrayRegridStore
!
! !INTERFACE:
      interface ESMF_IArrayRegridStore
  
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_IArrayRegridStoreOne
        module procedure ESMF_IArrayRegridStoreIndex

! !DESCRIPTION:
!    This interface provides an entry point for methods that compute
!    the data motion needed to execute a data regridding on either a 
!    single {\tt ESMF\_Array}, or a collection of {\tt ESMF\_Array}s
!    which are all based on the same {\tt ESMF\_IGrid}, have the same
!    {\tt ESMF\_FieldDataMap}, have the same {\tt ESMF\_RelLoc}, and
!    are the same data type.

!EOPI
      end interface
!

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_IArrayRegrid
!
! !INTERFACE:
      interface ESMF_IArrayRegrid
  
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_IArrayRegridRunOne
        module procedure ESMF_IArrayRegridRunList

! !DESCRIPTION:
!    This interface provides an entry point for methods that execute
!    the precomputed data motion needed to regrid data from either a 
!    single {\tt ESMF\_Array}, or a collection of {\tt ESMF\_Array}s
!    which are all based on the same {\tt ESMF\_IGrid}, have the same
!    {\tt ESMF\_FieldDataMap}, have the same {\tt ESMF\_RelLoc}, and
!    are the same data type.

!EOPI
      end interface
!

!==============================================================================

      contains

!==============================================================================
!
! This section includes the Regrid Create, Run, and Destroy methods.
!
! As part of adding optimized communication support for both packed and
! loose bundles (which shared a common IGrid, but may or may not have the
! same relloc, rank, datatype, etc.), the code must at some level support
! lists of arrays into a single subroutine call (as opposed to doing a user-
! level loop, passing each array into the subroutine one at a time) so that
! the lower level code has the chance to pack data from more than one array 
! into a single communication call.  (Whether it does or not depends on the
! results of real performance measurements and the specific details of the
! various data movement needed.)
!
! The actual computation of the regridding weights may not need to have
! a list or do a loop, if all the arrays share the same characteristics.
! If the arrays are different enough, it may be better to create different
! route handles and route tables for each regridding, rather than to complicate
! the existing structures with multiple data types, ranks, rellocs, etc.
!
! One option might be to allow a single routehandle to contain multiple
! route tables internally, to allow bundle-level communication calls which
! still take a single routehandle even if the datatypes require different
! precomputed tables.  The bundle-level communications also have the
! requirement that they take lists of field or array names to select only
! a subset of the bundle data to operate on.
!
! So the initial strategy for the code in this file is:
!   - keep a single Regrid Create call which takes a single array, igrid,
!     and datamap for both source and destination.
!   - make multiple entry points at the user API level for lists of arrays
!     vs a single array (only a single igrid, but maybe lists of datamaps also?)
!     and then decide internally whether to make a list of route tables or
!     whether a single table can apply to all arrays.
!   - if this strategy leads to convoluted or overly complex code, then
!     revisit the location where the list vs single item API is broken out.
! 
! nsc 27june05
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridCreate"
!BOPI
! !IROUTINE: ESMF_RegridCreate - Precomputes Regrid data

! !INTERFACE:
      subroutine ESMF_RegridCreate(srcArray, srcIGrid, srcDatamap, hasSrcData, &
                                   dstArray, dstIGrid, dstDatamap, hasDstData, &
                                   parentVM, routehandle, routeIndex, &
                                   regridmethod, regridNorm, srcMask, dstMask, &
                                   rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray),         intent(in   ) :: srcArray
      type(ESMF_IGrid),          intent(inout) :: srcIGrid
      type(ESMF_FieldDataMap),  intent(inout) :: srcDatamap
      logical,                  intent(inout) :: hasSrcData
      type(ESMF_InternArray),         intent(in   ) :: dstArray
      type(ESMF_IGrid),          intent(inout) :: dstIGrid
      type(ESMF_FieldDataMap),  intent(inout) :: dstDatamap
      logical,                  intent(inout) :: hasDstData
      type(ESMF_VM),            intent(in   ) :: parentVM
      type(ESMF_RouteHandle),   intent(inout) :: routehandle
      integer,                  intent(in   ) :: routeIndex
      type(ESMF_RegridMethod),  intent(in   ) :: regridmethod
      type(ESMF_RegridNormOpt), intent(in   ), optional :: regridNorm
      type(ESMF_Mask),          intent(in   ), optional :: srcMask
      type(ESMF_Mask),          intent(in   ), optional :: dstMask
      integer,                  intent(  out), optional :: rc
!
! !DESCRIPTION:
!     Given a source array, igrid, and datamap, this routine precomputes
!     both the communication pattern needed to move data to the proper
!     processors, and the sparse matrix of weights needed to compute the data
!     interpolation for moving data from one igrid to another.
!     This routine returns a handle to the precomputed information in the
!     {\tt routehandle} argument.  This same value should be supplied
!     at run time, along with the actual data pointers.  The same precomputed
!     handle can be used on any data which matches the data arrays, igrid,
!     and datamaps supplied here, so one does not have to generate multiple
!     routehandles for similar data values.
!
!     The arguments are:
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data.
!     \item [srcIGrid]
!           {\tt ESMF\_IGrid} which corresponds to how the data in the
!           source array has been decomposed.
!     \item [srcDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array maps to
!           the specified source igrid.
!     \item [hasSrcData]
!           Logical specifier for whether or not this DE has source data.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing destination data.
!     \item [dstIGrid]
!           {\tt ESMF\_IGrid} which corresponds to how the data in the
!           destination array should be decomposed.
!     \item [dstDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array should map to
!           the specified destination igrid.
!     \item [hasDstData]
!           Logical specifier for whether or not this DE has destination data.
!     \item [routehandle]
!           Returned value which identifies the precomputed Route and other
!           necessary information.
!     \item [routeIndex]
!           Which route (and transform values) inside a routehandle to set.
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
!   \item[ESMF\_REGRID\_METHOD\_BILINEAR  ] bilinear (logically-rectangular igrids)
!   \item[ESMF\_REGRID\_METHOD\_CONSERV1  ] first-order conservative
!   \item[ESMF\_REGRID\_METHOD\_LINEAR    ] linear for 1-d regridding
!   \end{description}
!
!EOPI

      ! TODO: the interfaces have changed - this will no longer be called
      !  with fields, but with the igrid, datamap, and array which are either
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
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVM,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDatamap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDatamap)
      if (hasSrcData) then
        ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit,srcArray,rc)
      endif
      if (hasDstData) then
        ESMF_INIT_CHECK_DEEP(ESMF_InternArrayGetInit,dstArray,rc)
      endif

! Remove to make way for new regrid code. CMD 8/07.
#if 0

      ! TODO: error check for regridnormOpt only for conservative methods
      
      ! Interface change:  the subroutines below used to be functions which
      ! returned a new routehandle.  but now you can add multiple routes to
      ! an existing handle, so the already-created routehandle must be passed
      ! into this routine and down into the method-specific construct code.
      ! it is now an error to pass in an uninitialized routehandle here.
      call ESMF_RouteHandleValidate(routehandle, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      ! Before going further down into the method-specific code, make sure
      ! that this DE has at least src or dst data.   If neither, return now.
      if ((.not.hasSrcData) .and. (.not.hasDstData)) then
          if (present(rc)) rc = ESMF_SUCCESS
          return
      endif

      ! Call the appropriate create routine based on method choice
      select case(regridmethod%regridMethod)

      !-------------
      ! ESMF_REGRID_METHOD_BILINEAR
      case(1)
        call ESMF_RegridConstructBilinear( routehandle,  &
                           srcArray, srcIGrid, srcDatamap, hasSrcData, &
                           dstArray, dstIGrid, dstDatamap, hasDstData, &
                           parentVM, routeIndex, srcMask, dstMask, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_BICUBIC
      case(2)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Bicubic not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_CONSERV1
      case(3)
        call ESMF_RegridConstructConserv( routehandle, &
                                 srcArray, srcIGrid, srcDatamap, hasSrcData, &
                                 dstArray, dstIGrid, dstDatamap, hasDstData, &
                                 parentVM, routeIndex, srcMask, dstMask, &
                                 regridnorm=regridNorm, order=1, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_CONSERV2
      case(4)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Second-order conservative method not yet supported", &
                   ESMF_CONTEXT, rc)
        return
      !   call ESMF_RegridConstructConserv(routehandle, &
      !                          srcArray, srcIGrid, srcDatamap, hasSrcData, &
      !                          dstArray, dstIGrid, dstDatamap, hasDstData, &
      !                          parentVM, routeIndex, srcMask, dstMask, &
      !                          regridnorm=regridNorm, order=2, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_RASTER
      case(5)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Raster method not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_NEAR_NBR
      case(6)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Nearest neighbor method not yet supported", &
                   ESMF_CONTEXT, rc)
        return
      !   call ESMF_RegridConstructNearNbr(routehandle, &
      !                          srcArray, srcIGrid, srcDatamap, hasSrcData, &
      !                          dstArray, dstIGrid, dstDatamap, hasDstData, &
      !                          parentVM, routeIndex, srcMask, dstMask, &
      !                          rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_FOURIER
      case(7)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Fourier transforms not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_LEGENDRE
      case(8)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Legendre transforms not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_INDEX
      case(9)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Index-space methods not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_LINEAR
      case(10)
        call ESMF_RegridConstructLinear( routehandle,  &
                            srcArray, srcIGrid, srcDatamap, hasSrcData, &
                            dstArray, dstIGrid, dstDatamap, hasDstData, &
                            parentVM, routeIndex, srcMask, dstMask, rc=localrc)

      !-------------
      ! ESMF_REGRID_METHOD_SPLINE
      case(11)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "1-d cubic splines not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_REGRIDCOPY
      case(51)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Regrid copied from another regrid not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_SHIFT
      case(52)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Regrid shifted from another regrid not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_ADJOINT
      case(53)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Adjoint from existing regrid not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_FILE
      case(89)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "Regrid read from file not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      ! ESMF_REGRID_METHOD_USER
      case(90)
        dummy=ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                   "User-defined regridding not yet supported", &
                   ESMF_CONTEXT, rc)
        return

      !-------------
      case default
        dummy=ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                    "Invalid method", &
                                    ESMF_CONTEXT, rc)
        return
      end select

      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
#endif

      end subroutine ESMF_RegridCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridRunR4"
!BOPI
! !IROUTINE: ESMF_RegridRunR4 - Performs a regridding between two arrays of type R4

! !INTERFACE:
      subroutine ESMF_RegridRunR4(srcArrayList, srcDataMap, hasSrcData, &
                                  dstArrayList, dstDataMap, hasDstData, &
                                  routehandle, routeIndex, rc)
!
! !ARGUMENTS:

      type(ESMF_InternArray),        intent(in   ) :: srcArrayList(:)
      type(ESMF_FieldDataMap), intent(inout) :: srcDatamap
      logical,                 intent(in   ) :: hasSrcData
      type(ESMF_InternArray),        intent(inout) :: dstArrayList(:)
      type(ESMF_FieldDataMap), intent(inout) :: dstDatamap
      logical,                 intent(in   ) :: hasDstData
      type(ESMF_RouteHandle),  intent(in   ) :: routehandle 
                                                  ! precomputed regrid structure
                                                  ! with regridding info
      integer,                 intent(in   ) :: routeIndex
      integer,                 intent(  out), optional :: rc
!
! !DESCRIPTION:
!   Given a list of source arrays and precomputed regridding information, 
!   this routine regrids the source arrays to new arrays on the destination
!   igrid.  
!
!EOPI

      integer :: localrc
      integer :: i, ii, i1, i2, n, d1, d2, s1, asize, rank, counts(ESMF_MAXDIM)
      integer :: na, narrays
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
      type(ESMF_TypeKind) :: typekind
      type(ESMF_Route) :: rh
      type(ESMF_LocalArray) :: srcindexarr, dstindexarr, weightsarr
      integer :: numlinks
      type(ESMF_LocalArray), pointer :: gatheredArrayList(:)
      type(ESMF_LocalArray), pointer :: srcLocalArrayList(:)
      type(ESMF_TransformValues) :: tv

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDatamap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDatamap)

! Remove to make way for new regrid code.  CMD 8/07.
#if 0

      ! Before going further down into this code, make sure
      ! that this DE has at least src or dst data.   If neither, return now.
      if ((.not.hasSrcData) .and. (.not.hasDstData)) then
          if (present(rc)) rc = ESMF_SUCCESS
          return
      endif

      zero = 0.0
      nullify(srcIndex, dstIndex, weights, gatheredData)
      nullify(dstData2D, dstData3D, gatheredArrayList, srcLocalArrayList)

      ! get the first route from the table and run it to gather the
      ! data values which overlap this bounding box.
      call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                               route=rh, rc=localrc)

      ! get the indirect indices and weights from the routehandle
      asize = 1
      if (hasDstData) then
        call ESMF_RouteHandleGet(routehandle, which_tv=routeIndex, &
                                 tdata=tv, rc=localrc)

        ! get a real f90 pointer from all the arrays
        ! srcIndex, dstIndex and weights TKR can be fixed, but unfortunately the
        ! gatheredData and dstData can be whatever the user wants - so this code
        ! might need to move into another file and be macroized heavily for TKR.
        call ESMF_TransformValuesGet(tv, numlist=numlinks, &
                                     srcindex=srcindexarr, &
                                     dstindex=dstindexarr, &
                                     weights=weightsarr, rc=rc)
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

      ! up to here the code does not depend on the src or dst lists

      narrays = size(srcArrayList)

      ! to get into this code we have already verified the types match in all
      ! the arrays, so just check the first one.
      call ESMF_InternArrayGet(srcArrayList(1), rank=rank, typekind=typekind, &
                           counts=counts, rc=localrc)
  
      allocate(gatheredArrayList(narrays), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "intermediate array pointers", &
                                     ESMF_CONTEXT, rc)) return
      allocate(srcLocalArrayList(narrays), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "intermediate array pointers", &
                                     ESMF_CONTEXT, rc)) return

      do na = 1, narrays

        gatheredArrayList(na) = ESMF_LocalArrayCreate(1, typekind, asize, &
                                                      rc=localrc)
        srcLocalArrayList(na) = srcArrayList(na)
      enddo 
  
      call ESMF_RouteRunList(rh, srcLocalArrayList, gatheredArrayList, rc=localrc)

      ! the rank can't change so we can reuse these.
      allocate(dstDimOrder(rank), &
               srcDimOrder(rank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dim orders", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_FieldDataMapGet(dstDataMap, dataIndexList=dstDimOrder, &
                                rc=localrc)
      call ESMF_FieldDataMapGet(srcDataMap, dataIndexList=srcDimOrder, &
                                rc=localrc)
  
      do na = 1, narrays
  
        ! break out here by rank   TODO: compare datarank to regridrank (or
        !                                compare datamaps)
        if (hasDstData) then
        select case(rank)
  
        ! TODO: apply the weights from src to destination
        ! does this need a nested loop and an array of ESMF_IArrays, one
        ! for each DE which sends data?  i think the answer is not for now
        ! because all data has been pushed into a single array.  but eventually
        ! if we want to start supporting vectors or other complicated data 
        ! shapes, we may have to start preserving the array and datamaps
        ! from the original locations.
  
        !-------------
        case(2) ! 2D data for regrid
  
          call ESMF_LocalArrayGetData(gatheredArrayList(na), gatheredData, &
                                      ESMF_DATA_REF, rc)
          call ESMF_InternArrayGetData(dstArrayList(na), dstData2D, &
            ESMF_DATA_REF, rc=rc)
  
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
  
          call ESMF_LocalArrayGetData(gatheredArrayList(na), gatheredData, &
                                      ESMF_DATA_REF, rc)
          call ESMF_InternArrayGetData(dstArrayList(na), dstData3D, &
            ESMF_DATA_REF, rc=rc)
  
          !*** initialize dest field to zero
     
          dstData3D = zero
  
          !*** for cases where datarank>igridrank, figure out the undecomposed
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
            i1 = lbound(dstData3D,1)
            i2 = ubound(dstData3D,1)
            ii = (i2 - i1) + 1
            do n = 1,numlinks
              d1 = dstIndex((n-1)*2 + 1)
              d2 = dstIndex((n-1)*2 + 2)
              s1 = (srcIndex(n)-1)*ii 
              do i = i1,i2
                dstData3D(i,d1,d2) = dstData3D(i,d1,d2) &
                                   + (gatheredData(s1+i) * weights(n))
              enddo
            enddo
  
          elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.rank) then
  
            !*** do the regrid
            i1 = lbound(dstData3D,rank)
            i2 = ubound(dstData3D,rank)
            do i = i1,i2
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
            i1 = lbound(dstData3D,rank)
            i2 = ubound(dstData3D,rank)
            ii = (i2 - i1) + 1
            do i = i1,i2
              do n = 1,numlinks
                d1 = dstIndex((n-1)*2 + 1)
                d2 = dstIndex((n-1)*2 + 2)
                s1 = (srcIndex(n)-1)*ii + i
                dstData3D(d1,d2,i) = dstData3D(d1,d2,i) &
                                   + (gatheredData(s1) * weights(n))
              enddo
            enddo
  
          elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.1) then
  
            !*** do the regrid
            i1 = lbound(dstData3D,1)
            i2 = ubound(dstData3D,1)
            do n = 1,numlinks
              d1 = dstIndex((n-1)*2 + 1)
              d2 = dstIndex((n-1)*2 + 2)
              do i = i1,i2
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
              dindex(1,1) = lbound(dstData3D,dstUndecomp)
              dindex(1,2) = ubound(dstData3D,dstUndecomp)
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
  
  
        ! nuke temp arrays one by one
        call ESMF_LocalArrayDestroy(gatheredArrayList(na), rc=localrc)
  
      enddo    ! do na=1, narrays

      deallocate(dstDimOrder, &
                 srcDimOrder, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate dim orders", &
                                     ESMF_CONTEXT, rc)) return

      deallocate(gatheredArrayList, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "intermediate array pointers", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(srcLocalArrayList, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "intermediate array pointers", &
                                     ESMF_CONTEXT, rc)) return

      ! set return codes
      if (present(rc)) rc = ESMF_SUCCESS
#endif

      end subroutine ESMF_RegridRunR4

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridRunR8"
!BOPI
! !IROUTINE: ESMF_RegridRunR8 - Performs a regridding between two arrays of type R8

! !INTERFACE:
      subroutine ESMF_RegridRunR8(srcArrayList, srcDataMap, hasSrcData, &
                                  dstArrayList, dstDataMap, hasDstData, &
                                  routehandle, routeIndex, rc)
!
! !ARGUMENTS:

      type(ESMF_InternArray),        intent(in   ) :: srcArrayList(:)
      type(ESMF_FieldDataMap), intent(inout) :: srcDatamap
      logical,                 intent(in   ) :: hasSrcData
      type(ESMF_InternArray),        intent(inout) :: dstArrayList(:)
      type(ESMF_FieldDataMap), intent(inout) :: dstDatamap
      logical,                 intent(in   ) :: hasDstData
      type(ESMF_RouteHandle),  intent(in   ) :: routehandle 
                                                  ! precomputed regrid structure
                                                  ! with regridding info
      integer,                 intent(in   ) :: routeIndex
      integer,                 intent(  out), optional :: rc
!
! !DESCRIPTION:
!   Given a list of source arrays and precomputed regridding information, 
!   this routine regrids the source arrays to new arrays on the destination
!   igrid.  
!
!EOPI

      integer :: localrc
      integer :: i, ii, i1, i2, n, d1, d2, s1, asize, rank, counts(ESMF_MAXDIM)
      integer :: na, narrays
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
      type(ESMF_TypeKind) :: typekind
      type(ESMF_Route) :: rh
      type(ESMF_LocalArray) :: srcindexarr, dstindexarr, weightsarr
      integer :: numlinks
      type(ESMF_LocalArray), pointer :: gatheredArrayList(:)
      type(ESMF_LocalArray), pointer :: srcLocalArrayList(:)
      type(ESMF_TransformValues) :: tv

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDatamap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDatamap)
      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

! Remove to make way for new regrid code. CMD 8/07.
#if 0

      ! Before going further down into this code, make sure
      ! that this DE has at least src or dst data.   If neither, return now.
      if ((.not.hasSrcData) .and. (.not.hasDstData)) then
          if (present(rc)) rc = ESMF_SUCCESS
          return
      endif

      zero = 0.0
      nullify(srcIndex, dstIndex, weights, gatheredData)
      nullify(dstData2D, dstData3D, gatheredArrayList, srcLocalArrayList)

      ! get the first route from the table and run it to gather the
      ! data values which overlap this bounding box.
      call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                               route=rh, rc=localrc)

      ! get the indirect indices and weights from the routehandle
      asize = 1
      if (hasDstData) then
        call ESMF_RouteHandleGet(routehandle, which_tv=routeIndex, &
                                 tdata=tv, rc=localrc)

        ! get a real f90 pointer from all the arrays
        ! srcIndex, dstIndex and weights TKR can be fixed, but unfortunately the
        ! gatheredData and dstData can be whatever the user wants - so this code
        ! might need to move into another file and be macroized heavily for TKR.
        call ESMF_TransformValuesGet(tv, numlist=numlinks, &
                                     srcindex=srcindexarr, &
                                     dstindex=dstindexarr, &
                                     weights=weightsarr, rc=rc)
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

      ! up to here the code does not depend on the src or dst lists

      narrays = size(srcArrayList)

      ! to get into this code we have already verified the types match in all
      ! the arrays, so just check the first one.
      if (hasSrcData) then
        call ESMF_InternArrayGet(srcArrayList(1), rank=rank, typekind=typekind, &
                           counts=counts, rc=localrc)
      else
        call ESMF_InternArrayGet(dstArrayList(1), rank=rank, typekind=typekind, &
                           counts=counts, rc=localrc)
      endif
  
      allocate(gatheredArrayList(narrays), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "intermediate array pointers", &
                                     ESMF_CONTEXT, rc)) return
      allocate(srcLocalArrayList(narrays), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "intermediate array pointers", &
                                     ESMF_CONTEXT, rc)) return

      do na = 1, narrays

        gatheredArrayList(na) = ESMF_LocalArrayCreate(1, typekind, asize, &
                                                      rc=localrc)
        srcLocalArrayList(na) = srcArrayList(na)
      enddo 
  
      call ESMF_RouteRunList(rh, srcLocalArrayList, gatheredArrayList, rc=localrc)

      ! the rank can't change so we can reuse these.
      allocate(dstDimOrder(rank), &
               srcDimOrder(rank), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dim orders", &
                                     ESMF_CONTEXT, rc)) return

      call ESMF_FieldDataMapGet(dstDataMap, dataIndexList=dstDimOrder, &
                                rc=localrc)
      call ESMF_FieldDataMapGet(srcDataMap, dataIndexList=srcDimOrder, &
                                rc=localrc)
  
      do na = 1, narrays
  
        ! break out here by rank   TODO: compare datarank to regridrank (or
        !                                compare datamaps)
        if (hasDstData) then
        select case(rank)
  
        ! TODO: apply the weights from src to destination
        ! does this need a nested loop and an array of ESMF_IArrays, one
        ! for each DE which sends data?  i think the answer is not for now
        ! because all data has been pushed into a single array.  but eventually
        ! if we want to start supporting vectors or other complicated data 
        ! shapes, we may have to start preserving the array and datamaps
        ! from the original locations.
  
        !-------------
        case(2) ! 2D data for regrid
  
          call ESMF_LocalArrayGetData(gatheredArrayList(na), gatheredData, &
                                      ESMF_DATA_REF, rc)
          call ESMF_InternArrayGetData(dstArrayList(na), dstData2D, &
            ESMF_DATA_REF, rc=rc)
  
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
  
          call ESMF_LocalArrayGetData(gatheredArrayList(na), gatheredData, &
                                      ESMF_DATA_REF, rc)
          call ESMF_InternArrayGetData(dstArrayList(na), dstData3D, &
            ESMF_DATA_REF, rc=rc)
  
          !*** initialize dest field to zero
     
          dstData3D = zero
  
          !*** for cases where datarank>igridrank, figure out the undecomposed
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
            i1 = lbound(dstData3D,1)
            i2 = ubound(dstData3D,1)
            ii = (i2 - i1) + 1
            do n = 1,numlinks
              d1 = dstIndex((n-1)*2 + 1)
              d2 = dstIndex((n-1)*2 + 2)
              s1 = (srcIndex(n)-1)*ii 
              do i = i1,i2
                dstData3D(i,d1,d2) = dstData3D(i,d1,d2) &
                                   + (gatheredData(s1+i) * weights(n))
              enddo
            enddo
  
          elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.rank) then
  
            !*** do the regrid
            i1 = lbound(dstData3D,rank)
            i2 = ubound(dstData3D,rank)
            do i = i1,i2
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
            i1 = lbound(dstData3D,rank)
            i2 = ubound(dstData3D,rank)
            ii = (i2 - i1) + 1
            do i = i1,i2
              do n = 1,numlinks
                d1 = dstIndex((n-1)*2 + 1)
                d2 = dstIndex((n-1)*2 + 2)
                s1 = (srcIndex(n)-1)*ii + i
                dstData3D(d1,d2,i) = dstData3D(d1,d2,i) &
                                   + (gatheredData(s1) * weights(n))
              enddo
            enddo
  
          elseif (srcUndecomp.eq.rank .and. dstUndecomp.eq.1) then
  
            !*** do the regrid
            i1 = lbound(dstData3D,1)
            i2 = ubound(dstData3D,1)
            do n = 1,numlinks
              d1 = dstIndex((n-1)*2 + 1)
              d2 = dstIndex((n-1)*2 + 2)
              do i = i1,i2
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
              dindex(1,1) = lbound(dstData3D,dstUndecomp)
              dindex(1,2) = ubound(dstData3D,dstUndecomp)
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
  
  
        ! nuke temp arrays one by one
        call ESMF_LocalArrayDestroy(gatheredArrayList(na), rc=localrc)
  
      enddo    ! do na=1, narrays

      deallocate(dstDimOrder, &
                 srcDimOrder, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "deallocate dim orders", &
                                     ESMF_CONTEXT, rc)) return

      deallocate(gatheredArrayList, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "intermediate array pointers", &
                                     ESMF_CONTEXT, rc)) return
      deallocate(srcLocalArrayList, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "intermediate array pointers", &
                                     ESMF_CONTEXT, rc)) return

      ! set return codes
      if (present(rc)) rc = ESMF_SUCCESS
#endif

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

!      integer :: localrc            ! Error status
!      !type(ESMF_RouteHandle) :: rhandle1
!      type(ESMF_Route) :: route
!      type(ESMF_TransformValues) :: tv

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

      !TODO: IS THIS CODE EVEN CALLED ANYMORE?   now that we don't have
      ! a regrid object, the ESMF_RegridRelease() code calls routehandle
      ! destroy, which frees the internal objects.   this code should be
      ! removed, i believe.   nsc 10/2005

!!     ! ! Does this destroy each of the routes?  it certainly needs to
!!     ! !  destroy the arrays in the TransformValues object.  (i think the
!!     ! !  answer is yes - this code should call route delete here.)
!!
!!      call ESMF_RouteHandleGet(routehandle, which_route=1, route=route, &
!!                                            which_tv=1, tdata=tv, rc=localrc)
!!      call ESMF_RouteDestroy(route, localrc)
!!      call ESMF_TransformValuesDestroy(tv, localrc)
!!      if (ESMF_LogMsgFoundError(localrc, &
!!                                ESMF_ERR_PASSTHRU, &
!!                                ESMF_CONTEXT, rc)) return
!!
!!      ! Set return values.
!!      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridValidate"
! TODO: this routine should be BOP once it is filled in
!BOPI
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
!EOPI

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

      ! 
      ! code goes here
      ! 

      end subroutine ESMF_RegridValidate


!==============================================================================
! These are the user-level, public entry points for calling regridding at
! the Array level. The Field and FieldBundle routines are expected to be more
! useful to the user. These exist, honestly, to fufill a user request that 
! array-level calls exist, but they need both igrid and datamap information, 
! so they are not fundamentally different from the field level calls.   
!
! If this data is on a logically-rectangular igrid, with no explicit coordinate
! information other than the actual index numbers, than it is feasible to
! envision a new index-space array object which is lighter weight than a Field.
! This new array object would need to contain enough information to include 
! what this code needs -- for example: the data relative location (e.g. cell
! centered, north-east corner, north face, etc), how data array indices map 
! to igrid indices (these are both currently in the datamap), plus how this 
! local chunk is decomposed across processors, what the local zero offset is 
! relative to the entire igrid, and what the overall igrid size is (this info 
! is currently in the distgrid object internal to a igrid) -- then these array
! level routines might make more sense as public entry points because they
! could operate with only a single index-space array as an argument and drop
! the igrid and datamap (and hasdata) args for both source and destination.
!
! nsc 27jun05
!==============================================================================

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRegridStoreOne"
!BOP
! !INTERFACE:
      ! Private interface; call using ESMF_IArrayRegridStore()
      subroutine ESMF_IArrayRegridStoreOne(srcArray, srcIGrid, srcDatamap, &
                                          dstArray, dstIGrid, dstDatamap, &
                                          parentVM, routehandle, &
                                          regridmethod, regridnorm, &
                                          srcmask, dstmask, routeOptions, rc) 
!
! !ARGUMENTS:
      type(ESMF_InternArray),         intent(in   ) :: srcArray
      type(ESMF_IGrid),          intent(inout) :: srcIGrid
      type(ESMF_FieldDataMap),  intent(inout) :: srcDatamap
      type(ESMF_InternArray),         intent(in   ) :: dstArray
      type(ESMF_IGrid),          intent(inout) :: dstIGrid
      type(ESMF_FieldDataMap),  intent(inout) :: dstDatamap
      type(ESMF_VM),            intent(in   ) :: parentVM
      type(ESMF_RouteHandle),   intent(out  ) :: routehandle
      type(ESMF_RegridMethod),  intent(in   ) :: regridmethod
      type(ESMF_RegridNormOpt), intent(in   ), optional :: regridnorm
      type(ESMF_Mask),          intent(in   ), optional :: srcmask
      type(ESMF_Mask),          intent(in   ), optional :: dstmask
      type(ESMF_RouteOptions),  intent(in   ), optional :: routeOptions
      integer,                  intent(  out), optional :: rc
!
! !DESCRIPTION:
! Used to Regrid data in an Array.
!
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data.
!     \item [srcIGrid]
!           {\tt ESMF\_IGrid} which corresponds to how the data in the
!           source array has been decomposed.  
!     \item [srcDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array maps to
!           the specified source igrid.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing destination data.
!     \item [dstIGrid]
!           {\tt ESMF\_IGrid} which corresponds to how the data in the
!           destination array should be decomposed.  
!     \item [dstDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array should map to
!           the specified destination igrid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Field}s,
!           most commonly the vm of the Coupler if the regridding is
!           inter-component, but could also be the individual vm for a
!           component if the regridding is intra-component.
!     \item [routehandle]
!           Returned value which identifies the precomputed Route and other
!           necessary information.  This routine creates the handle, so it
!           should not have a previous value or be created before calling
!           this routine.
!     \item [regridmethod]
!           Type of regridding to do.  A set of predefined methods are
!           supplied.
!     \item [{[regridnorm]}]
!           Normalization option, only for specific regrid types.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the regrid.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDatamap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDatamap)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVM,rc)

! Remove to make way for new regrid code.  CMD 8/07.
#if 0
      call ESMF_IArrayRegridStoreIndex(srcArray, srcIGrid, srcDatamap, &
                                      dstArray, dstIGrid, dstDatamap, &
                                      parentVM, routehandle, &
                                      1, ESMF_1TO1HANDLEMAP, 1, &
                                      regridmethod, regridnorm, &
                                      srcmask, dstmask, routeOptions, rc) 

#endif
      end subroutine ESMF_IArrayRegridStoreOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRegridStoreIndex"
!BOP
! !INTERFACE:
      ! Private interface; call using ESMF_IArrayRegridStore()
      subroutine ESMF_IArrayRegridStoreIndex(srcArray, srcIGrid, srcDatamap, &
                                           dstArray, dstIGrid, dstDatamap, &
                                           parentVM, routehandle, routeIndex, &
                                           handleMaptype, routeCount, &
                                           regridmethod, regridnorm, &
                                           srcmask, dstmask, routeOptions, rc) 
!
! !ARGUMENTS:
      type(ESMF_InternArray),         intent(in   ) :: srcArray
      type(ESMF_IGrid),          intent(inout) :: srcIGrid
      type(ESMF_FieldDataMap),  intent(inout) :: srcDatamap
      type(ESMF_InternArray),         intent(in   ) :: dstArray
      type(ESMF_IGrid),          intent(inout) :: dstIGrid
      type(ESMF_FieldDataMap),  intent(inout) :: dstDatamap
      type(ESMF_VM),            intent(in   ) :: parentVM
      type(ESMF_RouteHandle),   intent(inout) :: routehandle
      integer,                  intent(in   ) :: routeIndex
      integer,                  intent(in   ) :: handleMaptype
      integer,                  intent(in   ) :: routeCount
      type(ESMF_RegridMethod),  intent(in   ) :: regridmethod
      type(ESMF_RegridNormOpt), intent(in   ), optional :: regridnorm
      type(ESMF_Mask),          intent(in   ), optional :: srcmask
      type(ESMF_Mask),          intent(in   ), optional :: dstmask
      type(ESMF_RouteOptions),  intent(in   ), optional :: routeOptions
      integer,                  intent(  out), optional :: rc
!
! !DESCRIPTION:
! Used to Regrid data in an Array.
!
!     \begin{description}
!     \item [srcArray]
!           Representative {\tt ESMF\_Array} containing source data.
!     \item [srcIGrid]
!           {\tt ESMF\_IGrid} which corresponds to how the data in the
!           source array has been decomposed.  
!     \item [srcDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array maps to
!           the specified source igrid.
!     \item [dstArray]
!           Representative {\tt ESMF\_Array} containing destination data. 
!     \item [dstIGrid]
!           {\tt ESMF\_IGrid} which corresponds to how the data in the
!           destination array should be decomposed.  
!     \item [dstDatamap]
!           {\tt ESMF\_FieldDataMap} which describes how the array should map to
!           the specified destination igrid.
!     \item [parentVM]
!           {\tt ESMF\_VM} which encompasses both {\tt ESMF\_Field}s,
!           most commonly the vm of the Coupler if the regridding is
!           inter-component, but could also be the individual vm for a
!           component if the regridding is intra-component.
!     \item [routehandle]
!           Returned value which identifies the precomputed Route and other
!           necessary information.  If {\tt routeIndex} is 1, the routehandle
!           is created for the first time.  Otherwise it should be a valid
!           handle and new routes will be added to it.
!     \item [routeIndex]
!           Which route inside a route handle is to be computed.
!     \item [routeCount]
!           If {\tt routeIndex} is 1, this is the first route being created,
!           and the routehandle should be created.  The {\tt routeCount} helps
!           allocate the right amount of space for routes and transform values.
!           For all other values of {\tt routeIndex}, this is ignored.
!     \item [regridmethod]
!           Type of regridding to do.  A set of predefined methods are
!           supplied.
!     \item [{[regridnorm]}]
!           Normalization option, only for specific regrid types.
!     \item [{[srcmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid source data.
!     \item [{[dstmask]}]
!           Optional {\tt ESMF\_Mask} identifying valid destination data.
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the regrid.
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

    integer :: localrc        ! local error status
    type(ESMF_Route) :: route
    logical :: hasSrcData           ! does this DE contain localdata from src?
    logical :: hasDstData           ! does this DE contain localdata from dst?

    ! assume failure until success certain
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDatamap)
    ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDatamap)
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVM,rc)

#if 0
    ! TODO: add code here
    !  The form of this code depends on how we organize the interfaces
    !  between the Regrid code and this code.  This is the lowest level
    !  public interface to the Regrid code, so anything we do below
    !  here will be internal interfaces and not public.  The interfaces
    !  need to be as useful to the regrid code as possible.

    if (routeIndex .eq. 1) then
        routehandle = ESMF_RouteHandleCreate(rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! set the type and the eventual number of routes this handle will have
        call ESMF_RouteHandleSet(routehandle, htype=ESMF_REGRIDHANDLE, &
                                 route_count=routeCount, &
                                 rmaptype=handleMaptype, &
                                 tv_count=routeCount, &
                                 tvmaptype=handleMaptype, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
    else
       ! check handle
       ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

       ! make sure it is a valid handle before going on
       call ESMF_RouteHandleValidate(routehandle, rc=localrc)
       if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return
    endif

    ! determine if this local DE either src and/or dst data
    hasSrcData = ESMF_RegridHasData(srcIGrid, srcDatamap)
    hasDstData = ESMF_RegridHasData(dstIGrid, dstDatamap)

    ! Before going further down into this code, make sure
    ! that this DE has at least src or dst data.   If neither, return now.
    if ((.not.hasSrcData) .and. (.not.hasDstData)) then
        if (present(rc)) rc = ESMF_SUCCESS
        return
    endif


    call ESMF_RegridCreate(srcArray, srcIGrid, srcDatamap, hasSrcData, & 
                           dstArray, dstIGrid, dstDatamap, hasDstData, &
                           parentVM, routehandle, routeIndex, regridmethod, &
                           regridnorm=regridnorm, &
                           srcmask=srcmask, dstmask=dstmask, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, &
                              ESMF_ERR_PASSTHRU, &
                              ESMF_CONTEXT, rc)) return

    ! control over internal communications strategy
    if (present(routeOptions)) then
        call ESMF_RouteHandleGet(routehandle, which_route=routeIndex, &
                                 route=route, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        ! Set the route options
        call c_ESMC_RouteSet(route, routeOptions, localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
    endif

    ! set return code if user specified it
    if (present(rc)) rc = ESMF_SUCCESS
#endif

    end subroutine ESMF_IArrayRegridStoreIndex

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRegridRunOne"
!BOP
! !INTERFACE:
      ! Private interface; call using ESMF_IArrayRegrid()
      subroutine ESMF_IArrayRegridRunOne(srcArray, srcDataMap, hasSrcData, &
                                        dstArray, dstDataMap, hasDstData, &
                                        routehandle, routeIndex, &
                                        srcmask, dstmask, &
                                        blocking, commhandle, routeOptions, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray),        intent(inout) :: srcArray
      type(ESMF_FieldDataMap), intent(inout) :: srcDataMap
      logical,                 intent(in   ) :: hasSrcData
      type(ESMF_InternArray),        intent(inout) :: dstArray
      type(ESMF_FieldDataMap), intent(inout) :: dstDataMap
      logical,                 intent(in   ) :: hasDstData
      type(ESMF_RouteHandle),  intent(in   ) :: routehandle
      integer,                 intent(in   ), optional :: routeIndex
      type(ESMF_Mask),         intent(in   ), optional :: srcmask
      type(ESMF_Mask),         intent(in   ), optional :: dstmask
      type(ESMF_BlockingFlag), intent(in   ), optional :: blocking
      type(ESMF_CommHandle),   intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in   ), optional :: routeOptions
      integer,                 intent(  out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt Regrid} operation over the data in an {\tt ESMF\_Array}.
!     This routine updates the data inside the {\tt ESMF\_Array} in place.
!     It uses a precomputed {\tt ESMF\_Route} for the communications
!     pattern.
!
!     \begin{description}
!     \item [srcArray]
!           {\tt ESMF\_Array} containing source data to be regridded.
!     \item [srcDataMap]
!           {\tt ESMF\_FieldDataMap} which describes how the array maps to
!           the specified source igrid.
!     \item [hasSrcData]
!           Logical specifier for whether or not this DE has source data.
!     \item [dstArray]
!           {\tt ESMF\_Array} containing locations to put regridded data.
!     \item [dstDataMap]
!           {\tt ESMF\_FieldDataMap} which describes how the array maps to
!           the specified destination igrid.
!     \item [hasDstData]
!           Logical specifier for whether or not this DE has destination data.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} has been precomputed.
!     \item [{[routeIndex]}]
!           If specified, which route inside the handle to execute.  The
!           default value is 1.
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
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc        ! local error status
      logical :: dummy
      type(ESMF_TypeKind) :: srcTypeKind, dstTypeKind, dataTypeKind
      type(ESMF_Route) :: route
      type(ESMF_InternArray) :: srcArrayList(1), dstArrayList(1)
      integer :: routenum

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDataMap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDataMap)
      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

! Remove to make way for new regrid. CMD 8/07.
#if 0 
      ! Before going further down into this code, make sure
      ! that this DE has at least src or dst data.   If neither, return now.
      if ((.not.hasSrcData) .and. (.not.hasDstData)) then
          if (present(rc)) rc = ESMF_SUCCESS
          return
      endif

      ! get datakinds from the two arrays
      if (hasSrcData) then
        call ESMF_InternArrayGet(srcArray, typekind=srcTypeKind, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
        dataTypeKind = srcTypeKind
      endif
      if (hasDstData) then
        call ESMF_InternArrayGet(dstArray, typekind=dstTypeKind, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
        dataTypeKind = dstTypeKind
      endif

      ! default to 1 if routeIndex not specified
      if (present(routeIndex)) then
          routenum = routeIndex
      else
          routenum = 1
      endif

      ! control over internal communications strategy
      if (present(routeOptions)) then
          call ESMF_RouteHandleGet(routehandle, which_route=routenum, &
                                   route=route, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          ! Set the route options
          call c_ESMC_RouteSet(route, routeOptions, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      endif

      ! TODO: the R4 and R8 routines should have consistent arg lists.
      ! are the "has data" args required or not?

      srcArrayList(1) = srcArray
      dstArrayList(1) = dstArray

      ! Execute the communications call based on datakinds
      if (dataTypeKind.eq.ESMF_TYPEKIND_R4) then
        call ESMF_RegridRunR4(srcArrayList, srcDataMap, hasSrcData, &
                              dstArrayList, dstDataMap, hasDstData, &
                              routehandle, routenum, localrc)
      elseif (dataTypeKind.eq.ESMF_TYPEKIND_R8) then
        call ESMF_RegridRunR8(srcArrayList, srcDataMap, hasSrcData, &
                              dstArrayList, dstDataMap, hasDstData, &
                              routehandle, routenum, localrc)
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
#endif

      end subroutine ESMF_IArrayRegridRunOne

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRegridRunList"
!BOP
! !INTERFACE:
      ! Private interface; call using ESMF_IArrayRegrid()
      subroutine ESMF_IArrayRegridRunList(srcArrayList, srcDataMap, hasSrcData, &
                                         dstArrayList, dstDataMap, hasDstData, &
                                         routehandle, routeIndex, &
                                         srcmask, dstmask, &
                                         blocking, commhandle, routeOptions, rc)
!
! !ARGUMENTS:
      type(ESMF_InternArray),        intent(inout) :: srcArrayList(:)
      type(ESMF_FieldDataMap), intent(inout) :: srcDataMap
      logical,                 intent(in   ) :: hasSrcData
      type(ESMF_InternArray),        intent(inout) :: dstArrayList(:)
      type(ESMF_FieldDataMap), intent(inout) :: dstDataMap
      logical,                 intent(in   ) :: hasDstData
      type(ESMF_RouteHandle),  intent(in   ) :: routehandle
      integer,                 intent(in   ), optional :: routeIndex
      type(ESMF_Mask),         intent(in   ), optional :: srcmask
      type(ESMF_Mask),         intent(in   ), optional :: dstmask
      type(ESMF_BlockingFlag), intent(in   ), optional :: blocking
      type(ESMF_CommHandle),   intent(inout), optional :: commhandle
      type(ESMF_RouteOptions), intent(in   ), optional :: routeOptions
      integer,                 intent(  out), optional :: rc
!
! !DESCRIPTION:
!     Perform a {\tt Regrid} operation over the data in an {\tt ESMF\_Array}.
!     This routine updates the data inside the {\tt ESMF\_Array} in place.
!     It uses a precomputed {\tt ESMF\_Route} for the communications
!     pattern.
!
!     \begin{description}
!     \item [srcArrayList]
!           List of {\tt ESMF\_Array}s containing source data to be regridded.
!     \item [srcDataMap]
!           {\tt ESMF\_FieldDataMap} which describes how the arrays map to
!           the specified source igrid.
!     \item [hasSrcData]
!           Logical specifier for whether or not this DE has source data.
!     \item [dstArrayList]
!           List of {\tt ESMF\_Array}s containing locations to put regridded 
!           data into.
!     \item [dstDataMap]
!           {\tt ESMF\_FieldDataMap} which describes how the arrays map to
!           the specified destination igrid.
!     \item [hasDstData]
!           Logical specifier for whether or not this DE has destination data.
!     \item [routehandle]
!           {\tt ESMF\_RouteHandle} has been precomputed.
!     \item [{[routeIndex]}]
!           If specified, which route inside the handle to execute.  The
!           default value is 1.
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
!     \item [{[routeOptions]}]
!           Not normally specified.  Specify which internal strategy to select
!           when executing the communication needed to execute the
!           See Section~\ref{opt:routeopt} for possible values.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: localrc        ! local error status
      logical :: dummy
      type(ESMF_TypeKind) :: srcTypeKind, dstTypeKind
      type(ESMF_Route) :: route
      integer :: routenum

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDataMap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDataMap)
      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

! Remove to make way for new regrid. CMD 8/07.
#if 0 
      ! Before going further down into this code, make sure
      ! that this DE has at least src or dst data.   If neither, return now.
      if ((.not.hasSrcData) .and. (.not.hasDstData)) then
          if (present(rc)) rc = ESMF_SUCCESS
          return
      endif

      ! get datakinds from the two arrays
      call ESMF_InternArrayGet(srcArrayList(1), typekind=srcTypeKind, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_InternArrayGet(dstArrayList(1), typekind=dstTypeKind, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! default to 1 if routeIndex not specified
      if (present(routeIndex)) then
          routenum = routeIndex
      else
          routenum = 1
      endif

      ! control over internal communications strategy
      if (present(routeOptions)) then
          call ESMF_RouteHandleGet(routehandle, which_route=routenum, &
                                   route=route, rc=localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return

          ! Set the route options
          call c_ESMC_RouteSet(route, routeOptions, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      endif

      ! TODO: the R4 and R8 routines should have consistent arg lists.
      ! are the "has data" args required or not?

      ! Execute the communications call based on datakinds
      if (srcTypeKind.eq.ESMF_TYPEKIND_R4 .AND. &
          dstTypeKind.eq.ESMF_TYPEKIND_R4) then
        call ESMF_RegridRunR4(srcArrayList, srcDataMap, hasSrcData,  &
                              dstArrayList, dstDataMap, hasDstData, &
                              routehandle, routenum, localrc)
      elseif (srcTypeKind.eq.ESMF_TYPEKIND_R8 .AND. &
              dstTypeKind.eq.ESMF_TYPEKIND_R8) then
        call ESMF_RegridRunR8(srcArrayList, srcDataMap, hasSrcData, &
                              dstArrayList, dstDataMap, hasDstData, &
                              routehandle, routenum, localrc)
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
#endif

      end subroutine ESMF_IArrayRegridRunList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IArrayRegridRelease"
!BOP
! !INTERFACE:
      subroutine ESMF_IArrayRegridRelease(routehandle, rc)
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
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

! Remove to make way for new regrid. CMD 8/07.
#if 0

      call ESMF_RouteHandleDestroy(routehandle, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
#endif

      end subroutine ESMF_IArrayRegridRelease


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridHasData"
!BOPI
! !INTERFACE:
      function ESMF_RegridHasData(igrid, datamap, rc)
!
! !RETURN VALUE:
      logical :: ESMF_RegridHasData

! !ARGUMENTS:
      type(ESMF_IGrid), intent(inout) :: igrid
      type(ESMF_FieldDatamap), intent(inout) :: datamap
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return whether this local DE contains any data values or not.
!
!     \begin{description}
!     \item [igrid]
!           {\tt ESMF\_IGrid}.
!     \item [datamap]
!           {\tt ESMF\_FieldDataMap}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!
!EOPI

      integer :: localrc        ! local error status
      type(ESMF_Relloc) :: relloc
      integer :: itemcount

      ! initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

! Remove to make way for new regrid. CMD 8/07.
#if 0

      ESMF_RegridHasData = .false.

      call ESMF_IGridValidate(igrid, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_FieldDataMapValidate(datamap, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! determine if this local DE either src and/or dst data
      call ESMF_FieldDataMapGet(datamap, horzRelloc=relloc, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_IGridGetDELocalInfo(igrid, horzRelLoc=relloc, &
                                   localCellCount=itemcount, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! hasdata is false, only set it to true if
      ! this DE has more than 0 cells
      if (itemcount.gt.0) ESMF_RegridHasData = .true.


      if (present(rc)) rc = ESMF_SUCCESS
#endif

      end function ESMF_RegridHasData


   end module ESMF_RegridMod
