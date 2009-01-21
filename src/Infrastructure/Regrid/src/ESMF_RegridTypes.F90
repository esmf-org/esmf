! $Id: ESMF_RegridTypes.F90,v 1.95.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_RegridTypes.F90"
!
!     ESMF Regrid Types Module
      module ESMF_RegridTypesMod
!
!==============================================================================
!
! ***THIS CODE IS CURRENTLY NON-FUNCTIONAL WHILE WE BRING IN A NEW,
! ***MORE GENERAL REGRIDDING ENGINE.  It REMAINS HERE FOR THE TIME BEING
! ***BECAUSE WE ANTICIPATE REUSING PARTS OF THE INTERFACE.
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
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod       ! ESMF base   class
      use ESMF_VMMod
      use ESMF_DELayoutMod
      use ESMF_LocalArrayMod
      use ESMF_InternArrayDataMapMod
      use ESMF_InternArrayMod! ESMF internal array  class
      use ESMF_PhysGridMod   ! ESMF physical igrid class
      use ESMF_IGridTypesMod  ! ESMF igrid   class
      use ESMF_IGridMod       ! ESMF igrid   class
      use ESMF_RHandleMod    ! ESMF route handle class
      use ESMF_RouteMod      ! ESMF route  class
      use ESMF_FieldDataMapMod
      use ESMF_InternArrayCommMod
      use ESMF_InitMacrosMod

      implicit none

!------------------------------------------------------------------------------
!     !  ESMF_RegridIndexType
!
!     ! Description of ESMF_RegridIndexType.

      type ESMF_RegridIndexType
      sequence
      private

        integer, dimension(:), pointer :: srcMinIndex, srcMaxIndex

        integer, dimension(:,:), pointer :: dstMinIndex, dstMaxIndex

        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RegridIndex
!
!     !  The RegridIndex data structure that is passed between languages.

      type ESMF_RegridIndex
      sequence
      private
        type(ESMF_RegridIndexType), pointer :: ptr     ! pointer to a regrid index
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RegridMethod
!
!     ! Description of ESMF_RegridMethod

      type ESMF_RegridMethod
      sequence
        integer :: regridMethod
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RegridDistrbOpt
!
!     ! Description of ESMF_RegridDistrbOpt

      type ESMF_RegridDistrbOpt
      sequence
        integer :: regridDistrbOpt
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RegridNormOpt
!
!     ! Description of ESMF_RegridNormOpt

      type ESMF_RegridNormOpt
      sequence
        integer :: regridNormOpt
      end type

!------------------------------------------------------------------------------
!     !  ESMF_RegridType
!
!     ! Description of ESMF_RegridType.
!     ! TODO: the regrid type is not currently used -- as documented in 
!     !       ESMF_Regrid.F90, regrid currently returns a RouteHandle in order
!     !       to leverage that code.  This regrid object is left here as a
!     !       placeholder in case users ever request the capability to access
!     !       a regrid object that would allow them to query some of its
!     !       settings.  However, the RouteHandle contains all information
!     !       necessary for actually applying a regrid.

      type ESMF_RegridType
      sequence
      private
        type (ESMF_Base) :: base

        type(ESMF_InternArray) :: & 
           srcArray,   &! source array
           dstArray     ! destination array

        type (ESMF_IGrid) :: &
           srcIGrid,    &! source igrid
           dstIGrid      ! destination igrid

        type (ESMF_FieldDataMap) :: &
           srcDatamap,    &! source datamap
           dstDatamap      ! destination datamap

        type (ESMF_RegridMethod) ::       &
           method         ! method used for this regridding

        type (ESMF_RegridDistrbOpt) ::       &
           distrbOpt      ! option for distributing data for regrid

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
      ! Supported ESMF Regrid Methods
      !   NONE       =  no regridding or undefined regrid
      !   BILINEAR   =  bilinear (logically-rect igrids)
      !   BICUBIC    =  bicubic  (logically-rect igrids)
      !   CONSERV1   =  1st-order conservative
      !   CONSERV2   =  2nd-order conservative
      !   RASTER     =  regrid by rasterizing domain
      !   NEAR_NBR   =  nearest-neighbor dist-weighted avg
      !   FOURIER    =  Fourier transform
      !   LEGENDRE   =  Legendre transform
      !   INDEX      =  index-space regrid (shift, stencil)
      !   LINEAR     =  linear for 1-d regridding
      !   SPLINE     =  cubic spline for 1-d regridding
      !   REGRIDCOPY =  copy existing regrid
      !   SHIFT      =  shift addresses of existing regrid
      !   ADJOINT    =  create adjoint of existing regrid
      !   FILE       =  read a regrid from a file
      !   USER       =  user-supplied method
  
      type (ESMF_RegridMethod), parameter, public ::  &! regrid methods
         ESMF_REGRID_METHOD_NONE       = ESMF_RegridMethod( 0), &
         ESMF_REGRID_METHOD_BILINEAR   = ESMF_RegridMethod( 1), &
         ESMF_REGRID_METHOD_BICUBIC    = ESMF_RegridMethod( 2), &
         ESMF_REGRID_METHOD_CONSERV1   = ESMF_RegridMethod( 3), &
         ESMF_REGRID_METHOD_CONSERV2   = ESMF_RegridMethod( 4), &
         ESMF_REGRID_METHOD_RASTER     = ESMF_RegridMethod( 5), &
         ESMF_REGRID_METHOD_NEAR_NBR   = ESMF_RegridMethod( 6), &
         ESMF_REGRID_METHOD_FOURIER    = ESMF_RegridMethod( 7), &
         ESMF_REGRID_METHOD_LEGENDRE   = ESMF_RegridMethod( 8), &
         ESMF_REGRID_METHOD_INDEX      = ESMF_RegridMethod( 9), &
         ESMF_REGRID_METHOD_LINEAR     = ESMF_RegridMethod(10), &
         ESMF_REGRID_METHOD_SPLINE     = ESMF_RegridMethod(11), &
         ESMF_REGRID_METHOD_REGRIDCOPY = ESMF_RegridMethod(51), &
         ESMF_REGRID_METHOD_SHIFT      = ESMF_RegridMethod(52), &
         ESMF_REGRID_METHOD_ADJOINT    = ESMF_RegridMethod(53), &
         ESMF_REGRID_METHOD_FILE       = ESMF_RegridMethod(89), &
         ESMF_REGRID_METHOD_USER       = ESMF_RegridMethod(90)

      ! Supported ESMF Regrid Distribution Options
      !   NONE       =  no data motion required or undefined
      !   SOURCE     =  redistribute source field
      !   DEST       =  redistribute destination field
      !   BOTH       =  redistribute both 

      type (ESMF_RegridDistrbOpt), parameter, public ::  &! distribution options
         ESMF_REGRID_DISTRB_NONE   =  ESMF_RegridDistrbOpt(0), &
         ESMF_REGRID_DISTRB_SOURCE =  ESMF_RegridDistrbOpt(1), &
         ESMF_REGRID_DISTRB_DEST   =  ESMF_RegridDistrbOpt(2), &
         ESMF_REGRID_DISTRB_BOTH   =  ESMF_RegridDistrbOpt(3)

      ! Supported ESMF Regrid Normalization Options
      !   UNKNOWN    =  unknown or undefined normalization
      !   NONE       =  no normalization
      !   DSTAREA    = 
      !   FRACAREA   = 

      type (ESMF_RegridNormOpt), parameter, public ::  &! normalization options
         ESMF_REGRID_NORM_UNKNOWN  = ESMF_RegridNormOpt(0), &
         ESMF_REGRID_NORM_NONE     = ESMF_RegridNormOpt(1), &
         ESMF_REGRID_NORM_DSTAREA  = ESMF_RegridNormOpt(2), &
         ESMF_REGRID_NORM_FRACAREA = ESMF_RegridNormOpt(3)

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Regrid
      public ESMF_RegridMethod
      public ESMF_RegridDistrbOpt
      public ESMF_RegridNormOpt

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
    public ESMF_RegridAddLink         ! Adds address pair and weight to regrid
    public ESMF_RegridRouteConstruct  ! Constructs a Route used by Regrid to
                                      ! gather data
    public ESMF_RegridGet             ! returns a regrid attribute
    public ESMF_RegridSet             ! sets    a regrid attribute
    public ESMF_RegridCreateEmpty     ! creates an empty regrid structure
    public ESMF_RegridConstructEmpty  ! constructs an empty regrid structure
    public ESMF_RegridDestruct        ! deallocate memory associated with a regrid
    public ESMF_RegridIndexCreate     ! creates a RegridIndex structure
    public ESMF_RegridIndexDestroy    ! deallocate memory associated with a
                                      ! RegridIndex structure
    public ESMF_VertBinsCreate        ! creates vertical bins for use in regridding
    public ESMF_VertBinsDestroy       ! destroys vertical bins for use in regridding

    public operator(==), operator(/=) ! for overloading method and option
                                      ! comparison functions
!
!EOPI
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_RegridTypes.F90,v 1.95.2.2 2009/01/21 21:25:23 cdeluca Exp $'

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
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (==)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridDistrbOptEqual
         module procedure ESMF_RegridMethodEqual
         module procedure ESMF_RegridNormOptEqual

! !DESCRIPTION:
!     This interface overloads the equality operator for the specific
!     ESMF regrid method, normalization, and distribution data types.  It is
!     provided for easy comparisons of these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface operator (/=)

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_RegridDistrbOptNotEqual
         module procedure ESMF_RegridMethodNotEqual
         module procedure ESMF_RegridNormOptNotEqual

! !DESCRIPTION:
!     This interface overloads the inequality operator for the specific
!     ESMF regrid method, normalization, and distribution data types.  It is
!     provided for easy comparisons of these types with defined values.
!
!EOPI
      end interface
!
!------------------------------------------------------------------------------
!==============================================================================

      contains

!==============================================================================
!
! This section includes utility routines for performing actions on the
! main Regrid type.  All other create methods are implemented in another
! module to facilitate branching based on type of regridding required.
!
!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridIndexTypeGetInit"
 !BOPI
! !IROUTINE: ESMF_RegridIndexTypeGetInit - Get initialization status.

! !INTERFACE:
   function ESMF_RegridIndexTypeGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_RegridIndexType), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_RegridIndexTypeGetInit
!
! !DESCRIPTION:
! Get the initialization status of the
! shallow class {\tt RegridIndexType}.
!
! The arguments are:
! \begin{description}
! \item [s]
! {\tt ESMF\_RegridIndexType} from which to retreive status.
! \end{description}
!
!EOPI
       if (present(s)) then
         ESMF_RegridIndexTypeGetInit = ESMF_INIT_GET(s)
       else
         ESMF_RegridIndexTypeGetInit = ESMF_INIT_DEFINED
       endif

     end function ESMF_RegridIndexTypeGetInit


!------------------------------------------------------------------------------
#undef ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridIndexTypeInit"
 !BOPI
! !IROUTINE: ESMF_RegridIndexTypeInit - Initialize RegridIndexType variable.

! !INTERFACE:
   subroutine ESMF_RegridIndexTypeInit(s)
!
! !ARGUMENTS:
       type(ESMF_RegridIndexType) :: s
!
! !DESCRIPTION:
! Initialize the class {\tt RegridIndexType}.
!
! The arguments are:
! \begin{description}
! \item [s]
! {\tt ESMF\_RegridIndexType} to initialize.
! \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
     end subroutine ESMF_RegridIndexTypeInit
 
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridIndexTypeValidate"
!BOPI
! !IROUTINE: ESMF_RegridIndexTypeValidate - Check validity of a RegridIndexType
!
! !INTERFACE:
      subroutine ESMF_RegridIndexTypeValidate(s, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridIndexType) :: s
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Validates that the {\tt RegridIndexType} is internally consistent.
!     Currently this method determines if the {\tt RegridIndexType} is uninitialized
!     or already destroyed.  The method returns an error code if problems
!     are found.
!
!     The arguments are:
!     \begin{description}
!     \item[s]
!       The {\tt ESMF\_RegridIndexType} to validate.
!     \item[{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if the  {\tt RegridIndexType}
!       is valid.
!     \end{description}
!
!
!EOPI
     ESMF_INIT_CHECK_SHALLOW(ESMF_RegridIndexTypeGetInit,ESMF_RegridIndexTypeInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif


end subroutine ESMF_RegridIndexTypeValidate

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
!          needed when moving data from one igrid to another.
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
      !type(ESMF_InternArray) :: &! temps for use when re-sizing arrays
      !   srcAddTmp, dstAddTmp, weightsTmp

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)

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
      subroutine ESMF_RegridAddLink2D(tv, srcAdd, dstAdd, weight, aggregate, &
                                      index, rc)
!
! !ARGUMENTS:

      type(ESMF_TransformValues), intent(inout) :: tv
      integer, intent(in) :: srcAdd
      integer, dimension(2), intent(in) :: dstAdd
      real(kind=ESMF_KIND_R8), intent(in) :: weight
      logical, intent(in), optional :: aggregate
      type(ESMF_RegridIndex), intent(inout), optional :: index
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Adds an address pair and regrid weight to an existing regrid.
!
!     The arguments are:
!     \begin{description}
!     \item[tv]
!          Stored information related to the actual data transformation
!          needed when moving data from one igrid to another.
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
!     \item[{[index]}]
!          Optional regrid index structure to help speed up the search for
!          aggregated weights.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: !  TODO

      integer :: localrc
      integer :: numList, i, i1, startIndex, stopIndex
      integer, dimension(:), pointer :: srcPtr, dstPtr
      logical :: aggregateUse, newLink
      real(kind=ESMF_KIND_R8), dimension(:), pointer :: wgtPtr
      type(ESMF_LocalArray) :: srcIndex, dstIndex, weights

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_RegridIndexGetInit,index,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_TransformValuesGetInit,tv,rc)

      newLink      = .true.
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

      ! if the weights are being aggregated, calculate the search indices
      ! either from the default (the whole damn thang) or the optional
      ! RegridIndex
      startIndex = 1
      stopIndex  = numList
      if (aggregateUse .AND. present(index)) then
        startIndex = max(index%ptr%srcMinIndex(srcAdd), &
                         index%ptr%dstMinIndex(dstAdd(1),dstAdd(2)))
        stopIndex  = min(index%ptr%srcMaxIndex(srcAdd), &
                         index%ptr%dstMaxIndex(dstAdd(1),dstAdd(2)))
        if (startIndex.eq.0) then
          startIndex = 1
          stopIndex  = 1
        endif
      endif 

      ! if the aggregation flag is set, search through the already existing
      ! links for the current address pair
      if (aggregateUse .and. numList.ne.0) then
        do i = startIndex,stopIndex
          i1 = 2*(i-1)
          if ((dstPtr(i1+1) .eq. dstAdd(1)) .AND. &
              (dstPtr(i1+2) .eq. dstAdd(2)) .AND. &
              (srcPtr(i)    .eq. srcAdd)) then
            newLink   = .false.
            wgtPtr(i) = wgtPtr(i) + weight
            exit
          endif
        enddo
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
        if (present(index)) then
          if (index%ptr%srcMinIndex(srcAdd).eq.0) &
              index%ptr%srcMinIndex(srcAdd) = numList
          index%ptr%srcMaxIndex(srcAdd) = numList
          if (index%ptr%dstMinIndex(dstAdd(1),dstAdd(2)).eq.0) &
              index%ptr%dstMinIndex(dstAdd(1),dstAdd(2)) = numList
          index%ptr%dstMaxIndex(dstAdd(1),dstAdd(2)) = numList
        endif
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
      function ESMF_RegridRouteConstruct(dimCount, srcIGrid, dstIGrid, &
                                         recvDomainList, parentVM, &
                                         srcDatamap, dstDatamap, &
                                         hasSrcData, hasDstData, &
                                         srcArray, dstArray, &
                                         reorder, total, layer, rc)
!
! !RETURN VALUE:
      type(ESMF_Route) :: ESMF_RegridRouteConstruct
!
! !ARGUMENTS:

      integer,                 intent(in   ) :: dimCount
      type(ESMF_IGrid),         intent(inout) :: srcIGrid
      type(ESMF_IGrid),         intent(inout) :: dstIGrid
      type(ESMF_DomainList),   intent(inout) :: recvDomainList
      type(ESMF_VM),           intent(in   ) :: parentVM
      type(ESMF_FieldDataMap), intent(inout) :: srcDatamap
      type(ESMF_FieldDataMap), intent(inout) :: dstDatamap
      logical,                 intent(in   ), optional :: hasSrcData
      logical,                 intent(in   ), optional :: hasDstData
      type(ESMF_InternArray),        intent(in   ), optional :: srcArray
      type(ESMF_InternArray),        intent(in   ), optional :: dstArray
      logical,                 intent(in   ), optional :: reorder
      logical,                 intent(in   ), optional :: total
      logical,                 intent(in   ), optional :: layer
      integer,                 intent(  out), optional :: rc
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
      logical :: layerUse, totalUse, hasSrcDataUse, hasDstDataUse
      type(ESMF_DELayout) :: srcDELayout, dstDELayout
      type(ESMF_DomainList) :: sendDomainList
      type(ESMF_RelLoc) :: dstHorzRelLoc, srcHorzRelLoc
      type(ESMF_Route) :: route

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDatamap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDatamap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,recvDomainList)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit,parentVM,rc)

      ! use optional arguments if present
      totalUse = .false.
      if (present(total)) totalUse = total
      layerUse = .false.
      if (present(layer)) layerUse = layer
      hasSrcDataUse = .false.
      if (present(srcArray))   hasSrcDataUse = .true.
      if (present(hasSrcData)) hasSrcDataUse = hasSrcData
      hasDstDataUse = .false.
      if (present(dstArray))   hasDstDataUse = .true.
      if (present(hasDstData)) hasDstDataUse = hasDstData

      ! Extract some information for use in this regrid.
      call ESMF_FieldDataMapGet(srcDataMap, horzRelloc=srcHorzRelLoc, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call ESMF_FieldDataMapGet(dstDataMap, horzRelloc=dstHorzRelLoc, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! calculate intersections
      if (hasSrcDataUse) then
        call ESMF_IGridBoxIntersectSend(srcIGrid, dstIGrid, sendDomainList, &
                                       totalUse, layerUse, &
                                       srcHorzRelLoc, dstHorzRelLoc, rc=localrc)
        if (ESMF_LogMsgFoundError(localrc, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      call ESMF_IGridBoxIntersectRecv(srcIGrid, dstIGrid, &
                                     parentVM, recvDomainList, &
                                     hasSrcDataUse, hasDstDataUse, &
                                     totalUse, layerUse, &
                                     srcHorzRelLoc, dstHorzRelLoc, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Modify DomainLists for Array AI's and to add ranks larger than IGrid dimensions

      ! sendDomainList first
      if (hasSrcDataUse .AND. present(srcArray)) then
        call ESMF_RegridDomainListModify('send', dimCount, sendDomainList, srcArray, &
                                         srcIGrid, srcDataMap, totalUse, rc)
      endif

      ! recvDomainList next
      if (hasDstDataUse .AND. present(dstArray)) then
        call ESMF_RegridDomainListModify('recv', dimCount, recvDomainList, dstArray, &
                                         dstIGrid, dstDataMap, totalUse, rc)
      endif

      ! get layouts for Route calculation
      call ESMF_IGridGet(srcIGrid, delayout=srcDELayout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_IGridGet(dstIGrid, delayout=dstDELayout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Create Route
      route = ESMF_RouteCreate(parentVM, localrc)
      call ESMF_RoutePrecomputeDomList(route, dimCount, &
                                       srcDELayout, dstDELayout, &
                                       sendDomainList, recvDomainList, &
                                       hasSrcDataUse, hasDstDataUse, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! set size of recv items in Route
      call ESMF_RouteSetRecvItems(route, recvDomainList%total_points, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! Set return values
      ESMF_RegridRouteConstruct = route
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_RegridRouteConstruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridGet"
!BOPI
! !IROUTINE: ESMF_RegridGet - Get an attribute of a Regrid

! !INTERFACE:
      subroutine ESMF_RegridGet(regrid, name, srcArray, dstArray, &
                                srcIGrid,  dstIGrid, srcDatamap,  dstDatamap, &
                                method, rc)
!
! !ARGUMENTS:

      type(ESMF_Regrid),        intent(inout) :: regrid
      character (*),            intent(out), optional :: name
      type(ESMF_InternArray),        intent(out), optional :: srcArray
      type(ESMF_InternArray),        intent(out), optional :: dstArray
      type (ESMF_IGrid),         intent(out), optional :: srcIGrid
      type (ESMF_IGrid),         intent(out), optional :: dstIGrid
      type (ESMF_FieldDataMap), intent(out), optional :: srcDatamap
      type (ESMF_FieldDataMap), intent(out), optional :: dstDatamap
      type (ESMF_RegridMethod), intent(out), optional :: method
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
!     \item[{[srcIGrid]}]
!          
!     \item[{[dstIGrid]}]
!          
!     \item[{[srcDatamap]}]
!          
!     \item[{[dstDatamap]}]
!          
!     \item[{[method]}]
!          Type {\tt ESMF\_RegridMethod} indicating the method used in this
!          regrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  TODO

      integer :: localrc
      type(ESMF_RegridType), pointer :: rtype

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDatamap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDatamap)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)

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

      ! Get igrids if requested
      if (present(srcIGrid)) then
         srcIGrid = rtype%srcIGrid
      endif
      if (present(dstIGrid)) then
         dstIGrid = rtype%dstIGrid
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
                                srcIGrid,  dstIGrid,          &
                                srcDatamap,  dstDatamap,    &
                                method, name, rc)
!
! !ARGUMENTS:
      type (ESMF_Regrid),       intent(inout) :: regrid
      type(ESMF_InternArray),        intent(in   ), optional :: srcArray
      type(ESMF_InternArray),        intent(in   ), optional :: dstArray
      type (ESMF_IGrid),         intent(in   ), optional :: srcIGrid
      type (ESMF_IGrid),         intent(in   ), optional :: dstIGrid
      type (ESMF_FieldDataMap), intent(inout), optional :: srcDatamap
      type (ESMF_FieldDataMap), intent(inout), optional :: dstDatamap
      type (ESMF_RegridMethod), intent(in   ), optional :: method
      character (*),            intent(in   ), optional :: name
      integer,                  intent(  out), optional :: rc

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
!     \item[{[srcIGrid]}]
!
!     \item[{[dstIGrid]}]
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
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
 
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,srcDatamap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dstDatamap)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,srcIGrid,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,dstIGrid,rc)

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
           
      ! Set igrids if requested
      if (present(srcIGrid)) rtype%srcIGrid = srcIGrid
      if (present(dstIGrid)) rtype%dstIGrid = dstIGrid
 
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
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

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
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! initialize the base object
      call ESMF_BaseCreate(regrid%base, "Regrid", rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      ! TODO: add error handling

      ! initialize scalars
      regrid%method    = ESMF_REGRID_METHOD_NONE
      regrid%distrbOpt = ESMF_REGRID_DISTRB_NONE

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
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      regrid%ptr%method    = ESMF_REGRID_METHOD_NONE
      regrid%ptr%distrbOpt = ESMF_REGRID_DISTRB_NONE

      ! and free anything associated with the base object
      call ESMF_BaseDestroy(regrid%ptr%base, localrc)

      ! nullify the pointer
      nullify(regrid%ptr)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDestruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridDomainListModify"
!BOPI
! !IROUTINE: ESMF_RegridDomainListModify - modify a DomainList from a IGrid for an Array

! !INTERFACE:
      subroutine ESMF_RegridDomainListModify(option, dimCount, domainList, &
                                             array, igrid, dataMap, total, rc)
!
! !ARGUMENTS:
      character(4),            intent(in   ) :: option
      integer,                 intent(in   ) :: dimCount
      type(ESMF_DomainList),   intent(inout) :: domainList
      type(ESMF_InternArray),        intent(in   ) :: array
      type(ESMF_IGrid),         intent(inout) :: igrid
      type(ESMF_FieldDataMap), intent(inout) :: dataMap
      logical,                 intent(in   ) :: total
      integer,                 intent(  out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which modifies a regrid index structure, changing it from
!     being IGrid-based to Array-based.  This means adding in information for
!     Array ranks that do not correspond to a IGrid axis, and modifying the
!     domainList for the Array's halo and lower bounds.  Intended for internal
!     ESMF use only.
!
!     The arguments are:
!     \begin{description}
!     \item[option]
!          Character array of length four, indicating whether the domainList
!          is for receiving or sending data.  Valid values are "recv" and "send".
!     \item[dimCount]
!          2D array giving the size of the local destination igrid.
!     \item[domainList]
!          2D array giving the size of the local destination igrid.
!     \item[array]
!          2D array giving the size of the local destination igrid.
!     \item[igrid]
!          2D array giving the size of the local destination igrid.
!     \item[dataMap]
!          2D array giving the size of the local destination igrid.
!     \item[total]
!          2D array giving the size of the local destination igrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc
      integer :: count, haloWidth, i, j, nDE, nDEs
      integer, dimension(:), allocatable :: dimOrder, lbounds
      type(ESMF_AxisIndex), dimension(:), pointer :: thisAI
      type(ESMF_AxisIndex), dimension(:,:), pointer :: allAI
      type(ESMF_DELayout) :: layout

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dataMap)
      ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,domainList)
      ESMF_INIT_CHECK_DEEP(ESMF_IGridGetInit,igrid,rc)

      ! check variables
      ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,dataMap)

      call ESMF_IGridGet(igrid, delayout=layout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      call ESMF_DELayoutGet(layout, deCount=nDEs, rc=localrc)

      ! Modify DomainLists for Array AI's and to add ranks larger than IGrid dimensions
      allocate(  dimOrder(dimCount), &
                   thisAI(dimCount), &
               allAI(nDEs,dimCount), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "AI arrays", &
                                     ESMF_CONTEXT, rc)) return

      if (option.eq.'send') then
        allocate( lbounds(dimCount), stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "dimCount arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (total) then
        call ESMF_IArrayGetAllAxisIndices(array, igrid, dataMap, &
                                         totalindex=allAI, rc=localrc)
      else
        call ESMF_IArrayGetAllAxisIndices(array, igrid, dataMap, &
                                         compindex=allAI, rc=localrc)
      endif

      call ESMF_FieldDataMapGet(dataMap, dataIndexList=dimOrder, rc=localrc)

      do i   = 1,domainList%num_domains
        do j = 1,domainList%domains(i)%rank
          thisAI(j) = domainList%domains(i)%ai(j)
        enddo
        domainList%domains(i)%rank = dimCount

        ! modify thisAI to include Array haloWidth and possibly different lbounds
        if (option.eq.'send') then
          call ESMF_InternArrayGet(array, haloWidth=haloWidth, lbounds=lbounds, &
                             rc=localrc)
          do j = 1,dimCount
            if (dimOrder(j).eq.1) then
         !jw     thisAI(1)%min    = thisAI(1)%min    +   haloWidth + lbounds(j) - 1
         !jw     thisAI(1)%max    = thisAI(1)%max    +   haloWidth + lbounds(j) - 1
              thisAI(1)%min    = thisAI(1)%min    +   haloWidth
              thisAI(1)%max    = thisAI(1)%max    +   haloWidth
              thisAI(1)%stride = thisAI(1)%stride + 2*haloWidth
            elseif (dimOrder(j).eq.2) then
          !jw    thisAI(2)%min    = thisAI(2)%min    +   haloWidth + lbounds(j) - 1
          !jw    thisAI(2)%max    = thisAI(2)%max    +   haloWidth + lbounds(j) - 1
              thisAI(2)%min    = thisAI(2)%min    +   haloWidth 
              thisAI(2)%max    = thisAI(2)%max    +   haloWidth
              thisAI(2)%stride = thisAI(2)%stride + 2*haloWidth
            endif
          enddo
        endif

        ! load into appropriate spot of domainList
        do j = 1,dimCount
          nDE = domainList%domains(i)%DE + 1
          if (dimOrder(j).eq.1) then
            domainList%domains(i)%ai(j) = thisAI(1)
          elseif (dimOrder(j).eq.2) then
            domainList%domains(i)%ai(j) = thisAI(2)
          elseif (dimOrder(j).eq.0) then
            count = allAI(nDE,j)%max - allAI(nDE,j)%min + 1
            domainList%domains(i)%ai(j) = allAI(nDE,j)
            domainList%total_points     = domainList%total_points*count
          else
            !TODO: add error
          endif
        enddo
      enddo

      deallocate(dimOrder, &
                   thisAI, &
                    allAI, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dealloc AI arrays", &
                                     ESMF_CONTEXT, rc)) return

      if (option.eq.'send') then
        deallocate( lbounds, stat=localrc)
        if (ESMF_LogMsgFoundAllocError(localrc, "dealloc send arrays", &
                                       ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridDomainListModify

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridIndexCreate"
!BOPI
! !IROUTINE: ESMF_RegridIndexCreate - Create regrid index structure

! !INTERFACE:
      function ESMF_RegridIndexCreate(srcCount, dstCount, rc)
!
! !RETURN VALUE:
      type(ESMF_RegridIndex) :: ESMF_RegridIndexCreate
!
! !ARGUMENTS:
      integer, intent(in) :: srcCount
      integer, dimension(2), intent(in) :: dstCount
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which creates and initializes a regrid index structure.
!     The structure is later filled with appropriate data in the routine
!     addLink2D.  Intended for internal ESMF use only.
!
!     The arguments are:
!     \begin{description}
!     \item[srcCount]
!          Size of the local source igrid.
!     \item[dstCount]
!          2D array giving the size of the local destination igrid.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      integer :: localrc
      type(ESMF_RegridIndexType), pointer :: ritype    ! Pointer to new regrid index

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

!     Initialize pointers
      nullify(ritype)
      nullify(ESMF_RegridIndexCreate%ptr)

      allocate(ritype, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Regrid type", &
                                     ESMF_CONTEXT, rc)) return

!     allocate and initialize regrid index internals.
      allocate(ritype%srcMinIndex(srcCount), &
               ritype%srcMaxIndex(srcCount), stat=localrc)
      allocate(ritype%dstMinIndex(dstCount(1), dstCount(2)), &
               ritype%dstMaxIndex(dstCount(1), dstCount(2)), stat=localrc)
      ritype%srcMinIndex = 0
      ritype%srcMaxIndex = 0
      ritype%dstMinIndex = 0
      ritype%dstMaxIndex = 0

!     Set return values.
      ESMF_RegridIndexCreate%ptr => ritype

      ESMF_INIT_SET_CREATED(ESMF_RegridIndexCreate)
      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_RegridIndexCreate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridIndexDestroy"
!BOPI
! !IROUTINE: ESMF_RegridIndexDestroy - destroy regrid index structure

! !INTERFACE:
      subroutine ESMF_RegridIndexDestroy(index, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridIndex) :: index
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     ESMF routine which destroys a regrid index structure.  Intended for
!     internal ESMF use only.
!
!     The arguments are:
!     \begin{description}
!     \item[index]
!          {\tt RegridIndex} to be destroyed.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:

      ! local variables
      integer :: localrc                             ! Error status

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_RegridIndexGetInit,index,rc)

!     deallocate regrid index internals.
      deallocate(index%ptr%srcMinIndex, &
                 index%ptr%srcMaxIndex, &
                 index%ptr%dstMinIndex, &
                 index%ptr%dstMaxIndex, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "Regrid type", &
                                     ESMF_CONTEXT, rc)) return

!     Destroy all components of the regrid index structure
      nullify(index%ptr)

      ESMF_INIT_SET_DELETED(index)
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridIndexDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridIndexGetInit"
!BOPI
! !IROUTINE:  ESMF_RegridIndexGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_RegridIndexGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_RegridIndex), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_RegridIndexGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt RegridIndex}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_RegridIndex} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_RegridIndexGetInit = ESMF_INIT_GET(d)
       else
         ESMF_RegridIndexGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_RegridIndexGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridIndexValidate"

!BOPI
! !IROUTINE:  ESMF_RegridIndexValidate - Check validity of a RegridIndex

! !INTERFACE:
      subroutine ESMF_RegridIndexValidate(s, rc)
!
! !ARGUMENTS:
      type(ESMF_RegridIndex), intent(in) :: s
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt RegridIndex} is internally consistent.
!      Currently this method determines if the {\tt RegridIndex} is uninitialized
!      or already destroyed.  
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_RegridIndex} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt RegridIndex}
!           is valid.
!     \end{description}
!
!EOPI

      ! Check Init Status  
      ESMF_INIT_CHECK_DEEP(ESMF_RegridIndexGetInit,s,rc)

      ! If all checks pass return success
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_RegridIndexValidate


!----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VertBinsCreate"
!BOPI
! !IROUTINE: ESMF_VertBinsCreate - Create vertical bins

! !INTERFACE:
      subroutine ESMF_VertBinsCreate(rank, targetBinSize, start, stop, &
                                     numBins, binMin, binMax, &
                                     binAddrMin, binAddrMax, &
                                     cornerX1D, cornerY1D, &
                                     cornerX2D, cornerY2D, rc)

! !ARGUMENTS:

      integer, intent(in) :: rank
      integer, intent(in) :: targetBinSize
      integer, dimension(2), intent(in ) :: start
      integer, dimension(2), intent(in ) :: stop
      integer, dimension(2), intent(out) :: numBins
      real(ESMF_KIND_R8), dimension(:,:), pointer :: binMin
      real(ESMF_KIND_R8), dimension(:,:), pointer :: binMax
      integer, dimension(:), pointer :: binAddrMin
      integer, dimension(:), pointer :: binAddrMax
      real(ESMF_KIND_R8), dimension(:,:), intent(in), optional :: cornerX1D
      real(ESMF_KIND_R8), dimension(:,:), intent(in), optional :: cornerY1D
      real(ESMF_KIND_R8), dimension(:,:,:), intent(in), optional :: cornerX2D
      real(ESMF_KIND_R8), dimension(:,:,:), intent(in), optional :: cornerY2D
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: i, j, ijAddr, n
      integer :: localSize
      logical :: dummy
      real(ESMF_KIND_R8) :: localMinX, localMaxX, localMinY, localMaxY
      real(ESMF_KIND_R8) :: minX, maxX, minY, maxY, deltaBin(2)

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      if (rank.eq.1) then
        localMinX = minval(cornerX1D)
        localMinY = minval(cornerY1D)
        localMaxX = maxval(cornerX1D)
        localMaxY = maxval(cornerY1D)
        localSize = stop(1) - start(1) + 1
      else if (rank.eq.2) then
        localMinX = minval(cornerX2D)
        localMinY = minval(cornerY2D)
        localMaxX = maxval(cornerX2D)
        localMaxY = maxval(cornerY2D)
        localSize = (stop(2) - start(2) + 1) * (stop(1) - start(1) + 1)
      else
        !TODO: log entry
        return
      endif

      ! allocate and fill bins -- make sure there is at least one
      numBins(2) = max(localSize/targetBinSize, 1)

      ! makes no sense to have more bins than points in the vertical direction
      if (rank.eq.2) numBins(2) = min(numBins(2), stop(2)-start(2)+1)

      allocate(binAddrMin(numBins(2)), &
               binAddrMax(numBins(2)), &
                 binMin(2,numBins(2)), &
                 binMax(2,numBins(2)))

      deltaBin(2) = (localMaxY - localMinY)/numBins(2)
      do n = 1, numBins(2)
        binMin  (1,n) = localMaxX
        binMax  (1,n) = localMinX
        binMin  (2,n) = localMinY + (n-1)*deltaBin(2)
        binMax  (2,n) = localMinY +  n   *deltaBin(2)
        binAddrMin(n) = localSize + 1
        binAddrMax(n) = 0
      enddo

      select case(rank)

      !----------------
      case(1)
        do ijAddr = start(1), stop(1)
          minX    = minval(cornerX1D(:,ijAddr))
          minY    = minval(cornerY1D(:,ijAddr))
          maxX    = maxval(cornerX1D(:,ijAddr))
          maxY    = maxval(cornerY1D(:,ijAddr))
          do n    = 1, numBins(2)
            if (minY.le.binMax(2,n) .AND. maxY.ge.binMin(2,n)) then
              binAddrMin(n) = min(ijAddr,binAddrMin(n))
              binAddrMax(n) = max(ijAddr,binAddrMax(n))
            endif
            if (minX.lt.binMin(1,n)) binMin(1,n) = minX
            if (maxX.gt.binMax(1,n)) binMax(1,n) = maxX
          enddo
        enddo

      !----------------
      case(2)
        do j       = start(2), stop(2)
          do i     = start(1), stop(1)
            ijAddr = (j-start(2))*(stop(1)-start(1)+1) + (i-start(1)) + 1
            minX   = minval(cornerX2D(:,i,j))
            minY   = minval(cornerY2D(:,i,j))
            maxX   = maxval(cornerX2D(:,i,j))
            maxY   = maxval(cornerY2D(:,i,j))
            do n   = 1, numBins(2)
              if (minY.le.binMax(2,n) .AND. maxY.ge.binMin(2,n)) then
                binAddrMin(n) = min(ijAddr,binAddrMin(n))
                binAddrMax(n) = max(ijAddr,binAddrMax(n))
              endif
              if (minX.lt.binMin(1,n)) binMin(1,n) = minX
              if (maxX.gt.binMax(1,n)) binMax(1,n) = maxX
            enddo
          enddo
        enddo

      !----------------
      case default
        dummy = ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                      "Invalid rank", &
                                      ESMF_CONTEXT, rc)

      end select

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_VertBinsCreate

!----------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_VertBinsDestroy"
!BOPI
! !IROUTINE: ESMF_VertBinsDestroyCreate - Destroy vertical bins

! !INTERFACE:
      subroutine ESMF_VertBinsDestroy(binMin, binMax, &
                                      binAddrMin, binAddrMax, rc)

! !ARGUMENTS:

      real(ESMF_KIND_R8), dimension(:,:), pointer :: binMin
      real(ESMF_KIND_R8), dimension(:,:), pointer :: binMax
      integer, dimension(:), pointer :: binAddrMin
      integer, dimension(:), pointer :: binAddrMax
      integer, intent(out), optional :: rc

! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  TODO

      integer :: localrc

      ! Initialize return code; assume failure until success is certain
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      deallocate(binAddrMin, &
                 binAddrMax, &
                     binMin, &
                     binMax, stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, "dealloc bin arrays", &
                                     ESMF_CONTEXT, rc)) return

      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_VertBinsDestroy

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridDistrbOptEqual"
!BOPI
! !IROUTINE: ESMF_RegridDistrbOptEqual - equality of regrid distribution options
!
! !INTERFACE:
      function ESMF_RegridDistrbOptEqual(RegridDistrbOpt1, RegridDistrbOpt2)

! !RETURN VALUE:
      logical :: ESMF_RegridDistrbOptEqual

! !ARGUMENTS:
      type (ESMF_RegridDistrbOpt), intent(in) :: RegridDistrbOpt1
      type (ESMF_RegridDistrbOpt), intent(in) :: RegridDistrbOpt2

! !DESCRIPTION:
!     This routine compares two ESMF Regrid distribution option types to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[RegridDistrbOpt1, RegridDistrbOpt2]
!          Two regrid distribution option types to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegridDistrbOptEqual = (RegridDistrbOpt1%regridDistrbOpt == &
                                   RegridDistrbOpt2%regridDistrbOpt)

      end function ESMF_RegridDistrbOptEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridMethodEqual"
!BOPI
! !IROUTINE: ESMF_RegridMethodEqual - equality of regrid method types
!
! !INTERFACE:
      function ESMF_RegridMethodEqual(RegridMethod1, RegridMethod2)

! !RETURN VALUE:
      logical :: ESMF_RegridMethodEqual

! !ARGUMENTS:
      type (ESMF_RegridMethod), intent(in) :: RegridMethod1
      type (ESMF_RegridMethod), intent(in) :: RegridMethod2

! !DESCRIPTION:
!     This routine compares two ESMF Regrid method types to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[RegridMethod1, RegridMethod2]
!          Two regrid method types to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegridMethodEqual = (RegridMethod1%regridMethod == &
                                RegridMethod2%regridMethod)

      end function ESMF_RegridMethodEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridNormOptEqual"
!BOPI
! !IROUTINE: ESMF_RegridNormOptEqual - equality of regrid normalization options
!
! !INTERFACE:
      function ESMF_RegridNormOptEqual(RegridNormOpt1, RegridNormOpt2)

! !RETURN VALUE:
      logical :: ESMF_RegridNormOptEqual

! !ARGUMENTS:
      type (ESMF_RegridNormOpt), intent(in) :: RegridNormOpt1
      type (ESMF_RegridNormOpt), intent(in) :: RegridNormOpt2

! !DESCRIPTION:
!     This routine compares two ESMF Regrid normalization option types to see if
!     they are equivalent.
!
!     The arguments are:
!     \begin{description}
!     \item[RegridNormOpt1, RegridNormOpt2]
!          Two regrid normalization option types to compare for equality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegridNormOptEqual = (RegridNormOpt1%regridNormOpt == &
                                 RegridNormOpt2%regridNormOpt)

      end function ESMF_RegridNormOptEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridDistrbOptNotEqual"
!BOPI
! !IROUTINE: ESMF_RegridDistrbOptNotEqual - inequality of regrid distribution options
!
! !INTERFACE:
      function ESMF_RegridDistrbOptNotEqual(RegridDistrbOpt1, RegridDistrbOpt2)

! !RETURN VALUE:
      logical :: ESMF_RegridDistrbOptNotEqual

! !ARGUMENTS:
      type (ESMF_RegridDistrbOpt), intent(in) :: RegridDistrbOpt1
      type (ESMF_RegridDistrbOpt), intent(in) :: RegridDistrbOpt2

! !DESCRIPTION:
!     This routine compares two ESMF Regrid distribution option types to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[RegridDistrbOpt1, RegridDistrbOpt2]
!          Two regrid distribution option types to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegridDistrbOptNotEqual = (RegridDistrbOpt1%regridDistrbOpt /= &
                                      RegridDistrbOpt2%regridDistrbOpt)

      end function ESMF_RegridDistrbOptNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridMethodNotEqual"
!BOPI
! !IROUTINE: ESMF_RegridMethodNotEqual - inequality of regrid method types
!
! !INTERFACE:
      function ESMF_RegridMethodNotEqual(RegridMethod1, RegridMethod2)

! !RETURN VALUE:
      logical :: ESMF_RegridMethodNotEqual

! !ARGUMENTS:
      type (ESMF_RegridMethod), intent(in) :: RegridMethod1
      type (ESMF_RegridMethod), intent(in) :: RegridMethod2

! !DESCRIPTION:
!     This routine compares two ESMF Regrid method types to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[RegridMethod1, RegridMethod2]
!          Two regrid method types to compare for inequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegridMethodNotEqual = (RegridMethod1%regridMethod /= &
                                   RegridMethod2%regridMethod)

      end function ESMF_RegridMethodNotEqual

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RegridNormOptNotEqual"
!BOPI
! !IROUTINE: ESMF_RegridNormOptNotEqual - inequality of regrid normalization options
!
! !INTERFACE:
      function ESMF_RegridNormOptNotEqual(RegridNormOpt1, RegridNormOpt2)

! !RETURN VALUE:
      logical :: ESMF_RegridNormOptNotEqual

! !ARGUMENTS:
      type (ESMF_RegridNormOpt), intent(in) :: RegridNormOpt1
      type (ESMF_RegridNormOpt), intent(in) :: RegridNormOpt2

! !DESCRIPTION:
!     This routine compares two ESMF Regrid normalization option types to see if
!     they are unequal.
!
!     The arguments are:
!     \begin{description}
!     \item[RegridNormOpt1, RegridNormOpt2]
!          Two regrid normalization option types to compare for iinequality
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n

      ESMF_RegridNormOptNotEqual = (RegridNormOpt1%regridNormOpt /= &
                                    RegridNormOpt2%regridNormOpt)

      end function ESMF_RegridNormOptNotEqual

!------------------------------------------------------------------------------

      end module ESMF_RegridTypesMod
