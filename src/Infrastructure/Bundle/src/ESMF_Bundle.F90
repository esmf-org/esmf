
! $Id: ESMF_Bundle.F90,v 1.116 2007/10/31 02:04:31 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#define ESMF_FILENAME "ESMF_Bundle.F90"
!
!     ESMF Bundle Module
      module ESMF_BundleMod 
!
!==============================================================================
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_BundleMod
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_Bundle} class, which 
! represents a set of {\tt ESMF\_Fields} discretized on the same 
! {\tt ESMF\_Grid}.  {\tt ESMF\_Bundle}s offer the option to pack the data 
! from the {\tt ESMF\_Field}s they contain into a single buffer. 
!
!  This type is implemented in Fortran 90 and a corresponding
!  C++ interface is provided.

!
! !USES:
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_VMMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_InternArrayDataMapMod
      use ESMF_StaggerLocMod
      use ESMF_GridMod
      use ESMF_InternArrayMod
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod
      use ESMF_InitMacrosMod
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!! ESMF_PackFlag   
!!
!! Data type to set the status of data in this Bundle; it can either be
!! simply a collection of Fields which contain the data, or it can also 
!! have a private packed data buffer associated directly with the Bundle.

      type ESMF_PackFlag
      sequence
      !private
        integer :: packflag
      end type

      type(ESMF_PackFlag), parameter :: ESMF_PACKED_DATA = ESMF_PackFlag(1), &
                                        ESMF_NO_PACKED_DATA = ESMF_PackFlag(2)

!------------------------------------------------------------------------------
!! For ease of accessing data for an individual field for a packed array.

      type ESMF_BundleFieldAccess
      sequence
      private
         type(ESMF_InterleaveFlag) :: bfa_type
         integer :: bfa_start
         integer :: bfa_end
         integer :: bfa_strides
         ESMF_INIT_DECLARE
      end type


!------------------------------------------------------------------------------
!! ESMF_BundleFieldIntrlv
!!
!!  Data type to record the ordering information for multiple field
!!  data which is packed in a bundle.  Each has an associated
!!  {\tt ESMF\_FieldDataMap} object to track the ordering of that 
!!  {\tt ESMF\_Field}'s data in the packed buffer.

        type ESMF_BundleFieldIntrlv
        sequence
        private
          integer :: field_order                      ! index of this field
          type(ESMF_FieldDataMap) :: field_dm         ! copy of this field's dm
          type(ESMF_BundleFieldAccess) :: field_bfa   ! access info if packed
          ESMF_INIT_DECLARE
        end type

!------------------------------------------------------------------------------
!! For bookkeeping information which must be identical in each constituent
!! Field in order to optimize some of the communications calls.

      type ESMF_BundleCongrntData
      sequence
      private
        ! for starters:
        integer :: datarank
        type(ESMF_TypeKind) :: typekind
        integer :: indexorders(ESMF_MAXDIM)
        integer :: nonindexcounts(ESMF_MAXDIM) 
        type(ESMF_StaggerLoc) :: datastaggerloc
        integer :: haloWidth
        ESMF_INIT_DECLARE
      end type


!------------------------------------------------------------------------------
!! ESMF_LocalBundle
!
      type ESMF_LocalBundle
      sequence
      !private
        type(ESMF_InternArray) :: packed_data               ! local packed array
        type(ESMF_Status) :: gridstatus
        type(ESMF_Status) :: arraystatus
        integer :: accesscount
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!!     ESMF_BundleType
!
      type ESMF_BundleType
      sequence
      ! this data type is not private so the bundlecomm code can
      ! reach directly in and get at the localdata without a loop
      ! of subroutine calls.  but this causes problems with the 'pattern'
      ! declaration below - the bundlecongruentdata derived type is
      ! private and so it wants this to be private as well.
      ! since pattern is not being used yet, comment it out below, but this
      ! needs to be rationalized at some point soon.  perhaps the comm code
      ! will have to go through a subroutine interface.  this is where
      ! fortran needs a 'friend' type of access.
      !private
        type(ESMF_Base) :: base                   ! base class object
        type(ESMF_Field), dimension(:), pointer :: flist
        type(ESMF_Status) :: bundlestatus
        type(ESMF_Status) :: gridstatus

        type(ESMF_Grid) :: grid                  ! associated global Grid
        type(ESMF_LocalBundle) :: localbundle    ! this differs per DE
        type(ESMF_Packflag) :: pack_flag         ! is packed data present?
        type(ESMF_BundleFieldIntrlv) :: fil  ! ordering in buffer
        type(ESMF_IOSpec) :: iospec              ! iospec values
        type(ESMF_Status) :: iostatus            ! if unset, inherit from gcomp
        logical :: isCongruent                   ! are all fields identical?
        logical :: hasPattern                    ! first data field sets this
        !type(ESMF_BundleCongrntData) :: pattern ! what they must match
        integer :: field_count      
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!! ESMF_Bundle

!! The Bundle data structure that is passed between implementation and
!! calling languages.

      type ESMF_Bundle
      sequence
      !private
      type (ESMF_BundleType), pointer :: btypep 

      ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
      public ESMF_Bundle, ESMF_PackFlag, ESMF_PACKED_DATA, ESMF_NO_PACKED_DATA

      ! intended for internal ESMF use only but public for BundleComms
      public ESMF_BundleType           ! internal ESMF use only, for BundleComm
      public ESMF_LocalBundle          ! internal ESMF use only, for BundleComm
      public ESMF_BundleFieldIntrlv ! internal ESMF use only, for BundleComm


! !PUBLIC MEMBER FUNCTIONS:
!
       ! public ESMF_BundleFieldAccessValidate   ! For Standardized Initialization
       ! ESMF_BundleFieldAccess(Init) and (GetInit) are privite

       public ESMF_BundleFieldIntrlvInit     ! For Standardized Initialization
       public ESMF_BundleFieldIntrlvValidate ! For Standardized Initialization

       public ESMF_BundleFieldIntrlvGetInit  ! For Standardized Initialization

       ! public ESMF_BundleCongrntDataValidate  ! For Standardized Initialization
       ! ESMF_BundleCongrntData(Init) and (GetInit) are private

       public ESMF_LocalBundleInit     ! For Standardized Initialization
       public ESMF_LocalBundleValidate ! For Standardized Initialization
       public ESMF_LocalBundleGetInit  ! For Standardized Initialization

       public ESMF_BundleTypeInit      ! For Standardized Initialization
       public ESMF_BundleTypeValidate  ! For Standardized Initialization
       public ESMF_BundleTypeGetInit   ! For Standardized Initialization

       public ESMF_BundleGetInit       ! For Standardized Initialization

       public ESMF_BundleCreate       ! Create a new Bundle
       public ESMF_BundleDestroy      ! Destroy a Bundle

       public ESMF_BundleGet          ! Get Bundle information
       public ESMF_BundleGetFieldNames

       public ESMF_BundleSetAttribute       ! Set and Get attributes
       public ESMF_BundleGetAttribute       !   interface to Base class

       public ESMF_BundleGetAttributeCount  ! number of attribs
       public ESMF_BundleGetAttributeInfo   ! get type, length by name or number

       public ESMF_BundleGetField      ! Get one or more Fields by name or number
       public ESMF_BundleAddField      ! Add one or more Fields 
!      public ESMF_BundleRemoveField   ! Delete one or more Fields by name or number

      public ESMF_BundleSetGrid           ! In empty Bundle, set Grid

      public ESMF_BundleIsCongruent        ! private to framework

   ! These are the recommended entry points; the code itself is in Array:
   !public ESMF_BundleRedist   ! Redistribute existing arrays, matching Grids
   !public ESMF_BundleHalo     ! Halo updates

   !public ESMF_BundleGather   ! Combine 1 decomposed bundle into 1 on 1 DE
   !public ESMF_BundleAllGather! Combine 1 decomposed bundle into N copies on N DEs

   !public ESMF_BundleScatter  ! Split 1 bundle into a decomposed one over N DEs
   !public ESMF_BundleBroadcast! Send 1 bundle to all DEs, none decomposed
   !public ESMF_BundleAlltoAll ! might make sense with bundles; each DE could
                              ! call with a different non-decomposed bundle 
                              ! and the result would be a packed bundle of
                              ! data with decomposed bundles on each DE.

   !public ESMF_BundleReduce     ! Global reduction operation, return on 1 DE    
   !public ESMF_BundleAllReduce  ! Global reduction operation, return on each DE


    public ESMF_BundleSerialize    ! Convert to byte stream...
    public ESMF_BundleDeserialize  ! ... and back into an object again
    public ESMF_BundleValidate     ! Check internal consistency
    public ESMF_BundlePrint        ! Print contents of a Bundle

    public operator(.eq.), operator(.ne.)

!  !subroutine ESMF_BundleWriteRestart(bundle, iospec, rc)
!  !function ESMF_BundleReadRestart(name, iospec, rc)
!  !subroutine ESMF_BundleWrite(bundle, subarray, iospec, rc)
!  !function ESMF_BundleRead(name, iospec, rc)

! !PRIVATE MEMBER FUNCTIONS:
!  ! additional future signatures of ESMF_BundleCreate() functions:
!  !function ESMF_BundleCreateCopy(bundle, subarray, name, packflag, rc)
!  !function ESMF_BundleCreateRemap(bundle, grid, name, packflag, rc)

!EOPI


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleCreate - Create a new Bundle
!
! !INTERFACE:
     interface ESMF_BundleCreate

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleCreateNew
        module procedure ESMF_BundleCreateNoFields

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_BundleCreate} functions.
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleConstruct - Construct the internals of a new Bundle
!
! !INTERFACE:
     interface ESMF_BundleConstruct

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleConstructNew
        module procedure ESMF_BundleConstructNoFields

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_BundleConstruct} functions.
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleGetField - Retrieve Fields from a Bundle
!
! !INTERFACE:
     interface ESMF_BundleGetField

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleGetFieldByName
        module procedure ESMF_BundleGetFieldByNum

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_BundleGetField} functions.
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleAddField - Add Fields to a Bundle
!
! !INTERFACE:
     interface ESMF_BundleAddField

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleAddOneField
        module procedure ESMF_BundleAddFieldList

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_BundleAddField} functions.
!EOPI
      end interface


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set a Bundle attribute
!
! !INTERFACE:
      interface ESMF_BundleSetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleSetInt4Attr
        module procedure ESMF_BundleSetInt4ListAttr
        module procedure ESMF_BundleSetInt8Attr
        module procedure ESMF_BundleSetInt8ListAttr
        module procedure ESMF_BundleSetReal4Attr
        module procedure ESMF_BundleSetReal4ListAttr
        module procedure ESMF_BundleSetReal8Attr
        module procedure ESMF_BundleSetReal8ListAttr
        module procedure ESMF_BundleSetLogicalAttr
        module procedure ESMF_BundleSetLogicalListAttr
        module procedure ESMF_BundleSetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Bundle}.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Get a Bundle attribute
!
! !INTERFACE:
      interface ESMF_BundleGetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleGetInt4Attr
        module procedure ESMF_BundleGetInt4ListAttr
        module procedure ESMF_BundleGetInt8Attr
        module procedure ESMF_BundleGetInt8ListAttr
        module procedure ESMF_BundleGetReal4Attr
        module procedure ESMF_BundleGetReal4ListAttr
        module procedure ESMF_BundleGetReal8Attr
        module procedure ESMF_BundleGetReal8ListAttr
        module procedure ESMF_BundleGetLogicalAttr
        module procedure ESMF_BundleGetLogicalListAttr
        module procedure ESMF_BundleGetCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     attributes from an {\tt ESMF\_Bundle}.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleGetAttributeInfo - Get type, count from a Bundle attribute
!
! !INTERFACE:
      interface ESMF_BundleGetAttributeInfo
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleGetAttrInfoByName
        module procedure ESMF_BundleGetAttrInfoByNum

! !DESCRIPTION:
!     This interface provides a single entry point for methods that retrieve
!     information about attributes from an {\tt ESMF\_Bundle}.
 
!EOPI
      end interface

!------------------------------------------------------------------------------
interface operator (.eq.)
 module procedure ESMF_pfeq
end interface

interface operator (.ne.)
 module procedure ESMF_pfne
end interface


!------------------------------------------------------------------------------

      contains

!------------------------------------------------------------------------------
! function to compare two ESMF_PackFlags to see if they're the same or not

function ESMF_pfeq(pf1, pf2)
 logical ESMF_pfeq
 type(ESMF_PackFlag), intent(in) :: pf1, pf2

 ESMF_pfeq = (pf1%packflag .eq. pf2%packflag)
end function

function ESMF_pfne(pf1, pf2)
 logical ESMF_pfne
 type(ESMF_PackFlag), intent(in) :: pf1, pf2

 ESMF_pfne = (pf1%packflag .ne. pf2%packflag)
end function


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddOneField"
!BOP
! !IROUTINE: ESMF_BundleAddField - Add a Field to a Bundle
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddField()
      subroutine ESMF_BundleAddOneField(bundle, field, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_Field), intent(inout) :: field
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Adds a single {\tt field} to an existing {\tt bundle}.  The
!      {\tt field} must be associated with the same {\tt ESMF\_Grid} 
!      as the other {\tt ESMF\_Field}s in the {\tt bundle}.   
!      The {\tt field} is referenced by the {\tt bundle}, not copied.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} to add the {\tt ESMF\_Field} to.
!     \item [field] 
!           The {\tt ESMF\_Field} to add.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: status                                ! Error status
      logical :: dummy
      type(ESMF_Field) :: temp_list(1)
      type(ESMF_BundleType), pointer :: btype

      ! Initialize return code in case we return early.
      ! Otherwise, count on AddFieldList call to set rc
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      temp_list(1) = field

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep
    
      call ESMF_BundleTypeAddFieldList(btype, 1, temp_list, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! this resets the congruent flag as a side effect
      dummy = ESMF_BundleIsCongruent(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleAddOneField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddFieldList"
!BOP
! !IROUTINE: ESMF_BundleAddField - Add a list of Fields to a Bundle
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddField()
      subroutine ESMF_BundleAddFieldList(bundle, fieldCount, fieldList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle        
      integer, intent(in) :: fieldCount
      type(ESMF_Field), dimension(:), intent(inout) :: fieldList
      integer, intent(out), optional :: rc          
!
! !DESCRIPTION:
!      Adds a {\tt fieldList} to an existing {\tt ESMF\_Bundle}.  
!      The items added from the {\tt ESMF\_fieldList} must be associated 
!      with the same {\tt ESMF\_Grid} as the other {\tt ESMF\_Field}s in the 
!      {\tt bundle}.  The items in the {\tt fieldList} are referenced by
!      the {\tt bundle}, not copied.  
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt ESMF\_Bundle} to add {\tt ESMF\_Field}s to.
!     \item [fieldCount]
!           Number of {\tt ESMF\_Field}s to be added to the 
!           {\tt ESMF\_Bundle}; must be equal to or less than the 
!           number of items in the {\tt fieldList}.
!     \item [fieldList]
!           Array of existing {\tt ESMF\_Field}s.  The first {\tt fieldCount}
!           items will be added to the {\tt ESMF\_Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
! 
!EOP

      integer :: status                                ! Error status
      logical :: dummy
      type(ESMF_BundleType), pointer :: btype
      integer :: i

      ! Initialize return code in case we return early.
      ! Otherwise, count on AddFieldList call to set rc
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)
      do i=1,fieldCount
         ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fieldList(i),rc)
      enddo

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep
    
      call ESMF_BundleTypeAddFieldList(btype, fieldCount, fieldList, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      ! this resets the congruent flag as a side effect
      dummy = ESMF_BundleIsCongruent(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleAddFieldList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleCreateNew"
!BOP
! !IROUTINE: ESMF_BundleCreate - Create a Bundle from existing Fields
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleCreate()
      function ESMF_BundleCreateNew(fieldCount, fieldList, &
                                  packflag, bundleinterleave, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: fieldCount           
      type(ESMF_Field), dimension (:) :: fieldList
      type(ESMF_PackFlag), intent(in), optional :: packflag 
      type(ESMF_InterleaveFlag), intent(in), optional :: bundleinterleave
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!   Creates an {\tt ESMF\_Bundle} from a list of existing
!   {\tt ESMF\_Fields} stored in a {\tt fieldList}.  All items in 
!   the {\tt fieldList} must be associated with the same 
!   {\tt ESMF\_Grid}.  Returns a new {\tt ESMF\_Bundle}.
!
!   The arguments are:
!   \begin{description}
!   \item [fieldCount]
!      Number of fields to be added to the new {\tt ESMF\_Bundle}.
!      Must be equal to or less than the number of 
!      {\tt ESMF\_Field}s in the {\tt fieldList}.
!   \item [fieldList]
!      Array of existing {\tt ESMF\_Field}s.  The first {\tt ESMF\_FieldCount}
!      items will be added to the new {\tt ESMF\_Bundle}.
!   \item [{[packflag]}]
!      The packing option is not yet implemented.  
!      See Section~\ref{sec:bundlerest}
!      for a description of packing, and Section~\ref{opt:packflag} for 
!      anticipated values.  The current implementation corresponds to the
!      value {\tt ESMF\_NO\_PACKED\_DATA}, which means that every 
!      {\tt ESMF\_Field} is referenced separately rather 
!      than being copied into a single contiguous buffer.  
!      This is the case no matter what value, if any, is passed in for
!      this argument.
!   \item [{[bundleinterleave]}]
!      The interleave option is not yet implemented.  
!      See Section~\ref{sec:bundlerest}
!      for a brief description of interleaving, and 
!      Section~\ref{opt:bundleinterleave}
!      for anticipated values.  The flag is not applicable 
!      to the current implementation,
!      since it applies only to packed data (see the {\tt packflag} argument).
!   \item [{[name]}]
!      {\tt ESMF\_Bundle} name.  A default name is generated if
!      one is not specified.
!   \item [{[iospec]}]
!      The {\tt ESMF\_IOSpec} is not yet used by {\tt ESMF\_Bundle}s.  Any 
!      values passed in will be ignored.
!   \item [{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP

      type(ESMF_BundleType), pointer :: btypep         ! Pointer to new bundle
      logical :: dummy
      integer :: status                                ! Error status
      integer :: i      

      ! Initialize return code in case we return early.
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      do i=1,fieldCount
         ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fieldList(i),rc)
      enddo

      ! Initialize pointers
      nullify(btypep)
      nullify(ESMF_BundleCreateNew%btypep)

      allocate(btypep,  stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Bundle allocate", &
                                       ESMF_CONTEXT, rc)) return

      ! Call construction method to initialize bundle internals.
      call ESMF_BundleConstructNew(btypep, fieldCount, fieldList, &
                                   packflag, bundleinterleave, &
                                   name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(btypep, stat=status)
          return
      endif

      ! set the return bundle
      ESMF_BundleCreateNew%btypep => btypep


      ! do this before ESMF_BundleIsConguent so it doesn't complain
      ! about uniniitalize bundles
      ESMF_INIT_SET_CREATED(ESMF_BundleCreateNew)


      ! this resets the congruent flag as a side effect
      dummy = ESMF_BundleIsCongruent(ESMF_BundleCreateNew, rc)


      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS


      end function ESMF_BundleCreateNew


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleCreateNoFields"
!BOP
! !IROUTINE: ESMF_BundleCreate - Create a Bundle with no Fields
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleCreate()
      function ESMF_BundleCreateNoFields(grid, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleCreateNoFields
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in), optional :: grid
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!   Creates an {\tt ESMF\_Bundle} with no associated {\tt ESMF\_Fields}.
!
!   The arguments are:
!   \begin{description}
!   \item [{[grid]}]
!       The {\tt ESMF\_Grid} which all {\tt ESMF\_Field}s added to this
!       {\tt ESMF\_Bundle} must be associated with.  If not specified now, the 
!       grid associated with the first {\tt ESMF\_Field} added will be
!       used as the reference grid for the {\tt ESMF\_Bundle}.
!   \item [{[name]}]
!       {\tt ESMF\_Bundle} name.  A default name is generated if
!       one is not specified.
!   \item [{[iospec]}]
!       The {\tt ESMF\_IOSpec} is not yet used by {\tt ESMF\_Bundle}s.  Any 
!       values passed in will be ignored.
!   \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP


      type(ESMF_BundleType), pointer :: btypep   ! Pointer to new bundle
      integer :: status                          ! Error status

      ! Initialize pointers
      status = ESMF_RC_NOT_IMPL
      nullify(btypep)
      nullify(ESMF_BundleCreateNoFields%btypep)

      ! Initialize return code
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check inputs 
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      allocate(btypep, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Bundle allocate", &
                                       ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize bundle internals.
      call ESMF_BundleConstructNoFields(btypep, name, iospec, rc)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! If specified, set the Grid.  All Fields added to this Bundle
      !  must be based on this same Grid.

      if (present(grid)) then
          call ESMF_GridValidate(grid, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
          btypep%grid = grid
          btypep%gridstatus = ESMF_STATUS_READY
      endif

      ! Set return values.
      ESMF_BundleCreateNoFields%btypep => btypep
      ESMF_INIT_SET_CREATED(ESMF_BundleCreateNoFields)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_BundleCreateNoFields


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDestroy"
!BOP
! !IROUTINE: ESMF_BundleDestroy - Free all resources associated with a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleDestroy(bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle) :: bundle
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases resources associated with the {\tt bundle}.  This
!     method does not destroy the {\tt ESMF\_Field}s that the
!     {\tt bundle} contains.  The
!     {\tt bundle} should be destroyed before the {\tt ESMF\_Field}s
!     within it are.
!
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      ! Local variables
      integer :: status                           ! Error status
      type(ESMF_BundleType), pointer :: btype

      ! Initialize return code
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check inputs 
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)


      ! If already destroyed or never created, return ok
      btype => bundle%btypep
      if (.not. associated(btype)) then
        if (present(rc)) rc = ESMF_FAILURE   ! should this really be an error?
        return
      endif

      ! Destruct all bundle internals and then free field memory.
      call ESMF_BundleDestruct(btype, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      deallocate(bundle%btypep, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Bundle deallocate", &
                                       ESMF_CONTEXT, rc)) return
      nullify(bundle%btypep)
      ESMF_INIT_SET_DELETED(bundle)

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGet"
!BOP
! !IROUTINE: ESMF_BundleGet - Return information about a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleGet(bundle, grid, fieldCount, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_Grid), intent(out), optional :: grid
      integer, intent(out), optional :: fieldCount
      character (len = *), intent(out), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns information about the {\tt bundle}.  
!      If the {\tt ESMF\_Bundle} was originally created
!      without specifying a name, a unique name will have been generated
!      by the framework.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} object to query.
!     \item [{[grid]}]
!           The {\tt ESMF\_Grid} associated with the {\tt bundle}.
!     \item [{[fieldCount]}]
!           Number of {\tt ESMF\_Field}s in the {\tt bundle}.
!     \item [{[name]}]
!           A character string where the {\tt bundle} name is returned.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      type(ESMF_BundleType), pointer :: btype     ! internal data

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


     ! Set initialize fieldCount to 0
      if (present(fieldCount)) then
          fieldCount = 0
      endif

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      if (present(grid)) then
          ! Check to be sure bundle has grid before trying to return it.
          if (btype%gridstatus .ne. ESMF_STATUS_READY) then
               if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Bundle does not contain a Grid", &
                                 ESMF_CONTEXT, rc)) return
          endif
          
          ! OK to return grid
          grid = btype%grid
      endif

      if (present(fieldCount)) then
          ! Return Field count
          fieldCount = bundle%btypep%field_count
      endif

      if (present(name)) then
          call c_ESMC_GetName(btype%base, name, status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetAllFields"
!BOPI
! !IROUTINE: ESMF_BundleGetAllFields - Retrieve an array of Fields 
!
! !INTERFACE:
      subroutine ESMF_BundleGetAllFields(bundle, fieldList, fieldCount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_Field), dimension (:), optional :: fieldList
      integer, intent(out), optional :: fieldCount
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return all {\tt ESMF\_Field}s in an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} to query for the {\tt ESMF\_Field}s.
!     \item [{[fieldList]}]
!           {\tt ESMF\_Field} array.
!     \item [{[fieldCount]}]
!           Return the number of {\tt ESMF\_Field}s in the {\tt bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


      integer :: status                           ! Error status
      type(ESMF_BundleType), pointer :: btype     ! internal data
      integer :: nitems                           ! items in return array

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)


      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      ! Return Fields
      if (present(fieldList)) then
          nitems = size(fieldList)
          if (nitems .lt. btype%field_count) then
              if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                       "More Fields in Bundle than space in fieldList array", &
                                        ESMF_CONTEXT, rc)) return
          endif

          fieldList(1:btype%field_count) = btype%flist(1:btype%field_count)
      endif

      ! Return Field count
      if (present(fieldCount)) then
          fieldCount = btype%field_count
      endif


      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleGetAllFields


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve an attribute
!
! !INTERFACE:
!     subroutine ESMF_BundleGetAttribute(bundle, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Bundle), intent(inout) :: bundle  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!     Returns an attribute from the {\tt bundle}.
!     Supported values for <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(out) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(out) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(out) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(out) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(out) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(out) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(out) :: valueList
!     \item type(ESMF\_Logical), intent(out) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(out) :: valueList
!     \item character (len = *), intent(out), value
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [<value argument>]
!           The value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetInt4Attr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetInt4Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns a 4-byte integer attribute from the {\tt bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetInt4ListAttr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetInt4ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an integer list attribute from the {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the list.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status
      integer :: limit

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetInt8Attr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetInt8Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer attribute from the {\tt bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The integer value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetInt8ListAttr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetInt8ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte integer list attribute from the {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the list.
!     \item [valueList]
!           The integer values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status
      integer :: limit

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetReal4Attr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetReal4Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a 4-byte real attribute from the {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetReal4ListAttr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetReal4ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns a 4-byte real list attribute from the {\tt bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the list.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status
      integer :: limit

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetReal8Attr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetReal8Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an 8-byte real attribute from the {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The real value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetReal8ListAttr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetReal8ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an 8-byte real list attribute from the {\tt bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the list.
!     \item [valueList]
!           The real values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status
      integer :: limit

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetLogicalAttr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetLogicalAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical attribute from the {\tt bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The logical value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetLogicalListAttr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetLogicalListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a logical list attribute from the {\tt bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [count]
!           The number of values in the list.
!     \item [valueList]
!           The logical values of the named attribute.
!           The list must be at least {\tt count} items long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status
      integer :: limit

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetCharAttr"
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetCharAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a character attribute from the {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to retrieve.
!     \item [value]
!           The character value of the named attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetChar(bundle%btypep%base, name, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetCharAttr


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetAttributeCount"
!BOP
! !IROUTINE: ESMF_BundleGetAttributeCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_BundleGetAttributeCount(bundle, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of attributes associated with the given {\tt bundle}
!      in the argument {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [count]
!           The number of attributes associated with this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetCount(bundle%btypep%base, count, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetAttrInfoByName"
!BOP
! !IROUTINE: ESMF_BundleGetAttributeInfo - Query Bundle attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttributeInfo()
      subroutine ESMF_BundleGetAttrInfoByName(bundle, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character(len=*), intent(in) :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the named attribute, 
!      including {\tt typekind} and item {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to query.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           The number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: status                           ! Error status
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetAttrInfoName(bundle%btypep%base, name, &
        localTk, localCount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetAttrInfoByNum"
!BOP
! !IROUTINE: ESMF_BundleGetAttributeInfo - Query Bundle attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttributeInfo()
      subroutine ESMF_BundleGetAttrInfoByNum(bundle, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out), optional :: name
      type(ESMF_TypeKind), intent(out), optional :: typekind
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns information associated with the indexed attribute, 
!      including {\tt typekind} and item {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [attributeIndex]
!           The index number of the attribute to query.
!     \item [name]
!           Returns the name of the attribute.
!     \item [{[typekind]}]
!           The typekind of the attribute.
!     \item [{[count]}]
!           Returns the number of items in this attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                           ! Error status
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeGetAttrInfoNum(bundle%btypep%base, attributeIndex, &
        localName, localTk, localCount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetAttrInfoByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleIsCongruent"
!BOPI
! !IROUTINE: ESMF_BundleIsCongruent - Is data in Bundle the same?
!
! !INTERFACE:
      function ESMF_BundleIsCongruent(bundle, rc)

! !RETURN VALUE:
      logical :: ESMF_BundleIsCongruent
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Returns {\tt .TRUE.} if the data in all {\tt ESMF\_Fields} in the
!      {\tt bundle} are completely congruent, meaning they have the same
!      data rank, type, kind, index ordering, relative location in a cell, etc.
!      This may allow more optimized communication by grouping data together
!      and making fewer communcations calls.  Returns {\tt .FALSE.} if the
!      data is not congruent.   A {\tt ESMF\_Bundle} with no data, or on error
!      this routine returns {\tt .FALSE.}.
!
!      This routine also resets the internal bundle flag to a known state
!      before returning.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} object to query.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                            ! Error status
      integer :: i, newstart
      type(ESMF_BundleType), pointer :: btype      ! internal data
      type(ESMF_BundleCongrntData) :: pattern    ! values to compare against
      type(ESMF_BundleCongrntData) :: candidate  ! values being compared
      type(ESMF_Field), pointer :: fieldp
      type(ESMF_InternArray) :: array
      type(ESMF_FieldDataMap) :: datamap

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ESMF_BundleIsCongruent = .FALSE.
      bundle%btypep%isCongruent = .FALSE.

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      newstart = 1

      ! find the first field with data and set the pattern to be matched
      do i=1, btype%field_count
       
        fieldp => btype%flist(i)
        call ESMF_FieldGet(fieldp, array=array, datamap=datamap, &
                           staggerloc=pattern%datastaggerloc, &
                           haloWidth=pattern%haloWidth, rc=status)
        if (status .ne. ESMF_SUCCESS) cycle


        ! if you get here, this field has an array.  check it for 
        ! data types, etc.
        call ESMF_InternArrayGet(array, rank=pattern%datarank, &
                           typekind=pattern%typekind, &
                           rc=status)
        if (status .ne. ESMF_SUCCESS) cycle

        call ESMF_FieldDataMapGet(datamap, dataIndexList=pattern%indexorders, &
                                  counts=pattern%nonindexcounts, rc=status)
        if (status .ne. ESMF_SUCCESS) cycle

        newstart = i+1
        exit

      enddo
  
      ! if no fields had data, return now.
      if (newstart .le. 1) then
          rc = ESMF_SUCCESS
          return
      endif

      ! now starting from the pattern field, compare the rest to see if they
      ! match.  first nonmatch we can exit with return .FALSE.

      ! set this here so we can just return if we find a mismatch
      rc = ESMF_SUCCESS
      do i=newstart, btype%field_count
       
        fieldp => btype%flist(i)
        call ESMF_FieldGet(fieldp, array=array, datamap=datamap, &
                           staggerloc=candidate%datastaggerloc, &
                           haloWidth=candidate%haloWidth, rc=status)
        if (status .ne. ESMF_SUCCESS) return

        ! if you get here, this field has an array.  check it for 
        ! data types, etc.
        call ESMF_InternArrayGet(array, rank=candidate%datarank, &
                           typekind=candidate%typekind, &
                           rc=status)
        if (status .ne. ESMF_SUCCESS) return

        call ESMF_FieldDataMapGet(datamap, &
                                  dataIndexList=candidate%indexorders, &
                                  counts=candidate%nonindexcounts, rc=status)
        if (status .ne. ESMF_SUCCESS) return

        ! now we have all the info; compare and bail on first mismatch
        if (pattern%datarank .ne. candidate%datarank ) return
        if (pattern%typekind .ne. candidate%typekind ) return
        if (pattern%haloWidth .ne. candidate%haloWidth) return
        if (pattern%datastaggerloc .ne. candidate%datastaggerloc) return

        ! TODO: finish this
        !do j=1, gridrank
        !if (pattern%indexorders(ESMF_MAXDIM) .ne. &
        !     candidate%indexorders(ESMF_MAXDIM)) return
        !do j=1, datarank-gridrank 
        !if (pattern%nonindexcounts(ESMF_MAXDIM) .ne. &
        !     candidate%nonindexcounts(ESMF_MAXDIM)) return

      enddo
     
      ! if you get here, all fields matched
      ESMF_BundleIsCongruent = .TRUE.
      btype%isCongruent = .TRUE.

      if (present(rc)) rc = ESMF_SUCCESS


      end function ESMF_BundleIsCongruent


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetFieldByName"
!BOP
! !IROUTINE: ESMF_BundleGetField - Retrieve a Field by name
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetField()
      subroutine ESMF_BundleGetFieldByName(bundle, name, field, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      character (len = *), intent(in) :: name
      type(ESMF_Field), intent(out) :: field
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a {\tt field} from a {\tt bundle} using
!      the {\tt field}'s {\tt name}.
!
!     The arguments are:
!     \begin{description}
!
!     \item [bundle]
!           {\tt ESMF\_Bundle} to query for {\tt ESMF\_Field}.
!     \item [name]
!           {\tt ESMF\_Field} name.
!     \item [field]
!           Returned {\tt ESMF\_Field}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP


      integer :: status                           ! Error status
      integer :: i                                ! temp var
      logical :: found                            ! did we find a match?
      character (len=ESMF_MAXSTR) :: temp_name
      !type(ESMF_Field) :: temp_field
      type(ESMF_BundleType), pointer :: btype

      ! Initialize return code.  Assume routine not implemented.
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      found = .FALSE.

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      ! Check for an empty Bundle first
      if(btype%field_count .eq. 0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Empty Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Check each field for a match
      do i = 1, btype%field_count
  
       call ESMF_FieldGet(btype%flist(i), name=temp_name, rc=status)
       ! "Error getting Field name from Field ", i
       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       if (name .eq. temp_name) then
           field = bundle%btypep%flist(i) 
           found = .TRUE.
           ! found match, exit loop early
           exit
        endif
      enddo

      if (.not. found) then
        !"Field not found with name ", name
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Field not found with requested name", &
                                 ESMF_CONTEXT, rc)) return
      endif

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetFieldByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetFieldByNum"
!BOP
! !IROUTINE: ESMF_BundleGetField - Retrieve a Field by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetField()
      subroutine ESMF_BundleGetFieldByNum(bundle, fieldIndex, field, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      integer, intent(in) :: fieldIndex
      type(ESMF_Field), intent(out) :: field
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns a {\tt field} from a {\tt bundle} by index number.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt ESMF\_Bundle} to query for {\tt ESMF\_Field}.
!     \item [fieldIndex]
!           {\tt ESMF\_Field} index number; first {\tt fieldIndex} is 1.
!     \item [field]
!           Returned {\tt ESMF\_Field}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP


      integer :: status                           ! Error status
      logical :: found                            ! did we find a match?
      type(ESMF_BundleType), pointer :: btype

      ! Initialize return code.  Assume routine not implemented.
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      found = .FALSE.

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      ! Check for an empty Bundle first
      if(btype%field_count .eq. 0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Empty Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Check for out of range index number
      if ((fieldIndex .lt. 1) .or. (fieldIndex .gt. btype%field_count)) then
        ! "ERROR in ESMF_BundleGetField: fieldIndex ", fieldIndex, &
        !                "out of range. Min=1, max=", btype%field_count
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Index out of range", &
                                 ESMF_CONTEXT, rc)) return
        return
      endif

      ! Fetch requested field
      field = bundle%btypep%flist(fieldIndex) 

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetFieldByNum


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetFieldNames"
!BOP
! !IROUTINE: ESMF_BundleGetFieldNames - Return all Field names in a Bundle

! !INTERFACE:
      subroutine ESMF_BundleGetFieldNames(bundle, nameList, nameCount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle 
      character (len = *), intent(out) :: nameList(:)
      integer, intent(out), optional :: nameCount     
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Returns an array of {\tt ESMF\_Field} names in an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [nameList]
!           An array of character strings where each {\tt ESMF\_Field} name
!           is returned.  Must be at least as long as {\tt nameCount}.
!     \item [{[nameCount]}]
!           A count of how many {\tt ESMF\_Field} names were returned.  Same as
!           the number of {\tt ESMF\_Field}s in the {\tt ESMF\_Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: i, status
      type(ESMF_BundleType), pointer :: bp

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      bp => bundle%btypep

      if (present(nameCount)) nameCount = bp%field_count

      if (size(nameList) .lt. bp%field_count) then
          call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
                                  "nameList too short for number of fields", &
                                  ESMF_CONTEXT, rc)
          return
      endif

      do i=1, bp%field_count
          call ESMF_FieldGet(bp%flist(i), name=nameList(i), rc=status)      
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      enddo 

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleGetFieldNames


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundlePrint"
!BOP
! !IROUTINE: ESMF_BundlePrint - Print information about a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundlePrint(bundle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Prints diagnostic information about the {\tt bundle}
!      to {\tt stdout}.  
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [{[options]}]
!           Print options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP


      character(len=ESMF_MAXSTR) :: bname, fname
      !character(len=ESMF_MAXSTR) :: msgbuf
      type(ESMF_BundleType), pointer :: btype
      !type(ESMF_Field) :: field
      integer :: i
      integer :: status
      character(len=6) :: defaultopts

       ! print option is not implemented, but it has to pass to c_ESMC_BasePrint()
      defaultopts = "brief"

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

    !jw  call ESMF_LogWrite("Bundle Print:", ESMF_LOG_INFO)
      write (*, *)  "Bundle print:"

      if (.not. associated(bundle%btypep)) then
      !jw  call ESMF_LogWrite("Empty or Uninitialized Bundle", ESMF_LOG_INFO)
        write(*,*) "Empty or Uninitialized Bundle"
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      btype => bundle%btypep
      call c_ESMC_GetName(btype%base, bname, status)
    !jw  write (msgbuf, *)  "  Bundle name = ", trim(bname)
    !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write (*, *)  "  Bundle name = ", trim(bname)

    ! pli: print attributes 
      call c_ESMC_BasePrint(btype%base, defaultopts, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
    
    !jw  write (msgbuf, *)  "  Field count = ", btype%field_count
    !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write (*, *)  "  Field count = ", btype%field_count
    
      do i = 1, btype%field_count

       write (*, *)  "    Field", i, ": "
       call ESMF_FieldPrint(btype%flist(i),rc=status)  
       !call ESMF_FieldGet(btype%flist(i), name=fname, rc=status)
       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

     !jw  write (msgbuf, *)  "    Field", i, "name = ", trim(fname)
     !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      enddo

      ! TODO: add more code here for printing more info

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundlePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRead"
!BOPI
! !IROUTINE: ESMF_BundleRead - Create a Bundle from an external source
!
! !INTERFACE:
      function ESMF_BundleRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [{[iospec]}]
!           The file I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

!EOPI

!
!  TODO: code goes here
!
      type(ESMF_Bundle) :: b

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      allocate(b%btypep)

      b%btypep%bundlestatus = ESMF_STATUS_UNINIT

      ESMF_BundleRead = b

      end function ESMF_BundleRead

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleReadRestart"
!BOPI
! !IROUTINE: ESMF_BundleReadRestart - Read back a saved Bundle
!
! !INTERFACE:
      function ESMF_BundleReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleReadRestart
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name     
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a {\t ESMF\_Bundle} 
!      from the last call to WriteRestart.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [{[iospec]}]
!           The I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI


!
!  TODO: code goes here
!
      type(ESMF_Bundle) :: b

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      allocate(b%btypep)

      b%btypep%bundlestatus = ESMF_STATUS_UNINIT

      ESMF_BundleReadRestart = b

      end function ESMF_BundleReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRemoveField"
!BOPI
! !IROUTINE: ESMF_BundleRemoveField - Remove a Field from a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleRemoveField(bundle, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      character (len = *), intent(in) :: name
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Deletes an {\tt ESMF\_Field} reference from an existing {\tt bundle}
!      by {\tt name}.  
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} to remove the {\tt ESMF\_Field} from.
!     \item [name]
!           The name of the {\tt ESMF\_Field} to remove.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
      integer :: localrc                        ! local return code

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

!
!  TODO: code goes here
!
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleRemoveField


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleSetAttribute - Set an attribute
!
! !INTERFACE:
!     subroutine ESMF_BundleSetAttribute(bundle, name, <value argument>, rc)
!
! !ARGUMENTS:
!     type(ESMF_Bundle), intent(inout) :: bundle  
!     character (len = *), intent(in) :: name
!     <value argument>, see below for supported values
!     integer, intent(out), optional :: rc   
!
!
! !DESCRIPTION:
!     Attaches an attribute to the {\tt bundle}.  
!     The attribute has a {\tt name} and either a {\tt value} or a 
!     {\tt valueList}.
!     Supported values for the <value argument> are:
!     \begin{description}
!     \item integer(ESMF\_KIND\_I4), intent(in) :: value
!     \item integer(ESMF\_KIND\_I4), dimension(:), intent(in) :: valueList
!     \item integer(ESMF\_KIND\_I8), intent(in) :: value
!     \item integer(ESMF\_KIND\_I8), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R4), intent(in) :: value
!     \item real (ESMF\_KIND\_R4), dimension(:), intent(in) :: valueList
!     \item real (ESMF\_KIND\_R8), intent(in) :: value
!     \item real (ESMF\_KIND\_R8), dimension(:), intent(in) :: valueList
!     \item type(ESMF\_Logical), intent(in) :: value
!     \item type(ESMF\_Logical), dimension(:), intent(in) :: valueList
!     \item character (len = *), intent(in), value
!     \end{description}
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [<value argument>]
!           The value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetInt4Attr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetInt4Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte integer attribute to the {\tt bundle}.  
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [value]
!           The integer value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetInt4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetInt4ListAttr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetInt4ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte integer list attribute to the {\tt bundle}.  
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is given 
!     by {\tt count}. 
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
      integer :: status 
      integer :: limit

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetInt4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetInt8Attr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetInt8Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer(ESMF_KIND_I8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte integer attribute to the {\tt bundle}.  
!      The attribute has a {\tt name} and a {\tt value}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [value]
!           The integer value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetInt8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetInt8ListAttr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetInt8ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer(ESMF_KIND_I8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 8-byte integer list attribute to the {\tt bundle}.  
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of integer items in the {\tt valueList} is given 
!     by {\tt count}. 
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [count]
!           The number of integers in the {\tt valueList}.
!     \item [valueList]
!           The integer values of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
      integer :: status
      integer :: limit

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetInt8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetReal4Attr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetReal4Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R4), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a 4-byte real attribute to the {\tt bundle}.  
!      The attribute has a {\tt name} and a {\tt value}. 
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [value]
!           The real value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetReal4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetReal8Attr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetReal8Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      real(ESMF_KIND_R8), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an 8-byte real attribute to the {\tt bundle}.  
!      The attribute has a {\tt name} and a {\tt value}. 
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [value]
!           The real value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetReal8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetReal4ListAttr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetReal4ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R4), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a 4-byte real list attribute to the {\tt bundle}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is given 
!     by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
      integer :: limit
      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetReal4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetReal8ListAttr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetReal8ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real(ESMF_KIND_R8), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an 8-byte real list attribute to the {\tt bundle}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of real items in the {\tt valueList} is given 
!     by {\tt count}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [count]
!           The number of reals in the {\tt valueList}.
!     \item [value]
!           The real values of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
      integer :: limit
      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetReal8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetLogicalAttr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetLogicalAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a logical attribute to the {\tt bundle}.
!      The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [value]
!           The logical true/false value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetLogicalListAttr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetLogicalListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: valueList
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches a logical list attribute to the {\tt bundle}.
!     The attribute has a {\tt name} and a {\tt valueList}.
!     The number of logical items in the {\tt value} list is given 
!     by {\tt count}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [count]
!           The number of logicals in the {\tt valueList}.
!     \item [valueList]
!           The logical values of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
      integer :: limit
      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetCharAttr"
!BOPI
! !IROUTINE: ESMF_BundleSetAttribute - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleSetAttribute()
      subroutine ESMF_BundleSetCharAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle  
      character (len = *), intent(in) :: name
      character (len = *), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character attribute to the {\tt bundle}.
!      The attribute has a {\tt name} and a {\tt value}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the attribute to set.
!     \item [value]
!           The character value of the attribute to set.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI

      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_AttributeSetChar(bundle%btypep%base, name, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetDataValues"
!BOPI
! !IROUTINE: ESMF_BundleSetDataValues - Set contents of packed array
!
! !INTERFACE:
      subroutine ESMF_BundleSetDataValues(bundle, index, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      integer, dimension (:), intent(in) :: index
      real(ESMF_KIND_R8), dimension (:), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Allows data values associated with an {\tt ESMF\_Bundle} to be 
!     set through the {\tt ESMF\_Bundle} interface instead of 
!     detaching data and setting it in a loop.
!     Various restrictions on data types may be imposed.
! 
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} to operate on.
!     \item [index]
!           Index values to change.
!     \item [value]
!           Data value(s) to set.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
      integer :: localrc                        ! local return code

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

!
!  TODO: code goes here
!

      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleSetDataValues


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSetGrid"
!BOP
! !IROUTINE: ESMF_BundleSetGrid - Associate a Grid with an empty Bundle
! 
! !INTERFACE:
      subroutine ESMF_BundleSetGrid(bundle, grid, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Sets the {\tt grid} for a {\tt bundle} that contains no {\tt ESMF\_Field}s. 
!   All {\tt ESMF\_Field}s added to this {\tt bundle} must be
!   associated with the same {\tt ESMF\_Grid}.  Returns an error if 
!   there is already an {\tt ESMF\_Grid} associated with the {\tt bundle}.
!
!   The arguments are:
!   \begin{description}
!   \item [bundle]
!        An {\tt ESMF\_Bundle} object.
!   \item [grid]
!        The {\tt ESMF\_Grid} which all {\tt ESMF\_Field}s added to this
!        {\tt ESMF\_Bundle} must have.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP


      integer :: status                           ! Error status
      type(ESMF_BundleType), pointer :: btype     ! internal data

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_BundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep
   
      ! here we will only let someone associate a grid with a bundle
      ! if there is not one already associated with it.  
      if (btype%gridstatus .eq. ESMF_STATUS_READY) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Bundle is already associated with a Grid", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! OK to set grid, but validate it first
       call ESMF_GridValidate(grid, rc=status)
       if (ESMF_LogMsgFoundError(status, &
                                   ESMF_ERR_PASSTHRU, &
                                   ESMF_CONTEXT, rc)) return
      btype%grid = grid
      btype%gridstatus = ESMF_STATUS_READY

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleSetGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleValidate"
!BOP
! !IROUTINE: ESMF_BundleValidate - Check validity of a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleValidate(bundle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      character (len=*), intent(in), optional :: options 
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt bundle} is internally consistent.
!      Currently this method determines if the {\tt bundle} is uninitialized 
!      or already destroyed.  The method returns an error code if problems 
!      are found.  
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt ESMF\_Bundle} to validate.
!     \item [{[options]}]
!           Validation options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt bundle}
!           is valid.
!     \end{description}

!EOP


      ! Local variables
      integer :: status                           ! Error status

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)

      if (.not.associated(bundle%btypep)) then 
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif 

      if (bundle%btypep%bundlestatus .ne. ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif 

      ! TODO: add more code here

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleWrite"
!BOPI
! !IROUTINE: ESMF_BundleWrite - Save a Bundle to an external destination
!
! !INTERFACE:
      subroutine ESMF_BundleWrite(bundle, subarray, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle
      type(ESMF_InternArray), pointer, optional :: subarray
      type(ESMF_IOSpec), intent(in), optional :: iospec 
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [{[subarray]}]
!           The subset to write.
!     \item [{[iospec]}]
!           The I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc                        ! local return code

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

!
!  TODO: code goes here
!
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleWrite


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleWriteRestart"
!BOPI
! !IROUTINE: ESMF_BundleWriteRestart - Save Bundle in the quickest manner possible
!
! !INTERFACE:
      subroutine ESMF_BundleWriteRestart(bundle, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other format options).  Internally will use 
!      same I/O interfaces as Read/Write
!      but will default to the fastest options.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [{[iospec]}]
!           The I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      integer :: localrc                        ! local return code

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

!
!  TODO: code goes here
!
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_BundleWriteRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleTypeAddFieldList"
!BOPI
! !IROUTINE: ESMF_BundleTypeAddFieldList - Add a list of Fields to a Bundle.
!
! !INTERFACE:
      subroutine ESMF_BundleTypeAddFieldList(btype, fieldCount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype        
      integer, intent(in) :: fieldCount
      type(ESMF_Field), dimension(:), intent(inout) :: fields
      integer, intent(out), optional :: rc          
!
! !DESCRIPTION:
!  Add a Field reference to an existing {\tt ESMF\_Bundle}.  
!  The {\tt ESMF\_Field} must have the
!  same {\tt ESMF\_Grid} as the rest of the 
!  {\tt ESMF\_Field}s in the {\tt ESMF\_Bundle}.
!  If the {\tt ESMF\_Bundle} has
!  packed data this will mean making a copy of the data.
!  Note: packed data is currently not supported. 
!
!  The arguments are:
!  \begin{description}
!  \item [btype]
!        {\tt ESMF\_BundleType} to add {\tt ESMF\_Field}s into.
!  \item [fieldCount]
!        Number of fields to be added to the {\tt ESMF\_Bundle}.
!        Must be equal to or less than the number of 
!        {\tt ESMF\_Field}s in the following argument.
!  \item [fields]
!        Array of existing {\tt ESMF\_Field}s.  The first {\tt fieldCount}
!        items will be added to the {\tt ESMF\_Bundle}.
!  \item [{[rc]}]
!        Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
! 
!EOPI
      
      integer :: status                           ! Error status
      integer :: i                                ! temp var
      type(ESMF_Field), dimension(:), pointer :: temp_flist  
                                                  ! list of fields
      type(ESMF_Grid) :: testgrid, matchgrid
      logical :: wasempty

      ! Initialize return code.  Assume routine not implemented.
      status = ESMF_RC_NOT_IMPL
      if(present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleTypeGetInit,btype,rc)
      do i=1,fieldCount
         ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fields(i),rc)
      enddo

      ! Initial values
      nullify(temp_flist)
    
      ! early exit.
      if (fieldCount .le. 0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "called with no Fields", &
                                 ESMF_CONTEXT, rc)) return
      endif
      
      ! validate fields before moving ahead
      do i=1, fieldCount
         call ESMF_FieldValidate(fields(i), rc=status)
         if (ESMF_LogMsgFoundError(status, &
                        "Invalid Field found when trying to add into Bundle", &
                                   ESMF_CONTEXT, rc)) return
      enddo

      ! consistency checking.  logic is: 
      !    if bundle has grid, use it to compare against
      !    if bundle has no grid, find first field w/ grid and use it instead
      !    if field has no grid, skip it 
      !    if inconsistent grid found in list, exit w/ error leaving bundle
      !       unchanged
      !    if all ok, then if bundle had no grid originally, set it here 

      matchgrid%this = ESMF_NULL_POINTER
      if (btype%gridstatus .eq. ESMF_STATUS_UNINIT) then
          do i=1, fieldCount
            ! an error here is not fatal; just means field has no grid yet.
            call ESMF_FieldGet(fields(i), grid=testgrid, rc=status)
            if (status .ne. ESMF_SUCCESS) cycle

            ! use grid from first field in add list which contains one
            matchgrid = testgrid
            exit
          enddo
       else
          ! use the grid already associated with the bundle 
          matchgrid = btype%grid
       endif
  
       ! if bundle has no grid, and all new fields have no grid, then 
       ! we cannot do any grid consistency checks here.  so only continue
       ! here if someone somewhere has a grid to compare against.

! TODO:FIELDINTEGRATION This section checks new grid values against
! old grid.  It needs to be rewritten for new Grid, which, due to the
! switch from Fortran to C++, does not use Fortran pointers.

!       if (associated(matchgrid%ptr)) then
!          ! check matchgrid against each new grid in the add list
!          do i=1, fieldCount
!            call ESMF_FieldGet(fields(i), grid=testgrid, rc=status)
!            if (status .ne. ESMF_SUCCESS) cycle
!
!            if (.not. associated(matchgrid%ptr,testgrid%ptr)) then
!                if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
!                   "Fields with inconsistent Grids cannot be added to Bundle", &
!                                 ESMF_CONTEXT, rc)) return
!            endif
!          enddo
!          ! if this is the first field added with a grid, set the bundle grid
!          if (btype%gridstatus .eq. ESMF_STATUS_UNINIT) then
!            btype%grid = matchgrid
!            btype%gridstatus = ESMF_STATUS_READY
!          endif
!      endif

      ! if we get this far, either no one has any grids, or the grids
      ! have passed the consistency check.  add them to the bundle.

      ! Add the fields in the list, checking for consistency.
      if (btype%field_count .eq. 0) then
        
          wasempty = .TRUE. 

          allocate(btype%flist(fieldCount), stat=status)
          if (ESMF_LogMsgFoundAllocError(status, "Fieldlist allocate", &
                                       ESMF_CONTEXT, rc)) return
         
          ! now add the fields to the new list
          do i=1, fieldCount
            btype%flist(i) = fields(i)
          enddo

          btype%field_count = fieldCount
      else

          wasempty = .FALSE.

          ! make a list the right length
          allocate(temp_flist(btype%field_count + fieldCount), stat=status)
          if (ESMF_LogMsgFoundAllocError(status, "temp Fieldlist allocate", &
                                       ESMF_CONTEXT, rc)) return

          ! preserve old contents
          do i = 1, btype%field_count
            temp_flist(i) = btype%flist(i)
          enddo

          ! and append the new fields to the list
          do i=1, fieldCount
            temp_flist(btype%field_count+i) = fields(i)
          enddo

          ! delete old list
          deallocate(btype%flist, stat=status)
          if (ESMF_LogMsgFoundAllocError(status, "Fieldlist deallocate", &
                                       ESMF_CONTEXT, rc)) return

          ! and now make this the permanent list
          btype%flist => temp_flist
          btype%field_count = btype%field_count + fieldCount

      endif

      ! If packed data buffer requested, create or update it here.
      ! if (btype%pack_flag .eq. ESMF_PACKED_DATA) then

      !   call ESMF_BundleTypeRepackData(btype, rc=status)
      !   if (ESMF_LogMsgFoundAllocError(status, ESMF_ERR_PASSTHRU, &
      !   ESMF_CONTEXT, rcToReturn=rc)) return

      ! endif

      ! TODO: outstanding architectural issue:
      ! unless all the fields are required to contain data before they are
      ! added to the bundle, we cannot set the congruent flag yet -- it is
      ! possible with the current interfaces to add empty fields to a bundle
      ! and then add inconsistent grids to the fields -- which needs to be
      ! avoided; but also the data can be added afterwards and there is no
      ! obvious time to check for consistency/congruency.   the suggested
      ! fix is that a field being added to a bundle must already contain both
      ! a grid and the data array, so we can do the checking here.

      ! all the handling below is to fool around with maintaining information
      ! about whether the data fields inside this bundle are identical in
      ! rank, data type, staggerloc, index order, etc.  if we know they already
      ! are not, we can jump around the code to the continue.  otherwise we
      ! have to test the new fields to make sure they match in datatype.

      ! if already known that field data is not the same, then do not
      ! bother to test recently added fields.  but if true, then compare
      ! the fields to see if the types all match.   an optimization is to
      ! only compare the newly added fields and not search the old ones.
      if (btype%isCongruent) then
          ! compare the last fieldCount fields against the congruent data
          ! type stored in the bundle.  if the bundle is empty, set the
          ! congruent info from the first one and then proceed from fields
          ! 2 thru fieldCount.
         
          ! if not contradicted by the data, leave isCongruent .TRUE.
          ! else at the first mismatch, set isCongruent to .FALSE. and
          ! bail out of the loop.
          if (wasempty) then
              ! search the fields looking for one with data (it is possible
              ! none have data - in which case we cannot change the flag)
              ! the first one sets the congruent pattern which all others
              ! must match to optimize the communication of this bundle
              ! in the most extreme version.  (other optimizations are possible
              ! with non-congruent bundles.
              
              ! if all empty, get out
              ! if not, set the congruent datainfo here
          endif
          
      endif

      ! TODO: this code does nothing with the congruent flag right now.
      ! this needs to be fixed before the bundle communication code can 
      ! be considered robust.

10 continue
      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleTypeAddFieldList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleConstructNew"
!BOPI
! !IROUTINE: ESMF_BundleConstructNew - Construct the internals of a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleConstructNew(btype, fieldCount, fields, &
                                         packflag, bundleinterleave, &
                                         name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype 
      integer, intent(in) :: fieldCount           
      type(ESMF_Field), dimension (:) :: fields
      type(ESMF_PackFlag), intent(in), optional :: packflag 
      type(ESMF_InterleaveFlag), intent(in), optional :: bundleinterleave
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Constructs an {\tt ESMF\_Bundle} from a list of existing
!   gridded {\tt ESMF\_Fields}.  This routine requires an existing
!   {\tt ESMF\_Bundle} type as an input and fills in
!   the internals.  {\tt ESMF\_BundleCreateNew()} does
!   the allocation of an {\tt ESMF\_Bundle} type first and then
!   calls this routine.
!
!   The arguments are:
!   \begin{description}
!   \item [btype]
!      Pointer to an {\tt ESMF\_Bundle} object.
!   \item [fieldCount]
!      Number of fields to be added to the {\tt ESMF\_Bundle}.
!      Must be equal to or less than the number of
!      {\tt ESMF\_Field}s in the following argument.
!   \item [fields]
!      Array of existing {\tt ESMF\_Field}s.  The first {\tt fieldCount}
!      items will be added to the {\tt ESMF\_Bundle}.
!   \item [{[packflag]}]
!      If set to {\tt ESMF\_PACK\_FIELD\_DATA}, the {\tt ESMF\_Field}
!      data in individual {\tt ESMF\_Array}s will be collected
!      into a single data {\tt ESMF\_Array} for the entire {\tt ESMF\_Bundle}.
!      The default is {\tt ESMF\_NO\_PACKED\_DATA}.
!   \item [{[bundleinterleave]}]
!      {\tt ESMF\_Interleave} type.  Controls whether the data in
!      the packed bundle is interleaved by field or by item.
!      Valid options are {\tt ESMF\_INTERLEAVE\_BY\_BLOCK} or
!      {\tt ESMF\_INTERLEAVE\_BY\_ITEM}.  The default is by item.
!   \item [{[name]}]
!      {\tt ESMF\_Bundle} name.  A default name will be generated if
!      one is not specified.
!   \item [{[iospec]}]
!      I/O specification.
!   \item [{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
      
      integer :: status                           ! Error status
      integer :: i

      ! Initialize return code.  Assume routine not implemented.
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      do i=1,fieldCount
         ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fields(i),rc)
      enddo

      ! Initialize the derived type contents.
      call ESMF_BundleConstructNoFields(btype, name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! If specified, set packflag and interleave
      if(present(packflag)) btype%pack_flag = packflag
      if(present(bundleinterleave)) btype%fil%field_bfa%bfa_type = bundleinterleave

      ! Add the fields in the list, checking for consistency.
      call ESMF_BundleTypeAddFieldList(btype, fieldCount, fields, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleConstructNew


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleConstructNoFields"
!BOPI
! !IROUTINE: ESMF_BundleConstructNoFields - Construct the internals of a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleConstructNoFields(btype, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype 
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Constructs the internals of an {\tt ESMF\_Bundle}, given an existing
!     {\tt ESMF\_Bundle} type as an input.
!
!     The arguments are:
!     \begin{description}
!     \item [btype]
!           An existing {\tt ESMF\_Bundle} to be initialized.
!     \item [{[name]}]
!           {\tt ESMF\_Bundle} name.  A default name will be generated if
!           one is not specified.
!     \item [{[iospec]}]
!           I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      
      integer :: status                            ! Error status
      !character (len = ESMF_MAXSTR) :: defaultname ! Bundle name if not given

      ! Initialize return code.  Assume routine not implemented.
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Initialize the base object
      btype%base%this = ESMF_NULL_POINTER
      call ESMF_BaseCreate(btype%base, "Bundle", name, 0, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

   
      ! Initialize bundle contents.  An empty Bundle starts out with the
      ! status flags uninitialized, and assumes all data is congruent.
      ! As fields are added, the first non-compliant one turns the flag
      ! to false, and after it is false, there is no way to set it back
      ! to true.
      btype%localbundle%gridstatus = ESMF_STATUS_UNINIT
      btype%localbundle%arraystatus = ESMF_STATUS_UNINIT
      btype%gridstatus = ESMF_STATUS_UNINIT
      btype%isCongruent = .TRUE.
   
      btype%field_count = 0
      nullify(btype%flist)
      
      btype%pack_flag = ESMF_NO_PACKED_DATA
!     nullify(btype%localbundle%packed_data)
      btype%bundlestatus = ESMF_STATUS_READY
  

      ! Set as created 
      ESMF_INIT_SET_CREATED(btype)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleConstructNoFields


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDestruct"
!BOPI
! !IROUTINE: ESMF_BundleDestruct - Free contents of a Bundle 
!
! !INTERFACE:
      subroutine ESMF_BundleDestruct(btype, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype 
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Releases all resources except the {\tt ESMF\_Bundle} itself.
!
!     \begin{description}
!     \item [btype]
!           Pointer to an {\tt ESMF\_Bundle} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      ESMF_INIT_CHECK_DEEP(ESMF_BundleTypeGetInit,btype,rc)

      btype%bundlestatus = ESMF_STATUS_INVALID
      call ESMF_BaseDestroy(btype%base, status)

      ! TODO: if packed array exists, add code to delete the array here.

      if (associated(btype%flist)) then
          deallocate(btype%flist, stat=status)
          if (ESMF_LogMsgFoundAllocError(status, "Bundle deallocate", &
                                         ESMF_CONTEXT, rc)) return

      endif

      ! Set as deleted 
      ESMF_INIT_SET_DELETED(btype)

      if (present(rc)) rc = status


      end subroutine ESMF_BundleDestruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleSerialize"

!BOPI
! !IROUTINE: ESMF_BundleSerialize - Serialize bundle info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_BundleSerialize(bundle, buffer, length, offset, rc) 
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(inout) :: bundle 
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_Bundle} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_BundleWrite()} and {\tt ESMF\_BundleRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt ESMF\_Bundle} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc                     ! Error status
      integer :: i
      type(ESMF_BundleType), pointer :: bp   ! bundle type

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

! TODO:FIELDINTEGRATION This method is not supported yet.
#if 0
        type(ESMF_Base) :: base                   ! base class object
        type(ESMF_Field), dimension(:), pointer :: flist
        type(ESMF_Status) :: bundlestatus
        type(ESMF_Status) :: gridstatus
        integer :: field_count
        type(ESMF_Grid) :: grid                  ! associated global grid
        type(ESMF_LocalBundle) :: localbundle    ! this differs per DE
        type(ESMF_Packflag) :: pack_flag         ! is packed data present?
        type(ESMF_BundleFieldIntrlv) :: fil  ! ordering in buffer
        type(ESMF_IOSpec) :: iospec              ! iospec values
        type(ESMF_Status) :: iostatus            ! if unset, inherit from gcomp

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_BundleGetInit,bundle,rc)      

      ! shortcut to internals
      bp => bundle%btypep

      call c_ESMC_BaseSerialize(bp%base, buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_BundleSerialize(bp%bundlestatus, bp%gridstatus, &
                                 bp%field_count, bp%pack_flag, &
                                 bp%mapping, bp%iostatus, &
                                 buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      if (bp%gridstatus .eq. ESMF_STATUS_READY) then
          call ESMF_GridSerialize(bp%grid, buffer, length, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return

      endif

      ! TODO: decide if these need to be sent before or after
      do i = 1, bp%field_count
          call ESMF_FieldSerialize(bp%flist(i), buffer, length, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      enddo

    ! TODO: if shallow, call C directly?
      !call ESMF_IOSpecSerialize(bp%iospec, buffer, length, offset, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                           ESMF_ERR_PASSTHRU, &
      !                           ESMF_CONTEXT, rc)) return

     ! TODO: call C directly here?
      !call ESMF_ArraySerialize(bp%localbundle%localdata, buffer, length, &
      !                          offset, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                           ESMF_ERR_PASSTHRU, &
      !                           ESMF_CONTEXT, rc)) return

      if  (present(rc)) rc = ESMF_SUCCESS
#endif

      end subroutine ESMF_BundleSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleDeserialize"

!BOPI
! !IROUTINE: ESMF_BundleDeserialize - Deserialize a byte stream into a Bundle
!
! !INTERFACE:
      function ESMF_BundleDeserialize(vm, buffer, offset, rc) 
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleDeserialize   
!
! !ARGUMENTS:
      type(ESMF_VM), intent(in) :: vm
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a Bundle object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_BundleWrite()} and {\tt ESMF\_BundleRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [vm]
!           Current VM in which this object should be created.
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: localrc, status             ! Error status, allocation status
      integer :: i
      type(ESMF_BundleType), pointer :: bp   ! bundle type


      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! in case of error, make sure this is invalid.
      nullify(ESMF_BundleDeserialize%btypep)

! TODO:FIELDINTEGRATION This method is not supported yet.
#if 0

      ! shortcut to internals
      allocate(bp, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, &
                                     "space for new Bundle object", &
                                     ESMF_CONTEXT, rc)) return


      call ESMF_BaseCreate(bp%base, "Bundle", "dummy", 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! this overwrites the name and adds attributes to the base obj.
      call c_ESMC_BaseDeserialize(bp%base, buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_BundleDeserialize(bp%bundlestatus, bp%gridstatus, &
                                 bp%field_count, bp%pack_flag, &
                                 bp%mapping, bp%iostatus, &
                                 buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      if (bp%gridstatus .eq. ESMF_STATUS_READY) then
          bp%grid = ESMF_GridDeserialize(vm, buffer, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                     ESMF_ERR_PASSTHRU, &
                                     ESMF_CONTEXT, rc)) return
      endif

      ! TODO: decide if these need to be sent before or after
      allocate(bp%flist(bp%field_count), stat=localrc)
      if (ESMF_LogMsgFoundAllocError(localrc, &
                                     "Field list", &
                                     ESMF_CONTEXT, rc)) return

      do i = 1, bp%field_count
          bp%flist(i) = ESMF_FieldDeserialize(vm, buffer, offset, localrc)
          if (ESMF_LogMsgFoundError(localrc, &
                                    ESMF_ERR_PASSTHRU, &
                                    ESMF_CONTEXT, rc)) return
      enddo


    ! TODO: if shallow, call C directly?
      !call ESMF_IOSpecDeserialize(bp%iospec, buffer, offset, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                           ESMF_ERR_PASSTHRU, &
      !                           ESMF_CONTEXT, rc)) return

     ! TODO: call C directly here?
      !call ESMF_ArrayDeserialize(bp%localbundle%localdata, buffer, &
      !                          offset, localrc)
      !if (ESMF_LogMsgFoundError(localrc, &
      !                           ESMF_ERR_PASSTHRU, &
      !                           ESMF_CONTEXT, rc)) return

      ESMF_BundleDeserialize%btypep => bp

      ! Set as created
      ESMF_INIT_SET_CREATED(ESMF_BundleDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS
#endif

      end function ESMF_BundleDeserialize
!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleFieldAccessGetInit"
!BOPI
! !IROUTINE:  ESMF_BundleFieldAccessGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_BundleFieldAccessGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleFieldAccess), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_BundleFieldAccessGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt bundlefieldaccess}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleFieldAccess} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_BundleFieldAccessGetInit = ESMF_INIT_GET(s)
       else
         ESMF_BundleFieldAccessGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_BundleFieldAccessGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleFieldAccessInit"
!BOPI
! !IROUTINE:  ESMF_BundleFieldAccessInit - Initialize BundleFieldAccess

! !INTERFACE:
    subroutine ESMF_BundleFieldAccessInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleFieldAccess) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt bundlefieldaccess}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleFieldAccess} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_BundleFieldAccessInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleFieldAccessValidate"
!BOPI
! !IROUTINE:  ESMF_BundleFieldAccessValidate - Check validity of a BundleFieldAccess

! !INTERFACE:
    subroutine ESMF_BundleFieldAccessValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_BundleFieldAccess), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt BundleFieldAccess} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleFieldAccess} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt bundlefieldaccess}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_BundleFieldAccessGetInit, ESMF_BundleFieldAccessInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_BundleFieldAccessValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleFieldIntrlvGetInit"
!BOPI
! !INTERFACE:
    function ESMF_BundleFieldIntrlvGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleFieldIntrlv), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_BundleFieldIntrlvGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt bundlefieldinterleave}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleFieldInterleave} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_BundleFieldIntrlvGetInit = ESMF_INIT_GET(s)
       else
         ESMF_BundleFieldIntrlvGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_BundleFieldIntrlvGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleFieldIntrlvInit"
!BOPI
! !IROUTINE:  ESMF_BundleFieldIntrlvInit - Initialize BundleFieldInterleave

! !INTERFACE:
    subroutine ESMF_BundleFieldIntrlvInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleFieldIntrlv) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt bundlefieldinterleave}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleFieldInterleave} of which being initialized.
!     \end{description}
!
!EOPI

        ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_BundleFieldIntrlvInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleFieldIntrlvValidate"
!BOPI
! !IROUTINE:  ESMF_BundleFieldIntrlvValidate - Check validity of a BundleFieldInterleave

! !INTERFACE:
    subroutine ESMF_BundleFieldIntrlvValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_BundleFieldIntrlv), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt BundleFieldInterleave} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleFieldInterleave} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


     ESMF_INIT_CHECK_SHALLOW(ESMF_BundleFieldIntrlvGetInit, ESMF_BundleFieldIntrlvInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_BundleFieldIntrlvValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleCongrntDataGetInit"
!BOPI
! !IROUTINE:  ESMF_BundleCongrntDataGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_BundleCongrntDataGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleCongrntData), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_BundleCongrntDataGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt bundlecongruentdata}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ESMF_BundleCongrntData} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_BundleCongrntDataGetInit = ESMF_INIT_GET(s)
       else
         ESMF_BundleCongrntDataGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_BundleCongrntDataGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleCongrntDataInit"
!BOPI
! !IROUTINE:  ESMF_BundleCongrntDataInit - Initialize BundleCongruentData

! !INTERFACE:
    subroutine ESMF_BundleCongrntDataInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleCongrntData) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt bundlecongruentdata}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleCongruentData} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_BundleCongrntDataInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleCongrntDataValidate"
!BOPI
! !IROUTINE:  ESMF_BundleCongrntDataValidate - Check validity of a BundleCongruentData

! !INTERFACE:
    subroutine ESMF_BundleCongrntDataValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_BundleCongrntData), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt BundleCongruentData} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleCongruentData} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_BundleCongrntDataGetInit, ESMF_BundleCongrntDataInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif 
    end subroutine ESMF_BundleCongrntDataValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalBundleGetInit"
!BOPI
! !IROUTINE:  ESMF_LocalBundleGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LocalBundleGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_LocalBundle), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_LocalBundleGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt localbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalBundle} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_LocalBundleGetInit = ESMF_INIT_GET(s)
       else
         ESMF_LocalBundleGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_LocalBundleGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalBundleInit"
!BOPI
! !IROUTINE:  ESMF_LocalBundleInit - Initialize LocalBundle

! !INTERFACE:
    subroutine ESMF_LocalBundleInit(s)
!
! !ARGUMENTS:
       type(ESMF_LocalBundle) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt localbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalBundle} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_LocalBundleInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalBundleValidate"
!BOPI
! !IROUTINE:  ESMF_LocalBundleValidate - Check validity of a LocalBundle

! !INTERFACE:
    subroutine ESMF_LocalBundleValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_LocalBundle), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt LocalBundle} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalBundle} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_LocalBundleGetInit,ESMF_LocalBundleInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_LocalBundleValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleTypeGetInit"
!BOPI
! !IROUTINE:  ESMF_BundleTypeGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_BundleTypeGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleType), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_BundleTypeGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt bundletype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleType} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_BundleTypeGetInit = ESMF_INIT_GET(s)
       else
         ESMF_BundleTypeGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_BundleTypeGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleTypeInit"
!BOPI
! !IROUTINE:  ESMF_BundleTypeInit - Initialize BundleType

! !INTERFACE:
    subroutine ESMF_BundleTypeInit(s)
!
! !ARGUMENTS:
       type(ESMF_BundleType) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt bundletype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleType} of which being initialized.
!     \end{description}
!
!EOPI

        nullify(s%flist)

        ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_BundleTypeInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleTypeValidate"
!BOPI
! !IROUTINE:  ESMF_BundleTypeValidate - Check validity of a BundleType

! !INTERFACE:
    subroutine ESMF_BundleTypeValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_BundleType), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt BundleType} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_BundleType} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


     ESMF_INIT_CHECK_SHALLOW(ESMF_BundleTypeGetInit,ESMF_BundleTypeInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_BundleTypeValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetInit"
!BOPI
! !IROUTINE:  ESMF_BundleGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_BundleGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_Bundle), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_BundleGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_Bundle} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_BundleGetInit = ESMF_INIT_GET(d)
       else
         ESMF_BundleGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_BundleGetInit


!------------------------------------------------------------------------------


      end module ESMF_BundleMod
