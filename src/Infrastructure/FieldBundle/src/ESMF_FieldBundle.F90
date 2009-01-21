! $Id: ESMF_FieldBundle.F90,v 1.1.2.13 2009/01/21 21:25:20 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_FieldBundle.F90"
!
!     ESMF FieldBundle Module
      module ESMF_FieldBundleMod 
!
!==============================================================================
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: ESMF_FieldBundleMod
!
! !DESCRIPTION:
! The code in this file implements the {\tt ESMF\_FieldBundle} class, which 
! represents a set of {\tt ESMF\_Fields} discretized on the same 
! {\tt ESMF\_Grid}.  {\tt ESMF\_FieldBundle}s offer the option to pack the data 
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
      use ESMF_FieldMod
      use ESMF_FieldCreateMod
      use ESMF_FieldGetMod
      use ESMF_InitMacrosMod
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!! ESMF_PackFlag   
!!
!! Data type to set the status of data in this FieldBundle; it can either be
!! simply a collection of Fields which contain the data, or it can also 
!! have a private packed data buffer associated directly with the FieldBundle.

      type ESMF_PackFlag
      sequence
      !private
        integer :: packflag
      end type

      type(ESMF_PackFlag), parameter :: ESMF_PACKED_DATA = ESMF_PackFlag(1), &
                                        ESMF_NO_PACKED_DATA = ESMF_PackFlag(2)

!------------------------------------------------------------------------------
!! For bookkeeping information which must be identical in each constituent
!! Field in order to optimize some of the communications calls.

      type ESMF_FieldBundleCongrntData
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
!! ESMF_LocalFieldBundle
!
      type ESMF_LocalFieldBundle
      sequence
      !private
        type(ESMF_InternArray) :: packed_data               ! local packed array
        type(ESMF_Status) :: gridstatus
        type(ESMF_Status) :: arraystatus
        integer :: accesscount
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!!     ESMF_FieldBundleType
!
      type ESMF_FieldBundleType
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
        type(ESMF_LocalFieldBundle) :: localbundle    ! this differs per DE
        type(ESMF_Packflag) :: pack_flag         ! is packed data present?
        type(ESMF_IOSpec) :: iospec              ! iospec values
        type(ESMF_Status) :: iostatus            ! if unset, inherit from gcomp
        logical :: isCongruent                   ! are all fields identical?
        logical :: hasPattern                    ! first data field sets this
        logical :: is_proxy                      ! true if this is a proxy FB
        !type(ESMF_FieldBundleCongrntData) :: pattern ! what they must match
        integer :: field_count      
        ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!! ESMF_FieldBundle

!! The FieldBundle data structure that is passed between implementation and
!! calling languages.

      type ESMF_FieldBundle
      sequence
      !private
      type (ESMF_FieldBundleType), pointer :: btypep 

      ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
      public ESMF_FieldBundle, ESMF_PackFlag, ESMF_PACKED_DATA, ESMF_NO_PACKED_DATA

      ! intended for internal ESMF use only but public for FieldBundleComms
      public ESMF_FieldBundleType           ! internal ESMF use only, for FieldBundleComm
      public ESMF_LocalFieldBundle          ! internal ESMF use only, for FieldBundleComm


! !PUBLIC MEMBER FUNCTIONS:
!

       ! public ESMF_FieldBundleCongDataVdate  ! For Standardized Initialization
       ! ESMF_FieldBundleCongrntData(Init) and (GetInit) are private

       public ESMF_LocalFieldBundleInit     ! For Standardized Initialization
       public ESMF_LocalFieldBundleValidate ! For Standardized Initialization
       public ESMF_LocalFieldBundleGetInit  ! For Standardized Initialization

       public ESMF_FieldBundleTypeInit      ! For Standardized Initialization
       public ESMF_FieldBundleTypeValidate  ! For Standardized Initialization
       public ESMF_FieldBundleTypeGetInit   ! For Standardized Initialization

       public ESMF_FieldBundleGetInit       ! For Standardized Initialization

       public ESMF_FieldBundleCreate       ! Create a new FieldBundle
       public ESMF_FieldBundleDestroy      ! Destroy a FieldBundle

       public ESMF_FieldBundleGet          ! Get FieldBundle information
       public ESMF_FieldBundleAdd          ! Add field/fields to FieldBundle

!      public ESMF_FieldBundleRemoveField   ! Delete one or more Fields by name or number

      public ESMF_FieldBundleSetGrid           ! In empty FieldBundle, set Grid

      public ESMF_FieldBundleIsCongruent        ! private to framework

   ! These are the recommended entry points; the code itself is in Array:
   !public ESMF_FieldBundleRedist   ! Redistribute existing arrays, matching Grids
   !public ESMF_FieldBundleHalo     ! Halo updates

   !public ESMF_FieldBundleGather   ! Combine 1 decomposed bundle into 1 on 1 DE
   !public ESMF_FieldBundleAllGather! Combine 1 decomposed bundle into N copies on N DEs

   !public ESMF_FieldBundleScatter  ! Split 1 bundle into a decomposed one over N DEs
   !public ESMF_FieldBundleBroadcast! Send 1 bundle to all DEs, none decomposed
   !public ESMF_FieldBundleAlltoAll ! might make sense with bundles; each DE could
                              ! call with a different non-decomposed bundle 
                              ! and the result would be a packed bundle of
                              ! data with decomposed bundles on each DE.

   !public ESMF_FieldBundleReduce     ! Global reduction operation, return on 1 DE    
   !public ESMF_FieldBundleAllReduce  ! Global reduction operation, return on each DE


    public ESMF_FieldBundleSerialize    ! Convert to byte stream...
    public ESMF_FieldBundleDeserialize  ! ... and back into an object again
    public ESMF_FieldBundleValidate     ! Check internal consistency
    public ESMF_FieldBundlePrint        ! Print contents of a FieldBundle

    public ESMF_FieldBundleGetI4Attr
    public ESMF_FieldBundleGetI4ListAttr
    public ESMF_FieldBundleGetI8Attr
    public ESMF_FieldBundleGetI8ListAttr
    public ESMF_FieldBundleGetR4Attr
    public ESMF_FieldBundleGetR4ListAttr
    public ESMF_FieldBundleGetR8Attr
    public ESMF_FieldBundleGetR8ListAttr
    public ESMF_FieldBundleGetLogAttr
    public ESMF_FieldBundleGetLogListAttr
    public ESMF_FieldBundleGetCharAttr

    public ESMF_FieldBundleGetAttByName
    public ESMF_FieldBundleGetAttByNum
    public ESMF_FieldBundleGetAttCount

    public ESMF_FieldBundleSetI4Attr
    public ESMF_FieldBundleSetI4ListAttr
    public ESMF_FieldBundleSetI8Attr
    public ESMF_FieldBundleSetI8ListAttr
    public ESMF_FieldBundleSetR4Attr
    public ESMF_FieldBundleSetR4ListAttr
    public ESMF_FieldBundleSetR8Attr
    public ESMF_FieldBundleSetR8ListAttr
    public ESMF_FieldBundleSetLogAttr
    public ESMF_FieldBundleSetLogListAttr
    public ESMF_FieldBundleSetCharAttr

    public operator(.eq.), operator(.ne.)

!  !subroutine ESMF_FieldBundleWriteRestart(bundle, iospec, rc)
!  !function ESMF_FieldBundleReadRestart(name, iospec, rc)
!  !subroutine ESMF_FieldBundleWrite(bundle, subarray, iospec, rc)
!  !function ESMF_FieldBundleRead(name, iospec, rc)

! !PRIVATE MEMBER FUNCTIONS:
!  !additional future signatures of ESMF_FieldBundleCreate() functions:
!  !function ESMF_FieldBundleCreateCopy(bundle, subarray, name, packflag, rc)
!  !function ESMF_FieldBundleCreateRemap(bundle, grid, name, packflag, rc)

!EOPI


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleCreate - Create a new FieldBundle
!
! !INTERFACE:
     interface ESMF_FieldBundleCreate

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleCreateNew
        module procedure ESMF_FieldBundleCreateNoFields

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_FieldBundleCreate} functions.
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleConstruct - Construct the internals of a new FieldBundle
!
! !INTERFACE:
     interface ESMF_FieldBundleConstruct

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleConstructNew
        module procedure ESMF_FieldBundleConstructEmpty

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_FieldBundleConstruct} functions.
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleGet - Get information from a FieldBundle
!
! !INTERFACE:
     interface ESMF_FieldBundleGet

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleGetInfo
        module procedure ESMF_FieldBundleGetFieldNames
        module procedure ESMF_FieldBundleGetFieldByName
        module procedure ESMF_FieldBundleGetFieldByNum

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_FieldBundleGet} functions.
!EOPI
      end interface

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_FieldBundleAdd - Add Fields to a FieldBundle
!
! !INTERFACE:
     interface ESMF_FieldBundleAdd

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_FieldBundleAddOneField
        module procedure ESMF_FieldBundleAddFieldList

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of {\tt ESMF\_FieldBundleAdd} functions.
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
#define ESMF_METHOD "ESMF_FieldBundleAddOneField"
!BOP
! !IROUTINE: ESMF_FieldBundleAdd - Add a Field to a FieldBundle
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAdd()
      subroutine ESMF_FieldBundleAddOneField(bundle, field, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
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
!           The {\tt ESMF\_FieldBundle} to add the {\tt ESMF\_Field} to.
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
      type(ESMF_FieldBundleType), pointer :: btype

      ! Initialize return code in case we return early.
      ! Otherwise, count on AddFieldList call to set rc
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)
      ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc)

      temp_list(1) = field

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep
    
      call ESMF_FieldBundleTypeAddList(btype, 1, temp_list, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      ! this resets the congruent flag as a side effect
      dummy = ESMF_FieldBundleIsCongruent(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_FieldBundleAddOneField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleAddFieldList"
!BOP
! !IROUTINE: ESMF_FieldBundleAdd - Add a list of Fields to a FieldBundle
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleAdd()
      subroutine ESMF_FieldBundleAddFieldList(bundle, fieldCount, fieldList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle        
      integer, intent(in) :: fieldCount
      type(ESMF_Field), dimension(:), intent(inout) :: fieldList
      integer, intent(out), optional :: rc          
!
! !DESCRIPTION:
!      Adds a {\tt fieldList} to an existing {\tt ESMF\_FieldBundle}.  
!      The items added from the {\tt ESMF\_fieldList} must be associated 
!      with the same {\tt ESMF\_Grid} as the other {\tt ESMF\_Field}s in the 
!      {\tt bundle}.  The items in the {\tt fieldList} are referenced by
!      the {\tt bundle}, not copied.  
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt ESMF\_FieldBundle} to add {\tt ESMF\_Field}s to.
!     \item [fieldCount]
!           Number of {\tt ESMF\_Field}s to be added to the 
!           {\tt ESMF\_FieldBundle}; must be equal to or less than the 
!           number of items in the {\tt fieldList}.
!     \item [fieldList]
!           Array of existing {\tt ESMF\_Field}s.  The first {\tt fieldCount}
!           items will be added to the {\tt ESMF\_FieldBundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
! 
!EOP

      integer :: status                                ! Error status
      logical :: dummy
      type(ESMF_FieldBundleType), pointer :: btype
      integer :: i

      ! Initialize return code in case we return early.
      ! Otherwise, count on AddFieldList call to set rc
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)
      do i=1,fieldCount
         ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,fieldList(i),rc)
      enddo

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep
    
      call ESMF_FieldBundleTypeAddList(btype, fieldCount, fieldList, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return
      
      ! this resets the congruent flag as a side effect
      dummy = ESMF_FieldBundleIsCongruent(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_FieldBundleAddFieldList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleCreateNew"
!BOP
! !IROUTINE: ESMF_FieldBundleCreate - Create a FieldBundle from existing Fields
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleCreate()
      function ESMF_FieldBundleCreateNew(fieldCount, fieldList, &
                                    packflag, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_FieldBundle) :: ESMF_FieldBundleCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: fieldCount           
      type(ESMF_Field), dimension (:) :: fieldList
      type(ESMF_PackFlag), intent(in), optional :: packflag 
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!   Creates an {\tt ESMF\_FieldBundle} from a list of existing
!   {\tt ESMF\_Fields} stored in a {\tt fieldList}.  All items in 
!   the {\tt fieldList} must be associated with the same 
!   {\tt ESMF\_Grid}.  Returns a new {\tt ESMF\_FieldBundle}.
!
!   The arguments are:
!   \begin{description}
!   \item [fieldCount]
!      Number of fields to be added to the new {\tt ESMF\_FieldBundle}.
!      Must be equal to or less than the number of 
!      {\tt ESMF\_Field}s in the {\tt fieldList}.
!   \item [fieldList]
!      Array of existing {\tt ESMF\_Field}s.  The first {\tt ESMF\_FieldCount}
!      items will be added to the new {\tt ESMF\_FieldBundle}.
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
!   \item [{[name]}]
!      {\tt ESMF\_FieldBundle} name.  A default name is generated if
!      one is not specified.
!   \item [{[iospec]}]
!      The {\tt ESMF\_IOSpec} is not yet used by {\tt ESMF\_FieldBundle}s.  Any 
!      values passed in will be ignored.
!   \item [{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP

      type(ESMF_FieldBundleType), pointer :: btypep         ! Pointer to new bundle
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
      nullify(ESMF_FieldBundleCreateNew%btypep)

      allocate(btypep,  stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "FieldBundle allocate", &
                                       ESMF_CONTEXT, rc)) return

      ! Call construction method to initialize bundle internals.
      call ESMF_FieldBundleConstructNew(btypep, fieldCount, fieldList, &
                                   packflag, &
                                   name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) then
          deallocate(btypep, stat=status)
          return
      endif

      ! set the return bundle
      ESMF_FieldBundleCreateNew%btypep => btypep


      ! do this before ESMF_FieldBundleIsCongruent so it doesn't complain
      ! about uninitialized bundles
      ESMF_INIT_SET_CREATED(ESMF_FieldBundleCreateNew)


      ! this resets the congruent flag as a side effect
      dummy = ESMF_FieldBundleIsCongruent(ESMF_FieldBundleCreateNew, rc)


      ! Set return values.
      if (present(rc)) rc = ESMF_SUCCESS


      end function ESMF_FieldBundleCreateNew


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleCreateNoFields"
!BOP
! !IROUTINE: ESMF_FieldBundleCreate - Create a FieldBundle with no Fields
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleCreate()
      function ESMF_FieldBundleCreateNoFields(grid, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_FieldBundle) :: ESMF_FieldBundleCreateNoFields
!
! !ARGUMENTS:
      type(ESMF_Grid), intent(in), optional :: grid
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!   Creates an {\tt ESMF\_FieldBundle} with no associated {\tt ESMF\_Fields}.
!
!   The arguments are:
!   \begin{description}
!   \item [{[grid]}]
!       The {\tt ESMF\_Grid} which all {\tt ESMF\_Field}s added to this
!       {\tt ESMF\_FieldBundle} must be associated with.  If not specified now, the 
!       grid associated with the first {\tt ESMF\_Field} added will be
!       used as the reference grid for the {\tt ESMF\_FieldBundle}.
!   \item [{[name]}]
!       {\tt ESMF\_FieldBundle} name.  A default name is generated if
!       one is not specified.
!   \item [{[iospec]}]
!       The {\tt ESMF\_IOSpec} is not yet used by {\tt ESMF\_FieldBundle}s.  Any 
!       values passed in will be ignored.
!   \item [{[rc]}]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP


      type(ESMF_FieldBundleType), pointer :: btypep   ! Pointer to new bundle
      integer :: status                          ! Error status

      ! Initialize pointers
      status = ESMF_RC_NOT_IMPL
      nullify(btypep)
      nullify(ESMF_FieldBundleCreateNoFields%btypep)

      ! Initialize return code
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check inputs 
      ESMF_INIT_CHECK_DEEP(ESMF_GridGetInit,grid,rc)

      allocate(btypep, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "FieldBundle allocate", &
                                       ESMF_CONTEXT, rc)) return

      ! Call construction method to allocate and initialize bundle internals.
      call ESMF_FieldBundleConstructEmpty(btypep, name, iospec, rc)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! If specified, set the Grid.  All Fields added to this FieldBundle
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
      ESMF_FieldBundleCreateNoFields%btypep => btypep
      ESMF_INIT_SET_CREATED(ESMF_FieldBundleCreateNoFields)

      if (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldBundleCreateNoFields


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleDestroy"
!BOP
! !IROUTINE: ESMF_FieldBundleDestroy - Free all resources associated with a FieldBundle
!
! !INTERFACE:
      subroutine ESMF_FieldBundleDestroy(bundle, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle) :: bundle
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
!           An {\tt ESMF\_FieldBundle} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      ! Local variables
      integer :: status                           ! Error status
      type(ESMF_FieldBundleType), pointer :: btype

      ! Initialize return code
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check inputs 
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)


      ! If already destroyed or never created, return ok
      btype => bundle%btypep
      if (.not. associated(btype)) then
        if (present(rc)) rc = ESMF_FAILURE   ! should this really be an error?
        return
      endif

      ! Destruct all bundle internals and then free field memory.
      call ESMF_FieldBundleDestruct(btype, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      deallocate(bundle%btypep, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "FieldBundle deallocate", &
                                       ESMF_CONTEXT, rc)) return
      nullify(bundle%btypep)
      ESMF_INIT_SET_DELETED(bundle)

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_FieldBundleDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetInfo"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Return information about a FieldBundle
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGet()
      subroutine ESMF_FieldBundleGetInfo(bundle, grid, fieldCount, name, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
      type(ESMF_Grid), intent(out), optional :: grid
      integer, intent(out), optional :: fieldCount
      character (len = *), intent(out), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns information about the {\tt bundle}.  
!      If the {\tt ESMF\_FieldBundle} was originally created
!      without specifying a name, a unique name will have been generated
!      by the framework.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_FieldBundle} object to query.
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
      type(ESMF_FieldBundleType), pointer :: btype     ! internal data

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


     ! Set initialize fieldCount to 0
      if (present(fieldCount)) then
          fieldCount = 0
      endif

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      if (present(grid)) then
          ! Check to be sure bundle has grid before trying to return it.
          if (btype%gridstatus .ne. ESMF_STATUS_READY) then
               if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "FieldBundle does not contain a Grid", &
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
      end subroutine ESMF_FieldBundleGetInfo

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetAllFields"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAllFields - Retrieve an array of Fields 
!
! !INTERFACE:
      subroutine ESMF_FieldBundleGetAllFields(bundle, fieldList, fieldCount, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
      type(ESMF_Field), dimension (:), optional :: fieldList
      integer, intent(out), optional :: fieldCount
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return all {\tt ESMF\_Field}s in an {\tt ESMF\_FieldBundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_FieldBundle} to query for the {\tt ESMF\_Field}s.
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
      type(ESMF_FieldBundleType), pointer :: btype     ! internal data
      integer :: nitems                           ! items in return array

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)


      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      ! Return Fields
      if (present(fieldList)) then
          nitems = size(fieldList)
          if (nitems .lt. btype%field_count) then
              if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                       "More Fields in FieldBundle than space in fieldList array", &
                                        ESMF_CONTEXT, rc)) return
          endif

          fieldList(1:btype%field_count) = btype%flist(1:btype%field_count)
      endif

      ! Return Field count
      if (present(fieldCount)) then
          fieldCount = btype%field_count
      endif


      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_FieldBundleGetAllFields

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetI4Attr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetI4Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetI4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetI4ListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetI4ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetI4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetI8Attr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetI8Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetI8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetI8ListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetI8ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetI8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetR4Attr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetR4Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetR4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetR4ListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetR4ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetR4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetR8Attr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetR8Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetR8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetR8ListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetR8ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetR8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetLogAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetLogAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetLogAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetLogListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetLogListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetLogListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetCharAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAtt - Retrieve a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAtt()
      subroutine ESMF_FieldBundleGetCharAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetChar(bundle%btypep%base, name, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetCharAttr


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetAttCount"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAttCount - Query the number of attributes
!
! !INTERFACE:
      subroutine ESMF_FieldBundleGetAttCount(bundle, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
!     \item [count]
!           The number of attributes associated with this object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetCount(bundle%btypep%base, count, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetAttCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetAttByName"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAttInfo - Query FieldBundle attributes by name
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAttInfo()
      subroutine ESMF_FieldBundleGetAttByName(bundle, name, typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(in) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
!EOPI

      integer :: status                           ! Error status
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetAttrInfoName(bundle%btypep%base, name, &
        localTk, localCount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetAttByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetAttByNum"
!BOPI
! !IROUTINE: ESMF_FieldBundleGetAttInfo - Query FieldBundle attributes by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGetAttInfo()
      subroutine ESMF_FieldBundleGetAttByNum(bundle, attributeIndex, name, &
        typekind, count, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
      integer, intent(in) :: attributeIndex
      character(len=*), intent(out) :: name
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
!           An {\tt ESMF\_FieldBundle} object.
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
!EOPI

      integer :: status                           ! Error status
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_TypeKind) :: localTk
      integer :: localCount

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttGetAttrInfoNum(bundle%btypep%base, attributeIndex, &
        localName, localTk, localCount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      name = localName
      if (present(typekind)) typekind = localTk
      if (present(count)) count = localCount

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetAttByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleIsCongruent"
!BOPI
! !IROUTINE: ESMF_FieldBundleIsCongruent - Is data in FieldBundle the same?
!
! !INTERFACE:
      function ESMF_FieldBundleIsCongruent(bundle, rc)

! !RETURN VALUE:
      logical :: ESMF_FieldBundleIsCongruent
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
      integer, intent(out), optional :: rc   
!
! !DESCRIPTION:
!      Returns {\tt .TRUE.} if the data in all {\tt ESMF\_Fields} in the
!      {\tt bundle} are completely congruent, meaning they have the same
!      data rank, type, kind, index ordering, relative location in a cell, etc.
!      This may allow more optimized communication by grouping data together
!      and making fewer communcations calls.  Returns {\tt .FALSE.} if the
!      data is not congruent.   A {\tt ESMF\_FieldBundle} with no data, or on error
!      this routine returns {\tt .FALSE.}.
!
!      This routine also resets the internal bundle flag to a known state
!      before returning.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_FieldBundle} object to query.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP

      integer :: status                            ! Error status
      integer :: i, newstart
      type(ESMF_FieldBundleType), pointer :: btype      ! internal data
      type(ESMF_FieldBundleCongrntData) :: pattern    ! values to compare against
      type(ESMF_FieldBundleCongrntData) :: candidate  ! values being compared
      type(ESMF_Field), pointer :: fieldp
      type(ESMF_InternArray) :: array
!      type(ESMF_FieldDataMap) :: datamap

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)


! TODO:FIELDINTEGRATION Restore FieldBundleIsCongruent
#if 0
      ESMF_FieldBundleIsCongruent = .FALSE.
      bundle%btypep%isCongruent = .FALSE.

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
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
      ESMF_FieldBundleIsCongruent = .TRUE.
      btype%isCongruent = .TRUE.
#endif

      if (present(rc)) rc = ESMF_SUCCESS


      end function ESMF_FieldBundleIsCongruent


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetFieldByName"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Retrieve a Field by name
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGet()
      subroutine ESMF_FieldBundleGetFieldByName(bundle, name, field, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
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
!           {\tt ESMF\_FieldBundle} to query for {\tt ESMF\_Field}.
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
      type(ESMF_FieldBundleType), pointer :: btype

      ! Initialize return code.  Assume routine not implemented.
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      found = .FALSE.

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      ! Check for an empty FieldBundle first
      if(btype%field_count .eq. 0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Empty FieldBundle", &
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

      end subroutine ESMF_FieldBundleGetFieldByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetFieldByNum"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Retrieve a Field by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGet()
      subroutine ESMF_FieldBundleGetFieldByNum(bundle, fieldIndex, field, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
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
!           {\tt ESMF\_FieldBundle} to query for {\tt ESMF\_Field}.
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
      type(ESMF_FieldBundleType), pointer :: btype

      ! Initialize return code.  Assume routine not implemented.
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      found = .FALSE.

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep

      ! Check for an empty FieldBundle first
      if(btype%field_count .eq. 0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Empty FieldBundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Check for out of range index number
      if ((fieldIndex .lt. 1) .or. (fieldIndex .gt. btype%field_count)) then
        ! "ERROR in ESMF_FieldBundleGetField: fieldIndex ", fieldIndex, &
        !                "out of range. Min=1, max=", btype%field_count
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Index out of range", &
                                 ESMF_CONTEXT, rc)) return
        return
      endif

      ! Fetch requested field
      field = bundle%btypep%flist(fieldIndex) 

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleGetFieldByNum


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetFieldNames"
!BOP
! !IROUTINE: ESMF_FieldBundleGet - Return all Field names in a FieldBundle

! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleGet()
      subroutine ESMF_FieldBundleGetFieldNames(bundle, nameList, nameCount, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle 
      character (len = *), intent(out) :: nameList(:)
      integer, intent(out), optional :: nameCount     
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Returns an array of {\tt ESMF\_Field} names in an {\tt ESMF\_FieldBundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [nameList]
!           An array of character strings where each {\tt ESMF\_Field} name
!           is returned.  Must be at least as long as {\tt nameCount}.
!     \item [{[nameCount]}]
!           A count of how many {\tt ESMF\_Field} names were returned.  Same as
!           the number of {\tt ESMF\_Field}s in the {\tt ESMF\_FieldBundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP

      integer :: i, status
      type(ESMF_FieldBundleType), pointer :: bp

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

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
      end subroutine ESMF_FieldBundleGetFieldNames


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundlePrint"
!BOP
! !IROUTINE: ESMF_FieldBundlePrint - Print information about a FieldBundle
!
! !INTERFACE:
      subroutine ESMF_FieldBundlePrint(bundle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Prints diagnostic information about the {\tt bundle}
!     to {\tt stdout}.  
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_FieldBundle} object.
!     \item [{[options]}]
!           Print options are not yet supported.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP


      character(len=ESMF_MAXSTR) :: bname, fname
      !character(len=ESMF_MAXSTR) :: msgbuf
      type(ESMF_FieldBundleType), pointer :: btype
      !type(ESMF_Field) :: field
      integer :: i
      integer :: status
      character(len=6) :: defaultopts

       ! print option is not implemented, but it has to pass to c_ESMC_BasePrint()
      defaultopts = "brief"

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

    !jw  call ESMF_LogWrite("FieldBundle Print:", ESMF_LOG_INFO)
      write (*, *)  "FieldBundle print:"

      if (.not. associated(bundle%btypep)) then
      !jw  call ESMF_LogWrite("Empty or Uninitialized FieldBundle", ESMF_LOG_INFO)
        write(*,*) "Empty or Uninitialized FieldBundle"
        if (present(rc)) rc = ESMF_SUCCESS
        return
      endif

      btype => bundle%btypep
      call c_ESMC_GetName(btype%base, bname, status)
    !jw  write (msgbuf, *)  "  FieldBundle name = ", trim(bname)
    !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write (*, *)  "  FieldBundle name = ", trim(bname)

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
       !call ESMF_FieldPrint(btype%flist(i),rc=status)  
       !call ESMF_FieldGet(btype%flist(i), name=fname, rc=status)
       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

     !jw  write (msgbuf, *)  "    Field", i, "name = ", trim(fname)
     !jw  call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      enddo

      ! TODO: add more code here for printing more info

      if (present(rc)) rc = ESMF_SUCCESS
      end subroutine ESMF_FieldBundlePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRead"
!BOPI
! !IROUTINE: ESMF_FieldBundleRead - Create a FieldBundle from an external source
!
! !INTERFACE:
      function ESMF_FieldBundleRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_FieldBundle) :: ESMF_FieldBundleRead
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
!           An {\tt ESMF\_FieldBundle} object.
!     \item [{[iospec]}]
!           The file I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

!EOPI

!
!  TODO: code goes here
!
      type(ESMF_FieldBundle) :: b

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      allocate(b%btypep)

      b%btypep%bundlestatus = ESMF_STATUS_UNINIT

      ESMF_FieldBundleRead = b

      end function ESMF_FieldBundleRead

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleReadRestart"
!BOPI
! !IROUTINE: ESMF_FieldBundleReadRestart - Read back a saved FieldBundle
!
! !INTERFACE:
      function ESMF_FieldBundleReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_FieldBundle) :: ESMF_FieldBundleReadRestart
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name     
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a {\t ESMF\_FieldBundle} 
!      from the last call to WriteRestart.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_FieldBundle} object.
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
      type(ESMF_FieldBundle) :: b

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      allocate(b%btypep)

      b%btypep%bundlestatus = ESMF_STATUS_UNINIT

      ESMF_FieldBundleReadRestart = b

      end function ESMF_FieldBundleReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleRemoveField"
!BOPI
! !IROUTINE: ESMF_FieldBundleRemoveField - Remove a Field from a FieldBundle
!
! !INTERFACE:
      subroutine ESMF_FieldBundleRemoveField(bundle, name, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
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
!           The {\tt ESMF\_FieldBundle} to remove the {\tt ESMF\_Field} from.
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

      end subroutine ESMF_FieldBundleRemoveField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetI4Attr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set a 4-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetI4Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetI4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetI4ListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set a 4-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetI4ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I4, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetI4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetI8Attr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set an 8-byte integer attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetI8Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetI8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetI8ListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set an 8-byte integer list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetI8ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_I8, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetI8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetR4Attr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set a 4-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetR4Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetR4Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetR8Attr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set an 8-byte real attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetR8Attr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetR8Attr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetR4ListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set a 4-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetR4ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R4, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetR4ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetR8ListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set an 8-byte real list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetR8ListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_R8, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetR8ListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetLogAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set a logical attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetLogAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetLogAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetLogListAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set a logical list attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetLogListAttr(bundle, name, count, valueList, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      limit = size(valueList)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than valueList", &
                                 ESMF_CONTEXT, rc)) return
      endif

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetValue(bundle%btypep%base, name, &
        ESMF_TYPEKIND_LOGICAL, count, valueList, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetLogListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetCharAttr"
!BOPI
! !IROUTINE: ESMF_FieldBundleSetAtt - Set a character attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_FieldBundleSetAtt()
      subroutine ESMF_FieldBundleSetCharAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle  
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
!           An {\tt ESMF\_FieldBundle} object.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      call c_ESMC_BaseAttSetChar(bundle%btypep%base, name, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSetCharAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSetGrid"
!BOP
! !IROUTINE: ESMF_FieldBundleSetGrid - Associate a Grid with an empty FieldBundle
! 
! !INTERFACE:
      subroutine ESMF_FieldBundleSetGrid(bundle, grid, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
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
!        An {\tt ESMF\_FieldBundle} object.
!   \item [grid]
!        The {\tt ESMF\_Grid} which all {\tt ESMF\_Field}s added to this
!        {\tt ESMF\_FieldBundle} must have.
!   \item [{[rc]}]
!         Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP


      integer :: status                           ! Error status
      type(ESMF_FieldBundleType), pointer :: btype     ! internal data

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check variables
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      ! Validate bundle before going further
      call ESMF_FieldBundleValidate(bundle, rc=status)
      if (ESMF_LogMsgFoundError(status, &
                                ESMF_ERR_PASSTHRU, &
                                ESMF_CONTEXT, rc)) return

      btype => bundle%btypep
   
      ! here we will only let someone associate a grid with a bundle
      ! if there is not one already associated with it.  
      if (btype%gridstatus .eq. ESMF_STATUS_READY) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "FieldBundle is already associated with a Grid", &
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

      end subroutine ESMF_FieldBundleSetGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleValidate"
!BOP
! !IROUTINE: ESMF_FieldBundleValidate - Check validity of a FieldBundle
!
! !INTERFACE:
      subroutine ESMF_FieldBundleValidate(bundle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(in) :: bundle
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
!           {\tt ESMF\_FieldBundle} to validate.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)

      if (.not.associated(bundle%btypep)) then 
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed FieldBundle", &
                                 ESMF_CONTEXT, rc)) return
      endif 

      if (bundle%btypep%bundlestatus .ne. ESMF_STATUS_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed FieldBundle", &
                                 ESMF_CONTEXT, rc)) return
      endif 

      ! TODO: add more code here

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleWrite"
!BOPI
! !IROUTINE: ESMF_FieldBundleWrite - Save a FieldBundle to an external destination
!
! !INTERFACE:
      subroutine ESMF_FieldBundleWrite(bundle, subarray, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle
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
!           An {\tt ESMF\_FieldBundle} object.
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
      end subroutine ESMF_FieldBundleWrite


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleWriteRestart"
!BOPI
! !IROUTINE: ESMF_FieldBundleWriteRestart - Save FieldBundle in the quickest manner possible
!
! !INTERFACE:
      subroutine ESMF_FieldBundleWriteRestart(bundle, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle 
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
!           An {\tt ESMF\_FieldBundle} object.
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
      end subroutine ESMF_FieldBundleWriteRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleTypeAddList"
!BOPI
! !IROUTINE: ESMF_FieldBundleTypeAddList - Add a list of Fields to a FieldBundle.
!
! !INTERFACE:
      subroutine ESMF_FieldBundleTypeAddList(btype, fieldCount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundleType), pointer :: btype        
      integer, intent(in) :: fieldCount
      type(ESMF_Field), dimension(:), intent(inout) :: fields
      integer, intent(out), optional :: rc          
!
! !DESCRIPTION:
!  Add a Field reference to an existing {\tt ESMF\_FieldBundle}.  
!  The {\tt ESMF\_Field} must have the
!  same {\tt ESMF\_Grid} as the rest of the 
!  {\tt ESMF\_Field}s in the {\tt ESMF\_FieldBundle}.
!  If the {\tt ESMF\_FieldBundle} has
!  packed data this will mean making a copy of the data.
!  Note: packed data is currently not supported. 
!
!  The arguments are:
!  \begin{description}
!  \item [btype]
!        {\tt ESMF\_FieldBundleType} to add {\tt ESMF\_Field}s into.
!  \item [fieldCount]
!        Number of fields to be added to the {\tt ESMF\_FieldBundle}.
!        Must be equal to or less than the number of 
!        {\tt ESMF\_Field}s in the following argument.
!  \item [fields]
!        Array of existing {\tt ESMF\_Field}s.  The first {\tt fieldCount}
!        items will be added to the {\tt ESMF\_FieldBundle}.
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
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleTypeGetInit,btype,rc)
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
                        "Invalid Field found when trying to add into FieldBundle", &
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
!                   "Fields with inconsistent Grids cannot be added to FieldBundle", &
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

      !   call ESMF_FieldBundleTypeRepackData(btype, rc=status)
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

      end subroutine ESMF_FieldBundleTypeAddList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleConstructNew"
!BOPI
! !IROUTINE: ESMF_FieldBundleConstructNew - Construct the internals of a FieldBundle
!
! !INTERFACE:
      subroutine ESMF_FieldBundleConstructNew(btype, fieldCount, fields, &
                                         packflag, &
                                         name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundleType), pointer :: btype 
      integer, intent(in) :: fieldCount           
      type(ESMF_Field), dimension (:) :: fields
      type(ESMF_PackFlag), intent(in), optional :: packflag 
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Constructs an {\tt ESMF\_FieldBundle} from a list of existing
!   gridded {\tt ESMF\_Fields}.  This routine requires an existing
!   {\tt ESMF\_FieldBundle} type as an input and fills in
!   the internals.  {\tt ESMF\_FieldBundleCreateNew()} does
!   the allocation of an {\tt ESMF\_FieldBundle} type first and then
!   calls this routine.
!
!   The arguments are:
!   \begin{description}
!   \item [btype]
!      Pointer to an {\tt ESMF\_FieldBundle} object.
!   \item [fieldCount]
!      Number of fields to be added to the {\tt ESMF\_FieldBundle}.
!      Must be equal to or less than the number of
!      {\tt ESMF\_Field}s in the following argument.
!   \item [fields]
!      Array of existing {\tt ESMF\_Field}s.  The first {\tt fieldCount}
!      items will be added to the {\tt ESMF\_FieldBundle}.
!   \item [{[packflag]}]
!      If set to {\tt ESMF\_PACK\_FIELD\_DATA}, the {\tt ESMF\_Field}
!      data in individual {\tt ESMF\_Array}s will be collected
!      into a single data {\tt ESMF\_Array} for the entire {\tt ESMF\_FieldBundle}.
!      The default is {\tt ESMF\_NO\_PACKED\_DATA}.
!   \item [{[name]}]
!      {\tt ESMF\_FieldBundle} name.  A default name will be generated if
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
      call ESMF_FieldBundleConstructEmpty(btype, name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! If specified, set packflag
      if(present(packflag)) then
        if(packflag.eq.ESMF_PACKED_DATA) then
          call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, &
                                 "Packed data option not implemented", &
                                 ESMF_CONTEXT, rc) 
          return
        else
          btype%pack_flag = packflag
        endif
      endif  
 
      ! Add the fields in the list, checking for consistency.
      call ESMF_FieldBundleTypeAddList(btype, fieldCount, fields, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return


      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleConstructNew


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleConstructEmpty"
!BOPI
! !IROUTINE: ESMF_FieldBundleConstructEmpty - Construct the internals of a FieldBundle
!
! !INTERFACE:
      subroutine ESMF_FieldBundleConstructEmpty(btype, name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundleType), pointer :: btype 
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Constructs the internals of an {\tt ESMF\_FieldBundle}, given an existing
!     {\tt ESMF\_FieldBundle} type as an input.
!
!     The arguments are:
!     \begin{description}
!     \item [btype]
!           An existing {\tt ESMF\_FieldBundle} to be initialized.
!     \item [{[name]}]
!           {\tt ESMF\_FieldBundle} name.  A default name will be generated if
!           one is not specified.
!     \item [{[iospec]}]
!           I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
      
      integer :: status                            ! Error status
      !character (len = ESMF_MAXSTR) :: defaultname ! FieldBundle name if not given

      ! Initialize return code.  Assume routine not implemented.
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

       ! Initialize the base object
      btype%base%this = ESMF_NULL_POINTER
      call ESMF_BaseCreate(btype%base, "FieldBundle", name, 0, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

   
      ! Initialize bundle contents.  An empty FieldBundle starts out with the
      ! status flags uninitialized, and assumes all data is congruent.
      ! As fields are added, the first non-compliant one turns the flag
      ! to false, and after it is false, there is no way to set it back
      ! to true.
      btype%localbundle%gridstatus = ESMF_STATUS_UNINIT
      btype%localbundle%arraystatus = ESMF_STATUS_UNINIT
      btype%gridstatus = ESMF_STATUS_UNINIT
      btype%isCongruent = .TRUE.
   
      btype%field_count = 0
      btype%is_proxy = .false.
      nullify(btype%flist)
      
      btype%pack_flag = ESMF_NO_PACKED_DATA
!     nullify(btype%localbundle%packed_data)
      btype%bundlestatus = ESMF_STATUS_READY
  

      ! Set as created 
      ESMF_INIT_SET_CREATED(btype)

      if (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleConstructEmpty


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleDestruct"
!BOPI
! !IROUTINE: ESMF_FieldBundleDestruct - Free contents of a FieldBundle 
!
! !INTERFACE:
      subroutine ESMF_FieldBundleDestruct(btype, rc)
!
! !ARGUMENTS:
      type(ESMF_FieldBundleType), pointer :: btype 
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!     Releases all resources except the {\tt ESMF\_FieldBundle} itself.
!
!     \begin{description}
!     \item [btype]
!           Pointer to an {\tt ESMF\_FieldBundle} object.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI

      integer :: status, i

      ! Initialize return code; assume routine not implemented
      status = ESMF_RC_NOT_IMPL
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

      ! check input variables
      !ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleTypeGetInit,btype,rc)

      btype%bundlestatus = ESMF_STATUS_INVALID
      call ESMF_BaseDestroy(btype%base, status)

      if(btype%is_proxy) then
          call ESMF_GridDestroy(btype%grid, rc=status)
          if (ESMF_LogMsgFoundError(status, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
          do i = 1, btype%field_count
            call ESMF_FieldDestroy(btype%flist(i), rc=status)
            if (ESMF_LogMsgFoundError(status, &
                ESMF_ERR_PASSTHRU, &
                ESMF_CONTEXT, rc)) return
          enddo
      endif

      if (associated(btype%flist)) then
          deallocate(btype%flist, stat=status)
          if (ESMF_LogMsgFoundAllocError(status, "FieldBundle deallocate", &
                                         ESMF_CONTEXT, rc)) return

      endif

      ! Set as deleted 
      ESMF_INIT_SET_DELETED(btype)

      if (present(rc)) rc = status


      end subroutine ESMF_FieldBundleDestruct

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleSerialize"

!BOPI
! !IROUTINE: ESMF_FieldBundleSerialize - Serialize bundle info into a byte stream
!
! !INTERFACE:
      subroutine ESMF_FieldBundleSerialize(bundle, buffer, length, offset, rc) 
!
! !ARGUMENTS:
      type(ESMF_FieldBundle), intent(inout) :: bundle 
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: length
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_FieldBundle} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldBundleWrite()} and {\tt ESMF\_FieldBundleRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt ESMF\_FieldBundle} object to be serialized.
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
      type(ESMF_FieldBundleType), pointer :: bp   ! bundle type

      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL

      ! check inputs
      ESMF_INIT_CHECK_DEEP(ESMF_FieldBundleGetInit,bundle,rc)      

      ! shortcut to internals
      bp => bundle%btypep

      call c_ESMC_BaseSerialize(bp%base, buffer(1), length, offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_FieldBundleSerialize(bp%bundlestatus, bp%gridstatus, &
                                 bp%iostatus, &
                                 bp%field_count, bp%pack_flag, &
                                 bp%isCongruent, bp%hasPattern, &
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

      if  (present(rc)) rc = ESMF_SUCCESS

      end subroutine ESMF_FieldBundleSerialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleDeserialize"

!BOPI
! !IROUTINE: ESMF_FieldBundleDeserialize - Deserialize a byte stream into a FieldBundle
!
! !INTERFACE:
      function ESMF_FieldBundleDeserialize(vm, buffer, offset, rc) 
!
! !RETURN VALUE:
      type(ESMF_FieldBundle) :: ESMF_FieldBundleDeserialize   
!
! !ARGUMENTS:
      type(ESMF_VM), intent(in) :: vm
      integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
      integer, intent(inout) :: offset
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a FieldBundle object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_FieldBundleWrite()} and {\tt ESMF\_FieldBundleRead()}.
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
      type(ESMF_FieldBundleType), pointer :: bp   ! bundle type


      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL
      localrc = ESMF_RC_NOT_IMPL
      status = ESMF_RC_NOT_IMPL

      ! in case of error, make sure this is invalid.
      nullify(ESMF_FieldBundleDeserialize%btypep)

      ! shortcut to internals
      allocate(bp, stat=status)
      if (ESMF_LogMsgFoundAllocError(status, &
                                     "space for new FieldBundle object", &
                                     ESMF_CONTEXT, rc)) return


      call ESMF_BaseCreate(bp%base, "FieldBundle", "dummy", 0, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      ! this overwrites the name and adds attributes to the base obj.
      call c_ESMC_BaseDeserialize(bp%base, buffer(1), offset, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
                                 ESMF_ERR_PASSTHRU, &
                                 ESMF_CONTEXT, rc)) return

      call c_ESMC_FieldBundleDeserialize(bp%bundlestatus, bp%gridstatus, &
                                 bp%iostatus, &
                                 bp%field_count, bp%pack_flag, &
                                 bp%isCongruent, bp%hasPattern, &
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
                                    ESMF_CONTEXT, rc)) then
              deallocate(bp%flist)
              return
          endif
      enddo

      bp%is_proxy = .true.

      ESMF_FieldBundleDeserialize%btypep => bp

      ! Set as created
      ESMF_INIT_SET_CREATED(ESMF_FieldBundleDeserialize)

      if  (present(rc)) rc = ESMF_SUCCESS

      end function ESMF_FieldBundleDeserialize

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleCongDataGetInit"
!BOPI
! !IROUTINE:  ESMF_FieldBundleCongDataGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_FieldBundleCongDataGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_FieldBundleCongrntData), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_FieldBundleCongDataGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt bundlecongruentdata}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_ESMF_FieldBundleCongrntData} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_FieldBundleCongDataGetInit = ESMF_INIT_GET(s)
       else
         ESMF_FieldBundleCongDataGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_FieldBundleCongDataGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleCongrntDataInit"
!BOPI
! !IROUTINE:  ESMF_FieldBundleCongrntDataInit - Initialize FieldBundleCongruentData

! !INTERFACE:
    subroutine ESMF_FieldBundleCongrntDataInit(s)
!
! !ARGUMENTS:
       type(ESMF_FieldBundleCongrntData) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt bundlecongruentdata}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldBundleCongruentData} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_FieldBundleCongrntDataInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleCongDataVdate"
!BOPI
! !IROUTINE:  ESMF_FieldBundleCongDataVdate - Check validity of a FieldBundleCongruentData

! !INTERFACE:
    subroutine ESMF_FieldBundleCongDataVdate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_FieldBundleCongrntData), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt FieldBundleCongruentData} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldBundleCongruentData} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_FieldBundleCongDataGetInit, ESMF_FieldBundleCongrntDataInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif 
    end subroutine ESMF_FieldBundleCongDataVdate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalFieldBundleGetInit"
!BOPI
! !IROUTINE:  ESMF_LocalFieldBundleGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_LocalFieldBundleGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_LocalFieldBundle), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_LocalFieldBundleGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt localbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalFieldBundle} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_LocalFieldBundleGetInit = ESMF_INIT_GET(s)
       else
         ESMF_LocalFieldBundleGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_LocalFieldBundleGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalFieldBundleInit"
!BOPI
! !IROUTINE:  ESMF_LocalFieldBundleInit - Initialize LocalFieldBundle

! !INTERFACE:
    subroutine ESMF_LocalFieldBundleInit(s)
!
! !ARGUMENTS:
       type(ESMF_LocalFieldBundle) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt localbundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalFieldBundle} of which being initialized.
!     \end{description}
!
!EOPI

       ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_LocalFieldBundleInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalFieldBundleValidate"
!BOPI
! !IROUTINE:  ESMF_LocalFieldBundleValidate - Check validity of a LocalFieldBundle

! !INTERFACE:
    subroutine ESMF_LocalFieldBundleValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_LocalFieldBundle), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt LocalFieldBundle} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_LocalFieldBundle} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL

     ESMF_INIT_CHECK_SHALLOW(ESMF_LocalFieldBundleGetInit,ESMF_LocalFieldBundleInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_LocalFieldBundleValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleTypeGetInit"
!BOPI
! !IROUTINE:  ESMF_FieldBundleTypeGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_FieldBundleTypeGetInit(s)
!
! !ARGUMENTS:
       type(ESMF_FieldBundleType), intent(in), optional :: s
       ESMF_INIT_TYPE :: ESMF_FieldBundleTypeGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the shallow class {\tt bundletype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldBundleType} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(s)) then
         ESMF_FieldBundleTypeGetInit = ESMF_INIT_GET(s)
       else
         ESMF_FieldBundleTypeGetInit = ESMF_INIT_DEFINED
       endif

    end function ESMF_FieldBundleTypeGetInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleTypeInit"
!BOPI
! !IROUTINE:  ESMF_FieldBundleTypeInit - Initialize FieldBundleType

! !INTERFACE:
    subroutine ESMF_FieldBundleTypeInit(s)
!
! !ARGUMENTS:
       type(ESMF_FieldBundleType) :: s
!
! !DESCRIPTION:
!      Initialize the shallow class {\tt bundletype}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldBundleType} of which being initialized.
!     \end{description}
!
!EOPI

        nullify(s%flist)

        ESMF_INIT_SET_DEFINED(s)
    end subroutine ESMF_FieldBundleTypeInit

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleTypeValidate"
!BOPI
! !IROUTINE:  ESMF_FieldBundleTypeValidate - Check validity of a FieldBundleType

! !INTERFACE:
    subroutine ESMF_FieldBundleTypeValidate(s,rc)
!
! !ARGUMENTS:
       type(ESMF_FieldBundleType), intent(inout) :: s
       integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt FieldBundleType} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldBundleType} to validate.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if the {\tt localfield}
!           is valid.
!     \end{description}
!
!EOPI
      ! Initialize return code; assume routine not implemented
      if (present(rc)) rc = ESMF_RC_NOT_IMPL


     ESMF_INIT_CHECK_SHALLOW(ESMF_FieldBundleTypeGetInit,ESMF_FieldBundleTypeInit,s)

     ! return success
     if(present(rc)) then
       rc = ESMF_SUCCESS
     endif
    end subroutine ESMF_FieldBundleTypeValidate

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_FieldBundleGetInit"
!BOPI
! !IROUTINE:  ESMF_FieldBundleGetInit - Get initialization status.

! !INTERFACE:
    function ESMF_FieldBundleGetInit(d)
!
! !ARGUMENTS:
       type(ESMF_FieldBundle), intent(in), optional :: d
       ESMF_INIT_TYPE :: ESMF_FieldBundleGetInit
!
! !DESCRIPTION:
!      Get the initialization status of the Deep class {\tt bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           {\tt ESMF\_FieldBundle} from which to retreive status.
!     \end{description}
!
!EOPI

       if (present(d)) then
         ESMF_FieldBundleGetInit = ESMF_INIT_GET(d)
       else
         ESMF_FieldBundleGetInit = ESMF_INIT_CREATED
       endif

    end function ESMF_FieldBundleGetInit


!------------------------------------------------------------------------------


      end module ESMF_FieldBundleMod
