! $Id: ESMF_Bundle.F90,v 1.45 2004/06/08 11:59:27 nscollins Exp $
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
      use ESMF_BaseTypesMod    ! ESMF base class
      use ESMF_BaseMod
      use ESMF_LogErrMod
      use ESMF_IOSpecMod
      use ESMF_ArrayDataMapMod
      use ESMF_GridTypesMod
      use ESMF_GridMod
      use ESMF_ArrayMod
      use ESMF_FieldDataMapMod
      use ESMF_FieldMod
      use ESMF_BundleDataMapMod
      implicit none
!
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_PackFlag   
!     !
!     ! Data type to set the status of data in this Bundle; it can either be
!     ! simply a collection of Fields which contain the data, or it can also 
!     ! have a private packed data buffer associated directly with the Bundle.

      type ESMF_PackFlag
      sequence
      !private
        integer :: packflag
      end type

      type(ESMF_PackFlag), parameter :: ESMF_PACKED_DATA = ESMF_PackFlag(1), &
                                        ESMF_NO_PACKED_DATA = ESMF_PackFlag(2)

!------------------------------------------------------------------------------
!  ! For ease of accessing data for an individual field for a packed array.

      type ESMF_BundleFieldAccess
      sequence
      private
         type(ESMF_BundleInterleave) :: bfa_type
         integer :: bfa_start
         integer :: bfa_end
         integer :: bfa_strides
      end type



!------------------------------------------------------------------------------
!       ! ESMF_BundleFieldInterleave
!       !
!       !  Data type to record the ordering information for multiple field
!       !  data which is packed in a bundle.  Each has an associated
!       !  {\tt ESMF\_FieldDataMap} object to track the ordering of that 
!       !  {\tt ESMF\_Field}'s data in the packed buffer.
        type ESMF_BundleFieldInterleave
        sequence
        private
          integer :: field_order                      ! index of this field
          type(ESMF_FieldDataMap) :: field_dm         ! copy of this field's dm
          type(ESMF_BundleFieldAccess) :: field_bfa   ! access info if packed
        end type

!------------------------------------------------------------------------------
!     ! ESMF_LocalBundle
!
      type ESMF_LocalBundle
      sequence
      !private
        type(ESMF_Array) :: packed_data               ! local packed array
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        type(ESMF_GridClass), pointer :: gridp => NULL() ! local data
        type(ESMF_Status) :: gridstatus = ESMF_STATE_UNINIT    ! is grid set 
        type(ESMF_Status) :: arraystatus = ESMF_STATE_UNINIT   ! is array set 
        integer :: accesscount = 0                    ! reserved for future use
#else
        type(ESMF_GridClass), pointer :: gridp
        type(ESMF_Status) :: gridstatus
        type(ESMF_Status) :: arraystatus
        integer :: accesscount
#endif
      
      end type

!------------------------------------------------------------------------------
!     ESMF_BundleType
!
      type ESMF_BundleType
      sequence
      !private
        type(ESMF_Base) :: base                   ! base class object
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        type(ESMF_Status) :: bundlestatus = ESMF_STATE_UNINIT
        type(ESMF_Status) :: gridstatus = ESMF_STATE_UNINIT     ! is grid set
        type(ESMF_GridClass), pointer :: gridp => NULL() ! shortcut
        type(ESMF_Field), dimension(:), pointer :: flist => NULL() 
        integer :: field_count = 0                ! how many fields in here
#else
        type(ESMF_Status) :: bundlestatus
        type(ESMF_Status) :: gridstatus
        type(ESMF_GridClass), pointer :: gridp
        type(ESMF_Field), dimension(:), pointer :: flist
        integer :: field_count
#endif
        type(ESMF_Grid) :: grid                  ! associated global grid
        type(ESMF_LocalBundle) :: localbundle    ! this differs per DE
        type(ESMF_Packflag) :: pack_flag         ! is packed data present?
        type(ESMF_BundleFieldInterleave) :: fil  ! ordering in buffer
        type(ESMF_IOSpec) :: iospec              ! iospec values
        type(ESMF_Status) :: iostatus            ! if unset, inherit from gcomp
      
      end type

!------------------------------------------------------------------------------
!     ! ESMF_Bundle

!     ! The Bundle data structure that is passed between implementation and
!     ! calling languages.

      type ESMF_Bundle
      sequence
      !private
#if !defined(ESMF_NO_INITIALIZERS) && !defined(ESMF_AIX_8_INITBUG)
        type (ESMF_BundleType), pointer :: btypep => NULL()
#else
        type (ESMF_BundleType), pointer :: btypep 
#endif
      end type

!------------------------------------------------------------------------------
!
! !PUBLIC TYPES:
      public ESMF_Bundle, ESMF_PackFlag, ESMF_PACKED_DATA, ESMF_NO_PACKED_DATA
      ! intended for internal ESMF use only but public for BundleComms
      public ESMF_BundleType, ESMF_LocalBundle, ESMF_BundleFieldInterleave


! !PUBLIC MEMBER FUNCTIONS:
!
       public ESMF_BundleCreate       ! Create a new Bundle
       public ESMF_BundleDestroy      ! Destroy a Bundle

       public ESMF_BundleGet          ! Get Bundle information

       public ESMF_BundleAddAttribute       ! Set and Get Attributes
       public ESMF_BundleGetAttribute       !   interface to Base class

       public ESMF_BundleGetAttributeCount  ! number of attribs
       public ESMF_BundleGetAttributeInfo   ! get type, length by name or number

       public ESMF_BundleGetField      ! Get one or more Fields by name or number
       public ESMF_BundleAddField      ! Add one or more Fields 
!      public ESMF_BundleRemoveField   ! Delete one or more Fields by name or number

       public ESMF_BundlePackData     ! Pack bundle data into a single 
!                                     !   buffer

      public ESMF_BundleSetGrid           ! In empty Bundle, set Grid
!      public ESMF_BundleGetGlobalGridInfo ! Return global grid info
!      public ESMF_BundleGetLocalGridInfo  ! Return local grid info

!      public ESMF_BundleGetGlobalDataInfo ! Return global data info
!      public ESMF_BundleGetLocalDataInfo  ! Return local data info

   ! These are the recommended entry points; the code itself is in Array:
   !public ESMF_BundleRedist   ! Redistribute existing arrays, matching grids
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


       public ESMF_BundleValidate     ! Check internal consistency
       public ESMF_BundlePrint        ! Print contents of a Bundle

       public operator(.eq.), operator(.ne.)

!  !subroutine ESMF_BundleGetDataMap
!
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
! !IROUTINE: ESMF_BundleAddAttribute - Set a Bundle Attribute
!
! !INTERFACE:
      interface ESMF_BundleAddAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleAddIntAttr
        module procedure ESMF_BundleAddIntListAttr
        module procedure ESMF_BundleAddRealAttr
        module procedure ESMF_BundleAddRealListAttr
        module procedure ESMF_BundleAddLogicalAttr
        module procedure ESMF_BundleAddLogicalListAttr
        module procedure ESMF_BundleAddCharAttr

! !DESCRIPTION:
!     This interface provides a single entry point for methods that attach
!     attributes to an {\tt ESMF\_Bundle}.
 
!EOP
      end interface
!
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_BundleGetAttribute - Get a Bundle Attribute
!
! !INTERFACE:
      interface ESMF_BundleGetAttribute 
   
! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_BundleGetIntAttr
        module procedure ESMF_BundleGetIntListAttr
        module procedure ESMF_BundleGetRealAttr
        module procedure ESMF_BundleGetRealListAttr
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
! !IROUTINE: ESMF_BundleGetAttributeInfo - Get type, count from a Bundle Attribute
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
#define ESMF_METHOD "ESMF_BundleAddIntAttr"
!BOP
! !IROUTINE: ESMF_BundleAddAttribute - Set an integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddAttribute()
      subroutine ESMF_BundleAddIntAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an integer attribute to an {\tt ESMF\_Bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The integer value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_INTEGER, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAddIntAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddIntListAttr"
!BOP
! !IROUTINE: ESMF_BundleAddAttribute - Set an integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddAttribute()
      subroutine ESMF_BundleAddIntListAttr(bundle, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer, dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Attaches an integer list attribute to an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The integer values of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif
  
      limit = size(value)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than value list", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_INTEGER, count, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAddIntListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddRealAttr"
!BOP
! !IROUTINE: ESMF_BundleAddAttribute - Set a real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddAttribute()
      subroutine ESMF_BundleAddRealAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      real, intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a real attribute to an {\tt ESMF\_Bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The real value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_REAL, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAddRealAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddRealListAttr"
!BOP
! !IROUTINE: ESMF_BundleAddAttribute - Set a real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddAttribute()
      subroutine ESMF_BundleAddRealListAttr(bundle, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real, dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a real list attribute to an {\tt ESMF\_Bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The real values of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than value list", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_REAL, count, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAddRealListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddLogicalAttr"
!BOP
! !IROUTINE: ESMF_BundleAddAttribute - Set a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddAttribute()
      subroutine ESMF_BundleAddLogicalAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an logical attribute to an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The logical true/false value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_LOGICAL, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAddLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddLogicalListAttr"
!BOP
! !IROUTINE: ESMF_BundleAddAttribute - Set a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddAttribute()
      subroutine ESMF_BundleAddLogicalListAttr(bundle, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(in) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches an logical list attribute to an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [count]
!           The number of values to be set.
!     \item [value]
!           The logical true/false values of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than value list", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeSetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_LOGICAL, count, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAddLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddCharAttr"
!BOP
! !IROUTINE: ESMF_BundleAddAttribute - Set a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddAttribute()
      subroutine ESMF_BundleAddCharAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Attaches a character attribute to an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to set.
!     \item [value]
!           The character value of the Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeSetChar(bundle%btypep%base, name, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleAddCharAttr

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
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_Field), intent(in) :: field
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Add a single Field reference to an existing {\tt ESMF\_Bundle}.  The Field must have the
!      same {\tt ESMF\_Grid} as the rest of the {\tt ESMF\_Field}s in the {\tt ESMF\_Bundle}.   If the {\tt ESMF\_Bundle} has
!      packed data, this will mean copying the data to add this field.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} to add {\tt ESMF\_Field} to.
!     \item [field]
!           The {\tt ESMF\_Field} to add.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  FLD2.5.2

      type(ESMF_Field) :: temp_list(1)
      type(ESMF_BundleType), pointer :: btype

      ! Initialize return code in case we return early.
      ! Otherwise, count on AddFieldList call to set rc
      if(present(rc)) rc = ESMF_FAILURE

      temp_list(1) = field

      ! validate bundle before going further
      if (.not. associated(bundle%btypep)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
      btype => bundle%btypep
      if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
    
      call ESMF_BundleTypeAddFieldList(btype, 1, temp_list, rc)

      end subroutine ESMF_BundleAddOneField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleAddFieldList"
!BOP
! !IROUTINE: ESMF_BundleAddField - Add a list of Fields to a Bundle
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleAddField()
      subroutine ESMF_BundleAddFieldList(bundle, fieldCount, fields, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle        
      integer, intent(in) :: fieldCount
      type(ESMF_Field), dimension(:), intent(in) :: fields
      integer, intent(out), optional :: rc          
!
! !DESCRIPTION:
!      Add an {\tt ESMF\_Field} reference to an existing {\tt ESMF\_Bundle}.  
!      The {\tt ESMF\_Field} must have the
!      same {\tt ESMF\_Grid} as the rest of the {\tt ESMF\_Fields} in the 
!      {\tt ESMF\_Bundle}.   If the {\tt ESMF\_Bundle} has
!      packed data, this will mean copying the data to add this field.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt ESMF\_Bundle} to add {\tt ESMF\_Field}s into.
!     \item [fieldCount]
!           Number of fields to be added to the {\tt ESMF\_Bundle}.
!           Must be equal to or less than the number of 
!           {\tt ESMF\_Field}s in the following argument.
!     \item [fields]
!           Array of existing {\tt ESMF\_Field}s.  The first {\tt fieldCount}
!           items will be added to the {\tt ESMF\_Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
! 
!EOP
! !REQUIREMENTS:  FLD2.5.2

      type(ESMF_BundleType), pointer :: btype

      ! Initialize return code in case we return early.
      ! Otherwise, count on AddFieldList call to set rc
      if(present(rc)) rc = ESMF_FAILURE

      ! validate bundle before going further
      if (.not. associated(bundle%btypep)) then
        if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
      btype => bundle%btypep
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
    
      call ESMF_BundleTypeAddFieldList(btype, fieldCount, fields, rc)
      
      end subroutine ESMF_BundleAddFieldList


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleCreateNew"
!BOP
! !IROUTINE: ESMF_BundleCreate - Create a Bundle from existing Fields
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleCreate()
      function ESMF_BundleCreateNew(fieldCount, fields, &
                                  packflag, bundleInterleave, name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Bundle) :: ESMF_BundleCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: fieldCount           
      type(ESMF_Field), dimension (:) :: fields
      type(ESMF_PackFlag), intent(in), optional :: packflag 
      type(ESMF_BundleInterleave), intent(in), optional :: bundleInterleave
      character (len = *), intent(in), optional :: name 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!   Create an {\tt ESMF\_Bundle} from a list of existing
!   gridded {\tt ESMF\_Fields}.  Optionally create a packed
!   {\tt ESMF\_Array} which collects all {\tt ESMF\_Field} data into
!   a single contiguous memory buffer.  All {\tt ESMF\_Field}s
!   must share a common {\tt ESMF\_Grid}.  Return a new {\tt ESMF\_Bundle}.
!
!   The arguments are:
!   \begin{description}
!   \item [fieldCount]
!      Number of fields to be added to the {\tt ESMF\_Bundle}.
!      Must be equal to or less than the number of 
!      {\tt ESMF\_Field}s in the following argument.
!   \item [fields]
!      Array of existing {\tt ESMF\_Field}s.  The first {\tt ESMF\_FieldCount}
!      items will be added to the {\tt ESMF\_Bundle}.
!   \item [{[packflag]}]
!      If set to {\tt ESMF\_PACK\_FIELD\_DATA}, the {\tt ESMF\_Field}
!      data in individual {\tt ESMF\_Array}s will be collected
!      into a single data {\tt ESMF\_Array} for the entire {\tt ESMF\_Bundle}.
!      The default is {\tt ESMF\_NO\_PACKED\_DATA}.
!   \item [{[bundleInterleave]}]
!      {\tt ESMF\_BundleInterleave} type.  Controls whether the data in
!      the packed bundle is interleaved by field or by item.
!   \item [{[name]}]
!      {\tt ESMF\_Bundle} name.  A default name will be generated if
!      one is not specified.
!   \item [{[iospec]}]
!      I/O specification.
!   \item [{[rc]}]
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!
!EOP
! !REQUIREMENTS:  

      type(ESMF_BundleType), pointer :: btypep         ! Pointer to new bundle
      integer :: status                                ! Error status
      logical :: rcpresent                             ! Return code present

!     Initialize pointers
      nullify(btypep)
      nullify(ESMF_BundleCreateNew%btypep)

!     Initialize return code
      status = ESMF_FAILURE
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      else
        rcpresent = .FALSE.
      endif

      allocate(btypep,  stat=status)
      if (ESMF_LogMsgFoundAllocError(status, "Bundle allocate", &
                                       ESMF_CONTEXT, rc)) return

!     Call construction method to allocate and initialize bundle internals.
      call ESMF_BundleConstructNew(btypep, fieldCount, fields, &
                                   packflag, bundleInterleave, name, iospec, rc)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

!     Set return values.
      ESMF_BundleCreateNew%btypep => btypep
      if(rcpresent) rc = ESMF_SUCCESS


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
!     Create an empty {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [{[grid]}]
!           The {\tt ESMF\_Grid} which all {\tt ESMF\_Field}s added to this
!           {\tt ESMF\_Bundle} must have.  If not specified now, the 
!           grid associated with the first {\tt ESMF\_Field} added will be
!           used as the {\tt ESMF\_Grid} for this {\tt ESMF\_Bundle}.
!     \item [{[name]}]
!           {\tt ESMF\_Bundle} name.  A default name will be generated if
!           one is not specified.
!     \item [{[iospec]}]
!           I/O specification.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS:  


      type(ESMF_BundleType), pointer :: btypep   ! Pointer to new bundle
      integer :: status                          ! Error status
      logical :: rcpresent                       ! Return code present

      ! Initialize pointers
      status = ESMF_FAILURE
      rcpresent = .FALSE. 
      nullify(btypep)
      nullify(ESMF_BundleCreateNoFields%btypep)

      ! Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

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
          btypep%gridstatus = ESMF_STATE_READY
      endif

      ! Set return values.
      ESMF_BundleCreateNoFields%btypep => btypep
      if(rcpresent) rc = ESMF_SUCCESS



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
!     Releases all resources associated with the {\tt ESMF\_Bundle}.
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
! !REQUIREMENTS:  FLD2.4


      ! Local variables
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btype

      ! Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif    

      ! If already destroyed or never created, return ok
      btype => bundle%btypep
      if (.not. associated(btype)) then
        if(rcpresent) rc = ESMF_FAILURE   ! should this really be an error?
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

      if(rcpresent) rc = ESMF_SUCCESS


      end subroutine ESMF_BundleDestroy


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGet"
!BOP
! !IROUTINE: ESMF_BundleGet - Return information about the Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleGet(bundle, grid, fieldCount, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_Grid), intent(out), optional :: grid
      integer, intent(out), optional :: fieldCount
      character (len = *), intent(out), optional :: name
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Returns information about the {\tt ESMF\_Bundle}.  
!      If the {\tt ESMF\_Bundle} was originally created
!      without specifying a name, a unique name will have been generated
!      by the framework.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} object to query.
!     \item [{[grid]}]
!           The {\tt ESMF\_Grid} associated with the {\tt ESMF\_Bundle}.
!     \item [{[fieldCount]}]
!           Number of {\tt ESMF\_Field}s in the {\tt ESMF\_Bundle}.
!     \item [{[name]}]
!           A character string where the {\tt ESMF\_Bundle} name is returned.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS:  FLD2.6.1 


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btype     ! internal data

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      ! TODO: make this a common subroutine; all entry points need to do this

      ! Validate bundle before using it.
      btype => bundle%btypep
      if (.not. associated(btype)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

      if (present(grid)) then
          ! Validate bundle has grid before trying to return it.
          if (btype%gridstatus .ne. ESMF_STATE_READY) then
               if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Bad Grid", &
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
          !OK to query for name
          call c_ESMC_GetName(btype%base, name, status)
          if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return
      endif

      if (rcpresent) rc = ESMF_SUCCESS


      end subroutine ESMF_BundleGet


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetAllFields"
!BOPI
! !IROUTINE: ESMF_BundleGetAllFields - Retrieve an array of Fields 
!
! !INTERFACE:
      subroutine ESMF_BundleGetAllFields(bundle, fieldptrs, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_Field), pointer, dimension (:) :: fieldptrs
      integer, intent(out), optional :: count
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return pointers to all {\tt ESMF\_Field}s in an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} to query for the {\tt ESMF\_Field}s.
!     \item [fieldptrs]
!           {\tt ESMF\_Field} pointer array.
!     \item [{[count]}]
!           Number of fields in the bundle.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD2.5.6
!
!  TODO: code goes here
!
      end subroutine ESMF_BundleGetAllFields


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetIntAttr"
!BOP
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve an integer Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetIntAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an integer attribute from an {\tt ESMF\_Bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The integer value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_INTEGER, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetIntAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetIntListAttr"
!BOP
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve an integer list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetIntListAttr(bundle, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      integer, dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns an integer list attribute from an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the list.
!     \item [value]
!           The list (fortran array) of integer values of the named Attribute.
!           The list must be at least {\tt count} item long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than value list", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_INTEGER, count, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetIntListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetRealAttr"
!BOP
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a real Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetRealAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      real, intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns a real attribute from an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The real value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_REAL, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetRealAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetRealListAttr"
!BOP
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a real list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetRealListAttr(bundle, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      real, dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!     Returns a real list attribute from an {\tt ESMF\_Bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the list.
!     \item [value]
!           The list (fortran array) of real values of the named Attribute.
!           The list must be at least {\tt count} item long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than value list", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_REAL, count, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetRealListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetLogicalAttr"
!BOP
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a logical Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetLogicalAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      type(ESMF_Logical), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an logical attribute from an {\tt ESMF\_Bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The logical value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_LOGICAL, 1, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetLogicalAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetLogicalListAttr"
!BOP
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a logical list Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetLogicalListAttr(bundle, name, count, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      integer, intent(in) :: count   
      type(ESMF_Logical), dimension(:), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an logical list attribute from an {\tt ESMF\_Bundle}.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [count]
!           The number of values in the list.
!     \item [value]
!           The list (fortran array) of logical values of the named Attribute.
!           The list must be at least {\tt count} item long.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: limit

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      limit = size(value)
      if (count > limit) then
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "count longer than value list", &
                                 ESMF_CONTEXT, rc)) return
      endif

      call c_ESMC_AttributeGetValue(bundle%btypep%base, name, &
                                    ESMF_DATA_LOGICAL, count, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetLogicalListAttr

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetCharAttr"
!BOP
! !IROUTINE: ESMF_BundleGetAttribute - Retrieve a character Attribute
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttribute()
      subroutine ESMF_BundleGetCharAttr(bundle, name, value, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character (len = *), intent(in) :: name
      character (len = *), intent(out) :: value
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns an integer attribute from an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to retrieve.
!     \item [value]
!           The character value of the named Attribute.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetChar(bundle%btypep%base, name, value, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetCharAttr


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetAttributeCount"
!BOP
! !IROUTINE: ESMF_BundleGetAttributeCount - Query the number of Attributes
!
! !INTERFACE:
      subroutine ESMF_BundleGetAttributeCount(bundle, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      integer, intent(out) :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of values associated with the given attribute.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [count]
!           The number of attributes on this object.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetCount(bundle%btypep%base, count, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetAttributeCount

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetAttrInfoByName"
!BOP
! !IROUTINE: ESMF_BundleGetAttributeInfo - Query an Attribute by name
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttributeInfo()
      subroutine ESMF_BundleGetAttrInfoByName(bundle, name, type, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      character(len=*), intent(in) :: name
      type(ESMF_DataType), intent(out), optional :: type
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of values associated with the given attribute.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [name]
!           The name of the Attribute to query.
!     \item [type]
!           The type of the Attribute.
!     \item [count]
!           The number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_DataType) :: localDt
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetAttrInfoName(bundle%btypep%base, name, &
                                           localDt, localCount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(type)) type = localDt
      if (present(count)) count = localCount

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetAttrInfoByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetAttrInfoByNum"
!BOP
! !IROUTINE: ESMF_BundleGetAttributeInfo - Query an Attribute by number
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetAttributeInfo()
      subroutine ESMF_BundleGetAttrInfoByNum(bundle, num, name, type, count, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle  
      integer, intent(in) :: num
      character(len=*), intent(out), optional :: name
      type(ESMF_DataType), intent(out), optional :: type
      integer, intent(out), optional :: count   
      integer, intent(out), optional :: rc   

!
! !DESCRIPTION:
!      Returns the number of values associated with the given attribute.
! 
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [num]
!           The number of the Attribute to query.
!     \item [name]
!           Returns the name of the Attribute.
!     \item [type]
!           Returns the type of the Attribute.
!     \item [count]
!           Returns the number of items in this Attribute.  For character types,
!           the length of the character string.
!     \item [{[rc]}] 
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      character(len=ESMF_MAXSTR) :: localName
      type(ESMF_DataType) :: localDt
      integer :: localCount

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      call c_ESMC_AttributeGetAttrInfoNum(bundle%btypep%base, num, &
                                       localName, localDt, localCount, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if (present(name)) name = localName
      if (present(type)) type = localDt
      if (present(count)) count = localCount

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetAttrInfoByNum

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetDataMap"
!BOP
! !IROUTINE: ESMF_BundleGetDataMap - Get packed array ordering
!
! !INTERFACE:
      subroutine ESMF_BundleGetDataMap(bundle, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_BundleDataMap), intent(out) :: datamap
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      For querying current ordering of packed {\tt ESMF\_DataArray} type.
!      Information is returned in the {\tt ESMF\_BundleDataMap} type, 
!      and then can be queried by {\tt ESMF\_BundleDataMap} 
!      subroutines for details.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} object to query.
!     \item [datamap]
!           The current order/interleaf.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS:  FLD2.5.8 (pri 2)


!
!  TODO: code goes here
!
      end subroutine ESMF_BundleGetDataMap


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
      type(ESMF_Bundle), intent(in) :: bundle
      character (len = *), intent(in) :: name
      type(ESMF_Field), intent(out) :: field
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Return an {\tt ESMF\_Field} from an {\tt ESMF\_Bundle} by name.
!
!     The arguments are:
!     \begin{description}
!
!     \item [bundle]
!           {\tt ESMF\_Bundle} to query for {\tt ESMF\_Field}.
!
!     \item [name]
!           {\tt ESMF\_Field} name.
!
!     \item [field]
!           Returned {\tt ESMF\_Field}.
!
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:  FLD2.5.6


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! temp var
      logical :: found                            ! did we find a match?
      character (len=ESMF_MAXSTR) :: temp_name
      type(ESMF_Field) :: temp_field
      type(ESMF_BundleType), pointer :: btype

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      found = .FALSE.

      btype => bundle%btypep

!     Validate bundle before using it.
      if (.not. associated(btype)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

!     Check for an empty Bundle first
      if(btype%field_count .eq. 0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Empty Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

!     Check each field for a match
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

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetFieldByName

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetFieldByNum"
!BOP
! !IROUTINE: ESMF_BundleGetField - Retrieve a Field by index number
!
! !INTERFACE:
      ! Private name; call using ESMF_BundleGetField()
      subroutine ESMF_BundleGetFieldByNum(bundle, index, field, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      integer, intent(in) :: index
      type(ESMF_Field), intent(out) :: field
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Return an {\tt ESMF\_Field} from an {\tt ESMF\_Bundle} by index number.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           {\tt ESMF\_Bundle} to query for {\tt ESMF\_Field}.
!     \item [index]
!           {\tt ESMF\_Field} index number; first {\tt ESMF\_Field} index is 1.
!     \item [field]
!           Returned {\tt ESMF\_Field}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  FLD2.5.6


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      logical :: found                            ! did we find a match?
      type(ESMF_BundleType), pointer :: btype

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      found = .FALSE.

      btype => bundle%btypep

!     Validate bundle before using it.
      if (.not. associated(btype)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

!     Check for an empty Bundle first
      if(btype%field_count .eq. 0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Empty Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

!     Check for out of range index number
      if ((index .lt. 1) .or. (index .gt. btype%field_count)) then
        ! "ERROR in ESMF_BundleGetField: Index ", index, &
        !                "out of range. Min=1, max=", btype%field_count
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Index out of range", &
                                 ESMF_CONTEXT, rc)) return
        return
      endif

!     Fetch requested field
      field = bundle%btypep%flist(index) 

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleGetFieldByNum


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetFieldNames"
!BOPI
! !IROUTINE: ESMF_BundleGetFieldNames - Return all Field names in a Bundle

! !INTERFACE:
      subroutine ESMF_BundleGetFieldNames(bundle, nameList, nameCount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle 
      character (len = *), intent(inout) :: nameList(:) ! TODO: change to out 
                                                        ! after adding code
      integer, intent(out), optional :: nameCount     
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Return an array of {\tt ESMF\_Field} names in an {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [nameList]
!           An array of character strings where each {\tt ESMF\_Field} name
!           is returned. 
!     \item [{[nameCount]}]
!           A count of how many {\tt ESMF\_Field} names were returned.  Same as
!           the number of {\tt ESMF\_Field}s in the {\tt ESMF\_Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD2.6.2


!
!  TODO: code goes here.  comment protex doc back in when implemented.
!
      end subroutine ESMF_BundleGetFieldNames


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetGridCellCount"
!BOPI
! !IROUTINE: ESMF_BundleGetGridCellCount - Returns global and local grid cell count
!
! !INTERFACE:
      subroutine ESMF_BundleGetGridCellCount(bundle, localcount, globalcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      integer, intent(out), optional :: localcount(:)
      integer, intent(out), optional :: globalcount(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      For both the local decomposition and the global grid, return the 
!       number of items in each.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} object to query.
!     \item [{[localcount]}]
!           The local cell count.
!     \item [{[globalcount]}]
!           The global cell count.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD2.6.4
!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetGridCellCount


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetGridDimCount"
!BOPI
! !IROUTINE: ESMF_BundleGetGridDimCount - Get dimensionality of Grid
!
! !INTERFACE:
      subroutine ESMF_BundleGetGridDimCount(bundle, dimcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      integer, intent(out) :: dimcount
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Find out how many dimensions are in the {\tt ESMF\_Grid} associated with this {\tt ESMF\_Bundle}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object to query.
!     \item [dimcount]
!           The number of dimensions.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS: (none. added for completeness)


!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetGridDimCount



!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetGridDimSize"
!BOPI
! !IROUTINE: ESMF_BundleGetGridDimSize - Returns the number of items in each dimension
!
! !INTERFACE:
      subroutine ESMF_BundleGetGridDimSize(bundle, locallist, globallist, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      integer, intent(out), optional :: locallist(:)
      integer, intent(out), optional :: globallist(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      For both the local decomposition and the global grid, 
!      return the number of items in each dimension.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} object to query.
!     \item [{[locallist]}]
!           The size of the local list.
!     \item [{[globallist]}]
!           The size of the global list.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS: (none. added for completeness)


!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetGridDimSize


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetGridIndexOrder"
!BOPI
! !IROUTINE: ESMF_BundleGetGridIndexOrder - Returns the order of the indices
!
! !INTERFACE:
      subroutine ESMF_BundleGetGridIndexOrder(bundle, indexorder, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      integer, dimension(:), intent(out) :: indexorder
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Return in what order the indicies of the {\tt ESMF\_Grid} is specified.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} object to query.
!     \item [indexorder]
!           The description of ordering.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: (none. added for completeness)


!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetGridIndexOrder


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleGetGridPointCount"
!BOPI
! !IROUTINE: ESMF_BundleGetGridPointCount - Returns global and local grids point count
!
! !INTERFACE:
      subroutine ESMF_BundleGetGridPointCount(bundle, localcount, globalcount, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      integer, intent(out), optional :: localcount(:)
      integer, intent(out), optional :: globalcount(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      For both the local decomposition and the global grid, return the 
!      number of items in each.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} object to query.
!     \item [{[localcount]}]
!           The local point/vertex count.
!     \item [{[globalcount]}]
!           The global point/vertex count.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: FLD2.6.4


!
!  TODO: code goes here
!
        end subroutine ESMF_BundleGetGridPointCount


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundlePackData"
!BOPI
! !IROUTINE: ESMF_BundlePackData - Pack Field data into a single Array
!
! !INTERFACE:
      subroutine ESMF_BundlePackData(bundle, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_BundleDataMap), intent(in), optional :: datamap 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Packs the {\tt ESMF\_Field} data into a single {\tt ESMF\_Array}.  If new {\tt ESMF\_Field}s
!      are added to an {\tt ESMF\_Bundle} which already has Packed data, the data will
!      have to be copied into a new {\tt ESMF\_Array}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           Existing {\tt ESMF\_Bundle}.
!     \item [{[datamap]}]
!           Ordering and Interleaving information.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:  FLD2.1.1, FLD2.2, FLD2.5.9


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! temp var
      type(ESMF_Array) :: pkarray                 ! Array for packed data
      type(ESMF_BundleType), pointer :: btype     ! internal data

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      btype => bundle%btypep

!     Validate bundle before using it.
      if (.not. associated(btype)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif

!     pkarray = ESMF_ArrayCreate(arrayspec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      btype%pack_flag = ESMF_PACKED_DATA
!     btype%localbundle%packed_data = pkarray

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundlePackData

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundlePrint"
!BOP
! !IROUTINE: ESMF_BundlePrint - Print diagnostic information about a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundlePrint(bundle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Print information about an {\tt ESMF\_Bundle}.  The options control the
!      type of information and level of detail.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [{[options]}]
!           The print options.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  


!
!  TODO: code goes here
!
      character(len=ESMF_MAXSTR) :: bname, fname
      character(len=ESMF_MAXSTR) :: msgbuf
      type(ESMF_BundleType), pointer :: btype
      type(ESMF_Field) :: field
      integer :: i
      integer :: status

      print msgbuf, "Bundle print:"
      if (ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)) continue

      btype => bundle%btypep
      call c_ESMC_GetName(btype%base, bname, status)
      print msgbuf, "  Bundle name = ", trim(bname)
      if (ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)) continue
    
      print msgbuf, "  Field count = ", btype%field_count
      if (ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)) continue
    
      do i = 1, btype%field_count
  
       call ESMF_FieldGet(btype%flist(i), name=fname, rc=status)
       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       print msgbuf, "    Field", i, "name = ", trim(fname)
       if (ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)) continue
      enddo

      ! TODO: add more code here for printing more info

      end subroutine ESMF_BundlePrint

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRead"
!BOP
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

!EOP
! !REQUIREMENTS:  (which req number is this?)

!
!  TODO: code goes here
!
      type(ESMF_Bundle) :: b

      b%btypep%bundlestatus = ESMF_STATE_UNINIT

      ESMF_BundleRead = b

      end function ESMF_BundleRead

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleReadRestart"
!BOP
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
!EOP
! !REQUIREMENTS:  FLD2.5.10


!
!  TODO: code goes here
!
      type(ESMF_Bundle) :: b

      b%btypep%bundlestatus = ESMF_STATE_UNINIT

      ESMF_BundleReadRestart = b

      end function ESMF_BundleReadRestart

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleRemoveField"
!BOP
! !IROUTINE: ESMF_BundleRemoveField - Remove a Field from a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleRemoveField(bundle, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      character (len = *), intent(in) :: name
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Delete a Field reference from an existing {\tt ESMF\_Bundle}.  If the {\tt ESMF\_Bundle} 
!      has packed data this will mean copying the data to remove this field.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} to remove {\tt ESMF\_Field} from.
!     \item [name]
!           Name of {\tt ESMF\_Field} to remove.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS:  FLD2.5.2


!
!  TODO: code goes here
!
      if (ESMF_LogMsgFoundError(ESMF_RC_NOT_IMPL, &
                                "ESMF_BundleRemoveField", &
                                 ESMF_CONTEXT, rc)) return

      end subroutine ESMF_BundleRemoveField


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleReorder"
!BOPI
! !IROUTINE: ESMF_BundleReorder - Alter memory interleave in packed data
!
! !INTERFACE:
      subroutine ESMF_BundleReorder(bundle, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_BundleDataMap), intent(in) :: datamap
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Used to alter memory ordering of packed {\tt ESMF\_Data} array.  Implemented by 
!      setting the desired options in an {\tt ESMF\_BundleDataMap} type and then passing it in
!      as a parameter to this routine.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           The {\tt ESMF\_Bundle} to operate on.
!     \item [datamap]
!           The new interleave/order.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOPI
! !REQUIREMENTS:  FLD2.2, FLD2.3


!
!  TODO: code goes here
!
      end subroutine ESMF_BundleReorder

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
      type(ESMF_Bundle), intent(in) :: bundle
      integer, dimension (:), intent(in) :: index
      real, dimension (:), intent(in) :: value
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Allows data values associated with an {\tt ESMF\_Bundle} to be set through the
!      {\tt ESMF\_Bundle} interface instead of detaching data and setting it in a loop.
!      Various restrictions on data types may be imposed.
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
! !REQUIREMENTS:  FLD2.5.5


!
!  TODO: code goes here
!
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
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_Grid), intent(in) :: grid
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Sets the {\tt ESMF\_Grid} for an empty {\tt ESMF\_Bundle}.  All 
!     {\tt ESMF\_Field}s added to this {\tt ESMF\_Bundle} must have this
!     same {\tt ESMF\_Grid}.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [grid]
!           The {\tt ESMF\_Grid} which all {\tt ESMF\_Field}s added to this
!           {\tt ESMF\_Bundle} must have.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!
!EOP
! !REQUIREMENTS: FLD2.5.7


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      type(ESMF_BundleType), pointer :: btype     ! internal data

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      ! Validate bundle before using it.
      btype => bundle%btypep
      if (.not. associated(btype)) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
      if (btype%bundlestatus .ne. ESMF_STATE_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif
   
      ! here we will only let someone associate a grid with a bundle
      ! if there is not one already associated with it.  
      if (btype%gridstatus .eq. ESMF_STATE_READY) then
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
      btype%gridstatus = ESMF_STATE_READY

      if (rcpresent) rc = ESMF_SUCCESS


      end subroutine ESMF_BundleSetGrid


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleValidate"
!BOP
! !IROUTINE: ESMF_BundleValidate - Perform internal consistency checks
!
! !INTERFACE:
      subroutine ESMF_BundleValidate(bundle, options, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      character (len=*), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Validates that the {\tt ESMF\_Bundle} is internally consistent.
!      Returns error code if problems are found.
!
!     The arguments are:
!     \begin{description}
!     \item [bundle]
!           An {\tt ESMF\_Bundle} object.
!     \item [{[options]}]
!           The validate options.  See {\ref xx} for standard option strings.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

!EOP
! !REQUIREMENTS:  FLD4.1


      ! Local variables
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      if (.not.associated(bundle%btypep)) then 
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif 

      if (bundle%btypep%bundlestatus .ne. ESMF_STATE_READY) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                "Uninitialized or already destroyed Bundle", &
                                 ESMF_CONTEXT, rc)) return
      endif 

      ! TODO: add more code here

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleValidate


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleWrite"
!BOP
! !IROUTINE: ESMF_BundleWrite - Save a Bundle to an external destination
!
! !INTERFACE:
      subroutine ESMF_BundleWrite(bundle, subarray, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle
      type(ESMF_Array), pointer, optional :: subarray
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
!EOP
! !REQUIREMENTS:  FLD3.1, FLD3.2, FLD3.3, FLD3.4, FLD3.5


!
!  TODO: code goes here
!
      end subroutine ESMF_BundleWrite


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleWriteRestart"
!BOP
! !IROUTINE: ESMF_BundleWriteRestart - Save Bundle in the quickest manner possible
!
! !INTERFACE:
      subroutine ESMF_BundleWriteRestart(bundle, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Bundle), intent(in) :: bundle 
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
!EOP
! !REQUIREMENTS:  FLD2.5.10


!
!  TODO: code goes here
!
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
      type(ESMF_Field), dimension(:), intent(in) :: fields
      integer, intent(out), optional :: rc          
!
! !DESCRIPTION:
!      Add a Field reference to an existing {\tt ESMF\_Bundl}.  The {\tt ESMF\_Field} must have the
!      same Grid as the rest of the {\tt ESMF\_Fields} in the {\tt ESMF\_Bundle}.   If the {\tt ESMF\_Bundle} has
!      packed data, this will mean copying the data to add this field.
!
!     The arguments are:
!     \begin{description}
!     \item [btype]
!           {\tt ESMF\_BundleType} to add {\tt ESMF\_Field}s into.
!     \item [fieldCount]
!           Number of fields to be added to the {\tt ESMF\_Bundle}.
!           Must be equal to or less than the number of 
!           {\tt ESMF\_Field}s in the following argument.
!     \item [fields]
!           Array of existing {\tt ESMF\_Field}s.  The first {\tt fieldCount}
!           items will be added to the {\tt ESMF\_Bundle}.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
! 
!EOPI
      
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! temp var
      type(ESMF_Field), dimension(:), pointer :: temp_flist  
                                                  ! list of fields
      type(ESMF_Grid) :: grid

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initial values
      nullify(temp_flist)
    
      ! early exit.
      if (fieldCount .le. 0) then
         if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, &
                                 "called with no Fields", &
                                 ESMF_CONTEXT, rc)) return
      endif
      
   
! TODO: add consistency checks below
!       loop over field count, get grid and check to see it's the same

      ! Add the fields in the list, checking for consistency.
      if (btype%field_count .eq. 0) then
        
          allocate(btype%flist(fieldCount), stat=status)
          if (ESMF_LogMsgFoundAllocError(status, "Fieldlist allocate", &
                                       ESMF_CONTEXT, rc)) return
         
          ! now add the fields to the new list
          do i=1, fieldCount
            btype%flist(i) = fields(i)
          enddo

          btype%field_count = fieldCount
      else
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

      ! If no grid set yet, loop and set the first grid we find to be
      !  associated with the bundle.  Note that Fields can be created
      !  that don't have associated grids yet, so we have to be able to
      !  deal consistently with that.
      if (btype%gridstatus .eq. ESMF_STATE_UNINIT) then
          do i=1, fieldCount
            call ESMF_FieldGet(btype%flist(i), grid=grid, rc=status)
            if (status .ne. ESMF_SUCCESS) cycle

            btype%grid = grid
            btype%gridstatus = ESMF_STATE_READY
            status = ESMF_SUCCESS
            exit
          enddo
      endif

      ! If packed data buffer requested, create or update it here.
      if (btype%pack_flag .eq. ESMF_PACKED_DATA) then

         call ESMF_BundleTypeRepackData(btype, rc=rc)

      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleTypeAddFieldList

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleTypeRepackData"
!BOPI
! !IROUTINE: ESMF_BundleTypeRepackData - Pack Field data into a single Array
!
! !INTERFACE:
      subroutine ESMF_BundleTypeRepackData(btype, datamap, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype
      type(ESMF_BundleDataMap), intent(in), optional :: datamap 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Packs the {\tt ESMF\_Field} data into a single {\tt ESMF\_Array}.  If new {\tt ESMF\_Field}s
!      are added to an {\tt ESMF\_Bundle} which already has Packed data, the data will
!      have to be copied into a new {\tt ESMF\_Array}.
!
!     The arguments are:
!     \begin{description}
!     \item [btype]
!           {\tt ESMF\_BundleType} pointer.
!     \item [{[datamap]}]
!           Ordering and Interleaving information.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  FLD2.1.1, FLD2.2, FLD2.5.9


      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present
      integer :: i                                ! temp var
      type(ESMF_Array) :: pkarray                 ! Array for packed data

!     Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     pkarray = ESMF_ArrayCreate(arrayspec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      btype%pack_flag = ESMF_PACKED_DATA
!     btype%localbundle%packed_data = pkarray

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_BundleTypeRepackData


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_BundleConstructNew"
!BOPI
! !IROUTINE: ESMF_BundleConstructNew - Construct the internals of a Bundle
!
! !INTERFACE:
      subroutine ESMF_BundleConstructNew(btype, fieldCount, fields, &
                                         packflag, bundleInterleave, &
                                         name, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_BundleType), pointer :: btype 
      integer, intent(in) :: fieldCount           
      type(ESMF_Field), dimension (:) :: fields
      type(ESMF_PackFlag), intent(in), optional :: packflag 
      type(ESMF_BundleInterleave), intent(in), optional :: bundleInterleave
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
!   \item [{[bundleInterleave]}]
!      {\tt ESMF\_BundleInterleave} type.  Controls whether the data in
!      the packed bundle is interleaved by field or by item.
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
      
      integer :: i                                ! temp var
      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! Initialize the derived type contents.
      call ESMF_BundleConstructNoFields(btype, name, iospec, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! If specified, set packflag and interleave
      if(present(packflag)) btype%pack_flag = packflag
      if(present(bundleInterleave)) btype%fil%field_bfa%bfa_type = bundleInterleave

      ! Add the fields in the list, checking for consistency.
      call ESMF_BundleTypeAddFieldList(btype, fieldCount, fields, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      if(rcpresent) rc = ESMF_SUCCESS

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
      logical :: rcpresent                         ! Return code present
      character (len = ESMF_MAXSTR) :: defaultname ! Bundle name if not given

      ! Initialize return code.  Assume failure until success assured.
      status = ESMF_FAILURE
      rcpresent =.FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif


      ! Initialize the base object
      call ESMF_BaseCreate(btype%base, "Bundle", name, 0, status)
      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

      ! Initialize bundle contents
      
      btype%localbundle%gridstatus = ESMF_STATE_UNINIT
      btype%localbundle%arraystatus = ESMF_STATE_UNINIT
      btype%gridstatus = ESMF_STATE_UNINIT
   
      btype%field_count = 0
      nullify(btype%flist)
      
      btype%pack_flag = ESMF_NO_PACKED_DATA
!     nullify(btype%localbundle%packed_data)
      btype%bundlestatus = ESMF_STATE_READY
  

      if(rcpresent) rc = ESMF_SUCCESS

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

      logical :: rcpresent                        ! Return code present
      integer :: status

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

     btype%bundlestatus = ESMF_STATE_INVALID
     call ESMF_BaseDestroy(btype%base, status)

     !
     ! TODO: code goes here
     !


     if (rcpresent) rc = status


     end subroutine ESMF_BundleDestruct





      end module ESMF_BundleMod

