! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2023, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_HConfig.F90"
!==============================================================================
!
! ESMF HConfig Module
module ESMF_HConfigMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the HConfig class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_HConfigMod
!

!   F90 API wrapper of C++ implementation of HConfig
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod           ! ESMF utility types
  use ESMF_InitMacrosMod          ! ESMF initializer macros
  use ESMF_LogErrMod              ! ESMF error handling
  use ESMF_VMMod                  ! ESMF VM
  use ESMF_IOUtilMod              ! ESMF I/O utility layer

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
!     ! ESMF_HConfig
!
!------------------------------------------------------------------------------

  ! Fortran type to hold pointer to C++ object
  type ESMF_HConfig
#ifndef ESMF_NO_SEQUENCE
  sequence
#endif
  private
    ! 20 x 4-byte to safely store a few items on the C++ side. This size has
    ! been emperically determined, and might need to increase if the C++ side
    ! changes!
    ! Keeping this memory as shallow on the stack eliminates the need for
    ! complicated garbage collection around heap memory.
#ifndef ESMF_NO_INITIALIZERS
    integer(ESMF_KIND_I4), dimension(20) :: shallowMemory = 0
#else
    integer(ESMF_KIND_I4), dimension(20) :: shallowMemory
#endif
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_HConfig

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public operator(==)
  public operator(/=)

  public ESMF_HConfigCreate
  public ESMF_HConfigDestroy

  public ESMF_HConfigLoad
  public ESMF_HConfigLoadFile

  public ESMF_HConfigCreateAt
  public ESMF_HConfigCreateAtMapKey
  public ESMF_HConfigCreateAtMapVal

  public ESMF_HConfigGetSize
  public ESMF_HConfigGetMapKeySize
  public ESMF_HConfigGetMapValSize

  public ESMF_HConfigIsNull
  public ESMF_HConfigIsScalar
  public ESMF_HConfigIsSequence
  public ESMF_HConfigIsMap
  public ESMF_HConfigIsDefined

  public ESMF_HConfigIsMapKeyNull
  public ESMF_HConfigIsMapKeyScalar
  public ESMF_HConfigIsMapKeySequence
  public ESMF_HConfigIsMapKeyMap
  public ESMF_HConfigIsMapKeyDefined

  public ESMF_HConfigIsMapValNull
  public ESMF_HConfigIsMapValScalar
  public ESMF_HConfigIsMapValSequence
  public ESMF_HConfigIsMapValMap
  public ESMF_HConfigIsMapValDefined

  public ESMF_HConfigIsIterator
  public ESMF_HConfigIsSequenceIterator
  public ESMF_HConfigIsMapIterator

  public ESMF_HConfigIterBegin
  public ESMF_HConfigIterEnd
  public ESMF_HConfigIterMapKeyBegin
  public ESMF_HConfigIterMapKeyEnd
  public ESMF_HConfigIterMapValBegin
  public ESMF_HConfigIterMapValEnd
  public ESMF_HConfigIterNext

  public ESMF_HConfigAsString
  public ESMF_HConfigAsMapKeyString
  public ESMF_HConfigAsMapValString

  public ESMF_HConfigAsI4
  public ESMF_HConfigAsMapKeyI4
  public ESMF_HConfigAsMapValI4

  public ESMF_HConfigAsI8
  public ESMF_HConfigAsMapKeyI8
  public ESMF_HConfigAsMapValI8

  public ESMF_HConfigAsR4
  public ESMF_HConfigAsMapKeyR4
  public ESMF_HConfigAsMapValR4

  public ESMF_HConfigAsR8
  public ESMF_HConfigAsMapKeyR8
  public ESMF_HConfigAsMapValR8

! - ESMF-internal methods:
  public ESMF_HConfigGetInit
!EOPI
!------------------------------------------------------------------------------

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public interface ----------------------------
!BOP
! !IROUTINE: ESMF_HConfigOperator(==) - HConfig equality operator
!
! !INTERFACE:
  interface operator(==)
!   if (hconfig1 == hconfig2) then ... endif
!             OR
!   result = (hconfig1 == hconfig2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_HConfig), intent(in) :: hconfig1
!   type(ESMF_HConfig), intent(in) :: hconfig2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether hconfig1 and hconfig2 are valid aliases to the same ESMF
!   HConfig object in memory. For a more general comparison of two 
!   ESMF HConfigs, going beyond the simple alias test, the 
!   {\tt ESMF\_HConfigMatch()} function (not yet fully implemented) must 
!   be used.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig1]
!     The {\tt ESMF\_HConfig} object on the left hand side of the equality
!     operation.
!   \item[hconfig2]
!     The {\tt ESMF\_HConfig} object on the right hand side of the equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_HConfigEQ

  end interface
!------------------------------------------------------------------------------

! -------------------------- ESMF-public interface ----------------------------
!BOP
! !IROUTINE: ESMF_HConfigOperator(/=) - HConfig not equal operator
!
! !INTERFACE:
  interface operator(/=)
!   if (hconfig1 /= hconfig2) then ... endif
!             OR
!   result = (hconfig1 /= hconfig2)
! !RETURN VALUE:
!   logical :: result
!
! !ARGUMENTS:
!   type(ESMF_HConfig), intent(in) :: hconfig1
!   type(ESMF_HConfig), intent(in) :: hconfig2
!
! !STATUS:
! \begin{itemize}
! \item\apiStatusCompatibleVersion{5.2.0r}
! \end{itemize}
!
! !DESCRIPTION:
!   Test whether hconfig1 and hconfig2 are {\it not} valid aliases to the
!   same ESMF HConfig object in memory. For a more general comparison of two
!   ESMF HConfigs, going beyond the simple alias test, the
!   {\tt ESMF\_HConfigMatch()} function (not yet fully implemented) must 
!   be used.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig1]
!     The {\tt ESMF\_HConfig} object on the left hand side of the non-equality
!     operation.
!   \item[hconfig2]
!     The {\tt ESMF\_HConfig} object on the right hand side of the non-equality
!     operation.
!   \end{description}
!
!EOP
    module procedure ESMF_HConfigNE

  end interface
!------------------------------------------------------------------------------


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigEQ()"
!BOPI
! !IROUTINE:  ESMF_HConfigEQ - Compare two HConfigs for equality
!
! !INTERFACE:
  function ESMF_HConfigEQ(HConfig1, HConfig2)
!
! !RETURN VALUE:
    logical :: ESMF_HConfigEQ

! !ARGUMENTS:
    type(ESMF_HConfig), intent(in) :: HConfig1
    type(ESMF_HConfig), intent(in) :: HConfig2

! !DESCRIPTION:
!   Test if both {\tt HConfig1} and {\tt HConfig2} alias the same ESMF HConfig 
!   object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_INIT_TYPE init1, init2
    integer :: localrc1, localrc2
    logical :: lval1, lval2

    ! Use the following logic, rather than "ESMF-INIT-CHECK-DEEP", to gain 
    ! init checks on both args, and in the case where both are uninitialized,
    ! to distinguish equality based on uninitialized type (uncreated,
    ! deleted).

    ! check inputs
    init1 = ESMF_HConfigGetInit(HConfig1)
    init2 = ESMF_HConfigGetInit(HConfig2)

    ! TODO: this line must remain split in two for SunOS f90 8.3 127000-03
    if (init1 .eq. ESMF_INIT_CREATED .and. &
      init2 .eq. ESMF_INIT_CREATED) then
      ESMF_HConfigEQ = all(HConfig1%shallowMemory .eq. HConfig2%shallowMemory)
    else
      ESMF_HConfigEQ = .false.
    endif

  end function ESMF_HConfigEQ
!-------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigNE()"
!BOPI
! !IROUTINE:  ESMF_HConfigNE - Compare two HConfigs for non-equality
!
! !INTERFACE:
  function ESMF_HConfigNE(HConfig1, HConfig2)
!
! !RETURN VALUE:
    logical :: ESMF_HConfigNE

! !ARGUMENTS:
    type(ESMF_HConfig), intent(in) :: HConfig1
    type(ESMF_HConfig), intent(in) :: HConfig2

! !DESCRIPTION:
!   Test if both {\tt HConfig1} and {\tt HConfig2} alias the same
!   ESMF HConfig object.
!
!EOPI
!-------------------------------------------------------------------------------

    ESMF_HConfigNE = .not.ESMF_HConfigEQ(HConfig1, HConfig2)

  end function ESMF_HConfigNE
!-------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreate()"
!BOP
! !IROUTINE: ESMF_HConfigCreate - Create HConfig object

! !INTERFACE:
  function ESMF_HConfigCreate(keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreate
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create a new HConfig.
!
!     The arguments are:
!     \begin{description}
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code
    type(ESMF_HConfig)    :: hconfig        ! opaque pointer to new C++ HConfig

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    hconfig%shallowMemory = 0
    ESMF_HConfigCreate = hconfig

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreate(hconfig, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_HConfigCreate = hconfig

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreate)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigDestroy()"
!BOP
! !IROUTINE: ESMF_HConfigDestroy - Release resources associated with a HConfig 

! !INTERFACE:
  subroutine ESMF_HConfigDestroy(hconfig, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(inout)          :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
     integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Destroys an {\tt ESMF\_HConfig}, releasing the resources associated
!   with the object.
!
!   By default a small remnant of the object is kept in memory in order to 
!   prevent problems with dangling aliases. The default garbage collection
!   mechanism can be overridden with the {\tt noGarbage} argument.
!
! The arguments are:
! \begin{description}
! \item[hconfig] 
!      {\tt ESMF\_HConfig} object to be destroyed.
! \item[{[rc]}] 
!      Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigDestroy(hconfig, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Mark this HConfig as invalid
    hconfig%shallowMemory = 0

    ! Set init code
    ESMF_INIT_SET_DELETED(hconfig)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_HConfigDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigLoad()"
!BOP
! !IROUTINE: ESMF_HConfigLoad - Load string into HConfig

! !INTERFACE:
  subroutine ESMF_HConfigLoad(hconfig, content, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(len=*),   intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Load string into HConfig.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[content]}]
!     String containing the YAML text.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigLoad(hconfig, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_HConfigLoad
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigLoadFile()"
!BOP
! !IROUTINE: ESMF_HConfigLoadFile - Load file into HConfig

! !INTERFACE:
  subroutine ESMF_HConfigLoadFile(hconfig, filename, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(len=*),   intent(in)            :: filename
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Load string into HConfig.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[filename]}]
!     Name of the YAML file to be loaded.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigLoadFile(hconfig, filename, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_HConfigLoadFile
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateAt()"
!BOP
! !IROUTINE: ESMF_HConfigCreateAt - Return HConfig object at location

! !INTERFACE:
  function ESMF_HConfigCreateAt(hconfig, keywordEnforcer, index, key, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateAt
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: key
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create a new HConfig.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt key}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAt%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Check mutual exclusion of index and key
    if (present(index) .and. present(key)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="The 'index' and 'key' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key)) then
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtKey(hconfig, ESMF_HConfigCreateAt, key, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAt(hconfig, ESMF_HConfigCreateAt, index, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateAt)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateAtMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigCreateAtMapKey - Return HConfig object at location

! !INTERFACE:
  function ESMF_HConfigCreateAtMapKey(hconfig, keywordEnforcer, index, key, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateAtMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: key
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create a new HConfig.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt key}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAtMapKey%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Check mutual exclusion of index and key
    if (present(index) .and. present(key)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="The 'index' and 'key' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key)) then
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapKeyKey(hconfig, ESMF_HConfigCreateAtMapKey, key, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapKey(hconfig, ESMF_HConfigCreateAtMapKey, index, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateAtMapKey)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateAtMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigCreateAtMapVal - Return HConfig object at location

! !INTERFACE:
  function ESMF_HConfigCreateAtMapVal(hconfig, keywordEnforcer, index, key, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateAtMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: key
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!     Create a new HConfig.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt key}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAtMapVal%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Check mutual exclusion of index and key
    if (present(index) .and. present(key)) then
      call ESMF_LogSetError(ESMF_RC_ARG_BAD, &
        msg="The 'index' and 'key' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key)) then
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapValKey(hconfig, ESMF_HConfigCreateAtMapVal, key, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapVal(hconfig, ESMF_HConfigCreateAtMapVal, index, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateAtMapVal)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetSize()"
!BOP
! !IROUTINE: ESMF_HConfigGetSize - Get size of HConfig node

! !INTERFACE:
  function ESMF_HConfigGetSize(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetSize
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return sise of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetSize = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigGetSize(hconfig, ESMF_HConfigGetSize, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetMapKeySize()"
!BOP
! !IROUTINE: ESMF_HConfigGetMapKeySize - Get size of HConfig node

! !INTERFACE:
  function ESMF_HConfigGetMapKeySize(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetMapKeySize
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return sise of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetMapKeySize = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigGetMapKeySize(hconfig, ESMF_HConfigGetMapKeySize, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetMapValSize()"
!BOP
! !IROUTINE: ESMF_HConfigGetMapValSize - Get size of HConfig node

! !INTERFACE:
  function ESMF_HConfigGetMapValSize(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetMapValSize
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return sise of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetMapValSize = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigGetMapValSize(hconfig, ESMF_HConfigGetMapValSize, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsNull()"
!BOP
! !IROUTINE: ESMF_HConfigIsNull - Check whether HConfig node is Null

! !INTERFACE:
  function ESMF_HConfigIsNull(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsNull
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsNull = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsNull(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsNull = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsScalar()"
!BOP
! !IROUTINE: ESMF_HConfigIsScalar - Check whether HConfig node is Scalar

! !INTERFACE:
  function ESMF_HConfigIsScalar(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsScalar
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsScalar = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsScalar(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsScalar = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsSequence()"
!BOP
! !IROUTINE: ESMF_HConfigIsSequence - Check whether HConfig node is Sequence

! !INTERFACE:
  function ESMF_HConfigIsSequence(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsSequence
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsSequence = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsSequence(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsSequence = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMap()"
!BOP
! !IROUTINE: ESMF_HConfigIsMap - Check whether HConfig node is Map

! !INTERFACE:
  function ESMF_HConfigIsMap(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMap
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMap = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMap(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMap = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsDefined()"
!BOP
! !IROUTINE: ESMF_HConfigIsDefined - Check whether HConfig node is Defined

! !INTERFACE:
  function ESMF_HConfigIsDefined(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsDefined
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsDefined = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsDefined(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsDefined = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapKeyNull()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapKeyNull - Check whether HConfig node is Null

! !INTERFACE:
  function ESMF_HConfigIsMapKeyNull(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapKeyNull
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapKeyNull = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapKeyNull(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapKeyNull = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapKeyScalar()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapKeyScalar - Check whether HConfig node is Scalar

! !INTERFACE:
  function ESMF_HConfigIsMapKeyScalar(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapKeyScalar
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapKeyScalar = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapKeyScalar(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapKeyScalar = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapKeySequence()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapKeySequence - Check whether HConfig node is Sequence

! !INTERFACE:
  function ESMF_HConfigIsMapKeySequence(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapKeySequence
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapKeySequence = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapKeySequence(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapKeySequence = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapKeyMap()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapKeyMap - Check whether HConfig node is Map

! !INTERFACE:
  function ESMF_HConfigIsMapKeyMap(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapKeyMap
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapKeyMap = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapKeyMap(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapKeyMap = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapKeyDefined()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapKeyDefined - Check whether HConfig node is Defined

! !INTERFACE:
  function ESMF_HConfigIsMapKeyDefined(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapKeyDefined
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapKeyDefined = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapKeyDefined(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapKeyDefined = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapValNull()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapValNull - Check whether HConfig node is Null

! !INTERFACE:
  function ESMF_HConfigIsMapValNull(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapValNull
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapValNull = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapValNull(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapValNull = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapValScalar()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapValScalar - Check whether HConfig node is Scalar

! !INTERFACE:
  function ESMF_HConfigIsMapValScalar(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapValScalar
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapValScalar = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapValScalar(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapValScalar = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapValSequence()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapValSequence - Check whether HConfig node is Sequence

! !INTERFACE:
  function ESMF_HConfigIsMapValSequence(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapValSequence
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapValSequence = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapValSequence(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapValSequence = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapValMap()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapValMap - Check whether HConfig node is Map

! !INTERFACE:
  function ESMF_HConfigIsMapValMap(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapValMap
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapValMap = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapValMap(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapValMap = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapValDefined()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapValDefined - Check whether HConfig node is Defined

! !INTERFACE:
  function ESMF_HConfigIsMapValDefined(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapValDefined
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapValDefined = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapValDefined(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapValDefined = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsIterator()"
!BOP
! !IROUTINE: ESMF_HConfigIsIterator - Check whether HConfig node is Iterator

! !INTERFACE:
  function ESMF_HConfigIsIterator(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsIterator
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsIterator = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsIterator(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsIterator = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsSequenceIterator()"
!BOP
! !IROUTINE: ESMF_HConfigIsSequenceIterator - Check whether HConfig node is Sequence Iterator

! !INTERFACE:
  function ESMF_HConfigIsSequenceIterator(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsSequenceIterator
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsSequenceIterator = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsSeqIterator(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsSequenceIterator = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapIterator()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapIterator - Check whether HConfig node is Map Iterator

! !INTERFACE:
  function ESMF_HConfigIsMapIterator(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapIterator
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Null. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapIterator = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIsMapIterator(hconfig, flag, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_HConfigIsMapIterator = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterBegin()"
!BOP
! !IROUTINE: ESMF_HConfigIterBegin - Iterator at the beginning

! !INTERFACE:
  function ESMF_HConfigIterBegin(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterBegin
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to the first item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterBegin%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterBegin(hconfig, ESMF_HConfigIterBegin, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterBegin)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterEnd()"
!BOP
! !IROUTINE: ESMF_HConfigIterEnd - Iterator at the end

! !INTERFACE:
  function ESMF_HConfigIterEnd(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterEnd
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to one past the last item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterEnd%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterEnd(hconfig, ESMF_HConfigIterEnd, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterEnd)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterMapKeyBegin()"
!BOP
! !IROUTINE: ESMF_HConfigIterMapKeyBegin - Iterator at the beginning

! !INTERFACE:
  function ESMF_HConfigIterMapKeyBegin(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterMapKeyBegin
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to the first item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterMapKeyBegin%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterMapKeyBegin(hconfig, ESMF_HConfigIterMapKeyBegin, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterMapKeyBegin)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterMapKeyEnd()"
!BOP
! !IROUTINE: ESMF_HConfigIterMapKeyEnd - Iterator at the end

! !INTERFACE:
  function ESMF_HConfigIterMapKeyEnd(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterMapKeyEnd
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to one past the last item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterMapKeyEnd%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterMapKeyEnd(hconfig, ESMF_HConfigIterMapKeyEnd, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterMapKeyEnd)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterMapValBegin()"
!BOP
! !IROUTINE: ESMF_HConfigIterMapValBegin - Iterator at the beginning

! !INTERFACE:
  function ESMF_HConfigIterMapValBegin(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterMapValBegin
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to the first item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterMapValBegin%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterMapValBegin(hconfig, ESMF_HConfigIterMapValBegin, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterMapValBegin)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterMapValEnd()"
!BOP
! !IROUTINE: ESMF_HConfigIterMapValEnd - Iterator at the end

! !INTERFACE:
  function ESMF_HConfigIterMapValEnd(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterMapValEnd
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return an iterator that points to one past the last item in {\tt hconfig}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigIterMapValEnd%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterMapValEnd(hconfig, ESMF_HConfigIterMapValEnd, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterMapValEnd)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterNext()"
!BOP
! !IROUTINE: ESMF_HConfigIterNext - Step iterator forward

! !INTERFACE:
  subroutine ESMF_HConfigIterNext(hconfig, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(inout)         :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Step iterator {\tt hconfig} one step forward.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object must be iterator.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterNext(hconfig, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsString()"
!BOP
! !IROUTINE: ESMF_HConfigAsString - Return value as string

! !INTERFACE:
  function ESMF_HConfigAsString(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsString
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as string.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get length
    call c_ESMC_HConfigAsStringLen(hconfig, len, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! correctly size the character allocation
    allocate(character(len=len)::ESMF_HConfigAsString)

    ! Call into the C++ interface to get the string
    call c_ESMC_HConfigAsString(hconfig, ESMF_HConfigAsString, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapKeyString()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapKeyString - Return value as string

! !INTERFACE:
  function ESMF_HConfigAsMapKeyString(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsMapKeyString
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as string.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get length
    call c_ESMC_HConfigAsMapKeyStringLen(hconfig, len, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! correctly size the character allocation
    allocate(character(len=len)::ESMF_HConfigAsMapKeyString)

    ! Call into the C++ interface to get the string
    call c_ESMC_HConfigAsMapKeyString(hconfig, ESMF_HConfigAsMapKeyString, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapValString()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapValString - Return value as string

! !INTERFACE:
  function ESMF_HConfigAsMapValString(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsMapValString
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as string.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get length
    call c_ESMC_HConfigAsMapValStringLen(hconfig, len, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! correctly size the character allocation
    allocate(character(len=len)::ESMF_HConfigAsMapValString)

    ! Call into the C++ interface to get the string
    call c_ESMC_HConfigAsMapValString(hconfig, ESMF_HConfigAsMapValString, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4()"
!BOP
! !IROUTINE: ESMF_HConfigAsI4 - Return value as I4

! !INTERFACE:
  function ESMF_HConfigAsI4(hconfig, keywordEnforcer, index, key, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4) :: ESMF_HConfigAsI4
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: key
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as I4.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt key}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(key)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, key=key, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfigTemp, ESMF_HConfigAsI4, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfig, ESMF_HConfigAsI4, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapKeyI4()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapKeyI4 - Return value as I4

! !INTERFACE:
  function ESMF_HConfigAsMapKeyI4(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4) :: ESMF_HConfigAsMapKeyI4
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as I4.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the I4
    call c_ESMC_HConfigAsMapKeyI4(hconfig, ESMF_HConfigAsMapKeyI4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapValI4()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapValI4 - Return value as I4

! !INTERFACE:
  function ESMF_HConfigAsMapValI4(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4) :: ESMF_HConfigAsMapValI4
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as I4.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the I4
    call c_ESMC_HConfigAsMapValI4(hconfig, ESMF_HConfigAsMapValI4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8()"
!BOP
! !IROUTINE: ESMF_HConfigAsI8 - Return value as I8

! !INTERFACE:
  function ESMF_HConfigAsI8(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8) :: ESMF_HConfigAsI8
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as I8.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the I8
    call c_ESMC_HConfigAsI8(hconfig, ESMF_HConfigAsI8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapKeyI8()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapKeyI8 - Return value as I8

! !INTERFACE:
  function ESMF_HConfigAsMapKeyI8(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8) :: ESMF_HConfigAsMapKeyI8
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as I8.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the I8
    call c_ESMC_HConfigAsMapKeyI8(hconfig, ESMF_HConfigAsMapKeyI8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapValI8()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapValI8 - Return value as I8

! !INTERFACE:
  function ESMF_HConfigAsMapValI8(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8) :: ESMF_HConfigAsMapValI8
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as I8.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the I8
    call c_ESMC_HConfigAsMapValI8(hconfig, ESMF_HConfigAsMapValI8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4()"
!BOP
! !IROUTINE: ESMF_HConfigAsR4 - Return value as R4

! !INTERFACE:
  function ESMF_HConfigAsR4(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4) :: ESMF_HConfigAsR4
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as R4.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the R4
    call c_ESMC_HConfigAsR4(hconfig, ESMF_HConfigAsR4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapKeyR4()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapKeyR4 - Return value as R4

! !INTERFACE:
  function ESMF_HConfigAsMapKeyR4(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4) :: ESMF_HConfigAsMapKeyR4
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as R4.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the R4
    call c_ESMC_HConfigAsMapKeyR4(hconfig, ESMF_HConfigAsMapKeyR4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapValR4()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapValR4 - Return value as R4

! !INTERFACE:
  function ESMF_HConfigAsMapValR4(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4) :: ESMF_HConfigAsMapValR4
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as R4.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the R4
    call c_ESMC_HConfigAsMapValR4(hconfig, ESMF_HConfigAsMapValR4, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8()"
!BOP
! !IROUTINE: ESMF_HConfigAsR8 - Return value as R8

! !INTERFACE:
  function ESMF_HConfigAsR8(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8) :: ESMF_HConfigAsR8
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as R8.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the R8
    call c_ESMC_HConfigAsR8(hconfig, ESMF_HConfigAsR8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapKeyR8()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapKeyR8 - Return value as R8

! !INTERFACE:
  function ESMF_HConfigAsMapKeyR8(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8) :: ESMF_HConfigAsMapKeyR8
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as R8.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the R8
    call c_ESMC_HConfigAsMapKeyR8(hconfig, ESMF_HConfigAsMapKeyR8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsMapValR8()"
!BOP
! !IROUTINE: ESMF_HConfigAsMapValR8 - Return value as R8

! !INTERFACE:
  function ESMF_HConfigAsMapValR8(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8) :: ESMF_HConfigAsMapValR8
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value interpreted as R8.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig] 
!     {\tt ESMF\_HConfig} object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface to get the R8
    call c_ESMC_HConfigAsMapValR8(hconfig, ESMF_HConfigAsMapValR8, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetInit"
!BOPI
! !IROUTINE: ESMF_HConfigGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_HConfigGetInit(hconfig)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_HConfigGetInit
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in), optional :: hconfig
!
! !DESCRIPTION:
!   Access deep object init code.
!
!   The arguments are:
!   \begin{description}
!   \item [hconfig]
!     HConfig object.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(hconfig)) then
      ESMF_HConfigGetInit = ESMF_INIT_GET(hconfig)
    else
      ESMF_HConfigGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_HConfigGetInit
!------------------------------------------------------------------------------

end module ESMF_HConfigMod
