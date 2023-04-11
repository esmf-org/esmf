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
  public ESMF_HConfigSaveFile

  public ESMF_HConfigCreateAt
  public ESMF_HConfigCreateAtMapKey
  public ESMF_HConfigCreateAtMapVal

  public ESMF_HConfigAdd
  public ESMF_HConfigAddMapKey
  public ESMF_HConfigAddMapVal

  public ESMF_HConfigGetDocCount

  public ESMF_HConfigGetSize
  public ESMF_HConfigGetSizeMapKey
  public ESMF_HConfigGetSizeMapVal

  public ESMF_HConfigGetTag
  public ESMF_HConfigGetTagMapKey
  public ESMF_HConfigGetTagMapVal

  public ESMF_HConfigIsNull
  public ESMF_HConfigIsScalar
  public ESMF_HConfigIsSequence
  public ESMF_HConfigIsMap
  public ESMF_HConfigIsDefined

  public ESMF_HConfigIsNullMapKey
  public ESMF_HConfigIsScalarMapKey
  public ESMF_HConfigIsSequenceMapKey
  public ESMF_HConfigIsMapMapKey
  public ESMF_HConfigIsDefinedMapKey

  public ESMF_HConfigIsNullMapVal
  public ESMF_HConfigIsScalarMapVal
  public ESMF_HConfigIsSequenceMapVal
  public ESMF_HConfigIsMapMapVal
  public ESMF_HConfigIsDefinedMapVal

  public ESMF_HConfigIsIterator
  public ESMF_HConfigIsSequenceIterator
  public ESMF_HConfigIsMapIterator

  public ESMF_HConfigIterBegin
  public ESMF_HConfigIterEnd
  public ESMF_HConfigIterBeginMapKey
  public ESMF_HConfigIterEndMapKey
  public ESMF_HConfigIterBeginMapVal
  public ESMF_HConfigIterEndMapVal
  public ESMF_HConfigIterNext

  public ESMF_HConfigAsString
  public ESMF_HConfigAsStringMapKey
  public ESMF_HConfigAsStringMapVal

  public ESMF_HConfigAsStringSeq
  public ESMF_HConfigAsStringSeqMapKey
  public ESMF_HConfigAsStringSeqMapVal

  public ESMF_HConfigAsLogical
  public ESMF_HConfigAsLogicalMapKey
  public ESMF_HConfigAsLogicalMapVal

  public ESMF_HConfigAsLogicalSeq
  public ESMF_HConfigAsLogicalSeqMapKey
  public ESMF_HConfigAsLogicalSeqMapVal

  public ESMF_HConfigAsI4
  public ESMF_HConfigAsI4MapKey
  public ESMF_HConfigAsI4MapVal

  public ESMF_HConfigAsI4Seq
  public ESMF_HConfigAsI4SeqMapKey
  public ESMF_HConfigAsI4SeqMapVal

  public ESMF_HConfigAsI8
  public ESMF_HConfigAsI8MapKey
  public ESMF_HConfigAsI8MapVal

  public ESMF_HConfigAsI8Seq
  public ESMF_HConfigAsI8SeqMapKey
  public ESMF_HConfigAsI8SeqMapVal

  public ESMF_HConfigAsR4
  public ESMF_HConfigAsR4MapKey
  public ESMF_HConfigAsR4MapVal

  public ESMF_HConfigAsR4Seq
  public ESMF_HConfigAsR4SeqMapKey
  public ESMF_HConfigAsR4SeqMapVal

  public ESMF_HConfigAsR8
  public ESMF_HConfigAsR8MapKey
  public ESMF_HConfigAsR8MapVal

  public ESMF_HConfigAsR8Seq
  public ESMF_HConfigAsR8SeqMapKey
  public ESMF_HConfigAsR8SeqMapVal

  public ESMF_HConfigSet
  public ESMF_HConfigSetMapKey
  public ESMF_HConfigSetMapVal

  public ESMF_HConfigRemove

! - ESMF-internal methods:
  public ESMF_HConfigGetInit
!EOPI
!------------------------------------------------------------------------------

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

  interface ESMF_HConfigCreate
    module procedure ESMF_HConfigCreateDefault
    module procedure ESMF_HConfigCreateHConfig
    module procedure ESMF_HConfigCreateLogical
    module procedure ESMF_HConfigCreateI4
    module procedure ESMF_HConfigCreateI8
    module procedure ESMF_HConfigCreateR4
    module procedure ESMF_HConfigCreateR8
    module procedure ESMF_HConfigCreateI4Seq
    module procedure ESMF_HConfigCreateI8Seq
    module procedure ESMF_HConfigCreateR4Seq
    module procedure ESMF_HConfigCreateR8Seq
  end interface

  interface ESMF_HConfigAdd
    module procedure ESMF_HConfigAddHConfig
    module procedure ESMF_HConfigAddString
    module procedure ESMF_HConfigAddLogical
    module procedure ESMF_HConfigAddI4
    module procedure ESMF_HConfigAddI8
    module procedure ESMF_HConfigAddR4
    module procedure ESMF_HConfigAddR8
  end interface

  interface ESMF_HConfigAddMapKey
    module procedure ESMF_HConfigAddMapKeyHConfig
    module procedure ESMF_HConfigAddMapKeyString
    module procedure ESMF_HConfigAddMapKeyLogical
    module procedure ESMF_HConfigAddMapKeyI4
    module procedure ESMF_HConfigAddMapKeyI8
    module procedure ESMF_HConfigAddMapKeyR4
    module procedure ESMF_HConfigAddMapKeyR8
  end interface

  interface ESMF_HConfigAddMapVal
    module procedure ESMF_HConfigAddMapValHConfig
    module procedure ESMF_HConfigAddMapValString
    module procedure ESMF_HConfigAddMapValLogical
    module procedure ESMF_HConfigAddMapValI4
    module procedure ESMF_HConfigAddMapValI8
    module procedure ESMF_HConfigAddMapValR4
    module procedure ESMF_HConfigAddMapValR8
  end interface

  interface ESMF_HConfigSet
    module procedure ESMF_HConfigSetHConfig
    module procedure ESMF_HConfigSetString
    module procedure ESMF_HConfigSetLogical
    module procedure ESMF_HConfigSetI4
    module procedure ESMF_HConfigSetI8
    module procedure ESMF_HConfigSetR4
    module procedure ESMF_HConfigSetR8
  end interface

  interface ESMF_HConfigSetMapKey
    module procedure ESMF_HConfigSetMapKeyHConfig
    module procedure ESMF_HConfigSetMapKeyString
    module procedure ESMF_HConfigSetMapKeyLogical
    module procedure ESMF_HConfigSetMapKeyI4
    module procedure ESMF_HConfigSetMapKeyI8
    module procedure ESMF_HConfigSetMapKeyR4
    module procedure ESMF_HConfigSetMapKeyR8
  end interface

  interface ESMF_HConfigSetMapVal
    module procedure ESMF_HConfigSetMapValHConfig
    module procedure ESMF_HConfigSetMapValString
    module procedure ESMF_HConfigSetMapValLogical
    module procedure ESMF_HConfigSetMapValI4
    module procedure ESMF_HConfigSetMapValI8
    module procedure ESMF_HConfigSetMapValR4
    module procedure ESMF_HConfigSetMapValR8
  end interface

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
#define ESMF_METHOD "ESMF_HConfigCreateDefault()"
!BOP
! !IROUTINE: ESMF_HConfigCreateDefault - Create HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateDefault(keywordEnforcer, content, filename, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateDefault
!
! !ARGUMENTS:
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),   intent(in),  optional :: content
    character(len=*),   intent(in),  optional :: filename
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object. The object is empty unless either the
!   {\tt content} or {\tt filename} argument is specified.
!
!   The arguments are:
!   \begin{description}
!   \item[{[content]}]
!     String containing the YAML text. Mutually exclusive with
!     {\tt filename} argument.
!   \item[{[filename]}]
!     Name of the YAML file to be loaded. Mutually exclusive with
!     {\tt content} argument.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
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
    ESMF_HConfigCreateDefault = hconfig

    if (present(content) .and. present(filename)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'content' and 'filename' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreate(hconfig, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_HConfigCreateDefault = hconfig

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateDefault)

    ! handle content and filename
    if (present(content)) then
      ! load content
      call ESMF_HConfigLoad(ESMF_HConfigCreateDefault, content=content, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else if (present(filename)) then
      ! load filename
      call ESMF_HConfigLoadFile(ESMF_HConfigCreateDefault, filename=filename, &
        rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateDefault
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateHConfig()"
!BOP
! !IROUTINE: ESMF_HConfigCreateHConfig - Create HConfig object from HConfig

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateHConfig(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateHConfig
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object from existing HConfig object as a deep copy.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     HConfig content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigCreateHConfig = ESMF_HConfigCreate(rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(ESMF_HConfigCreateHConfig, content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateHConfig
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateLogical()"
!BOP
! !IROUTINE: ESMF_HConfigCreateLogical - Create HConfig object from logical

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateLogical(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateLogical
!
! !ARGUMENTS:
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     Logical content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                       :: localrc        ! local return code
    character(len=:), allocatable :: sContent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    sContent = "False"
    if (content) sContent = "True"

    ESMF_HConfigCreateLogical = ESMF_HConfigCreate(content=sContent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateLogical
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateI4()"
!BOP
! !IROUTINE: ESMF_HConfigCreateI4 - Create HConfig object from I4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateI4(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateI4
!
! !ARGUMENTS:
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     I4 content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateI4%shallowMemory = 0

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreateI4(ESMF_HConfigCreateI4, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateI4)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateI8()"
!BOP
! !IROUTINE: ESMF_HConfigCreateI8 - Create HConfig object from I8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateI8(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateI8
!
! !ARGUMENTS:
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     I8 content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateI8%shallowMemory = 0

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreateI8(ESMF_HConfigCreateI8, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateI8)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateI8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateR4()"
!BOP
! !IROUTINE: ESMF_HConfigCreateR4 - Create HConfig object from R4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateR4(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateR4
!
! !ARGUMENTS:
    real(ESMF_KIND_R4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     R4 content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateR4%shallowMemory = 0

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreateR4(ESMF_HConfigCreateR4, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateR4)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateR8()"
!BOP
! !IROUTINE: ESMF_HConfigCreateR8 - Create HConfig object from R8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateR8(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateR8
!
! !ARGUMENTS:
    real(ESMF_KIND_R8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,             intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     R8 content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateR8%shallowMemory = 0

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_HConfigCreateR8(ESMF_HConfigCreateR8, content, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigCreateR8)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateI4Seq()"
!BOP
! !IROUTINE: ESMF_HConfigCreateI4Seq - Create HConfig seq object from I4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateI4Seq(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateI4Seq
!
! !ARGUMENTS:
    integer(ESMF_KIND_I4),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     Array of I4 content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code
    integer               :: count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateI4Seq%shallowMemory = 0

    count = size(content)
    if (count>0) then
      ! call into the C++ interface, which will sort out optional arguments
      call c_ESMC_HConfigCreateI4Seq(ESMF_HConfigCreateI4Seq, content(1), &
        count, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code
      ESMF_INIT_SET_CREATED(ESMF_HConfigCreateI4Seq)
    else
      ! empty hconfig
      ESMF_HConfigCreateI4Seq = ESMF_HConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateI4Seq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateI8Seq()"
!BOP
! !IROUTINE: ESMF_HConfigCreateI8Seq - Create HConfig seq object from I8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateI8Seq(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateI8Seq
!
! !ARGUMENTS:
    integer(ESMF_KIND_I8),  intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     Array of I8 content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code
    integer               :: count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateI8Seq%shallowMemory = 0

    count = size(content)
    if (count>0) then
      ! call into the C++ interface, which will sort out optional arguments
      call c_ESMC_HConfigCreateI8Seq(ESMF_HConfigCreateI8Seq, content(1), &
        count, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code
      ESMF_INIT_SET_CREATED(ESMF_HConfigCreateI8Seq)
    else
      ! empty hconfig
      ESMF_HConfigCreateI8Seq = ESMF_HConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateI8Seq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateR4Seq()"
!BOP
! !IROUTINE: ESMF_HConfigCreateR4Seq - Create HConfig seq object from R4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateR4Seq(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateR4Seq
!
! !ARGUMENTS:
    real(ESMF_KIND_R4),     intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     Array of R4 content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code
    integer               :: count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateR4Seq%shallowMemory = 0

    count = size(content)
    if (count>0) then
      ! call into the C++ interface, which will sort out optional arguments
      call c_ESMC_HConfigCreateR4Seq(ESMF_HConfigCreateR4Seq, content(1), &
        count, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code
      ESMF_INIT_SET_CREATED(ESMF_HConfigCreateR4Seq)
    else
      ! empty hconfig
      ESMF_HConfigCreateR4Seq = ESMF_HConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateR4Seq
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateR8Seq()"
!BOP
! !IROUTINE: ESMF_HConfigCreateR8Seq - Create HConfig seq object from R8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigCreate()
  function ESMF_HConfigCreateR8Seq(content, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateR8Seq
!
! !ARGUMENTS:
    real(ESMF_KIND_R8),     intent(in)            :: content(:)
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object.
!
!   The arguments are:
!   \begin{description}
!   \item[content]
!     Array of R8 content.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc        ! local return code
    integer               :: count

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateR8Seq%shallowMemory = 0

    count = size(content)
    if (count>0) then
      ! call into the C++ interface, which will sort out optional arguments
      call c_ESMC_HConfigCreateR8Seq(ESMF_HConfigCreateR8Seq, content(1), &
        count, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Set init code
      ESMF_INIT_SET_CREATED(ESMF_HConfigCreateR8Seq)
    else
      ! empty hconfig
      ESMF_HConfigCreateR8Seq = ESMF_HConfigCreate(rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_HConfigCreateR8Seq
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
!BOPI
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
!   Load YAML syntax string into HConfig.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     String containing the YAML text.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
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
  subroutine ESMF_HConfigLoadFile(hconfig, filename, keywordEnforcer, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(len=*),   intent(in)            :: filename
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Load YAML file into {\tt hconfig}.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[filename]
!     Name of the YAML file to be loaded.
!   \item[{[doc]}]
!     The doc index. If specified, only this single document is loaded from file.
!     The result is a single document {\tt hconfig} object.
!     Defaults to {\em all} docs.
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
    call c_ESMC_HConfigLoadFile(hconfig, filename, doc, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_HConfigLoadFile
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSaveFile()"
!BOP
! !IROUTINE: ESMF_HConfigSaveFile - Save HConfig to file

! !INTERFACE:
  subroutine ESMF_HConfigSaveFile(hconfig, filename, keywordEnforcer, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(len=*),   intent(in)            :: filename
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Save HConfig into YAML file. Only {\tt localPet == 0} does the writing.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[filename]
!     Name of the YAML file into which to save.
!   \item[{[doc]}]
!     The doc index. Defaults to {\em all} docs.
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
    call c_ESMC_HConfigSaveFile(hconfig, filename, doc, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_HConfigSaveFile
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigCreateAt()"
!BOP
! !IROUTINE: ESMF_HConfigCreateAt - Return HConfig object at location

! !INTERFACE:
  function ESMF_HConfigCreateAt(hconfig, keywordEnforcer, index, key, &
    keyString, doc, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateAt
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    type(ESMF_HConfig), intent(in),  optional :: key
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object at the current iteration, or
!   as specified by {\tt index}, {\tt key} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with
!     {\tt key} and {\tt keyString}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with
!     {\tt index} and {\tt keyString},
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with
!     {\tt index} and {\tt key}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAt%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Check mutual exclusions
    if ((present(index) .and. (present(key) .or. present(keyString))) &
      .or. (present(key) .and. present(keyString))) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'index', 'key', and 'keyString' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key) .or. present(keyString)) then
      if (present(keyString)) then
        hkey = ESMF_HConfigCreate(content=keyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, key, rc)
        hkey = key
      endif
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtKey(hconfig, ESMF_HConfigCreateAt, &
        hkey, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (present(keyString)) then
        call ESMF_HConfigDestroy(hkey, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAt(hconfig, ESMF_HConfigCreateAt, &
        index, doc, localrc)
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
  function ESMF_HConfigCreateAtMapKey(hconfig, keywordEnforcer, index, key, &
    keyString, doc, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateAtMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    type(ESMF_HConfig), intent(in),  optional :: key
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object for a map key at the current iteration, or
!   as specified by {\tt index}, {\tt key} or {\tt keyString}.
!   The {\tt hconfig} {\em must} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with
!     {\tt key} and {\tt keyString}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with
!     {\tt index} and {\tt keyString},
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with
!     {\tt index} and {\tt key}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAtMapKey%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Check mutual exclusions
    if ((present(index) .and. (present(key) .or. present(keyString))) &
      .or. (present(key) .and. present(keyString))) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'index', 'key', and 'keyString' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key) .or. present(keyString)) then
      if (present(keyString)) then
        hkey = ESMF_HConfigCreate(content=keyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, key, rc)
        hkey = key
      endif
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapKeyKey(hconfig, ESMF_HConfigCreateAtMapKey, &
        hkey, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (present(keyString)) then
        call ESMF_HConfigDestroy(hkey, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapKey(hconfig, ESMF_HConfigCreateAtMapKey, &
        index, doc, localrc)
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
  function ESMF_HConfigCreateAtMapVal(hconfig, keywordEnforcer, index, key, &
    keyString, doc, rc)
!
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigCreateAtMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    type(ESMF_HConfig), intent(in),  optional :: key
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new HConfig object for a map value at the current iteration, or
!   as specified by {\tt index}, {\tt key} or {\tt keyString}.
!   The {\tt hconfig} {\em must} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with
!     {\tt key} and {\tt keyString}.
!   \item[{[key]}]
!     Attempt to access by key if specified. Mutural exclusive with
!     {\tt index} and {\tt keyString},
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with
!     {\tt index} and {\tt key}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! invalidate return value
    ESMF_HConfigCreateAtMapVal%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Check mutual exclusions
    if ((present(index) .and. (present(key) .or. present(keyString))) &
      .or. (present(key) .and. present(keyString))) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'index', 'key', and 'keyString' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(key) .or. present(keyString)) then
      if (present(keyString)) then
        hkey = ESMF_HConfigCreate(content=keyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, key, rc)
        hkey = key
      endif
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapValKey(hconfig, ESMF_HConfigCreateAtMapVal, &
        hkey, doc, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (present(keyString)) then
        call ESMF_HConfigDestroy(hkey, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigCreateAtMapVal(hconfig, ESMF_HConfigCreateAtMapVal, &
        index, doc, localrc)
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
#define ESMF_METHOD "ESMF_HConfigAddHConfig()"
!BOP
! !IROUTINE: ESMF_HConfigAddHConfig - Add hconfig to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAdd()
  subroutine ESMF_HConfigAddHConfig(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add the content of a HConfig object to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp, hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(addKey).or.present(addKeyString)) then
      if (present(addKeyString)) then
        hkey = ESMF_HConfigCreate(content=addKeyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, addKey, rc)
        hkey = addKey
      endif
    endif

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKey(hconfigTemp, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAdd(hconfigTemp, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKey(hconfig, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAdd(hconfig, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    if (present(addKeyString)) then
      call ESMF_HConfigDestroy(hkey, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddString()"
!BOP
! !IROUTINE: ESMF_HConfigAddString - Add string to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAdd()
  subroutine ESMF_HConfigAddString(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add a string to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddLogical()"
!BOP
! !IROUTINE: ESMF_HConfigAddLogical - Add logical to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAdd()
  subroutine ESMF_HConfigAddLogical(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add a logical to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddI4()"
!BOP
! !IROUTINE: ESMF_HConfigAddI4 - Add I4 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAdd()
  subroutine ESMF_HConfigAddI4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an I4 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddI8()"
!BOP
! !IROUTINE: ESMF_HConfigAddI8 - Add I8 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAdd()
  subroutine ESMF_HConfigAddI8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an I8 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddR4()"
!BOP
! !IROUTINE: ESMF_HConfigAddR4 - Add R4 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAdd()
  subroutine ESMF_HConfigAddR4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an R4 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddR8()"
!BOP
! !IROUTINE: ESMF_HConfigAddR8 - Add R8 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAdd()
  subroutine ESMF_HConfigAddR8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an R8 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAdd(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyHConfig()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapKeyHConfig - Add hconfig to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapKey()
  subroutine ESMF_HConfigAddMapKeyHConfig(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add the content of a HConfig object to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp, hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(addKey).or.present(addKeyString)) then
      if (present(addKeyString)) then
        hkey = ESMF_HConfigCreate(content=addKeyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, addKey, rc)
        hkey = addKey
      endif
    endif

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKey(hconfigTemp, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAdd(hconfigTemp, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKeyMapKey(hconfig, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAddMapKey(hconfig, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    if (present(addKeyString)) then
      call ESMF_HConfigDestroy(hkey, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyString()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapKeyString - Add string to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapKey()
  subroutine ESMF_HConfigAddMapKeyString(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add a string to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyLogical()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapKeyLogical - Add logical to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapKey()
  subroutine ESMF_HConfigAddMapKeyLogical(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add a logical to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyI4()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapKeyI4 - Add I4 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapKey()
  subroutine ESMF_HConfigAddMapKeyI4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an I4 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyI8()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapKeyI8 - Add I8 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapKey()
  subroutine ESMF_HConfigAddMapKeyI8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an I8 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyR4()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapKeyR4 - Add R4 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapKey()
  subroutine ESMF_HConfigAddMapKeyR4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an R4 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapKeyR8()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapKeyR8 - Add R8 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapKey()
  subroutine ESMF_HConfigAddMapKeyR8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an R8 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapKey(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValHConfig()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapValHConfig - Add hconfig to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapVal()
  subroutine ESMF_HConfigAddMapValHConfig(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add the content of a HConfig object to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp, hKey

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(addKey).or.present(addKeyString)) then
      if (present(addKeyString)) then
        hkey = ESMF_HConfigCreate(content=addKeyString, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, addKey, rc)
        hkey = addKey
      endif
    endif

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKey(hconfigTemp, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAdd(hconfigTemp, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to add content
      if (present(addKey).or.present(addKeyString)) then
        call c_ESMC_HConfigAddKeyMapVal(hconfig, content, hkey, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        call c_ESMC_HConfigAddMapVal(hconfig, content, localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      endif
    endif

    if (present(addKeyString)) then
      call ESMF_HConfigDestroy(hkey, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValString()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapValString - Add string to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapVal()
  subroutine ESMF_HConfigAddMapValString(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add a string to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValLogical()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapValLogical - Add logical to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapVal()
  subroutine ESMF_HConfigAddMapValLogical(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add a logical to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValI4()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapValI4 - Add I4 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapVal()
  subroutine ESMF_HConfigAddMapValI4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an I4 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValI8()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapValI8 - Add I8 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapVal()
  subroutine ESMF_HConfigAddMapValI8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig),     intent(in),  optional :: addKey
    character(*),           intent(in),  optional :: addKeyString
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an I8 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValR4()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapValR4 - Add R4 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapVal()
  subroutine ESMF_HConfigAddMapValR4(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an R4 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAddMapValR8()"
!BOP
! !IROUTINE: ESMF_HConfigAddMapValR8 - Add R8 to HConfig object

! !INTERFACE:
  ! Private name; call using ESMF_HConfigAddMapVal()
  subroutine ESMF_HConfigAddMapValR8(hconfig, content, keywordEnforcer, &
    addKey, addKeyString, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    type(ESMF_HConfig), intent(in),  optional :: addKey
    character(*),       intent(in),  optional :: addKeyString
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Add an R8 to the current iteration, or
!   as specified by {\tt index} or {\tt keyString} (mutually exclusive!).
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   If either {\tt addKey} or {\tt addKeyString} (mutually exclusive!) is
!   specified, then add a new map element with the respective {\em key}.
!   Otherwise add a new list element at the end of the list. Error checking
!   is implemented to ensure respective conditions are met.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be added.
!   \item[{[addKey]}]
!     The key under which to add the new map item.
!     Mutural exclusive with {\tt addKeyString}.
!   \item[{[addKeyString]}]
!     The key string under which to add the new map item.
!     Mutural exclusive with {\tt addKey}.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigAddMapVal(hconfig, hcontent, &
      addKey=addKey, addKeyString=addKeyString, &
      index=index, keyString=keyString, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetDocCount()"
!BOP
! !IROUTINE: ESMF_HConfigGetDocCount - Get number of docs in HConfig

! !INTERFACE:
  function ESMF_HConfigGetDocCount(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetDocCount
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the number of documents held by {\tt hconfig}.
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

    ESMF_HConfigGetDocCount = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigGetDocCount(hconfig, ESMF_HConfigGetDocCount, localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
  function ESMF_HConfigGetSize(hconfig, keywordEnforcer, index, keyString, doc, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetSize
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return size of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: size
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetSize = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSize(hconfigTemp, ESMF_HConfigGetSize, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSize(hconfig, ESMF_HConfigGetSize, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetSizeMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigGetSizeMapKey - Get size of HConfig node

! !INTERFACE:
  function ESMF_HConfigGetSizeMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetSizeMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return size of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: size
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetSizeMapKey = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSize(hconfigTemp, ESMF_HConfigGetSizeMapKey, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSizeMapKey(hconfig, ESMF_HConfigGetSizeMapKey, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetSizeMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigGetSizeMapVal - Get size of HConfig node

! !INTERFACE:
  function ESMF_HConfigGetSizeMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    integer :: ESMF_HConfigGetSizeMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return size of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: size
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigGetSizeMapVal = 0   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSize(hconfigTemp, ESMF_HConfigGetSizeMapVal, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigGetSizeMapVal(hconfig, ESMF_HConfigGetSizeMapVal, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetTag()"
!BOP
! !IROUTINE: ESMF_HConfigGetTag - Get tag of HConfig node

! !INTERFACE:
  function ESMF_HConfigGetTag(hconfig, keywordEnforcer, index, keyString, doc, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigGetTag
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return tag string of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagLen(hconfigTemp, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTag)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTag(hconfigTemp, ESMF_HConfigGetTag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagLen(hconfig, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTag)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTag(hconfig, ESMF_HConfigGetTag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetTagMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigGetTagMapKey - Get tag of map key node

! !INTERFACE:
  function ESMF_HConfigGetTagMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigGetTagMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return tag string of map key of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagLen(hconfigTemp, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTagMapKey)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTag(hconfigTemp, ESMF_HConfigGetTagMapKey, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagMapKeyLen(hconfig, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTagMapKey)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTagMapKey(hconfig, ESMF_HConfigGetTagMapKey, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigGetTagMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigGetTagMapVal - Get tag of map key node

! !INTERFACE:
  function ESMF_HConfigGetTagMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigGetTagMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return tag string of map key of the {\tt hconfig} node.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagLen(hconfigTemp, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTagMapVal)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTag(hconfigTemp, ESMF_HConfigGetTagMapVal, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigGetTagMapValLen(hconfig, len, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigGetTagMapVal)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigGetTagMapVal(hconfig, ESMF_HConfigGetTagMapVal, &
        localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

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
  function ESMF_HConfigIsNull(hconfig, keywordEnforcer, index, keyString, doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsNull
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsNull = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNull(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNull(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
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
  function ESMF_HConfigIsScalar(hconfig, keywordEnforcer, index, keyString, doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsScalar
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Scalar. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsScalar = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalar(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalar(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
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
  function ESMF_HConfigIsSequence(hconfig, keywordEnforcer, index, keyString, doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsSequence
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Sequence. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsSequence = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequence(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequence(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
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
  function ESMF_HConfigIsMap(hconfig, keywordEnforcer, index, keyString, doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMap
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Map. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMap = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMap(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMap(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
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
  function ESMF_HConfigIsDefined(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsDefined
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt hconfig} node is Defined. Otherwise return
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsDefined = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefined(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefined(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsDefined = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsNullMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIsNullMapKey - Check whether HConfig node is Null

! !INTERFACE:
  function ESMF_HConfigIsNullMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsNullMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsNullMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNull(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNullMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsNullMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsScalarMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIsScalarMapKey - Check whether HConfig node is Scalar

! !INTERFACE:
  function ESMF_HConfigIsScalarMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsScalarMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsScalarMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalar(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalarMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsScalarMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsSequenceMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIsSequenceMapKey - Check whether HConfig node is Sequence

! !INTERFACE:
  function ESMF_HConfigIsSequenceMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsSequenceMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsSequenceMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequence(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequenceMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsSequenceMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapMapKey - Check whether HConfig node is Map

! !INTERFACE:
  function ESMF_HConfigIsMapMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMap(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMapMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsMapMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsDefinedMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIsDefinedMapKey - Check whether HConfig node is Defined

! !INTERFACE:
  function ESMF_HConfigIsDefinedMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsDefinedMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsDefinedMapKey = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefined(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefinedMapKey(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsDefinedMapKey = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsNullMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIsNullMapVal - Check whether HConfig node is Null

! !INTERFACE:
  function ESMF_HConfigIsNullMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsNullMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsNullMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNull(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsNullMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsNullMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsScalarMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIsScalarMapVal - Check whether HConfig node is Scalar

! !INTERFACE:
  function ESMF_HConfigIsScalarMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsScalarMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsScalarMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalar(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsScalarMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsScalarMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsSequenceMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIsSequenceMapVal - Check whether HConfig node is Sequence

! !INTERFACE:
  function ESMF_HConfigIsSequenceMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsSequenceMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsSequenceMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequence(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsSequenceMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsSequenceMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsMapMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIsMapMapVal - Check whether HConfig node is Map

! !INTERFACE:
  function ESMF_HConfigIsMapMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsMapMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsMapMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMap(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsMapMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsMapMapVal = flag

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIsDefinedMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIsDefinedMapVal - Check whether HConfig node is Defined

! !INTERFACE:
  function ESMF_HConfigIsDefinedMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigIsDefinedMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
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
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_Logical)    :: flag
    type(ESMF_HConfig)    :: hconfigTemp

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_HConfigIsDefinedMapVal = .false.   ! initialize

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefined(hconfigTemp, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface, which will sort out optional arguments.
      call c_ESMC_HConfigIsDefinedMapVal(hconfig, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigIsDefinedMapVal = flag

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
#define ESMF_METHOD "ESMF_HConfigIterBeginMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIterBeginMapKey - Iterator at the beginning

! !INTERFACE:
  function ESMF_HConfigIterBeginMapKey(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterBeginMapKey
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
    ESMF_HConfigIterBeginMapKey%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterBeginMapKey(hconfig, ESMF_HConfigIterBeginMapKey, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterBeginMapKey)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterEndMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigIterEndMapKey - Iterator at the end

! !INTERFACE:
  function ESMF_HConfigIterEndMapKey(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterEndMapKey
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
    ESMF_HConfigIterEndMapKey%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterEndMapKey(hconfig, ESMF_HConfigIterEndMapKey, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterEndMapKey)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterBeginMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIterBeginMapVal - Iterator at the beginning

! !INTERFACE:
  function ESMF_HConfigIterBeginMapVal(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterBeginMapVal
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
    ESMF_HConfigIterBeginMapVal%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterBeginMapVal(hconfig, ESMF_HConfigIterBeginMapVal, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterBeginMapVal)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigIterEndMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigIterEndMapVal - Iterator at the end

! !INTERFACE:
  function ESMF_HConfigIterEndMapVal(hconfig, keywordEnforcer, rc)
! !RETURN VALUE:
    type(ESMF_HConfig) :: ESMF_HConfigIterEndMapVal
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
    ESMF_HConfigIterEndMapVal%shallowMemory = 0

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_HConfigIterEndMapVal(hconfig, ESMF_HConfigIterEndMapVal, &
      localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_HConfigIterEndMapVal)

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
  function ESMF_HConfigAsString(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsString
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as string.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringLen(hconfigTemp, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsString)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsString(hconfigTemp, ESMF_HConfigAsString, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringLen(hconfig, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsString)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsString(hconfig, ESMF_HConfigAsString, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsStringMapKey - Return map key as string

! !INTERFACE:
  function ESMF_HConfigAsStringMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsStringMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as string.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringLen(hconfigTemp, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsStringMapKey)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsString(hconfigTemp, ESMF_HConfigAsStringMapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringMapKeyLen(hconfig, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsStringMapKey)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsStringMapKey(hconfig, ESMF_HConfigAsStringMapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsStringMapVal - Return map value as string

! !INTERFACE:
  function ESMF_HConfigAsStringMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsStringMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as string.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    integer               :: len
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringLen(hconfigTemp, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsStringMapVal)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsString(hconfigTemp, ESMF_HConfigAsStringMapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get length
      call c_ESMC_HConfigAsStringMapValLen(hconfig, len, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return

      ! correctly size the character allocation
      allocate(character(len=len)::ESMF_HConfigAsStringMapVal)

      ! Call into the C++ interface to get the string
      call c_ESMC_HConfigAsStringMapVal(hconfig, ESMF_HConfigAsStringMapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringSeq()"
!BOP
! !IROUTINE: ESMF_HConfigAsStringSeq - Return Seq array of String values

! !INTERFACE:
  function ESMF_HConfigAsStringSeq(hconfig, stringLen, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsStringSeq(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    integer,            intent(in)            :: stringLen
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as String.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[stringLen]
!     Number of characters in each of the output strings. Longer actual strings
!     are tuncated, while shorter actual strings are padded with white space.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(character(len=stringLen)::ESMF_HConfigAsStringSeq(1))
      ESMF_HConfigAsStringSeq(1) = ESMF_HConfigAsString(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(character(len=stringLen)::ESMF_HConfigAsStringSeq(size))
        do i=1, size
          ESMF_HConfigAsStringSeq(i) = ESMF_HConfigAsString(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringSeqMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsStringSeqMapKey - Return Seq array map key as String

! !INTERFACE:
  function ESMF_HConfigAsStringSeqMapKey(hconfig, stringLen, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsStringSeqMapKey(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    integer,            intent(in)            :: stringLen
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as String.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[stringLen]
!     Number of characters in each of the output strings. Longer actual strings
!     are tuncated, while shorter actual strings are padded with white space.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(character(len=stringLen)::ESMF_HConfigAsStringSeqMapKey(1))
      ESMF_HConfigAsStringSeqMapKey(1) = ESMF_HConfigAsStringMapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(character(len=stringLen)::ESMF_HConfigAsStringSeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsStringSeqMapKey(i) = ESMF_HConfigAsString(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsStringSeqMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsStringSeqMapVal - Return Seq array map value as String

! !INTERFACE:
  function ESMF_HConfigAsStringSeqMapVal(hconfig, stringLen, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    character(len=:), allocatable :: ESMF_HConfigAsStringSeqMapVal(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    integer,            intent(in)            :: stringLen
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as String.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[stringLen]
!     Number of characters in each of the output strings. Longer actual strings
!     are tuncated, while shorter actual strings are padded with white space.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(character(len=stringLen)::ESMF_HConfigAsStringSeqMapVal(1))
      ESMF_HConfigAsStringSeqMapVal(1) = ESMF_HConfigAsStringMapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(character(len=stringLen)::ESMF_HConfigAsStringSeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsStringSeqMapVal(i) = ESMF_HConfigAsString(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogical()"
!BOP
! !IROUTINE: ESMF_HConfigAsLogical - Return value as Logical

! !INTERFACE:
  function ESMF_HConfigAsLogical(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigAsLogical
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as Logical.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: value
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogical(hconfigTemp, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogical(hconfig, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigAsLogical = value

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsLogicalMapKey - Return map key as Logical

! !INTERFACE:
  function ESMF_HConfigAsLogicalMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigAsLogicalMapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as Logical.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: value
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogical(hconfigTemp, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogicalMapKey(hconfig, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigAsLogicalMapKey = value

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsLogicalMapVal - Return map value as Logical

! !INTERFACE:
  function ESMF_HConfigAsLogicalMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    logical :: ESMF_HConfigAsLogicalMapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as Logical.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: value
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogical(hconfigTemp, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the Logical
      call c_ESMC_HConfigAsLogicalMapVal(hconfig, value, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! convert ESMF_Logical -> logical
    ESMF_HConfigAsLogicalMapVal = value

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalSeq()"
!BOP
! !IROUTINE: ESMF_HConfigAsLogicalSeq - Return Seq array of Logical values

! !INTERFACE:
  function ESMF_HConfigAsLogicalSeq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    logical, allocatable :: ESMF_HConfigAsLogicalSeq(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as Logical.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsLogicalSeq(1))
      ESMF_HConfigAsLogicalSeq(1) = ESMF_HConfigAsLogical(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsLogicalSeq(size))
        do i=1, size
          ESMF_HConfigAsLogicalSeq(i) = ESMF_HConfigAsLogical(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalSeqMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsLogicalSeqMapKey - Return Seq array map key as Logical

! !INTERFACE:
  function ESMF_HConfigAsLogicalSeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    logical, allocatable :: ESMF_HConfigAsLogicalSeqMapKey(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as Logical.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsLogicalSeqMapKey(1))
      ESMF_HConfigAsLogicalSeqMapKey(1) = ESMF_HConfigAsLogicalMapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsLogicalSeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsLogicalSeqMapKey(i) = ESMF_HConfigAsLogical(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsLogicalSeqMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsLogicalSeqMapVal - Return Seq array map value as Logical

! !INTERFACE:
  function ESMF_HConfigAsLogicalSeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    logical, allocatable :: ESMF_HConfigAsLogicalSeqMapVal(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as Logical.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsLogicalSeqMapVal(1))
      ESMF_HConfigAsLogicalSeqMapVal(1) = ESMF_HConfigAsLogicalMapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsLogicalSeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsLogicalSeqMapVal(i) = ESMF_HConfigAsLogical(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

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
  function ESMF_HConfigAsI4(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4) :: ESMF_HConfigAsI4
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as I4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfigTemp, ESMF_HConfigAsI4, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfig, ESMF_HConfigAsI4, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4MapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsI4MapKey - Return map key as I4

! !INTERFACE:
  function ESMF_HConfigAsI4MapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4) :: ESMF_HConfigAsI4MapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as I4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfigTemp, ESMF_HConfigAsI4MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4MapKey(hconfig, ESMF_HConfigAsI4MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4MapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsI4MapVal - Return map value as I4

! !INTERFACE:
  function ESMF_HConfigAsI4MapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4) :: ESMF_HConfigAsI4MapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as I4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4(hconfigTemp, ESMF_HConfigAsI4MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I4
      call c_ESMC_HConfigAsI4MapVal(hconfig, ESMF_HConfigAsI4MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4Seq()"
!BOP
! !IROUTINE: ESMF_HConfigAsI4Seq - Return Seq array of value as I4

! !INTERFACE:
  function ESMF_HConfigAsI4Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4), allocatable :: ESMF_HConfigAsI4Seq(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as I4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI4Seq(1))
      ESMF_HConfigAsI4Seq(1) = ESMF_HConfigAsI4(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI4Seq(size))
        do i=1, size
          ESMF_HConfigAsI4Seq(i) = ESMF_HConfigAsI4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4SeqMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsI4SeqMapKey - Return Seq array of map key as I4

! !INTERFACE:
  function ESMF_HConfigAsI4SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4), allocatable :: ESMF_HConfigAsI4SeqMapKey(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as I4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI4SeqMapKey(1))
      ESMF_HConfigAsI4SeqMapKey(1) = ESMF_HConfigAsI4MapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI4SeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsI4SeqMapKey(i) = ESMF_HConfigAsI4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI4SeqMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsI4SeqMapVal - Return Seq arraay of map value as I4

! !INTERFACE:
  function ESMF_HConfigAsI4SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I4), allocatable :: ESMF_HConfigAsI4SeqMapVal(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as I4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI4SeqMapVal(1))
      ESMF_HConfigAsI4SeqMapVal(1) = ESMF_HConfigAsI4MapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI4SeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsI4SeqMapVal(i) = ESMF_HConfigAsI4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

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
  function ESMF_HConfigAsI8(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8) :: ESMF_HConfigAsI8
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as I8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8(hconfigTemp, ESMF_HConfigAsI8, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8(hconfig, ESMF_HConfigAsI8, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8MapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsI8MapKey - Return map key as I8

! !INTERFACE:
  function ESMF_HConfigAsI8MapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8) :: ESMF_HConfigAsI8MapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as I8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8(hconfigTemp, ESMF_HConfigAsI8MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8MapKey(hconfig, ESMF_HConfigAsI8MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8MapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsI8MapVal - Return map value as I8

! !INTERFACE:
  function ESMF_HConfigAsI8MapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8) :: ESMF_HConfigAsI8MapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as I8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8(hconfigTemp, ESMF_HConfigAsI8MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the I8
      call c_ESMC_HConfigAsI8MapVal(hconfig, ESMF_HConfigAsI8MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8Seq()"
!BOP
! !IROUTINE: ESMF_HConfigAsI8Seq - Return Seq array of value as I8

! !INTERFACE:
  function ESMF_HConfigAsI8Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8), allocatable :: ESMF_HConfigAsI8Seq(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as I8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI8Seq(1))
      ESMF_HConfigAsI8Seq(1) = ESMF_HConfigAsI8(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI8Seq(size))
        do i=1, size
          ESMF_HConfigAsI8Seq(i) = ESMF_HConfigAsI8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8SeqMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsI8SeqMapKey - Return Seq array of map key as I8

! !INTERFACE:
  function ESMF_HConfigAsI8SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8), allocatable :: ESMF_HConfigAsI8SeqMapKey(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as I8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI8SeqMapKey(1))
      ESMF_HConfigAsI8SeqMapKey(1) = ESMF_HConfigAsI8MapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI8SeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsI8SeqMapKey(i) = ESMF_HConfigAsI8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsI8SeqMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsI8SeqMapVal - Return Seq arraay of map value as I8

! !INTERFACE:
  function ESMF_HConfigAsI8SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    integer(ESMF_KIND_I8), allocatable :: ESMF_HConfigAsI8SeqMapVal(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as I8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsI8SeqMapVal(1))
      ESMF_HConfigAsI8SeqMapVal(1) = ESMF_HConfigAsI8MapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsI8SeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsI8SeqMapVal(i) = ESMF_HConfigAsI8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

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
  function ESMF_HConfigAsR4(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4) :: ESMF_HConfigAsR4
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as R4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4(hconfigTemp, ESMF_HConfigAsR4, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4(hconfig, ESMF_HConfigAsR4, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4MapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsR4MapKey - Return map key as R4

! !INTERFACE:
  function ESMF_HConfigAsR4MapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4) :: ESMF_HConfigAsR4MapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as R4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4(hconfigTemp, ESMF_HConfigAsR4MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4MapKey(hconfig, ESMF_HConfigAsR4MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4MapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsR4MapVal - Return map value as R4

! !INTERFACE:
  function ESMF_HConfigAsR4MapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4) :: ESMF_HConfigAsR4MapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as R4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4(hconfigTemp, ESMF_HConfigAsR4MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R4
      call c_ESMC_HConfigAsR4MapVal(hconfig, ESMF_HConfigAsR4MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4Seq()"
!BOP
! !IROUTINE: ESMF_HConfigAsR4Seq - Return Seq array of value as R4

! !INTERFACE:
  function ESMF_HConfigAsR4Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4), allocatable :: ESMF_HConfigAsR4Seq(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as R4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR4Seq(1))
      ESMF_HConfigAsR4Seq(1) = ESMF_HConfigAsR4(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR4Seq(size))
        do i=1, size
          ESMF_HConfigAsR4Seq(i) = ESMF_HConfigAsR4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4SeqMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsR4SeqMapKey - Return Seq array of map key as R4

! !INTERFACE:
  function ESMF_HConfigAsR4SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4), allocatable :: ESMF_HConfigAsR4SeqMapKey(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as R4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR4SeqMapKey(1))
      ESMF_HConfigAsR4SeqMapKey(1) = ESMF_HConfigAsR4MapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR4SeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsR4SeqMapKey(i) = ESMF_HConfigAsR4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR4SeqMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsR4SeqMapVal - Return Seq arraay of map value as R4

! !INTERFACE:
  function ESMF_HConfigAsR4SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R4), allocatable :: ESMF_HConfigAsR4SeqMapVal(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as R4.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR4SeqMapVal(1))
      ESMF_HConfigAsR4SeqMapVal(1) = ESMF_HConfigAsR4MapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR4SeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsR4SeqMapVal(i) = ESMF_HConfigAsR4(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

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
  function ESMF_HConfigAsR8(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8) :: ESMF_HConfigAsR8
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as R8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8(hconfigTemp, ESMF_HConfigAsR8, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8(hconfig, ESMF_HConfigAsR8, flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8MapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsR8MapKey - Return map key as R8

! !INTERFACE:
  function ESMF_HConfigAsR8MapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8) :: ESMF_HConfigAsR8MapKey
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as R8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8(hconfigTemp, ESMF_HConfigAsR8MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8MapKey(hconfig, ESMF_HConfigAsR8MapKey, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8MapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsR8MapVal - Return map value as R8

! !INTERFACE:
  function ESMF_HConfigAsR8MapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8) :: ESMF_HConfigAsR8MapVal
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as R8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hconfigTemp
    type(ESMF_Logical)    :: flag

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8(hconfigTemp, ESMF_HConfigAsR8MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to get the R8
      call c_ESMC_HConfigAsR8MapVal(hconfig, ESMF_HConfigAsR8MapVal, &
        flag, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! handle asOkay
    if (present(asOkay)) then
      asOkay = flag
    else if (flag == ESMF_FALSE) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Conversion to the requested typekind is not supported", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8Seq()"
!BOP
! !IROUTINE: ESMF_HConfigAsR8Seq - Return Seq array of value as R8

! !INTERFACE:
  function ESMF_HConfigAsR8Seq(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8), allocatable :: ESMF_HConfigAsR8Seq(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the value of item {\tt hconfig} interpreted as R8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalar(hconfig, index=index, keyString=keyString, &
      doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR8Seq(1))
      ESMF_HConfigAsR8Seq(1) = ESMF_HConfigAsR8(hconfig, index=index, &
        keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequence(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR8Seq(size))
        do i=1, size
          ESMF_HConfigAsR8Seq(i) = ESMF_HConfigAsR8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8SeqMapKey()"
!BOP
! !IROUTINE: ESMF_HConfigAsR8SeqMapKey - Return Seq array of map key as R8

! !INTERFACE:
  function ESMF_HConfigAsR8SeqMapKey(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8), allocatable :: ESMF_HConfigAsR8SeqMapKey(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map key of item {\tt hconfig} interpreted as R8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapKey(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR8SeqMapKey(1))
      ESMF_HConfigAsR8SeqMapKey(1) = ESMF_HConfigAsR8MapKey(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR8SeqMapKey(size))
        do i=1, size
          ESMF_HConfigAsR8SeqMapKey(i) = ESMF_HConfigAsR8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigAsR8SeqMapVal()"
!BOP
! !IROUTINE: ESMF_HConfigAsR8SeqMapVal - Return Seq arraay of map value as R8

! !INTERFACE:
  function ESMF_HConfigAsR8SeqMapVal(hconfig, keywordEnforcer, index, keyString, &
    doc, asOkay, rc)
! !RETURN VALUE:
    real(ESMF_KIND_R8), allocatable :: ESMF_HConfigAsR8SeqMapVal(:)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    logical,            intent(out), optional :: asOkay
    integer,            intent(out), optional :: rc

! !DESCRIPTION:
!   Return the map value of item {\tt hconfig} interpreted as R8.
!   The returned value is only valid if {\tt rc == ESMF\_SUCCESS}, and, if
!   provided, {\tt asOkay == .true.}.
!
! The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
!   \item[{[asOkay]}]
!     Set to {\tt .true.} for successful convertion to the requested typekind.
!     Set to {\tt .false.} otherwise. By default, i.e. without {\tt asOkay},
!     the latter condition leads to {\tt rc /= ESMF\_SUCCESS}.
!     Providing {\tt asOkay} returns {\tt rc == ESMF\_SUCCESS} in either case,
!     and the validity of the returned converted value is determined by
!     {\tt asOkay}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    logical               :: isScalar, isSequence
    type(ESMF_HConfig)    :: hconfigTemp
    integer               :: i, size

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    isScalar = ESMF_HConfigIsScalarMapVal(hconfig, index=index, &
      keyString=keyString, doc=doc, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    if (isScalar) then
      allocate(ESMF_HConfigAsR8SeqMapVal(1))
      ESMF_HConfigAsR8SeqMapVal(1) = ESMF_HConfigAsR8MapVal(hconfig, &
        index=index, keyString=keyString, doc=doc, asOkay=asOkay, rc=localrc)
    else
      isSequence = ESMF_HConfigIsSequenceMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      if (isSequence) then
        ! access the sequence
        hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
          keyString=keyString, doc=doc, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        ! loop through the sequence
        size = ESMF_HConfigGetSize(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
        allocate(ESMF_HConfigAsR8SeqMapVal(size))
        do i=1, size
          ESMF_HConfigAsR8SeqMapVal(i) = ESMF_HConfigAsR8(hconfigTemp, &
            index=i, asOkay=asOkay, rc=localrc)
          if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
            ESMF_CONTEXT, rcToReturn=rc)) return
          if (present(asOkay)) then
            if (.not.asOkay) exit
          endif
        enddo
        ! clean up
        call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
        if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
          ESMF_CONTEXT, rcToReturn=rc)) return
      else
        ! this is an error condition... neither scalar and nor sequence
        call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
          msg="Must either be scalar or sequence to use Seq interface", &
          ESMF_CONTEXT, rcToReturn=rc)
        return
      endif
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetHConfig()"
!BOP
! !IROUTINE: ESMF_HConfigSetHConfig - Set HConfig object at location to hconfig

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSet()
  subroutine ESMF_HConfigSetHConfig(hconfig, content, keywordEnforcer, index, keyString, &
    doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of a HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
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

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAt(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSet(hconfigTemp, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSet(hconfig, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetString()"
!BOP
! !IROUTINE: ESMF_HConfigSetString - Set HConfig object at location to string

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSet()
  subroutine ESMF_HConfigSetString(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of a HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetLogical()"
!BOP
! !IROUTINE: ESMF_HConfigSetLogical - Set HConfig object at location to logical

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSet()
  subroutine ESMF_HConfigSetLogical(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of a HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetI4()"
!BOP
! !IROUTINE: ESMF_HConfigSetI4 - Set HConfig object at location to I4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSet()
  subroutine ESMF_HConfigSetI4(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of a HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetI8()"
!BOP
! !IROUTINE: ESMF_HConfigSetI8 - Set HConfig object at location to I8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSet()
  subroutine ESMF_HConfigSetI8(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of a HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetR4()"
!BOP
! !IROUTINE: ESMF_HConfigSetR4 - Set HConfig object at location to R4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSet()
  subroutine ESMF_HConfigSetR4(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of a HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetR8()"
!BOP
! !IROUTINE: ESMF_HConfigSetR8 - Set HConfig object at location to R8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSet()
  subroutine ESMF_HConfigSetR8(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of a HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSet(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyHConfig()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapKeyHConfig - Set Map Key HConfig object at location to hconfig

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapKey()
  subroutine ESMF_HConfigSetMapKeyHConfig(hconfig, content, keywordEnforcer, index, keyString, &
    doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map key of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
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

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapKey(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSet(hconfigTemp, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSetMapKey(hconfig, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyString()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapKeyString - Set Map Key HConfig object at location to string

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapKey()
  subroutine ESMF_HConfigSetMapKeyString(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map key of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyLogical()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapKeyLogical - Set Map Key HConfig object at location to logical

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapKey()
  subroutine ESMF_HConfigSetMapKeyLogical(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map key of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyI4()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapKeyI4 - Set Map Key HConfig object at location to I4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapKey()
  subroutine ESMF_HConfigSetMapKeyI4(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map key of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyI8()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapKeyI8 - Set Map Key HConfig object at location to I8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapKey()
  subroutine ESMF_HConfigSetMapKeyI8(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map key of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyR4()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapKeyR4 - Set Map Key HConfig object at location to R4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapKey()
  subroutine ESMF_HConfigSetMapKeyR4(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map key of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapKeyR8()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapKeyR8 - Set Map Key HConfig object at location to R8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapKey()
  subroutine ESMF_HConfigSetMapKeyR8(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map key of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapKey(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValHConfig()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapValHConfig - Set Map Val HConfig object at location to hconfig

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapVal()
  subroutine ESMF_HConfigSetMapValHConfig(hconfig, content, keywordEnforcer, index, keyString, &
    doc, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    type(ESMF_HConfig), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(in),  optional :: doc
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map val of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[doc]}]
!     The doc index. Defaults to the first document.
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

    if (present(index).or.present(keyString).or.present(doc)) then
      hconfigTemp = ESMF_HConfigCreateAtMapVal(hconfig, index=index, &
        keyString=keyString, doc=doc, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSet(hconfigTemp, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ! clean up
      call ESMF_HConfigDestroy(hconfigTemp, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      ! Call into the C++ interface to set content
      call c_ESMC_HConfigSetMapVal(hconfig, content, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValString()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapValString - Set Map Val HConfig object at location to string

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapVal()
  subroutine ESMF_HConfigSetMapValString(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    character(*),       intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map val of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValLogical()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapValLogical - Set Map Val HConfig object at location to logical

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapVal()
  subroutine ESMF_HConfigSetMapValLogical(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    logical,            intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map val of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValI4()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapValI4 - Set Map Val HConfig object at location to I4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapVal()
  subroutine ESMF_HConfigSetMapValI4(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I4),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map val of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValI8()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapValI8 - Set Map Val HConfig object at location to I8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapVal()
  subroutine ESMF_HConfigSetMapValI8(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig),     intent(in)            :: hconfig
    integer(ESMF_KIND_I8),  intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: index
    character(*),           intent(in),  optional :: keyString
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map val of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValR4()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapValR4 - Set Map Val HConfig object at location to R4

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapVal()
  subroutine ESMF_HConfigSetMapValR4(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R4), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map val of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigSetMapValR8()"
!BOP
! !IROUTINE: ESMF_HConfigSetMapValR8 - Set Map Val HConfig object at location to R8

! !INTERFACE:
  ! Private name; call using ESMF_HConfigSetMapVal()
  subroutine ESMF_HConfigSetMapValR8(hconfig, content, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
    real(ESMF_KIND_R8), intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Set the content of the map val of HConfig object at the current iteration, or
!   as specified by {\tt index} or {\tt keyString}.
!   The {\tt hconfig} must {\em not} be a map iterator.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[content]
!     The content to be set.
!   \item[{[index]}]
!     Attempt to access by index if specified. Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified. Mutural exclusive with {\tt index}.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer               :: localrc                ! local return code
    type(ESMF_HConfig)    :: hcontent

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_HConfigGetInit, hconfig, rc)

    hcontent = ESMF_HConfigCreate(content=content, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call ESMF_HConfigSetMapVal(hconfig, hcontent, index=index, keyString=keyString, &
      rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! clean up
    call ESMF_HConfigDestroy(hcontent, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_HConfigRemove()"
!BOP
! !IROUTINE: ESMF_HConfigRemove - Remove element from HConfig object

! !INTERFACE:
  subroutine ESMF_HConfigRemove(hconfig, keywordEnforcer, index, keyString, rc)
!
! !ARGUMENTS:
    type(ESMF_HConfig), intent(in)            :: hconfig
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(in),  optional :: index
    character(*),       intent(in),  optional :: keyString
    integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Remove an element from a squence or map HConfig object. Either {\tt index}
!   (for sequence) or {\tt keyString} (for map) must be provided. An error is
!   flagged if neither optional argument is specified.
!
!   The arguments are:
!   \begin{description}
!   \item[hconfig]
!     {\tt ESMF\_HConfig} object.
!   \item[{[index]}]
!     Attempt to access by index if specified.
!     Mutural exclusive with {\tt keyString}.
!   \item[{[keyString]}]
!     Attempt to access by key string if specified.
!     Mutural exclusive with {\tt index}.
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

    ! Check mutual exclusions
    if (present(index) .and. present(keyString)) then
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="The 'index' and 'keyString' arguments are mutual exclusive", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    if (present(index)) then
      ! Call into the C++ interface to set remove element by index
      call c_ESMC_HConfigRemoveIndex(hconfig, index, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else if (present(keyString)) then
      ! Call into the C++ interface to set remove element by keyString
      call c_ESMC_HConfigRemoveKeyString(hconfig, keyString, localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    else
      call ESMF_LogSetError(ESMF_RC_ARG_INCOMP, &
        msg="Either 'index' or 'keyString' MUST be sepcified", &
        ESMF_CONTEXT, rcToReturn=rc)
      return
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine
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
