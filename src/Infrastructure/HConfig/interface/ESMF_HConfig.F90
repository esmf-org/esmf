! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2022, University Corporation for Atmospheric Research, 
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
    type(ESMF_Pointer) :: this
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
      ESMF_HConfigEQ = HConfig1%this .eq. HConfig2%this
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
    integer,                       intent(out), optional :: rc
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
    hconfig%this = ESMF_NULL_POINTER
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
    integer,             intent(out),  optional :: rc  
!         
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
    hconfig%this = ESMF_NULL_POINTER

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
    type(ESMF_HConfig),       intent(in)            :: hconfig
    character(len=*),         intent(in)            :: content
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                  intent(out), optional :: rc
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
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [hconfig]
!           HConfig object.
!     \end{description}
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
