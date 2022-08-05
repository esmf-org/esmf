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
#define ESMF_FILENAME "ESMF_NamedAlias.F90"
!==============================================================================

#include "ESMF_Macros.inc"
#include "ESMF.h"

!==============================================================================
!==============================================================================

module ESMF_NamedAliasMod

use ESMF_UtilTypesMod     ! ESMF utility types
use ESMF_InitMacrosMod    ! ESMF initializer macros
use ESMF_BaseMod          ! ESMF base class
use ESMF_LogErrMod        ! ESMF error handling


use ESMF_StateMod
use ESMF_CompMod
use ESMF_GridCompMod
use ESMF_CplCompMod
use ESMF_SciCompMod
use ESMF_FieldBundleMod
use ESMF_FieldMod
use ESMF_FieldGetMod
use ESMF_ArrayBundleMod
use ESMF_ArrayMod

implicit none

!==============================================================================
!==============================================================================

private
public ESMF_NamedAlias

!==============================================================================
!==============================================================================

interface ESMF_NamedAlias
  module procedure ESMF_NamedAliasState
  module procedure ESMF_NamedAliasGridComp
  module procedure ESMF_NamedAliasCplComp
  module procedure ESMF_NamedAliasSciComp
  module procedure ESMF_NamedAliasFieldBundle
  module procedure ESMF_NamedAliasField
  module procedure ESMF_NamedAliasArrayBundle
  module procedure ESMF_NamedAliasArray
end interface

contains !=====================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_NamedAlias - Generate a Named Alias
!
! !INTERFACE:
!   function ESMF_NamedAlias(object, name, rc)
!
! !RETURN VALUE:
!   type(ESMF_*)        :: ESMF_NamedAlias
! !ARGUMENTS:
!   type(ESMF_*),       intent(in)            :: object
!   character(len = *), intent(in),  optional :: name
!   integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Generate a named alias to {\tt object}. The supported classes are:
!   \begin{itemize}
!   \item {\tt ESMF\_State}
!   \item {\tt ESMF\_GridComp}
!   \item {\tt ESMF\_CplComp}
!   \item {\tt ESMF\_SciComp}
!   \item {\tt ESMF\_FieldBundle}
!   \item {\tt ESMF\_Field}
!   \item {\tt ESMF\_ArrayBundle}
!   \item {\tt ESMF\_Array}
!   \end{itemize}
!
!   The arguments are:
!   \begin{description}
!   \item[object]
!     The incoming object for which a named alias is generated.
!   \item [{[name]}]
!     The name of the named alias. By default use the name of {\tt object}.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasState()"
!BOPI
! !IROUTINE: ESMF_NamedAliasState - Named Alias
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAlias()
  function ESMF_NamedAliasState(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_State) :: ESMF_NamedAliasState
!
! !ARGUMENTS:
    type(ESMF_State),intent(in)               :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc
    character(ESMF_MAXSTR)  :: nameDefault

    if (present(rc)) rc = ESMF_SUCCESS

    ! first create regular alias
    ESMF_NamedAliasState = object

    ! next mark as namedAlias
    ESMF_NamedAliasState%isNamedAlias = .true.

    ! finally set name
    if (present(name)) then
      ESMF_NamedAliasState%name = trim(name)
    else
      call ESMF_StateGet(object, name=nameDefault, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_NamedAliasState%name = trim(nameDefault)
    endif

  end function ESMF_NamedAliasState
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGridComp()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGridComp - Named Alias
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAlias()
  function ESMF_NamedAliasGridComp(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_GridComp) :: ESMF_NamedAliasGridComp
!
! !ARGUMENTS:
    type(ESMF_GridComp),intent(in)            :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc
    character(ESMF_MAXSTR)  :: nameDefault

    if (present(rc)) rc = ESMF_SUCCESS

    ! first create regular alias
    ESMF_NamedAliasGridComp = object

    ! next mark as namedAlias
    ESMF_NamedAliasGridComp%isNamedAlias = .true.

    ! finally set name
    if (present(name)) then
      ESMF_NamedAliasGridComp%name = trim(name)
    else
      call ESMF_GridCompGet(object, name=nameDefault, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_NamedAliasGridComp%name = trim(nameDefault)
    endif

  end function ESMF_NamedAliasGridComp
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasCplComp()"
!BOPI
! !IROUTINE: ESMF_NamedAliasCplComp - Named Alias
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAlias()
  function ESMF_NamedAliasCplComp(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_CplComp) :: ESMF_NamedAliasCplComp
!
! !ARGUMENTS:
    type(ESMF_CplComp),intent(in)             :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc
    character(ESMF_MAXSTR)  :: nameDefault

    if (present(rc)) rc = ESMF_SUCCESS

    ! first create regular alias
    ESMF_NamedAliasCplComp = object

    ! next mark as namedAlias
    ESMF_NamedAliasCplComp%isNamedAlias = .true.

    ! finally set name
    if (present(name)) then
      ESMF_NamedAliasCplComp%name = trim(name)
    else
      call ESMF_CplCompGet(object, name=nameDefault, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_NamedAliasCplComp%name = trim(nameDefault)
    endif

  end function ESMF_NamedAliasCplComp
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasSciComp()"
!BOPI
! !IROUTINE: ESMF_NamedAliasSciComp - Named Alias
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAlias()
  function ESMF_NamedAliasSciComp(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_SciComp) :: ESMF_NamedAliasSciComp
!
! !ARGUMENTS:
    type(ESMF_SciComp),intent(in)             :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc
    character(ESMF_MAXSTR)  :: nameDefault

    if (present(rc)) rc = ESMF_SUCCESS

    ! first create regular alias
    ESMF_NamedAliasSciComp = object

    ! next mark as namedAlias
    ESMF_NamedAliasSciComp%isNamedAlias = .true.

    ! finally set name
    if (present(name)) then
      ESMF_NamedAliasSciComp%name = trim(name)
    else
      call ESMF_SciCompGet(object, name=nameDefault, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_NamedAliasSciComp%name = trim(nameDefault)
    endif

  end function ESMF_NamedAliasSciComp
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasFieldBundle()"
!BOPI
! !IROUTINE: ESMF_NamedAliasFieldBundle - Named Alias
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAlias()
  function ESMF_NamedAliasFieldBundle(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_FieldBundle) :: ESMF_NamedAliasFieldBundle
!
! !ARGUMENTS:
    type(ESMF_FieldBundle),intent(in)         :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc
    character(ESMF_MAXSTR)  :: nameDefault

    if (present(rc)) rc = ESMF_SUCCESS

    ! first create regular alias
    ESMF_NamedAliasFieldBundle = object

    ! next mark as namedAlias
    ESMF_NamedAliasFieldBundle%isNamedAlias = .true.

    ! finally set name
    if (present(name)) then
      ESMF_NamedAliasFieldBundle%name = trim(name)
    else
      call ESMF_FieldBundleGet(object, name=nameDefault, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_NamedAliasFieldBundle%name = trim(nameDefault)
    endif

  end function ESMF_NamedAliasFieldBundle
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasField()"
!BOPI
! !IROUTINE: ESMF_NamedAliasField - Named Alias
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAlias()
  function ESMF_NamedAliasField(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_Field) :: ESMF_NamedAliasField
!
! !ARGUMENTS:
    type(ESMF_Field),intent(in)               :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc
    character(ESMF_MAXSTR)  :: nameDefault

    if (present(rc)) rc = ESMF_SUCCESS

    ! first create regular alias
    ESMF_NamedAliasField = object

    ! next mark as namedAlias
    ESMF_NamedAliasField%isNamedAlias = .true.

    ! finally set name
    if (present(name)) then
      ESMF_NamedAliasField%name = trim(name)
    else
      call ESMF_FieldGet(object, name=nameDefault, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_NamedAliasField%name = trim(nameDefault)
    endif

  end function ESMF_NamedAliasField
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasArrayBundle()"
!BOPI
! !IROUTINE: ESMF_NamedAliasArrayBundle - Named Alias
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAlias()
  function ESMF_NamedAliasArrayBundle(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_ArrayBundle) :: ESMF_NamedAliasArrayBundle
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle),intent(in)         :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc
    character(ESMF_MAXSTR)  :: nameDefault

    if (present(rc)) rc = ESMF_SUCCESS

    ! first create regular alias
    ESMF_NamedAliasArrayBundle = object

    ! next mark as namedAlias
    ESMF_NamedAliasArrayBundle%isNamedAlias = .true.

    ! finally set name
    if (present(name)) then
      ESMF_NamedAliasArrayBundle%name = trim(name)
    else
      call ESMF_ArrayBundleGet(object, name=nameDefault, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_NamedAliasArrayBundle%name = trim(nameDefault)
    endif

  end function ESMF_NamedAliasArrayBundle
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasArray()"
!BOPI
! !IROUTINE: ESMF_NamedAliasArray - Named Alias
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAlias()
  function ESMF_NamedAliasArray(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
    type(ESMF_Array) :: ESMF_NamedAliasArray
!
! !ARGUMENTS:
    type(ESMF_Array),intent(in)               :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc
    character(ESMF_MAXSTR)  :: nameDefault

    if (present(rc)) rc = ESMF_SUCCESS

    ! first create regular alias
    ESMF_NamedAliasArray = object

    ! next mark as namedAlias
    ESMF_NamedAliasArray%isNamedAlias = .true.

    ! finally set name
    if (present(name)) then
      ESMF_NamedAliasArray%name = trim(name)
    else
      call ESMF_ArrayGet(object, name=nameDefault, rc=localrc)
      if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      ESMF_NamedAliasArray%name = trim(nameDefault)
    endif

  end function ESMF_NamedAliasArray
!------------------------------------------------------------------------------

end module ESMF_NamedAliasMod
