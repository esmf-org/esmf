! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2026, University Corporation for Atmospheric Research,
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
use ESMF_InfoMod
use ESMF_InfoSyncMod

implicit none

!==============================================================================
!==============================================================================

private
public ESMF_NamedAlias
public ESMF_NamedAliasGet

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

interface ESMF_NamedAliasGet
  module procedure ESMF_NamedAliasGetState
  module procedure ESMF_NamedAliasGetGridComp
  module procedure ESMF_NamedAliasGetCplComp
  module procedure ESMF_NamedAliasGetSciComp
  module procedure ESMF_NamedAliasGetFieldBundle
  module procedure ESMF_NamedAliasGetField
  module procedure ESMF_NamedAliasGetArrayBundle
  module procedure ESMF_NamedAliasGetArray
end interface

contains !=====================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_NamedAlias - Generate a NamedAlias
!
! !INTERFACE:
!   function ESMF_NamedAlias(object, keywordEnforcer, name, rc)
!
! !RETURN VALUE:
!   type(ESMF_*)        :: ESMF_NamedAlias
! !ARGUMENTS:
!   type(ESMF_*),       intent(in)            :: object
!   type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
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
!     The incoming object (alias or named alias) for which a named alias is
!     generated.
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
    type(ESMF_Info)         :: info

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

    ! handle unique named alias id
    call ESMF_InfoGetFromHost(object, info=info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_NamedAliasState%id = handleNamedAliasId(info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    type(ESMF_Info)         :: info

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

    ! handle unique named alias id
    call ESMF_InfoGetFromHost(object, info=info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_NamedAliasGridComp%id = handleNamedAliasId(info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    type(ESMF_Info)         :: info

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

    ! handle unique named alias id
    call ESMF_InfoGetFromHost(object, info=info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_NamedAliasCplComp%id = handleNamedAliasId(info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    type(ESMF_Info)         :: info

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

    ! handle unique named alias id
    call ESMF_InfoGetFromHost(object, info=info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_NamedAliasSciComp%id = handleNamedAliasId(info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    type(ESMF_Info)         :: info

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

    ! handle unique named alias id
    call ESMF_InfoGetFromHost(object, info=info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_NamedAliasFieldBundle%id = handleNamedAliasId(info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    type(ESMF_Info)         :: info

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

    ! handle unique named alias id
    call ESMF_InfoGetFromHost(object, info=info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_NamedAliasField%id = handleNamedAliasId(info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    type(ESMF_Info)         :: info

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

    ! handle unique named alias id
    call ESMF_InfoGetFromHost(object, info=info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_NamedAliasArrayBundle%id = handleNamedAliasId(info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

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
    type(ESMF_Info)         :: info

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

    ! handle unique named alias id
    call ESMF_InfoGetFromHost(object, info=info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ESMF_NamedAliasArray%id = handleNamedAliasId(info, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end function ESMF_NamedAliasArray
!------------------------------------------------------------------------------

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "handleNamedAliasId()"
!BOPI
! !IROUTINE: handleNamedAliasId - increment NamedAliasCount and return as id
!
! !INTERFACE:
  function handleNamedAliasId(info, rc)
!
! !RETURN VALUE:
    integer                         :: handleNamedAliasId
!
! !ARGUMENTS:
    type(ESMF_Info),    intent(inout)         :: info
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    ! get the current NamedAliasCount
    call ESMF_InfoGet(info, key="/ESMF/Instance/NamedAliasCount", &
      value=handleNamedAliasId, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! increment as id
    handleNamedAliasId = handleNamedAliasId + 1

    ! set back as new NamedAliasCount
    call ESMF_InfoSet(info, key="/ESMF/Instance/NamedAliasCount", &
      value=handleNamedAliasId, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end function handleNamedAliasId
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_NamedAliasGet - Get NamedAlias information
!
! !INTERFACE:
!   subroutine ESMF_NamedAliasGet(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
!   type(ESMF_*),       intent(in)            :: object
!   type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
!   integer,            intent(out), optional :: id
!   integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Query an object for named alias information. The supported classes are:
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
!     The incoming object (alias or named alias) that is queried.
!   \item [{[id]}]
!     For a named alias {\tt object}, the returned {\tt id} > 0 identifies the
!     specific named alias. Every named alias of the same ESMF object has its
!     unique {\tt id}. Regular aliases, on the other hand, all return the same
!     {\tt id} == 0 value.
!   \item [{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGetState()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGetState - Get NamedAlias information
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAliasGet()
  subroutine ESMF_NamedAliasGetState(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
    type(ESMF_State),   intent(in)            :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: id
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(id)) then
      if (object%isNamedAlias) then
        id = object%id  ! return actual id for named alias
      else
        id = 0          ! return 0 for regular alias
      endif
    endif

  end subroutine ESMF_NamedAliasGetState
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGetGridComp()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGetGridComp - Get NamedAlias information
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAliasGet()
  subroutine ESMF_NamedAliasGetGridComp(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
    type(ESMF_GridComp),intent(in)            :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: id
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(id)) then
      if (object%isNamedAlias) then
        id = object%id  ! return actual id for named alias
      else
        id = 0          ! return 0 for regular alias
      endif
    endif

  end subroutine ESMF_NamedAliasGetGridComp
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGetCplComp()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGetCplComp - Get NamedAlias information
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAliasGet()
  subroutine ESMF_NamedAliasGetCplComp(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
    type(ESMF_CplComp), intent(in)            :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: id
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(id)) then
      if (object%isNamedAlias) then
        id = object%id  ! return actual id for named alias
      else
        id = 0          ! return 0 for regular alias
      endif
    endif

  end subroutine ESMF_NamedAliasGetCplComp
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGetSciComp()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGetSciComp - Get NamedAlias information
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAliasGet()
  subroutine ESMF_NamedAliasGetSciComp(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
    type(ESMF_SciComp), intent(in)            :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: id
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(id)) then
      if (object%isNamedAlias) then
        id = object%id  ! return actual id for named alias
      else
        id = 0          ! return 0 for regular alias
      endif
    endif

  end subroutine ESMF_NamedAliasGetSciComp
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGetFieldBundle()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGetFieldBundle - Get NamedAlias information
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAliasGet()
  subroutine ESMF_NamedAliasGetFieldBundle(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
    type(ESMF_FieldBundle), intent(in)        :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: id
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(id)) then
      if (object%isNamedAlias) then
        id = object%id  ! return actual id for named alias
      else
        id = 0          ! return 0 for regular alias
      endif
    endif

  end subroutine ESMF_NamedAliasGetFieldBundle
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGetField()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGetField - Get NamedAlias information
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAliasGet()
  subroutine ESMF_NamedAliasGetField(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
    type(ESMF_Field),   intent(in)            :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: id
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(id)) then
      if (object%isNamedAlias) then
        id = object%id  ! return actual id for named alias
      else
        id = 0          ! return 0 for regular alias
      endif
    endif

  end subroutine ESMF_NamedAliasGetField
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGetArrayBundle()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGetArrayBundle - Get NamedAlias information
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAliasGet()
  subroutine ESMF_NamedAliasGetArrayBundle(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
    type(ESMF_ArrayBundle), intent(in)        :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: id
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(id)) then
      if (object%isNamedAlias) then
        id = object%id  ! return actual id for named alias
      else
        id = 0          ! return 0 for regular alias
      endif
    endif

  end subroutine ESMF_NamedAliasGetArrayBundle
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_NamedAliasGetArray()"
!BOPI
! !IROUTINE: ESMF_NamedAliasGetArray - Get NamedAlias information
!
! !INTERFACE:
  ! Private name; call using ESMF_NamedAliasGet()
  subroutine ESMF_NamedAliasGetArray(object, keywordEnforcer, id, rc)
!
! !ARGUMENTS:
    type(ESMF_Array),   intent(in)            :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,            intent(out), optional :: id
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc

    if (present(rc)) rc = ESMF_SUCCESS

    if (present(id)) then
      if (object%isNamedAlias) then
        id = object%id  ! return actual id for named alias
      else
        id = 0          ! return 0 for regular alias
      endif
    endif

  end subroutine ESMF_NamedAliasGetArray
!------------------------------------------------------------------------------

end module ESMF_NamedAliasMod
