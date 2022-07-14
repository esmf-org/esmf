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

use ESMF_VMMod
use ESMF_StateMod
use ESMF_StateItemMod
use ESMF_DistGridMod
use ESMF_FieldMod
use ESMF_FieldGetMod
use ESMF_FieldBundleMod
use ESMF_CompMod
use ESMF_GridCompMod
use ESMF_CplCompMod
use ESMF_SciCompMod
use ESMF_ArrayMod
use ESMF_ArrayBundleMod
use ESMF_InfoMod
use ESMF_UtilTypesMod
use ESMF_GeomBaseMod
use ESMF_MeshMod
use ESMF_GridMod
use ESMF_XGridMod
use ESMF_XGridGetMod
use ESMF_LocStreamMod
use ESMF_RHandleMod
use ESMF_InfoDescribeMod

implicit none

!==============================================================================
!==============================================================================

private
public ESMF_NamedAlias

!==============================================================================
!==============================================================================

interface ESMF_NamedAlias
  module procedure ESMF_NamedAliasArray
#if 0
  module procedure ESMF_NamedAliasArrayBundle
  module procedure ESMF_NamedAliasCplComp
  module procedure ESMF_NamedAliasGridComp
  module procedure ESMF_NamedAliasSciComp
  module procedure ESMF_NamedAliasDistGrid
  module procedure ESMF_NamedAliasField
  module procedure ESMF_NamedAliasFieldBundle
  module procedure ESMF_NamedAliasGrid
  module procedure ESMF_NamedAliasState
  module procedure ESMF_NamedAliasLocStream
  module procedure ESMF_NamedAliasMesh
#endif
end interface

contains !=====================================================================

! -------------------------- ESMF-public method -------------------------------
!BOP
! !IROUTINE: ESMF_NamedAlias - Named Alias
!
! !INTERFACE:
!   function ESMF_NamedAlias(object, name, rc)
!
! !ARGUMENTS:
!   type(ESMF_*),       intent(in)            :: object
!   character(len = *), intent(in),  optional :: name
!   integer,            intent(out), optional :: rc
!
! !DESCRIPTION:
!   Generate a named alias to {\tt object}.
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
    type(ESMF_Array),   intent(in)            :: object
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *), intent(in),  optional :: name
    integer,            intent(out), optional :: rc
!EOPI
!------------------------------------------------------------------------------

    ESMF_NamedAliasArray = object

  end function ESMF_NamedAliasArray
!------------------------------------------------------------------------------


end module ESMF_NamedAliasMod
