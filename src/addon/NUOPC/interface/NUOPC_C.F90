! $Id$
!
! Earth System Modeling Framework
! Copyright (c) 2002-2025, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define FILENAME "src/addon/NUOPC/interface/NUOPC_C.F90"
!==============================================================================
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
subroutine f_nuopc_compspecialize(gcomp, specLabel, specRoutine, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_nuopc_compspecialize"
  use ESMF
  use NUOPC
  implicit none

  type(ESMF_GridComp)   :: gcomp  !in
  character(len=*), intent(in)            :: specLabel
    interface
      subroutine specRoutine(gridcomp, rc)
        use ESMF
        implicit none
        type(ESMF_GridComp)        :: gridcomp ! must not be optional
        integer, intent(out)       :: rc       ! must not be optional
      end subroutine
    end interface
  integer, intent(out)  :: rc     !out

  integer :: localrc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call NUOPC_CompSpecialize(gcomp, specLabel, specRoutine=specRoutine, &
    rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
    ESMF_CONTEXT, rcToReturn=rc)) return

  ! Return successfully
  rc = ESMF_SUCCESS
end subroutine f_nuopc_compspecialize
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
subroutine f_nuopc_modelsetservices(gcomp, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_nuopc_modelsetservices"
  use ESMF
  use NUOPC
  use NUOPC_Model, only: SetServices
  implicit none

  type(ESMF_GridComp)   :: gcomp  !in
  integer, intent(out)  :: rc     !out

  integer :: localrc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call SetServices(gcomp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
    ESMF_CONTEXT, rcToReturn=rc)) return

  ! Return successfully
  rc = ESMF_SUCCESS
end subroutine f_nuopc_modelsetservices
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
subroutine f_nuopc_modelsetvm(gcomp, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_nuopc_modelsetvm"
  use ESMF
  use NUOPC
  use NUOPC_Model, only: SetVM
  implicit none

  type(ESMF_GridComp)   :: gcomp  !in
  integer, intent(out)  :: rc     !out

  integer :: localrc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call SetVM(gcomp, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
    ESMF_CONTEXT, rcToReturn=rc)) return

  ! Return successfully
  rc = ESMF_SUCCESS
end subroutine f_nuopc_modelsetvm
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
subroutine f_nuopc_modelgetexportstate(gcomp, state, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_nuopc_modelgetexportstate"
  use ESMF
  use NUOPC
  use NUOPC_Model, only: NUOPC_ModelGet
  implicit none

  type(ESMF_GridComp)   :: gcomp  !in
  type(ESMF_State)      :: state  !out
  integer, intent(out)  :: rc     !out

  integer :: localrc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call NUOPC_ModelGet(gcomp, exportState=state, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
    ESMF_CONTEXT, rcToReturn=rc)) return

  ! Return successfully
  rc = ESMF_SUCCESS
end subroutine f_nuopc_modelgetexportstate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
subroutine f_nuopc_modelgetimportstate(gcomp, state, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_nuopc_modelgetimportstate"
  use ESMF
  use NUOPC
  use NUOPC_Model, only: NUOPC_ModelGet
  implicit none

  type(ESMF_GridComp)   :: gcomp  !in
  type(ESMF_State)      :: state  !out
  integer, intent(out)  :: rc     !out

  integer :: localrc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call NUOPC_ModelGet(gcomp, importState=state, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
    ESMF_CONTEXT, rcToReturn=rc)) return

  ! Return successfully
  rc = ESMF_SUCCESS
end subroutine f_nuopc_modelgetimportstate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
subroutine f_nuopc_advertise(state, standardName, fieldName, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_nuopc_advertise"
  use ESMF
  use NUOPC
  implicit none

  type(ESMF_State)                        :: state        !in
  character(len=*), intent(in)            :: standardName !in
  character(len=*), intent(in)            :: fieldName    !in
  integer, intent(out)                    :: rc           !out

  integer :: localrc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call NUOPC_Advertise(state, standardName, name=fieldName, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
    ESMF_CONTEXT, rcToReturn=rc)) return

  ! Return successfully
  rc = ESMF_SUCCESS
end subroutine f_nuopc_advertise
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
subroutine f_nuopc_realize(state, field, rc)
#undef  ESMF_METHOD
#define ESMF_METHOD "f_nuopc_realize"
  use ESMF
  use NUOPC
  implicit none

  type(ESMF_State)                        :: state        !in
  type(ESMF_Field)                        :: field        !in
  integer, intent(out)                    :: rc           !out

  integer :: localrc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call NUOPC_Realize(state, field, rc=localrc)
  if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU,  &
    ESMF_CONTEXT, rcToReturn=rc)) return

  ! Return successfully
  rc = ESMF_SUCCESS
end subroutine f_nuopc_realize
!------------------------------------------------------------------------------
