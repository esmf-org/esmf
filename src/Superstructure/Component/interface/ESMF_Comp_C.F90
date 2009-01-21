!  $Id: ESMF_Comp_C.F90,v 1.45.2.3 2009/01/21 21:25:24 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_COMP_C.F90"
!==============================================================================
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable 
!   arrays, or ...
!
!==============================================================================
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_Comp_C.F90,v 1.45.2.3 2009/01/21 21:25:24 cdeluca Exp $'
!==============================================================================

!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
! 
! The code in this file implements the interface code between C++ and F90
!  for the {\tt Component} entry points.  When the user calls an
!  {\tt ESMC_Comp}XXX method, that code calls these functions, which
!  in turn call the F90 module code.  C++ cannot call directly into an
!  F90 module because the module routine names are altered in a similar
!  fashion as C++ name mangling.
! 
!EOP
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compsetvminfo"
recursive subroutine f_esmf_compsetvminfo(comp, vm_info, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_VMMod

  implicit none

  type(ESMF_CWrap)   :: comp
  type(ESMF_Pointer) :: vm_info
  integer            :: rc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_CompSet(compp=comp%compp, vm_info=vm_info, rc=rc)
end subroutine f_esmf_compsetvminfo

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compgetvmparent"
recursive subroutine f_esmf_compgetvmparent(comp, vm_parent, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_VMMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CWrap) :: comp
  type(ESMF_VM)    :: vm_parent
  integer          :: rc
  
  type(ESMF_VM)      :: local_vm_parent
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_CompGet(compp=comp%compp, vm_parent=local_vm_parent, rc=rc)

  call ESMF_VMGetThis(local_vm_parent, this, rc=rc)  ! Get C++ address
  call ESMF_VMSetThis(vm_parent, this, rc=rc)        ! Set C++ address
end subroutine f_esmf_compgetvmparent

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compgetvmplan"
recursive subroutine f_esmf_compgetvmplan(comp, vmplan, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_VMMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CWrap)   :: comp
  type(ESMF_VMPlan)  :: vmplan
  integer            :: rc

  type(ESMF_VMPlan)  :: local_vmplan
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL


  call ESMF_CompGet(compp=comp%compp, vmplan=local_vmplan, rc=rc)

  call ESMF_VMPlanGetThis(local_vmplan, this, rc=rc)  ! Get C++ address
  call ESMF_VMPlanSetThis(vmplan, this, rc=rc)        ! Set C++ address
end subroutine f_esmf_compgetvmplan


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compinsertvm"
recursive subroutine f_esmf_compinsertvm(comp, vm, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_InitMacrosMod     ! ESMF initializer macros
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_VMMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CWrap) :: comp
  type(ESMF_VM)    :: vm
  integer          :: rc

  type(ESMF_VM)      :: local_vm
  type(ESMF_Pointer) :: this
  

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_VMGetThis(vm, this, rc=rc)       ! Get address of C++ object
  call ESMF_VMSetThis(local_vm, this, rc=rc) ! Set address of C++ object
  call ESMF_VMSetInitCreated(local_vm)       ! Set init code

  call ESMF_CompSet(compp=comp%compp, vm=local_vm, rc=rc)
end subroutine f_esmf_compinsertvm

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compget"
recursive subroutine f_esmf_compget(comp, ctype, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CWrap) :: comp
  type(ESMF_CompType) :: ctype
  integer :: rc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_CompGet(comp%compp, ctype=ctype, rc=rc)
end subroutine f_esmf_compget

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compreplicate"
recursive subroutine f_esmf_compreplicate(comp, comp_src, vm, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_VMMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CWrap) :: comp
  type(ESMF_CWrap) :: comp_src
  type(ESMF_VM)    :: vm
  integer :: rc

  type(ESMF_VM) :: vm_cpy
  type(ESMF_Pointer) :: this
  type (ESMF_CompClass), pointer :: compclass  
  
  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  nullify(comp%compp)
  nullify(compclass)
  allocate(compclass)
  compclass = comp_src%compp
  call ESMF_CompClassSetInitCreated(compclass)
  call ESMF_VMGetThis(vm, this)
  call ESMF_VMSetThis(vm_cpy, this)
  call ESMF_VMSetInitCreated(vm_cpy)
  call ESMF_CompSet(compclass, vm=vm_cpy)
  comp%compp => compclass
  call ESMF_CWrapSetInitCreated(comp)
end subroutine f_esmf_compreplicate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compcopy"
recursive subroutine f_esmf_compcopy(comp, comp_src, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_VMMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CWrap) :: comp_src
  type(ESMF_CWrap) :: comp
  integer :: rc

  type (ESMF_CompClass), pointer :: compclass_src, compclass
  
  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  compclass_src => comp_src%compp
  compclass => comp%compp
  compclass = compclass_src
  call  ESMF_CompClassSetInitCreated(compclass_src)
  call  ESMF_CompClassSetInitCreated(compclass)
end subroutine f_esmf_compcopy

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compdelete"
recursive subroutine f_esmf_compdelete(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CWrap) :: comp
  integer :: rc
  
  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL


  deallocate(comp%compp)
  nullify(comp%compp)
end subroutine f_esmf_compdelete


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompcreate"
subroutine f_esmf_gridcompcreate(gcomp, name, mtype, grid, config, configFile, &
  clock, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ConfigMod
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_GridMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: gcomp
  character(len=*), optional :: name
  type(ESMF_GridCompType), optional :: mtype
  type(ESMF_Grid), optional :: grid
  type(ESMF_Config), optional :: config
  character(len=*), optional :: configFile
  type(ESMF_Clock), optional :: clock
  integer, optional :: rc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  gcomp = ESMF_GridCompCreate(name=name, gridcomptype=mtype, grid=grid, &
    config=config, configFile=configFile, clock=clock, rc=rc)
end subroutine f_esmf_gridcompcreate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompdestroy"
subroutine f_esmf_gridcompdestroy(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: comp
  integer, optional :: rc              

  call ESMF_GridCompDestroy(comp, rc)
end subroutine f_esmf_gridcompdestroy

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompinitialize"
subroutine f_esmf_gridcompinitialize(comp, importState, exportState, clock, &
  phase, blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompInitialize(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_gridcompinitialize

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcomprun"
subroutine f_esmf_gridcomprun(comp, importState, exportState, clock, phase, &
  blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompRun(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_gridcomprun

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompfinalize"
subroutine f_esmf_gridcompfinalize(comp, importState, exportState, clock, &
  phase, blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompFinalize(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_gridcompfinalize

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompset"
subroutine f_esmf_gridcompset(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: comp      
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompSet(comp)

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine f_esmf_gridcompset

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompget"
subroutine f_esmf_gridcompget(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: comp      
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompGet(comp)
end subroutine f_esmf_gridcompget

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompvalidate"
subroutine f_esmf_gridcompvalidate(comp, options, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: comp      
  character(len=*), optional :: options
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompValidate(comp, options, rc)
end subroutine f_esmf_gridcompvalidate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompprint"
subroutine f_esmf_gridcompprint(comp, options, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp) :: comp      
  character(len=*), optional :: options
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompPrint(comp, options, rc)
end subroutine f_esmf_gridcompprint


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompcreate"
subroutine f_esmf_cplcompcreate(ccomp, name, config, configFile, clock, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_CompMod
  use ESMF_ConfigMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: ccomp
  character(len=*), optional :: name
  type(ESMF_Config), optional :: config
  character(len=*), optional :: configFile
  type(ESMF_Clock), optional :: clock
  integer, optional :: rc

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  ccomp = ESMF_CplCompCreate(name=name, config=config, configFile=configFile, &
    clock=clock, rc=rc)
end subroutine f_esmf_cplcompcreate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompdestroy"
subroutine f_esmf_cplcompdestroy(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: comp
  integer, optional :: rc              

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompDestroy(comp, rc)
end subroutine f_esmf_cplcompdestroy

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompinitialize"
subroutine f_esmf_cplcompinitialize(comp, importState, exportState, clock, &
  phase, blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompInitialize(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_cplcompinitialize

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcomprun"
subroutine f_esmf_cplcomprun(comp, importState, exportState, clock, phase, &
  blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_StateMod
  use ESMF_ClockTypeMod
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompRun(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_cplcomprun

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompfinalize"
subroutine f_esmf_cplcompfinalize(comp, importState, exportState, clock, &
  phase, blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompFinalize(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_cplcompfinalize

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompset"
subroutine f_esmf_cplcompset(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: comp      
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompSet(comp)
  if (present(rc)) rc = ESMF_SUCCESS

end subroutine f_esmf_cplcompset

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompget"
subroutine f_esmf_cplcompget(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: comp      
  integer, optional :: rc     

  if (present(rc)) rc = ESMF_SUCCESS

  call ESMF_CplCompGet(comp)
end subroutine f_esmf_cplcompget

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompvalidate"
subroutine f_esmf_cplcompvalidate(comp, options, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: comp      
  character(len=*), optional :: options
  integer, optional :: rc     

  call ESMF_CplCompValidate(comp, options, rc)
end subroutine f_esmf_cplcompvalidate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompprint"
subroutine f_esmf_cplcompprint(comp, options, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp) :: comp      
  character(len=*), optional :: options
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompPrint(comp, options, rc)
end subroutine f_esmf_cplcompprint
