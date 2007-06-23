!  $Id: ESMF_Comp_C.F90,v 1.43 2007/06/23 04:00:57 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_Comp_C.F90,v 1.43 2007/06/23 04:00:57 cdeluca Exp $'
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

  type (ESMF_CompClass), pointer :: compclass  
  
  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  nullify(comp%compp)
  nullify(compclass)
  allocate(compclass)
  compclass = comp_src%compp
  call  ESMF_CompClassSetInitCreated(compclass)
  call ESMF_CompSet(compclass, vm=vm)
  comp%compp => compclass
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
#define ESMF_METHOD "f_esmf_igridcompcreate"
subroutine f_esmf_igridcompcreate(gcomp, name, mtype, igrid, config, configFile, &
  clock, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ConfigMod
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_IGridTypesMod
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: gcomp
  character(len=*), optional :: name
  type(ESMF_IGridCompType), optional :: mtype
  type(ESMF_IGrid), optional :: igrid
  type(ESMF_Config), optional :: config
  character(len=*), optional :: configFile
  type(ESMF_Clock), optional :: clock
  integer, optional :: rc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  gcomp = ESMF_IGridCompCreate(name=name, igridcomptype=mtype, igrid=igrid, &
    config=config, configFile=configFile, clock=clock, rc=rc)
end subroutine f_esmf_igridcompcreate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_igridcompdestroy"
subroutine f_esmf_igridcompdestroy(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: comp
  integer, optional :: rc              

  call ESMF_IGridCompDestroy(comp, rc)
end subroutine f_esmf_igridcompdestroy

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_igridcompinitialize"
subroutine f_esmf_igridcompinitialize(comp, importState, exportState, clock, &
  phase, blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_IGridCompInitialize(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_igridcompinitialize

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_igridcomprun"
subroutine f_esmf_igridcomprun(comp, importState, exportState, clock, phase, &
  blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_IGridCompRun(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_igridcomprun

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_igridcompfinalize"
subroutine f_esmf_igridcompfinalize(comp, importState, exportState, clock, &
  phase, blockingFlag, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: comp      
  type(ESMF_State), optional :: importState
  type(ESMF_State), optional :: exportState
  type(ESMF_Clock), optional :: clock
  integer, optional :: phase
  type(ESMF_BlockingFlag), optional :: blockingFlag
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_IGridCompFinalize(comp, importState, exportState, clock, phase, &
    blockingFlag, rc)
end subroutine f_esmf_igridcompfinalize

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_igridcompset"
subroutine f_esmf_igridcompset(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: comp      
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_IGridCompSet(comp)

  if (present(rc)) rc = ESMF_SUCCESS
end subroutine f_esmf_igridcompset

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_igridcompget"
subroutine f_esmf_igridcompget(comp, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: comp      
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_IGridCompGet(comp)
end subroutine f_esmf_igridcompget

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_igridcompvalidate"
subroutine f_esmf_igridcompvalidate(comp, options, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: comp      
  character(len=*), optional :: options
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_IGridCompValidate(comp, options, rc)
end subroutine f_esmf_igridcompvalidate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_igridcompprint"
subroutine f_esmf_igridcompprint(comp, options, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_IGridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_IGridComp) :: comp      
  character(len=*), optional :: options
  integer, optional :: rc     

  ! Initialize return code; assume routine not implemented
  if (present(rc)) rc = ESMF_RC_NOT_IMPL

  call ESMF_IGridCompPrint(comp, options, rc)
end subroutine f_esmf_igridcompprint


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
