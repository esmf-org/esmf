!  $Id: ESMF_Comp_C.F90,v 1.60.2.1 2010/02/05 20:03:59 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_Comp_C.F90"
!==============================================================================
!
! F77 interfaces for C++ layer calling into F90 implementation layer.
!
!==============================================================================
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!character(*), parameter, private :: version = &
!  '$Id: ESMF_Comp_C.F90,v 1.60.2.1 2010/02/05 20:03:59 svasquez Exp $'
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
#define ESMF_METHOD "f_esmf_compgetvminfo"
recursive subroutine f_esmf_compgetvminfo(comp, vm_info, rc)
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

  call ESMF_CompGet(compp=comp%compp, vm_info=vm_info, rc=rc)
end subroutine f_esmf_compgetvminfo

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
#define ESMF_METHOD "f_esmf_compgetctype"
recursive subroutine f_esmf_compgetctype(comp, ctype, rc)
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
end subroutine f_esmf_compgetctype

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
  integer :: localrc
  
  ! Initialize return code; assume routine not implemented
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  nullify(comp%compp)
  nullify(compclass)
  allocate(compclass)
  compclass = comp_src%compp
  call ESMF_CompClassSetInitCreated(compclass)
  call c_ESMC_FTableCreate(compclass%this, localrc) 
  if (ESMF_LogMsgFoundError(localrc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rc)) return
  call ESMF_VMGetThis(vm, this)
  call ESMF_VMSetThis(vm_cpy, this)
  call ESMF_VMSetInitCreated(vm_cpy)
  call ESMF_CompSet(compclass, vm=vm_cpy)
  comp%compp => compclass
  call ESMF_CWrapSetInitCreated(comp)
  
  ! return successfully
  rc = ESMF_SUCCESS
end subroutine f_esmf_compreplicate

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_comprefcopy"
recursive subroutine f_esmf_comprefcopy(comp, comp_src, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_CompMod
  use ESMF_VMMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CWrap) :: comp_src
  type(ESMF_CWrap) :: comp
  integer :: rc

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  comp%compp => comp_src%compp
  call ESMF_CWrapSetInitCreated(comp)

  ! return successfully
  rc = ESMF_SUCCESS
end subroutine f_esmf_comprefcopy

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

  ! return successfully
  rc = ESMF_SUCCESS
end subroutine f_esmf_compdelete


#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_compcollectgarbage()"
subroutine f_esmf_compcollectgarbage(comp, rc)
  use ESMF_UtilTypesMod
  use ESMF_BaseMod
  use ESMF_LogErrMod
  use ESMF_CompMod

  type(ESMF_CWrap)     :: comp
  integer, intent(out) :: rc     

  integer :: localrc              

  ! initialize return code; assume routine not implemented
  localrc = ESMF_RC_NOT_IMPL
  rc = ESMF_RC_NOT_IMPL

  !print *, "collecting Component garbage"

  ! destruct internal data allocations
  call ESMF_CompDestruct(comp%compp, rc=localrc)
  if (ESMF_LogMsgFoundError(localrc, &
    ESMF_ERR_PASSTHRU, &
    ESMF_CONTEXT, rc)) return

  ! deallocate actual CompClass allocation      
  if (associated(comp%compp)) then
    deallocate(comp%compp, stat=localrc)
    if (ESMF_LogMsgFoundAllocError(localrc, "Deallocating Comp", &
      ESMF_CONTEXT, rc)) return
  endif
  nullify(comp%compp)

  ! return successfully  
  rc = ESMF_SUCCESS

end subroutine f_esmf_compcollectgarbage
  
  
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompcreate"
subroutine f_esmf_gridcompcreate(gcomp, name, mtype, configFile, clock, rc)
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

  type(ESMF_GridComp)       :: gcomp
  character(len=*)          :: name
  type(ESMF_GridCompType)   :: mtype
  character(len=*)          :: configFile
  type(ESMF_Clock)          :: clock
  integer                   :: rc

  type(ESMF_Clock)   :: local_clock
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! Construct a local copy of the incoming clock with initializers
  call ESMF_ClockGetThis(clock, this, rc=rc)
  call ESMF_ClockSetThis(local_clock, this, rc=rc)
  call ESMF_ClockSetInitCreated(local_clock)

  gcomp = ESMF_GridCompCreate(name=name, gridcomptype=mtype, &
    configFile=configFile, clock=local_clock, rc=rc)
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
  integer             :: rc              

  call ESMF_GridCompDestroy(comp, rc)
end subroutine f_esmf_gridcompdestroy

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompinitialize"
subroutine f_esmf_gridcompinitialize(comp, importState, exportState, clock, &
  phase, blockingFlag, userRc, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp)     :: comp      
  type(ESMF_State)        :: importState
  type(ESMF_State)        :: exportState
  type(ESMF_Clock)        :: clock
  integer                 :: phase
  type(ESMF_BlockingFlag) :: blockingFlag
  integer                 :: userRc
  integer                 :: rc

  type(ESMF_Clock)   :: local_clock
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! Construct a local copy of the incoming clock with initializers
  call ESMF_ClockGetThis(clock, this, rc=rc)
  call ESMF_ClockSetThis(local_clock, this, rc=rc)
  call ESMF_ClockSetInitCreated(local_clock)

  call ESMF_GridCompInitialize(comp, importState, exportState, local_clock, &
    phase, blockingFlag, userRc, rc)
end subroutine f_esmf_gridcompinitialize

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcomprun"
subroutine f_esmf_gridcomprun(comp, importState, exportState, clock, phase, &
  blockingFlag, userRc, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp)     :: comp      
  type(ESMF_State)        :: importState
  type(ESMF_State)        :: exportState
  type(ESMF_Clock)        :: clock
  integer                 :: phase
  type(ESMF_BlockingFlag) :: blockingFlag
  integer                 :: userRc
  integer                 :: rc

  type(ESMF_Clock)   :: local_clock
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! Construct a local copy of the incoming clock with initializers
  call ESMF_ClockGetThis(clock, this, rc=rc)
  call ESMF_ClockSetThis(local_clock, this, rc=rc)
  call ESMF_ClockSetInitCreated(local_clock)

  call ESMF_GridCompRun(comp, importState, exportState, local_clock, &
    phase, blockingFlag, userRc, rc)
end subroutine f_esmf_gridcomprun

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_gridcompfinalize"
subroutine f_esmf_gridcompfinalize(comp, importState, exportState, clock, &
  phase, blockingFlag, userRc, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_GridCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_GridComp)     :: comp      
  type(ESMF_State)        :: importState
  type(ESMF_State)        :: exportState
  type(ESMF_Clock)        :: clock
  integer                 :: phase
  type(ESMF_BlockingFlag) :: blockingFlag
  integer                 :: userRc
  integer                 :: rc

  type(ESMF_Clock)   :: local_clock
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! Construct a local copy of the incoming clock with initializers
  call ESMF_ClockGetThis(clock, this, rc=rc)
  call ESMF_ClockSetThis(local_clock, this, rc=rc)
  call ESMF_ClockSetInitCreated(local_clock)

  call ESMF_GridCompFinalize(comp, importState, exportState, local_clock, &
    phase, blockingFlag, userRc, rc)
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
  integer             :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompSet(comp)

  rc = ESMF_SUCCESS
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
  integer             :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

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
  character(len=*)    :: options
  integer             :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

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
  character(len=*)    :: options
  integer             :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_GridCompPrint(comp, options, rc)
end subroutine f_esmf_gridcompprint


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompcreate"
subroutine f_esmf_cplcompcreate(ccomp, name, configFile, clock, rc)
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
  character(len=*)   :: name
  character(len=*)   :: configFile
  type(ESMF_Clock)   :: clock
  integer            :: rc

  type(ESMF_Clock)   :: local_clock
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! Construct a local copy of the incoming clock with initializers
  call ESMF_ClockGetThis(clock, this, rc=rc)
  call ESMF_ClockSetThis(local_clock, this, rc=rc)
  call ESMF_ClockSetInitCreated(local_clock)

  ccomp = ESMF_CplCompCreate(name=name, configFile=configFile, &
    clock=local_clock, rc=rc)
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
  integer            :: rc              

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompDestroy(comp, rc)
end subroutine f_esmf_cplcompdestroy

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompinitialize"
subroutine f_esmf_cplcompinitialize(comp, importState, exportState, clock, &
  phase, blockingFlag, userRc, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp)      :: comp      
  type(ESMF_State)        :: importState
  type(ESMF_State)        :: exportState
  type(ESMF_Clock)        :: clock
  integer                 :: phase
  type(ESMF_BlockingFlag) :: blockingFlag
  integer                 :: userRc     
  integer                 :: rc     

  type(ESMF_Clock)   :: local_clock
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! Construct a local copy of the incoming clock with initializers
  call ESMF_ClockGetThis(clock, this, rc=rc)
  call ESMF_ClockSetThis(local_clock, this, rc=rc)
  call ESMF_ClockSetInitCreated(local_clock)

  call ESMF_CplCompInitialize(comp, importState, exportState, local_clock, &
    phase, blockingFlag, userRc, rc)
end subroutine f_esmf_cplcompinitialize

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcomprun"
subroutine f_esmf_cplcomprun(comp, importState, exportState, clock, phase, &
  blockingFlag, userRc, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp)      :: comp      
  type(ESMF_State)        :: importState
  type(ESMF_State)        :: exportState
  type(ESMF_Clock)        :: clock
  integer                 :: phase
  type(ESMF_BlockingFlag) :: blockingFlag
  integer                 :: userRc     
  integer                 :: rc     

  type(ESMF_Clock)   :: local_clock
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! Construct a local copy of the incoming clock with initializers
  call ESMF_ClockGetThis(clock, this, rc=rc)
  call ESMF_ClockSetThis(local_clock, this, rc=rc)
  call ESMF_ClockSetInitCreated(local_clock)

  call ESMF_CplCompRun(comp, importState, exportState, local_clock, &
    phase, blockingFlag, userRc, rc)
end subroutine f_esmf_cplcomprun

#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_cplcompfinalize"
subroutine f_esmf_cplcompfinalize(comp, importState, exportState, clock, &
  phase, blockingFlag, userRc, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_ClockMod
  use ESMF_ClockTypeMod
  use ESMF_StateMod
  use ESMF_CompMod
  use ESMF_CplCompMod
  use ESMF_InitMacrosMod
  implicit none

  type(ESMF_CplComp)      :: comp      
  type(ESMF_State)        :: importState
  type(ESMF_State)        :: exportState
  type(ESMF_Clock)        :: clock
  integer                 :: phase
  type(ESMF_BlockingFlag) :: blockingFlag
  integer                 :: userRc
  integer                 :: rc

  type(ESMF_Clock)   :: local_clock
  type(ESMF_Pointer) :: this

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  ! Construct a local copy of the incoming clock with initializers
  call ESMF_ClockGetThis(clock, this, rc=rc)
  call ESMF_ClockSetThis(local_clock, this, rc=rc)
  call ESMF_ClockSetInitCreated(local_clock)

  call ESMF_CplCompFinalize(comp, importState, exportState, local_clock, &
    phase, blockingFlag, userRc, rc)
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
  integer            :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompSet(comp)
  rc = ESMF_SUCCESS

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
  integer            :: rc     

  rc = ESMF_SUCCESS

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
  character(len=*)   :: options
  integer            :: rc     

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
  character(len=*)   :: options
  integer            :: rc     

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

  call ESMF_CplCompPrint(comp, options, rc)
end subroutine f_esmf_cplcompprint
