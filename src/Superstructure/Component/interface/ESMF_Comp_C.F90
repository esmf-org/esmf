!  $Id: ESMF_Comp_C.F90,v 1.24 2004/09/22 20:28:43 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
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
!      '$Id: ESMF_Comp_C.F90,v 1.24 2004/09/22 20:28:43 nscollins Exp $'
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


   recursive subroutine f_esmf_compinsertvm(comp, vm, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_VMMod

       type(ESMF_CWrap) :: comp
       type(ESMF_VM)    :: vm
       integer :: rc

       call ESMF_CompSet(comp%compp, vm=vm)

   end subroutine f_esmf_compinsertvm

   recursive subroutine f_esmf_compget(comp, ctype, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod

       type(ESMF_CWrap) :: comp
       type(ESMF_CompType) :: ctype
       integer :: rc

       call ESMF_CompGet(comp%compp, ctype=ctype, rc=rc)

   end subroutine f_esmf_compget

   recursive subroutine f_esmf_compreplicate(comp, comp_src, vm, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_VMMod

       type(ESMF_CWrap) :: comp
       type(ESMF_CWrap) :: comp_src
       type(ESMF_VM)    :: vm
       integer :: rc

       type (ESMF_CompClass), pointer :: compclass  
       
       nullify(comp%compp)
       nullify(compclass)
       allocate(compclass)
       compclass = comp_src%compp
       call ESMF_CompSet(compclass, vm=vm)
       comp%compp => compclass

   end subroutine f_esmf_compreplicate

   recursive subroutine f_esmf_compcopy(comp, comp_src, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_VMMod

       type(ESMF_CWrap) :: comp_src
       type(ESMF_CWrap) :: comp
       integer :: rc

       type (ESMF_CompClass), pointer :: compclass_src, compclass
       
       compclass_src => comp_src%compp
       compclass => comp%compp
       compclass = compclass_src

   end subroutine f_esmf_compcopy

   recursive subroutine f_esmf_compdelete(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       
       type(ESMF_CWrap) :: comp
       integer :: rc
       
       deallocate(comp%compp)
       nullify(comp%compp)
       
   end subroutine f_esmf_compdelete

   subroutine f_esmf_gridcompcreate(gcomp, name, mtype, grid, &
                                     config, configFile, clock, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_ConfigMod
       use ESMF_ClockMod
       use ESMF_GridTypesMod
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: gcomp
       character(len=*) :: name
       type(ESMF_GridCompType) :: mtype
       type(ESMF_Grid) :: grid
       type(ESMF_Config) :: config
       character(len=*) :: configFile
       type(ESMF_Clock) :: clock
       integer :: rc

       gcomp = ESMF_GridCompCreate(name, mtype, grid, &
                                       config, configFile, clock, rc)
    
   end subroutine f_esmf_gridcompcreate

   subroutine f_esmf_gridcompdestroy(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp
       integer :: rc              

       call ESMF_GridCompDestroy(comp, rc)
    
   end subroutine f_esmf_gridcompdestroy

   subroutine f_esmf_gridcompinitialize(comp, importState, exportState, &
                                        clock, phase, blockingFlag, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       type(ESMF_State) :: importState
       type(ESMF_State) :: exportState
       type(ESMF_Clock) :: clock
       integer :: phase
       type(ESMF_BlockingFlag) :: blockingFlag
       integer :: rc     

       call ESMF_GridCompInitialize(comp, importState, exportState, &
                                    clock, phase, blockingFlag, rc)

   end subroutine f_esmf_gridcompinitialize

   subroutine f_esmf_gridcomprun(comp, importState, exportState, &
                                        clock, phase, blockingFlag, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       type(ESMF_State) :: importState
       type(ESMF_State) :: exportState
       type(ESMF_Clock) :: clock
       integer :: phase
       type(ESMF_BlockingFlag) :: blockingFlag
       integer :: rc     

       call ESMF_GridCompRun(comp, importState, exportState, &
                             clock, phase, blockingFlag, rc)

   end subroutine f_esmf_gridcomprun

   subroutine f_esmf_gridcompfinalize(comp, importState, exportState, &
                                        clock, phase, blockingFlag, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       type(ESMF_State) :: importState
       type(ESMF_State) :: exportState
       type(ESMF_Clock) :: clock
       integer :: phase
       type(ESMF_BlockingFlag) :: blockingFlag
       integer :: rc     

       call ESMF_GridCompFinalize(comp, importState, exportState, &
                                  clock, phase, blockingFlag, rc)

   end subroutine f_esmf_gridcompfinalize
 
   subroutine f_esmf_gridcompset(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       integer :: rc     

       call ESMF_GridCompSet(comp)

   end subroutine f_esmf_gridcompset
 
   subroutine f_esmf_gridcompget(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       integer :: rc     

       call ESMF_GridCompGet(comp)

   end subroutine f_esmf_gridcompget

   subroutine f_esmf_gridcompvalidate(comp, options, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       character(len=*) :: options
       integer :: rc     

       call ESMF_GridCompValidate(comp, options, rc)

   end subroutine f_esmf_gridcompvalidate

   subroutine f_esmf_gridcompprint(comp, options, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       character(len=*) :: options
       integer :: rc     

       call ESMF_GridCompPrint(comp, options, rc)

   end subroutine f_esmf_gridcompprint


!------------------------------------------------------------------------------

   subroutine f_esmf_cplcompcreate(ccomp, name, config, configFile, clock, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: ccomp
       character(len=*) :: name
       type(ESMF_Config) :: config
       character(len=*) :: configFile
       type(ESMF_Clock) :: clock
       integer :: rc

       ccomp = ESMF_CplCompCreate(name, config, configFile, clock, rc)
    
   end subroutine f_esmf_cplcompcreate

   subroutine f_esmf_cplcompdestroy(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp
       integer :: rc              

       call ESMF_CplCompDestroy(comp, rc)
    
   end subroutine f_esmf_cplcompdestroy

   subroutine f_esmf_cplcompinitialize(comp, importState, exportState, &
                                        clock, phase, blockingFlag, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: importState
       type(ESMF_State) :: exportState
       type(ESMF_Clock) :: clock
       integer :: phase
       type(ESMF_BlockingFlag) :: blockingFlag
       integer :: rc     

       call ESMF_CplCompInitialize(comp, importState, exportState, &
                                    clock, phase, blockingFlag, rc)

   end subroutine f_esmf_cplcompinitialize

   subroutine f_esmf_cplcomprun(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: importState
       type(ESMF_State) :: exportState
       type(ESMF_Clock) :: clock
       integer :: phase
       type(ESMF_BlockingFlag) :: blockingFlag
       integer :: rc     

       call ESMF_CplCompRun(comp, importState, exportState, &
                             clock, phase, blockingFlag, rc)

   end subroutine f_esmf_cplcomprun

   subroutine f_esmf_cplcompfinalize(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: importState
       type(ESMF_State) :: exportState
       type(ESMF_Clock) :: clock
       integer :: phase
       type(ESMF_BlockingFlag) :: blockingFlag
       integer :: rc     

       call ESMF_CplCompFinalize(comp, importState, exportState, &
                                  clock, phase, blockingFlag, rc)

   end subroutine f_esmf_cplcompfinalize
 
   subroutine f_esmf_cplcompset(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       integer :: rc     

       call ESMF_CplCompSet(comp)

   end subroutine f_esmf_cplcompset
 
   subroutine f_esmf_cplcompget(comp, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       integer :: rc     

       call ESMF_CplCompGet(comp)

   end subroutine f_esmf_cplcompget

   subroutine f_esmf_cplcompvalidate(comp, options, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       character(len=*) :: options
       integer :: rc     

       call ESMF_CplCompValidate(comp, options, rc)

   end subroutine f_esmf_cplcompvalidate

   subroutine f_esmf_cplcompprint(comp, options, rc)
       use ESMF_BaseTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       character(len=*) :: options
       integer :: rc     

       call ESMF_CplCompPrint(comp, options, rc)

   end subroutine f_esmf_cplcompprint


!------------------------------------------------------------------------------


 

