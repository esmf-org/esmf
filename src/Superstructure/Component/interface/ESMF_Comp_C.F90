!  $Id: ESMF_Comp_C.F90,v 1.7 2003/10/16 20:23:29 nscollins Exp $
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
!      '$Id: ESMF_Comp_C.F90,v 1.7 2003/10/16 20:23:29 nscollins Exp $'
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
   subroutine f_esmf_frameworkinitialize(lang, rc)
       use ESMF_CompMod

       integer :: lang
       integer :: rc

       call ESMF_FrameworkInternalInit(lang, rc)

   end subroutine f_esmf_frameworkinitialize

   subroutine f_esmf_frameworkfinalize(rc)
       use ESMF_CompMod

       integer :: rc

       call ESMF_FrameworkFinalize(rc)

   end subroutine f_esmf_frameworkfinalize

#if 0
   function f_esmf_appcompcreate(name, rc)
       !use ESMF_BaseMod    ! ESMF base class
       !use ESMF_CompMod
       use ESMF_AppCompMod

       type(ESMF_AppComp) :: f_esmf_appcompcreate
       character(*) :: name
       integer :: rc              

       f_esmf_appcompcreate = ESMF_AppCompCreate(name, rc=rc)
    
   end function f_esmf_appcompcreate

   subroutine f_esmf_appcompdestroy(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       !use ESMF_CompMod
       use ESMF_AppCompMod

       type(ESMF_AppComp) :: comp
       integer :: rc              

       call ESMF_AppCompDestroy(comp, rc)
    
   end subroutine f_esmf_appcompdestroy
#endif

!------------------------------------------------------------------------------

   subroutine f_esmf_gridcompcreate(gcomp, name, layout, mtype, grid, &
                                     config, configfile, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_DELayoutMod
       use ESMF_GridMod
       use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: gcomp
       character(len=*) :: name
       type(ESMF_DELayout) :: layout
       type(ESMF_ModelType) :: mtype
       type(ESMF_Grid) :: grid
       type(ESMF_Config) :: config
       character(len=*) :: configfile
       integer :: rc

       gcomp = ESMF_GridCompCreate(name, layout, mtype, grid, &
                                       config, configfile, rc)
    
   end subroutine f_esmf_gridcompcreate

   subroutine f_esmf_gridcompdestroy(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       !use ESMF_CompMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp
       integer :: rc              

       call ESMF_GridCompDestroy(comp, rc)
    
   end subroutine f_esmf_gridcompdestroy

   subroutine f_esmf_gridcompinitialize(comp, importstate, exportstate, &
                                        clock, phase, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       type(ESMF_State) :: importstate
       type(ESMF_State) :: exportstate
       type(ESMF_Clock) :: clock
       integer :: phase
       integer :: rc     

       call ESMF_GridCompInitialize(comp, importstate, exportstate, &
                                    clock, phase, rc)

   end subroutine f_esmf_gridcompinitialize

   subroutine f_esmf_gridcomprun(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       type(ESMF_State) :: importstate
       type(ESMF_State) :: exportstate
       type(ESMF_Clock) :: clock
       integer :: phase
       integer :: rc     

       call ESMF_GridCompRun(comp, importstate, exportstate, &
                             clock, phase, rc)

   end subroutine f_esmf_gridcomprun

   subroutine f_esmf_gridcompfinalize(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       type(ESMF_State) :: importstate
       type(ESMF_State) :: exportstate
       type(ESMF_Clock) :: clock
       integer :: phase
       integer :: rc     

       call ESMF_GridCompFinalize(comp, importstate, exportstate, &
                                  clock, phase, rc)

   end subroutine f_esmf_gridcompfinalize
 
   subroutine f_esmf_gridcompset(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       integer :: rc     

       call ESMF_GridCompSet(comp)

   end subroutine f_esmf_gridcompset
 
   subroutine f_esmf_gridcompget(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       integer :: rc     

       call ESMF_GridCompGet(comp)

   end subroutine f_esmf_gridcompget

   subroutine f_esmf_gridcompvalidate(comp, options, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       character(len=*) :: options
       integer :: rc     

       call ESMF_GridCompValidate(comp, options, rc)

   end subroutine f_esmf_gridcompvalidate

   subroutine f_esmf_gridcompprint(comp, options, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_GridCompMod

       type(ESMF_GridComp) :: comp      
       character(len=*) :: options
       integer :: rc     

       call ESMF_GridCompPrint(comp, options, rc)

   end subroutine f_esmf_gridcompprint


!------------------------------------------------------------------------------

   subroutine f_esmf_cplcompcreate(ccomp, name, layout, config, configfile, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_DELayoutMod
       use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: ccomp
       character(len=*) :: name
       type(ESMF_DELayout) :: layout
       type(ESMF_Config) :: config
       character(len=*) :: configfile
       integer :: rc

       ccomp = ESMF_CplCompCreate(name, layout, config, configfile, rc)
    
   end subroutine f_esmf_cplcompcreate

   subroutine f_esmf_cplcompdestroy(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       !use ESMF_CompMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp
       integer :: rc              

       call ESMF_CplCompDestroy(comp, rc)
    
   end subroutine f_esmf_cplcompdestroy

   subroutine f_esmf_cplcompinitialize(comp, importstate, exportstate, &
                                        clock, phase, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: importstate
       type(ESMF_State) :: exportstate
       type(ESMF_Clock) :: clock
       integer :: phase
       integer :: rc     

       call ESMF_CplCompInitialize(comp, importstate, exportstate, &
                                    clock, phase, rc)

   end subroutine f_esmf_cplcompinitialize

   subroutine f_esmf_cplcomprun(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: importstate
       type(ESMF_State) :: exportstate
       type(ESMF_Clock) :: clock
       integer :: phase
       integer :: rc     

       call ESMF_CplCompRun(comp, importstate, exportstate, &
                             clock, phase, rc)

   end subroutine f_esmf_cplcomprun

   subroutine f_esmf_cplcompfinalize(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_ClockMod
       use ESMF_StateMod
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       type(ESMF_State) :: importstate
       type(ESMF_State) :: exportstate
       type(ESMF_Clock) :: clock
       integer :: phase
       integer :: rc     

       call ESMF_CplCompFinalize(comp, importstate, exportstate, &
                                  clock, phase, rc)

   end subroutine f_esmf_cplcompfinalize
 
   subroutine f_esmf_cplcompset(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       integer :: rc     

       call ESMF_CplCompSet(comp)

   end subroutine f_esmf_cplcompset
 
   subroutine f_esmf_cplcompget(comp, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       integer :: rc     

       call ESMF_CplCompGet(comp)

   end subroutine f_esmf_cplcompget

   subroutine f_esmf_cplcompvalidate(comp, options, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       character(len=*) :: options
       integer :: rc     

       call ESMF_CplCompValidate(comp, options, rc)

   end subroutine f_esmf_cplcompvalidate

   subroutine f_esmf_cplcompprint(comp, options, rc)
       !use ESMF_BaseMod    ! ESMF base class
       use ESMF_CplCompMod

       type(ESMF_CplComp) :: comp      
       character(len=*) :: options
       integer :: rc     

       call ESMF_CplCompPrint(comp, options, rc)

   end subroutine f_esmf_cplcompprint


!------------------------------------------------------------------------------


 

