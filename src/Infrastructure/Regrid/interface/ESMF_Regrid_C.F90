! $Id: ESMF_Regrid_C.F90,v 1.5.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
!
!     ESMF Regrid module
      module ESMF_RegridMod
!
!==============================================================================
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_Regrid - Glue code between C++ and an F90 implementation.
!
! !DESCRIPTION:
!
! The code in this file is called by C++ and calls into an F90 implementation
! of the {\tt Regrid} class.
!
!
!EOP

!==============================================================================
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Regrid_C.F90,v 1.5.2.2 2009/01/21 21:25:23 cdeluca Exp $'

!==============================================================================
! 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Regrid Create and Destroy methods for 
!  deep classes only.  See the Init methods for shallow classes.

!------------------------------------------------------------------------------
    subroutine f_esmf_regridcreate(regridp, arg1, arg2, arg3, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      integer, intent(in) :: arg3
      integer, intent(out), :: rc

      ! Local variable
      type(ESMF_Regrid), target :: theregrid
     
      theregrid = ESMF_RegridCreate(regridp, arg1, arg2, arg3, rc)
    
      regridp => theregrid
    end subroutine f_esmf_regridcreate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_regriddestroy(regridp, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      integer, intent(out), :: rc

     
      call ESMF_RegridDestroy(regridp, rc)
    
    end subroutine f_esmf_regriddestroy
  

!------------------------------------------------------------------------------
!! for shallow classes, use init instead of create and destroy

!------------------------------------------------------------------------------
    subroutine f_esmf_regridinit(regridp, arg1, arg2, arg3, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      integer, intent(in) :: arg3
      integer, intent(out), :: rc
     

      call ESMF_RegridInit(regridp, arg1, arg2, arg3, rc)
    
    end subroutine f_esmf_regridinit
  

!------------------------------------------------------------------------------
    subroutine f_esmf_regridgetconfig(regridp, config, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      type(ESMF_RegridConfig), pointer :: config
      integer, intent(out), :: rc
     

      call ESMF_RegridGetConfig(regridp, config, rc)
    
    end subroutine f_esmf_regridgetconfig
  
!------------------------------------------------------------------------------
    subroutine f_esmf_regridsetconfig(regridp, config, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      type(ESMF_RegridConfig), pointer :: config
      integer, intent(out), :: rc
     

      call ESMF_RegridSetConfig(regridp, config, rc)
    
    end subroutine f_esmf_regridsetconfig
  
!------------------------------------------------------------------------------
    subroutine f_esmf_regridget(regridp, <value>, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      type(ESMF_<Value>) :: <value>
      integer, intent(out), :: rc
     

      call ESMF_RegridGet(regridp, <value>, rc)
    
    end subroutine f_esmf_regridget
  
!------------------------------------------------------------------------------
    subroutine f_esmf_regridset(regridp, <value>, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      type(ESMF_<Value>) :: <value>
      integer, intent(out), :: rc
     

      call ESMF_RegridSet(regridp, <value>, rc)
    
    end subroutine f_esmf_regridset
  
!------------------------------------------------------------------------------
    subroutine f_esmf_regridvalidate(regridp, options, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      character(len=*) :: options
      integer, intent(out), :: rc
     

      call ESMF_RegridValidate(regridp, options, rc)
    
    end subroutine f_esmf_regridvalidate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_regridprint(regridp, options, rc)
      use ESMF_UtilTypesMod
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_RegridMod

      type(ESMF_Regrid), pointer :: regridp
      character(len=*) :: options
      integer, intent(out), :: rc
     

      call ESMF_RegridPrint(regridp, options, rc)
    
    end subroutine f_esmf_regridprint
  

