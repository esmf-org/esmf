! $Id: ESMF_BundleDataMap_C.F90,v 1.1 2004/05/05 15:40:26 nscollins Exp $
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
!     ESMF BundleDataMap module
      module ESMF_BundleDataMapMod
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
! !MODULE: ESMF_BundleDataMap - Glue code between C++ and an F90 implementation.
!
! !DESCRIPTION:
!
! The code in this file is called by C++ and calls into an F90 implementation
! of the {\tt BundleDataMap} class.
!
!
!EOP

!==============================================================================
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_BundleDataMap_C.F90,v 1.1 2004/05/05 15:40:26 nscollins Exp $'

!==============================================================================
! 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the BundleDataMap Create and Destroy methods for 
!  deep classes only.  See the Init methods for shallow classes.

!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapcreate(bundledatamapp, arg1, arg2, arg3, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      integer, intent(in) :: arg3
      integer, intent(out), :: rc

      ! Local variable
      type(ESMF_BundleDataMap), target :: thebundledatamap
     
      thebundledatamap = ESMF_BundleDataMapCreate(bundledatamapp, arg1, arg2, arg3, rc)
    
      bundledatamapp => thebundledatamap
    end subroutine f_esmf_bundledatamapcreate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapdestroy(bundledatamapp, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      integer, intent(out), :: rc

     
      call ESMF_BundleDataMapDestroy(bundledatamapp, rc)
    
    end subroutine f_esmf_bundledatamapdestroy
  

!------------------------------------------------------------------------------
!! for shallow classes, use init instead of create and destroy

!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapinit(bundledatamapp, arg1, arg2, arg3, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      integer, intent(in) :: arg3
      integer, intent(out), :: rc
     

      call ESMF_BundleDataMapInit(bundledatamapp, arg1, arg2, arg3, rc)
    
    end subroutine f_esmf_bundledatamapinit
  

!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapgetconfig(bundledatamapp, config, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      type(ESMF_BundleDataMapConfig), pointer :: config
      integer, intent(out), :: rc
     

      call ESMF_BundleDataMapGetConfig(bundledatamapp, config, rc)
    
    end subroutine f_esmf_bundledatamapgetconfig
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapsetconfig(bundledatamapp, config, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      type(ESMF_BundleDataMapConfig), pointer :: config
      integer, intent(out), :: rc
     

      call ESMF_BundleDataMapSetConfig(bundledatamapp, config, rc)
    
    end subroutine f_esmf_bundledatamapsetconfig
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapget(bundledatamapp, value, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      integer, intent(out) :: value
      integer, intent(out) :: rc
     

      call ESMF_BundleDataMapGet(bundledatamapp, <value>, rc)
    
    end subroutine f_esmf_bundledatamapget
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapset(bundledatamapp, <value>, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      integer, intent(in) :: value
      integer, intent(out) :: rc
     

      call ESMF_BundleDataMapSet(bundledatamapp, <value>, rc)
    
    end subroutine f_esmf_bundledatamapset
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapvalidate(bundledatamapp, options, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      character(len=*) :: options
      integer, intent(out), :: rc
     

      call ESMF_BundleDataMapValidate(bundledatamapp, options, rc)
    
    end subroutine f_esmf_bundledatamapvalidate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapprint(bundledatamapp, options, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bundledatamapp
      character(len=*) :: options
      integer, intent(out), :: rc
     

      call ESMF_BundleDataMapPrint(bundledatamapp, options, rc)
    
    end subroutine f_esmf_bundledatamapprint
  

