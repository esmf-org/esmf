! $Id: ESMF_BundleDataMap_C.F90,v 1.10 2007/03/31 05:50:54 cdeluca Exp $
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
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_BundleDataMap_C.F90,v 1.10 2007/03/31 05:50:54 cdeluca Exp $'
!==============================================================================
! 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! for shallow classes, use init instead of create and destroy

!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapsetdefault(bdmp, btype, rc)
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_InternArrayDataMapMod
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bdmp
      type(ESMF_InterleaveFlag), intent(in) :: btype
      integer, intent(out), optional :: rc
     

      call ESMF_BundleDataMapSetDefault(bdmp, btype, rc)
    
    end subroutine f_esmf_bundledatamapsetdefault
  

!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapget(bdmp, btype, rc)
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_InternArrayDataMapMod
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bdmp
      type(ESMF_InterleaveFlag), intent(out) :: btype
      integer, intent(out) :: rc
     

      call ESMF_BundleDataMapGet(bdmp, btype, rc)
    
    end subroutine f_esmf_bundledatamapget
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapset(bdmp, btype, rc)
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_InternArrayDataMapMod
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bdmp
      type(ESMF_InterleaveFlag), intent(in) :: btype
      integer, intent(out) :: rc
     

      call ESMF_BundleDataMapSet(bdmp, btype, rc)
    
    end subroutine f_esmf_bundledatamapset
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapvalidate(bdmp, options, rc)
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bdmp
      character(len=*) :: options
      integer, intent(out), optional :: rc
     

      call ESMF_BundleDataMapValidate(bdmp, options, rc)
    
    end subroutine f_esmf_bundledatamapvalidate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_bundledatamapprint(bdmp, options, rc)
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_BundleDataMapMod

      type(ESMF_BundleDataMap), pointer :: bdmp
      character(len=*) :: options
      integer, intent(out), optional :: rc
     

      call ESMF_BundleDataMapPrint(bdmp, options, rc)
    
    end subroutine f_esmf_bundledatamapprint
  

