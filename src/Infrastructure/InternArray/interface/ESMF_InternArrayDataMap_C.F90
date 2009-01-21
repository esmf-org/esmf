! $Id: ESMF_InternArrayDataMap_C.F90,v 1.4.2.2 2009/01/21 21:25:21 cdeluca Exp $
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
! F77 interface files for C++ layer calling into F90 implementation layer.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_ArrayDataMap - Glue code between C++ and an F90 implementation.
!
! !DESCRIPTION:
!
! The code in this file is called by C++ and calls into an F90 implementation
! of the {\tt ArrayDataMap} class.
!
!
!EOP

!==============================================================================
! The following line turns the CVS identifier string into a printable variable.
!   character(*), parameter, private :: version = &
!     '$Id: ESMF_InternArrayDataMap_C.F90,v 1.4.2.2 2009/01/21 21:25:21 cdeluca Exp $'
!==============================================================================
! 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! for shallow classes, use init instead of create and destroy
!
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapsetdefault(admp, dataRank, dataIndexList, counts, rc)
      use ESMF_UtilTypesMod    ! ESMF base class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_InternArrayDataMapMod

      type(ESMF_InternArrayDataMap) :: admp
      integer, intent(in) :: dataRank 
      integer, dimension(:), intent(in) :: dataIndexList
      integer, dimension(:), intent(in), optional :: counts
      integer, optional, intent(out) :: rc
     

      call ESMF_ArrayDataMapSetDefault(admp, dataRank, dataIndexList, counts, rc)
    
    end subroutine f_esmf_arraydatamapsetdefault
  

!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapget(admp, rc)
      use ESMF_UtilTypesMod  ! ESMF generic types class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_InternArrayDataMapMod

      type(ESMF_InternArrayDataMap) :: admp
      ! TODO: add values to get and set
      integer, optional, intent(out) :: rc
     

      call ESMF_ArrayDataMapGet(admp, rc=rc)
    
    end subroutine f_esmf_arraydatamapget
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapset(admp, rc)
      use ESMF_UtilTypesMod  ! ESMF generic types class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_InternArrayDataMapMod

      type(ESMF_InternArrayDataMap) :: admp
      ! TODO: add values to get and set
      integer, optional, intent(out) :: rc
     

      call ESMF_ArrayDataMapSet(admp, rc=rc)
    
    end subroutine f_esmf_arraydatamapset
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapvalidate(admp, options, rc)
      use ESMF_UtilTypesMod  ! ESMF generic types class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_InternArrayDataMapMod

      type(ESMF_InternArrayDataMap) :: admp
      character(len=*) :: options
      integer, optional, intent(out) :: rc
     

      call ESMF_ArrayDataMapValidate(admp, options, rc)
    
    end subroutine f_esmf_arraydatamapvalidate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapprint(admp, options, rc)
      use ESMF_UtilTypesMod  ! ESMF generic types class
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_InternArrayDataMapMod

      type(ESMF_InternArrayDataMap) :: admp
      character(len=*) :: options
      integer, optional, intent(out) :: rc
     

      call ESMF_ArrayDataMapPrint(admp, options, rc)
    
    end subroutine f_esmf_arraydatamapprint
  

