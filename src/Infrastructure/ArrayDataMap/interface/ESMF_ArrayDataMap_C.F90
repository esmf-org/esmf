! $Id: ESMF_ArrayDataMap_C.F90,v 1.1 2004/05/03 16:12:58 nscollins Exp $
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
!     ESMF ArrayDataMap module
      module ESMF_ArrayDataMapMod
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
      character(*), parameter, private :: version = &
      '$Id: ESMF_ArrayDataMap_C.F90,v 1.1 2004/05/03 16:12:58 nscollins Exp $'

!==============================================================================
! 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the ArrayDataMap Create and Destroy methods for 
!  deep classes only.  See the Init methods for shallow classes.

!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapcreate(arraydatamapp, arg1, arg2, arg3, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      integer, intent(in) :: arg3
      integer, intent(out), :: rc

      ! Local variable
      type(ESMF_ArrayDataMap), target :: thearraydm
     
      thearraydm = ESMF_ArrayDataMapCreate(arraydmp, arg1, arg2, arg3, rc)
    
      arraydmp => thearraydm
    end subroutine f_esmf_arraydatamapcreate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapdestroy(arraydatamapp, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      integer, intent(out), :: rc

     
      call ESMF_ArrayDataMapDestroy(arraydmp, rc)
    
    end subroutine f_esmf_arraydatamapdestroy
  

!------------------------------------------------------------------------------
!! for shallow classes, use init instead of create and destroy

!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapinit(arraydatamapp, arg1, arg2, arg3, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      integer, intent(in) :: arg3
      integer, intent(out), :: rc
     

      call ESMF_ArrayDataMapInit(arraydmp, arg1, arg2, arg3, rc)
    
    end subroutine f_esmf_arraydatamapinit
  

!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapgetconfig(arraydatamapp, config, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      type(ESMF_ArrayDataMapConfig), pointer :: config
      integer, intent(out), :: rc
     

      call ESMF_ArrayDataMapGetConfig(arraydmp, config, rc)
    
    end subroutine f_esmf_arraydatamapgetconfig
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapsetconfig(arraydatamapp, config, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      type(ESMF_ArrayDataMapConfig), pointer :: config
      integer, intent(out), :: rc
     

      call ESMF_ArrayDataMapSetConfig(arraydmp, config, rc)
    
    end subroutine f_esmf_arraydatamapsetconfig
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapget(arraydatamapp, value, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      type(ESMF_<Value>) :: value
      integer, intent(out), :: rc
     

      call ESMF_ArrayDataMapGet(arraydmp, value, rc)
    
    end subroutine f_esmf_arraydatamapget
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapset(arraydatamapp, value, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      type(ESMF_<Value>) :: value
      integer, intent(out), :: rc
     

      call ESMF_ArrayDataMapSet(arraydmp, value, rc)
    
    end subroutine f_esmf_arraydatamapset
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapvalidate(arraydatamapp, options, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      character(len=*) :: options
      integer, intent(out), :: rc
     

      call ESMF_ArrayDataMapValidate(arraydmp, options, rc)
    
    end subroutine f_esmf_arraydatamapvalidate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_arraydatamapprint(arraydatamapp, options, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_ArrayDataMapMod

      type(ESMF_ArrayDataMap), pointer :: arraydmp
      character(len=*) :: options
      integer, intent(out), :: rc
     

      call ESMF_ArrayDataMapPrint(arraydmp, options, rc)
    
    end subroutine f_esmf_arraydatamapprint
  

