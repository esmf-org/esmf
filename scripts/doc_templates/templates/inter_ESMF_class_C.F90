! $Id: inter_ESMF_class_C.F90,v 1.3.2.3 2010/02/01 20:48:49 svasquez Exp $
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
!
!     ESMF <Class> module
      module ESMF_<Class>Mod
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
! !MODULE: ESMF_<Class> - Glue code between C++ and an F90 implementation.
!
! !DESCRIPTION:
!
! The code in this file is called by C++ and calls into an F90 implementation
! of the {\tt <Class>} class.
!
!
!EOP

!==============================================================================
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: inter_ESMF_class_C.F90,v 1.3.2.3 2010/02/01 20:48:49 svasquez Exp $'

!==============================================================================
! 
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the <Class> Create and Destroy methods for 
!  deep classes only.  See the Init methods for shallow classes.

!------------------------------------------------------------------------------
    subroutine f_esmf_<class>create(<class>p, arg1, arg2, arg3, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      integer, intent(in) :: arg3
      integer, intent(out), :: rc

      ! Local variable
      type(ESMF_<Class>), target :: the<class>
     
      the<class> = ESMF_<Class>Create(<class>p, arg1, arg2, arg3, rc)
    
      <class>p => the<class>
    end subroutine f_esmf_<class>create
  
!------------------------------------------------------------------------------
    subroutine f_esmf_<class>destroy(<class>p, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      integer, intent(out), :: rc

     
      call ESMF_<Class>Destroy(<class>p, rc)
    
    end subroutine f_esmf_<class>destroy
  

!------------------------------------------------------------------------------
!! for shallow classes, use init instead of create and destroy

!------------------------------------------------------------------------------
    subroutine f_esmf_<class>init(<class>p, arg1, arg2, arg3, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      integer, intent(in) :: arg1
      integer, intent(in) :: arg2
      integer, intent(in) :: arg3
      integer, intent(out), :: rc
     

      call ESMF_<Class>Init(<class>p, arg1, arg2, arg3, rc)
    
    end subroutine f_esmf_<class>init
  

!------------------------------------------------------------------------------
    subroutine f_esmf_<class>getconfig(<class>p, config, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      type(ESMF_<Class>Config), pointer :: config
      integer, intent(out), :: rc
     

      call ESMF_<Class>GetConfig(<class>p, config, rc)
    
    end subroutine f_esmf_<class>getconfig
  
!------------------------------------------------------------------------------
    subroutine f_esmf_<class>setconfig(<class>p, config, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      type(ESMF_<Class>Config), pointer :: config
      integer, intent(out), :: rc
     

      call ESMF_<Class>SetConfig(<class>p, config, rc)
    
    end subroutine f_esmf_<class>setconfig
  
!------------------------------------------------------------------------------
    subroutine f_esmf_<class>get(<class>p, <value>, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      type(ESMF_<Value>) :: <value>
      integer, intent(out), :: rc
     

      call ESMF_<Class>Get(<class>p, <value>, rc)
    
    end subroutine f_esmf_<class>get
  
!------------------------------------------------------------------------------
    subroutine f_esmf_<class>set(<class>p, <value>, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      type(ESMF_<Value>) :: <value>
      integer, intent(out), :: rc
     

      call ESMF_<Class>Set(<class>p, <value>, rc)
    
    end subroutine f_esmf_<class>set
  
!------------------------------------------------------------------------------
    subroutine f_esmf_<class>validate(<class>p, options, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      character(len=*) :: options
      integer, intent(out), :: rc
     

      call ESMF_<Class>Validate(<class>p, options, rc)
    
    end subroutine f_esmf_<class>validate
  
!------------------------------------------------------------------------------
    subroutine f_esmf_<class>print(<class>p, options, rc)
      use ESMF_BaseMod       ! ESMF_Base class
      use ESMF_<Class>Mod

      type(ESMF_<Class>), pointer :: <class>p
      character(len=*) :: options
      integer, intent(out), :: rc
     

      call ESMF_<Class>Print(<class>p, options, rc)
    
    end subroutine f_esmf_<class>print
  

