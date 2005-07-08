!  $Id: ESMF_LogErr_C.F90,v 1.5 2005/07/08 21:24:58 nscollins Exp $
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
!  This cannot use any F90 modules.
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
!      '$Id: ESMF_LogErr_C.F90,v 1.5 2005/07/08 21:24:58 nscollins Exp $'
!==============================================================================

   subroutine f_esmf_logwritenoform(message, length, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod         ! ESMF base class
       use ESMF_LogErrMod
     character(len=ESMF_MAXSTR*2), intent(in) :: message
     integer, intent(in) :: length
     integer, intent(out), optional :: rc              

     integer :: localrc              

     ! TODO: fix this
     !call ESMF_LogWriteNoForm(message, ESMF_LOG_ERROR, rc=localrc)
     localrc = ESMF_FAILURE
    
     if (present(rc)) rc = localrc
   end subroutine f_esmf_logwritenoform


