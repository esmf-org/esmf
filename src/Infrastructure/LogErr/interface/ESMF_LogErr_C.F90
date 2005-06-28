!  $Id: ESMF_LogErr_C.F90,v 1.1 2005/06/28 19:54:47 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_LogErr_C.F90,v 1.1 2005/06/28 19:54:47 nscollins Exp $'
!==============================================================================

   subroutine f_esmf_logwrite(message, rc)

       use ESMF_UtilTypesMod    ! ESMF utility functions and parameters
       use ESMF_LogErrMod

     character(len=*), intent(in) :: message
     integer, intent(out) :: rc              

     integer :: localrc              

     call ESMF_LogWrite(message, ESMF_LOG_INFO, rc=local)
    
     rc = localrc

   end subroutine f_esmf_logwrite


