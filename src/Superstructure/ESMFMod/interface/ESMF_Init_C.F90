!  $Id: ESMF_Init_C.F90,v 1.3 2004/04/23 17:30:06 nscollins Exp $
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
!      '$Id: ESMF_Init_C.F90,v 1.3 2004/04/23 17:30:06 nscollins Exp $'
!==============================================================================

!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
! 
! The code in this file implements the interface code between C++ and F90
!  for the Framework-wide Initialization and Finalization methods.
! 
!EOP
!------------------------------------------------------------------------------
   subroutine f_esmf_frameworkinitialize(lang, defaultConfigFileName, &
                                        defaultCalendar, defaultLogFileName, rc)
       use ESMF_CalendarMod
       use ESMF_CompMod
       use ESMF_InitMod

       integer :: lang
       character(len=*) :: defaultConfigFileName
       type(ESMF_CalendarType) :: defaultCalendar
       character(len=*) :: defaultLogFileName
       integer :: rc

       call ESMF_FrameworkInternalInit(lang, defaultConfigFilename, &
                                       defaultCalendar, defaultLogFileName, rc)

   end subroutine f_esmf_frameworkinitialize

   subroutine f_esmf_frameworkfinalize(rc)
       use ESMF_CompMod
       use ESMF_InitMod

       integer :: rc

       call ESMF_Finalize(rc)

   end subroutine f_esmf_frameworkfinalize


