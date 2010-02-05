!  $Id: ESMF_Init_C.F90,v 1.12.4.1 2010/02/05 20:04:36 svasquez Exp $
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
!      '$Id: ESMF_Init_C.F90,v 1.12.4.1 2010/02/05 20:04:36 svasquez Exp $'
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
                                        defaultCalendar, defaultLogFileName, &
                                        defaultLogType, rc)
       use ESMF_LogErrMod
       use ESMF_CalendarMod
       use ESMF_CompMod
       use ESMF_InitMod

       integer :: lang
       character(len=*) :: defaultConfigFileName
       type(ESMF_CalendarType) :: defaultCalendar
       character(len=*) :: defaultLogFileName
       type(ESMF_LogType) :: defaultLogType
       integer :: rc

       call ESMF_FrameworkInternalInit(lang=lang, &
         defaultConfigFilename=defaultConfigFilename, &
         defaultCalendar=defaultCalendar,defaultLogFileName=defaultLogFileName,&
         defaultLogType=defaultLogType, rc=rc)

   end subroutine f_esmf_frameworkinitialize

   subroutine f_esmf_frameworkfinalize(rc)
       use ESMF_CompMod
       use ESMF_InitMod

       integer :: rc

       call ESMF_Finalize(rc=rc)

   end subroutine f_esmf_frameworkfinalize


