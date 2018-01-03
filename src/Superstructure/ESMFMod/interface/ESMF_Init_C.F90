!  $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2018, University Corporation for Atmospheric Research, 
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
!      '$Id$'
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
                                        defaultCalKind, defaultLogFileName, &
                                        logkindflag, rc)
       use ESMF_LogErrMod
       use ESMF_CalendarMod
       use ESMF_CompMod
       use ESMF_InitMod
       
       implicit none

       integer :: lang
       character(len=*) :: defaultConfigFileName
       type(ESMF_CalKind_Flag) :: defaultCalKind
       character(len=*) :: defaultLogFileName
       type(ESMF_LogKind_Flag) :: logkindflag
       integer :: rc

       call ESMF_FrameworkInternalInit(lang=lang, &
         defaultConfigFilename=defaultConfigFilename, &
         defaultCalKind=defaultCalKind,defaultLogFileName=defaultLogFileName,&
         logkindflag=logkindflag, rc=rc)

   end subroutine f_esmf_frameworkinitialize

   subroutine f_esmf_frameworkfinalize(rc)
       use ESMF_CompMod
       use ESMF_InitMod

       implicit none

       integer :: rc

       call ESMF_Finalize(rc=rc)

   end subroutine f_esmf_frameworkfinalize
