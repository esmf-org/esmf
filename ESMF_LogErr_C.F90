!  $Id: ESMF_LogErr_C.F90,v 1.1 2004/03/19 07:45:39 cpboulder Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massach usetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot  use any F90 syntax, including modules, or allocatable
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
!      '$Id: ESMF_LogErr_C.F90,v 1.1 2004/03/19 07:45:39 cpboulder Exp $'
!==============================================================================
   subroutine f_esmf_logopenfortran(isOpen, unitNumber, nameLogFile)
         use ESMF_BaseMod    ! ESMF base class
         use ESMF_LogErrMod
      type(ESMF_Logical), intent(inout) :: isOpen !if file successfully opened
                                            !isOpen set to ESMF_LOG_TRUE
                                            !otherwise set to ESMF_LOG_FALSE
      integer, intent(inout) ::  unitNumber ! Fortran unit number for I/O
      character (len=32), intent(in) :: nameLogFile
      print *,"f_esmf_logopen_fortran"
      call ESMF_LogOpenFortran(isOpen, unitNumber, nameLogFile)

   end subroutine f_esmf_logopenfortran

   subroutine f_esmf_logclosefortran(unitNumber)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_LogErrMod
       integer, intent(in) :: unitNumber

       call ESMF_LogCloseFortran(unitNumber)

   end subroutine f_esmf_logclosefortran


   subroutine f_esmf_logprintstring(unitNumber, stringData,flushSet)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_LogErrMod
       integer, intent(in) :: unitNumber   
       character(len=*), intent(in) :: stringData  
       type(ESMF_Logical), intent(in) :: flushSet   

       call ESMF_LogPrintString(unitNumber, stringData,flushSet)

   end subroutine f_esmf_logprintstring


   subroutine f_esmf_logprintnewline(unitNumber, flushSet)
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_LogErrMod
       integer, intent(in) :: unitNumber   
       type(ESMF_Logical), intent(in) :: flushSet   

       call ESMF_LogPrintNewline(unitNumber, flushSet)

   end subroutine f_esmf_logprintnewline


