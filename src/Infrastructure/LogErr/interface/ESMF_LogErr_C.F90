!  $Id: ESMF_LogErr_C.F90,v 1.6 2005/09/21 17:28:19 nscollins Exp $
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
!      '$Id: ESMF_LogErr_C.F90,v 1.6 2005/09/21 17:28:19 nscollins Exp $'
!==============================================================================

#if 0
   subroutine f_esmf_logwritenoform(message, msgtype, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod         ! ESMF base class
       use ESMF_LogErrMod
     character(len=*), intent(in) :: message
     type(ESMF_MsgType), intent(in) :: msgtype
     integer, intent(out), optional :: rc              

     integer :: localrc              

     call ESMF_LogWriteNoForm(message, msgtype, rc=localrc)
    
     if (present(rc)) rc = localrc

   end subroutine f_esmf_logwritenoform
#endif


   subroutine f_esmf_logwrite(msg,msgtype,line,file,method,log,rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod         ! ESMF base class
       use ESMF_LogErrMod
        character(len=*), intent(in)                :: msg
        type(ESMF_MsgType), intent(in)              :: msgtype
        integer, intent(in), optional               :: line
        character(len=*), intent(in), optional      :: file
        character(len=*), intent(in), optional      :: method
        type(ESMF_LOG),target,optional              :: log
        integer, intent(out),optional               :: rc

        call ESMF_LogWrite(msg, msgtype, rc=rc)
        !call ESMF_LogWrite(msg, msgtype, line, file, method, rc=rc)

   end subroutine f_esmf_logwrite


