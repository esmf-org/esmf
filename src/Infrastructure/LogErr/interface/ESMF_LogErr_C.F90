!  $Id: ESMF_LogErr_C.F90,v 1.8 2005/11/09 21:47:51 eschwab Exp $
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
!      '$Id: ESMF_LogErr_C.F90,v 1.8 2005/11/09 21:47:51 eschwab Exp $'
!==============================================================================

   subroutine f_esmf_logwrite0(msg,msgtype,rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod         ! ESMF base class
       use ESMF_LogErrMod
        character(len=*), intent(in)                :: msg
        type(ESMF_MsgType), intent(in)              :: msgtype
        integer, intent(out)                        :: rc

        call ESMF_LogWrite(msg, msgtype, rc=rc)

   end subroutine f_esmf_logwrite0

   subroutine f_esmf_logwrite1(msg,msgtype,line,file,method,rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod         ! ESMF base class
       use ESMF_LogErrMod
        character(len=*), intent(in)                :: msg
        type(ESMF_MsgType), intent(in)              :: msgtype
        integer, intent(in)                         :: line
        character(len=*), intent(in)                :: file
        character(len=*), intent(in)                :: method
        integer, intent(out)                        :: rc

        call ESMF_LogWrite(msg, msgtype, line, file, method, rc=rc)

   end subroutine f_esmf_logwrite1
