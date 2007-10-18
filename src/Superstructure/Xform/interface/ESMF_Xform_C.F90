!  $Id: ESMF_Xform_C.F90,v 1.3.4.3 2007/10/18 02:44:09 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_Xform_C.F90,v 1.3.4.3 2007/10/18 02:44:09 cdeluca Exp $'
!==============================================================================

!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
! 
! The code in this file implements the interface code between C++ and F90
!  for the {\tt Transform} entry points.  When the user calls an
!  {\tt ESMC_Xform}XXX method, that code calls these functions, which
!  in turn call the F90 module code.  C++ cannot call directly into an
!  F90 module because the module routine names are altered in a similar
!  fashion as C++ name mangling.
! 
!EOP
!------------------------------------------------------------------------------

   ! TODO: for an init method, does this need to call thru the interface?
   function f_esmf_xforminit(name, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_XformMod

       character(*) :: name
       integer :: rc              

       !f_esmf_xforminit = ESMF_XformInit(name, rc)
       f_esmf_xforminit = 0
    
   end function f_esmf_xforminit

   subroutine f_esmf_xformget(xformp, name, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_XformMod

       type(ESMF_Xform), pointer :: xformp      
       character(*) :: name
       integer :: rc     

       !call ESMF_XformGet(xformp, rc)

   end subroutine f_esmf_xformget

   subroutine f_esmf_xformset(xformp, name, rc)
       use ESMF_UtilTypesMod
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_XformMod

       type(ESMF_Xform), pointer :: xformp      
       character(*) :: name
       integer :: rc     

       !call ESMF_XformSet(xformp, rc)

   end subroutine f_esmf_xformset

