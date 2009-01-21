!  $Id: ESMF_Field_C.F90,v 1.7.2.3 2009/01/21 21:25:20 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_Field_C.F90,v 1.7.2.3 2009/01/21 21:25:20 cdeluca Exp $'
!==============================================================================
   subroutine f_esmf_fieldcreate(fieldp, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_FieldMod
     type(ESMF_Field), pointer :: fieldp
     type(ESMF_Field), target :: thefield
     integer, intent(out), optional :: rc              

     integer :: localrc              

   ! initialize return code; assume routine not implemented
     localrc = ESMF_RC_NOT_IMPL
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

     thefield = ESMF_FieldCreateNoData(rc=localrc)
    
     fieldp => thefield
     if (present(rc)) rc = localrc
   end subroutine f_esmf_fieldcreate

   subroutine f_esmf_fielddestroy(fieldp, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_FieldMod
     type(ESMF_Field), pointer :: fieldp      
     integer, intent(out), optional :: rc     

     integer :: localrc              

   ! initialize return code; assume routine not implemented
     localrc = ESMF_RC_NOT_IMPL
     if (present(rc)) rc = ESMF_RC_NOT_IMPL

     call ESMF_FieldDestroy(fieldp, rc=localrc)

     if (present(rc)) rc = localrc

   end subroutine f_esmf_fielddestroy

