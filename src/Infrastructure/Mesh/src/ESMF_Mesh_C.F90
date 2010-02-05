!  $Id: ESMF_Mesh_C.F90,v 1.2.4.1 2010/02/05 19:59:44 svasquez Exp $
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
!      '$Id: ESMF_Mesh_C.F90,v 1.2.4.1 2010/02/05 19:59:44 svasquez Exp $'
!==============================================================================
   subroutine f_esmf_getmeshdistgrid(dgrid, count, indicies, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_DistGridMod
     type(ESMF_DistGrid), intent(inout) :: dgrid
     integer, intent(in)               :: count
     integer, intent(inout)            :: indicies(count)
     integer, intent(out)              :: rc              

   ! initialize return code; assume routine not implemented
     rc = ESMF_RC_NOT_IMPL
     
     dgrid = ESMF_DistGridCreate(indicies, rc)

   end subroutine f_esmf_getmeshdistgrid

