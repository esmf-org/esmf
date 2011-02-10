!  $Id: ESMF_Mesh_C.F90,v 1.9 2011/02/10 04:18:46 ESRL\ryan.okuinghttons Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2011, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_Mesh_C.F90,v 1.9 2011/02/10 04:18:46 ESRL\ryan.okuinghttons Exp $'
!==============================================================================
   subroutine f_esmf_getmeshdistgrid(dgrid, count, indices, rc)
     use ESMF_UtilTypesMod    ! ESMF base class
     use ESMF_BaseMod    ! ESMF base class
     use ESMF_DistGridMod
     
     implicit none
     
     type(ESMF_DistGrid), intent(inout) :: dgrid
     integer, intent(in)               :: count
     integer, intent(inout)            :: indices(count)
     integer, intent(out)              :: rc              

     integer, allocatable :: indicesLocal(:)

   ! initialize return code; assume routine not implemented
     rc = ESMF_RC_NOT_IMPL

     allocate(indicesLocal(count))

     if (count > 0) then
       indicesLocal(1:count) = indices(1:count)
     endif

     dgrid = ESMF_DistGridCreate(indicesLocal, rc=rc)

     deallocate(indicesLocal)

   end subroutine f_esmf_getmeshdistgrid

