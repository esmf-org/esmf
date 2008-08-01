!  $Id: ESMF_Mesh_C.F90,v 1.2 2008/08/01 18:56:59 dneckels Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
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
!      '$Id: ESMF_Mesh_C.F90,v 1.2 2008/08/01 18:56:59 dneckels Exp $'
!==============================================================================
   function f_esmf_getmeshdistgrid(count, indicies, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_BaseMod    ! ESMF base class
       use ESMF_DistGridMod
     integer, intent(in)               :: count
     integer, intent(inout)            :: indicies(count)
     integer, intent(out)              :: rc              
     type(ESMF_DistGrid)  :: f_esmf_getmeshdistgrid

   ! initialize return code; assume routine not implemented
     rc = ESMF_RC_NOT_IMPL
     
     f_esmf_getmeshdistgrid = ESMF_DistGridCreate(indicies, rc)

   end function f_esmf_getmeshdistgrid

