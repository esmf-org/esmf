!  $Id: ESMF_SolverUtil_C.F90,v 1.2 2009/03/16 16:31:28 oehmke Exp $
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
!      '$Id: ESMF_SolverUtil_C.F90,v 1.2 2009/03/16 16:31:28 oehmke Exp $'
!==============================================================================
   subroutine f_esmf_lapack_iworksize(minmn,iworksize)
     integer, intent(in)              :: minmn 
     integer, intent(out)             :: iworksize
     integer :: smlsiz              
     integer ::  nlvl

#ifdef ESMF_LAPACK
 
    smlsiz=ILAENV(9,'DGELSD',' ',0,0,0,0)   
 
    nlvl = MAX( INT( LOG( DBLE( minmn ) / DBLE( smlsiz+1 ) ) / &
           LOG(2.0) ) + 1, 0 )
   
    iworksize = 3*minmn*nlvl+11*minmn +10 
#endif
  end subroutine f_esmf_lapack_iworksize

