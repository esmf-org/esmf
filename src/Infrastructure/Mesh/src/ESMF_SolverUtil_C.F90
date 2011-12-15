!  $Id: ESMF_SolverUtil_C.F90,v 1.9 2011/12/15 23:11:27 peggyli Exp $
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
!      '$Id: ESMF_SolverUtil_C.F90,v 1.9 2011/12/15 23:11:27 peggyli Exp $'
!==============================================================================
   subroutine f_esmf_lapack_iworksize(minmn,iworksize)
    
     implicit none
   
     integer, intent(in)              :: minmn 
     integer, intent(out)             :: iworksize

     integer, external :: ILAENV

     integer :: smlsiz              
     integer ::  nlvl
 
    smlsiz=ILAENV(9,'DGELSD',' ',0,0,0,0)   
 
    nlvl = MAX( INT( LOG( DBLE( minmn ) / DBLE( smlsiz+1 ) ) / &
           LOG(2.0) ) + 1, 0 )
   
    iworksize = 3*minmn*nlvl+11*minmn +10 

  end subroutine f_esmf_lapack_iworksize

