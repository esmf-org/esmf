! $Id: ESMF_DELayoutEx.F90,v 1.2 2004/06/18 21:47:30 theurich Exp $
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

!==============================================================================
!EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_DELayoutEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  integer:: localPet, petCount, peCount, ssiId
  type(ESMF_DELayout):: delayout
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
! \subsubsection{Default 1-D DELayout}
! 
! With no additional parameters provided the created {\tt ESMF\_DELayout} will\
! default into a 1-dimensional DELayout with as many DEs as there are PETs in
! the associated VM object.
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(vm, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_DELayoutPrint(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
! \subsubsection{1-D DELayout with fixed number of DEs}
! 
! The {\tt deCountList} has to functions. First it specifies the total number
! of DEs and second it specifies the dimensionallity of the DELayout. Here a 
! 1-dimensional DELayout will be created with 4 DEs. 
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(vm, deCountList=(/4/), rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_DELayoutPrint(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
! \subsubsection{2-D DELayout with fixed number of DEs}
! 
! Here a 2-dimensional DELayout will be created with 2x3 DEs. 
!EOE
!BOC
  delayout = ESMF_DELayoutCreate(vm, deCountList=(/2, 3/), rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_DELayoutPrint(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC  
  call ESMF_DELayoutDestroy(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE


  call ESMF_Finalize(rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_DELayoutEx.F90"
  else
    print *, "FAIL: ESMF_DELayoutEx.F90"
  endif
end program
