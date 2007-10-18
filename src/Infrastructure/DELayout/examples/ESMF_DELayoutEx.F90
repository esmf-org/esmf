! $Id: ESMF_DELayoutEx.F90,v 1.6.4.3 2007/10/18 02:42:31 cdeluca Exp $
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

!==============================================================================
!EXAMPLE        String used by test script to count examples.
!==============================================================================

program ESMF_DELayoutEx

  use ESMF_Mod
  
  implicit none
  
  ! local variables
  integer:: rc
  type(ESMF_VM):: vm
  type(ESMF_DELayout):: delayout
  type(ESMF_Logical):: otoflag
  ! result code
  integer :: finalrc
  finalrc = ESMF_SUCCESS
  call ESMF_Initialize(vm=vm, rc=rc)
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
! \subsubsection{Default 1-D DELayout}
! 
! Without additional parameters the created {\tt ESMF\_DELayout} will\
! default into a 1-dimensional DELayout with as many DEs as there are PETs in
! the associated VM object. Consequently the resulting DELayout will always
! be 1-to-1, i.e. each DE maps onto exactly one PET of the VM.
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
! \subsubsection{1-D DELayout with Fixed Number of DEs}
! 
! The {\tt deCountList} argument has two functions when present. First it
! specifies the total number
! of DEs and second it specifies the dimensionality of the DELayout. Here a 
! 1-dimensional DELayout will be created with 4 DEs. Note that it depends on the
! VM whether this will be a 1-to-1 DELayout or not. If the VM contains
! 4 PETs or more the DELayout will be 1-to-1, otherwise there will be virtual
! DEs present.
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
  call ESMF_DELayoutGet(delayout, oneToOneFlag=otoflag, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
!BOC
  if (otoflag==ESMF_TRUE) then
    print *, 'This is a 1-to-1 DELayout'
  else 
    print *, 'This is a DELayout with virutal DEs'
  endif
    
  call ESMF_DELayoutDestroy(delayout, rc=rc)
!EOC  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE

!BOE
! \subsubsection{2-D DELayout with Fixed Number of DEs}
! 
! Here a 2-dimensional DELayout will be created with 6 DEs, layed out as 2x3.
! As in the previous example  it depends on the
! VM whether this will be a 1-to-1 DELayout or not. If the VM contains
! 6 PETs or more the DELayout will be 1-to-1, otherwise there will be virtual
! DEs present.
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
