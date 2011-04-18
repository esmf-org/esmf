! $Id: ESMF_WebServicesEx.F90,v 1.2 2011/04/18 23:01:45 theurich Exp $
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

!==============================================================================
!ESMF_EXAMPLE        String used by test script to count examples.
!==============================================================================

!-------------------------------------------------------------------------
!BOE
!\subsubsection{Making a Component available through WebServices}
!      
!  In this example a standard ESMF Component is made available through
!  the WebServices interface.
!EOE
!BOC
program WebServicesEx
  ! ESMF Framework module
  use ESMF_Mod
!EOC
  implicit none

  ! Local variables
  integer :: rc
  
  integer :: finalrc
  finalrc = ESMF_SUCCESS

  call ESMF_Initialize(defaultlogfilename="WebServicesEx.Log", &
                    defaultlogtype=ESMF_LOG_MULTI, rc=rc)
  if (rc/=ESMF_SUCCESS) then
    finalrc = ESMF_FAILURE
    goto 10
  endif

!BOC
  ! here goes the code 
!EOC

10 continue
  call ESMF_Finalize(rc=rc)
  
  if (rc/=ESMF_SUCCESS) finalrc = ESMF_FAILURE
  if (finalrc==ESMF_SUCCESS) then
    print *, "PASS: ESMF_WebServicesEx.F90"
  else
    print *, "FAIL: ESMF_WebServicesEx.F90"
  endif

!BOC
end program WebServicesEx
!EOC
