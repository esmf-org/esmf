! $Id: ESMF_WebServicesEx.F90,v 1.1 2011/04/18 18:05:33 theurich Exp $
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
program WebServComp
  ! arguments
  type(ESMF_GridComp):: gcomp
  type(ESMF_State):: importState, exportState
  type(ESMF_Clock):: clock
  integer, intent(out):: rc

  ! here goes the code    

end program ESMF_AttachMethodsEx
!EOC
