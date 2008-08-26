! $Id: ESMF_MyRegistrationInFortran.F90,v 1.1 2008/08/26 20:46:50 theurich Exp $
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

subroutine myRegistrationInFortran(gcomp, rc)
  use ESMF_Mod
  implicit none
  type(ESMF_GridComp) :: gcomp
  integer, intent(out) :: rc
  ! put some code here
  print *, "I am in myRegistrationInFortran()"
  ! return successfully
  rc = ESMF_SUCCESS
end subroutine
