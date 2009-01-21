! $Id: ESMF_FortranWordsizeHelper.F90,v 1.1.2.2 2009/01/21 21:25:24 cdeluca Exp $
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
#define ESMF_FILENAME "ESMF_FortranWordsizeHelper.F90"

subroutine ESMF_FortranUDTPointerSizeInit
  ! This routine is called by ESMF_FortranUDTPointerSize(), but must be
  ! in a separate file because some compilers don't like the common block
  ! to look different within the same file.
  implicit none
  character :: comchars(256)
  common /udtcom/ comchars
  integer :: i
  comchars = (/ (char (i), i=0, 255) /)
end subroutine ESMF_FortranUDTPointerSizeInit
