! $Id: ESMF_LogErr.F90,v 1.28 2004/03/09 23:05:25 svasquez Exp $
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
!

#include "ESMF.h"
!#include "ESMF_Macros.inc"
#include "ESMF_LogConstants.inc"

module ESMF_LogErrMod

   use ESMF_BaseMod
   use ESMF_IOMod

!BOPI
!============================================================================
! !MODULE: Fortran Interface to Log class. 
!
! !
! \end{description}
!EOPI

  integer :: i,istat

  write(unitNumber,10) stringToPrint
  if (flushSet .eq. ESMF_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10 format(A)
 end subroutine ESMF_LogPrintString

  
end module ESMF_LogErrMod
