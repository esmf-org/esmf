! $Id: ESMF_LogErr.F90,v 1.29 2004/03/09 23:51:49 jwolfe Exp $
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
!     ESMF LogErr module
      module ESMF_LogErrMod
!
!==============================================================================
!
! This file contains the Field class definition and all Field
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!#include "ESMF_Macros.inc"
#include "ESMF_LogConstants.inc"

!------------------------------------------------------------------------------
!
!BOPI
! !MODULE: Fortran Interface to Log class. 
!
! !
! \end{description}
!
!------------------------------------------------------------------------------
! !USES:
   use ESMF_BaseMod
   use ESMF_IOMod
   implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
       private
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

!
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_LogErr.F90,v 1.29 2004/03/09 23:51:49 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================
!
      contains
!
!==============================================================================
!BOP
! !IROUTINE:  ESMF_LogPrintString

! !INTERFACE:
      subroutine ESMF_LogPrintString(unitNumber, stringToPrint, &
                                     flushSet, rc)
!
! !ARGUMENTS:
      integer, intent(in) :: unitNumber
      character (len=*), intent(in) :: stringToPrint
      type(ESMF_Logical), intent(in) :: flushSet
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!
!     The arguments are:
!     \begin{description}
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 
      integer :: i,istat

      write(unitNumber,10) stringToPrint
      if (flushSet .eq. ESMF_TRUE) call ESMF_IOFlush(unitNumber, istat)
  10  format(A)

      end subroutine ESMF_LogPrintString
  
end module ESMF_LogErrMod
