! $Id: ESMF_Comm.F90,v 1.2 2003/03/25 22:09:33 nscollins Exp $
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
!     ESMF Comm module
      module ESMF_CommMod
!
!==============================================================================
!
! This file contains the Comm class definition and all Comm
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include <ESMF.h>
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_CommMod - F90 Interface to C++ ESMC_Comm class
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Comm} class and associated functions and subroutines.  
!
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
!     ! ESMF_Comm
!
!     ! Comm data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_Comm
      sequence
      private
        type(ESMF_Pointer) :: this       ! opaque pointer to the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Comm
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_CommSendRecv

!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Comm.F90,v 1.2 2003/03/25 22:09:33 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Comm Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_CommSendRecv - send / receive data transfer

! !INTERFACE:
      subroutine ESMF_CommSendRecv(comm, sarray, rarray, sarraylen, rarraylen, &
				sde, rde, rc)
!
! !ARGUMENTS:

      type(ESMF_Comm) :: comm
      real(4), intent(in) :: sarray(:), rarray(:)
      integer, intent(in) :: sarraylen
      integer, intent(in) :: rarraylen
      integer, intent(in) :: sde, rde
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Perform a send and receive operation.
!
!  The arguments are:
!  \begin{description}
! 
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
      
!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ creation routine.
      call c_ESMC_CommSendRecv(comm, sarray, rarray, sarraylen, rarraylen, &
     				sde, rde, rc)
      if (status .ne. ESMF_SUCCESS) then
        print *, "Comm send recv error"
        return
      endif

!     set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_CommSendRecv

      end module ESMF_CommMod

