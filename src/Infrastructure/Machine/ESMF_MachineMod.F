! $Id: ESMF_MachineMod.F,v 1.1 2001/11/13 18:35:47 dneckels Exp $
	module ESMF_MachineMod
!===============================================================================
!BOP
! !MODULE: ESMF_MachineMod
!
! !USES:
!
! !PUBLIC TYPES:
#include "conf.h"
	implicit none

	type ESMF_Machine
	private
	character(ESMC_POINTER_SIZE) :: this
	end type
!
! !PUBLIC MEMBER FUNCTIONS:
!     ESMF_MachineNew
!     ESMF_MachineDelete
!     ESMF_MachinePInfo
!
! !DESCRIPTION:
! Encapsulates the machine model.
!EOP
!===============================================================================
	contains
!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_MachineNew
!
! !INTERFACE:
	function ESMF_MachineNew(rc)
!
! !RETURN VALUE:
	type(ESMF_Machine) :: ESMF_MachineNew
! !PARAMETERS:
	integer, intent(out), optional :: rc       ! Return code.
!
! !DESCRIPTION:
!     Creates the Machine model.
!
!EOP
!------------------------------------------------------------------------------- 
	integer stub

	call ESMC_MachineNew(ESMF_MachineNew, stub)
	if (present(rc)) rc = stub

	end function

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_MachineDelete
!
! !INTERFACE:
	subroutine ESMF_MachineDelete(machine, rc)
! !PARAMETERS:
	type(ESMF_Machine) :: machine
	integer, intent(out), optional :: rc       ! Return code.
!
! !DESCRIPTION:
!     Destroys the machine object.
!
!EOP
!------------------------------------------------------------------------------- 
	integer stub

	call ESMC_MachineDelete(machine, stub)
	if (present(rc)) rc = stub

	end subroutine

!===============================================================================
!BOP
!
! !IROUTINE:  ESMF_MachinePinfo
!
! !INTERFACE:
	subroutine ESMF_MachinePinfo(node, process, thread, rc)
! !PARAMETERS:
	integer :: node                            ! The node caller is on.
	integer :: process                         ! The process of caller.
	integer :: thread                          ! The thread of caller.
	integer, intent(out), optional :: rc       ! Return code.
!
!
! !DESCRIPTION:
!     Returns process information.
!
!EOP
!------------------------------------------------------------------------------- 
	integer stub

	call ESMC_MachinePinfo(node, process, thread, stub)
	if (present(rc)) rc = stub

	end subroutine
	
	end module
