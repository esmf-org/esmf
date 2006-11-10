! $Id: ESMF_InitMacros.F90,v 1.1 2006/11/10 23:01:08 oehmke Exp $
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
!     ESMF InitMacro Module
      module ESMF_InitMacroMod


!
!==============================================================================
!
! This file contains funtions to support the Initialization Macros
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!------------------------------------------------------------------------------
!
! !USES:
    ! inherit from ESMF base class
    use ESMF_UtilTypesMod



! !PUBLIC MEMBER FUNCTIONS:
   public ESMF_InitCheckDeep


contains
!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_InitCheckDeep - Translate isInit value to return code

! !INTERFACE: 
	function ESMF_InitCheckDeep(isInit)
!
! !RETURN VALUE:
	integer                                         :: ESMF_InitCheckDeep
! !ARGUMENTS:
!	
	integer (ESMF_KIND_I8), intent(in)              :: isInit
	
! !DESCRIPTION:
!      This function takes a classes' isInit component (declared by
!      the initialization macros) and returns an error return code.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [isInit]
!            Initialization macro defined type component. 
!      \end{description}
! 
!EOP
	
    ! base return code on isInit value
    if (isInit .eq. ESMF_INIT_CREATED) then
        ESMF_InitCheckDeep=ESMF_SUCCESS
    else if (isInit .eq. ESMF_INIT_DELETED) then
        ESMF_InitCheckDeep=ESMF_RC_OBJ_DELETED
    else if (isInit .eq. ESMF_INIT_DEFINED) then
        ESMF_InitCheckDeep=ESMF_RC_OBJ_NOT_CREATED
    else 
        ESMF_InitCheckDeep=ESMF_RC_OBJ_NOT_CREATED
    endif	

end function ESMF_InitCheckDeep


end module ESMF_InitMacroMod


