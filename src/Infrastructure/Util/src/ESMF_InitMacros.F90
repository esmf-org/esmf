! $Id: ESMF_InitMacros.F90,v 1.6 2006/11/26 22:15:36 oehmke Exp $
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
!
!     ESMF InitMacro Module
      module ESMF_InitMacrosMod


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
    use ESMF_LogErrMod


! !PUBLIC MEMBER FUNCTIONS:
   public ESMF_IMErr


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
	ESMF_INIT_TYPE, intent(in)              :: isInit
	
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

!--------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_IMErr - Init Macros Error Handling

! !INTERFACE: 
	function ESMF_IMErr(isInit, line, file, method, &
                                       rc)
!
! !RETURN VALUE:
	logical                                         ::ESMF_IMErr
! !ARGUMENTS:
!	
	ESMF_INIT_TYPE, intent(in)              :: isInit
	integer, intent(in), optional                   :: line
	character(len=*), intent(in), optional          :: file
	character(len=*), intent(in), optional	        :: method
	integer, intent(out),optional                   :: rc
	

! !DESCRIPTION:
!      This function returns a logical true for ESMF initialization
!      codes that indicate an error.  A predefined error message will
!      be added to the {\tt ESMF\_Log} along with
!      a user added {\tt line}, {\tt file} and {\tt method}.  
!      Additionally, {\tt rc} will be set to an appropriate return code.
!
!      The arguments are:
!      \begin{description}
! 	
!      \item [isInit]
!            Initialization code to check.
!      \item [{[line]}]
!            Integer source line number.  Expected to be set by
!            using the preprocessor macro {\tt \_\_LINE\_\_} macro.
!      \item [{[file]}]
!            User-provided source file name. 
!      \item [{[method]}]
!            User-provided method string.
!      \item [{[rc]}]
!            If specified, put the return code into {\tt rc}.
!            This is not the return code for this function; it allows
!            the calling code to do an assignment of the error code
!            at the same time it is testing the value.
!            of the default Log.
!      
!      \end{description}
! 
!EOP
	

    ESMF_IMErr=ESMF_LogMsgFoundError(ESMF_InitCheckDeep(isInit), &
                                   "Bad Object", &
                                   line, file, method, &
                                   rc)
       
end function ESMF_IMErr


end module ESMF_InitMacrosMod


