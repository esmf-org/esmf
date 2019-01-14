! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2019, University Corporation for Atmospheric Research,
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
    use ESMF_LogErrMod
    use ESMF_UtilTypesMod

    implicit none
    private

! !PUBLIC MEMBER FUNCTIONS:
    public ESMF_IMErr
    public ESMF_IMErrS
    public ESMF_InitPrint

contains



!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InitCheckDeep"
!BOPI
! !IROUTINE: ESMF_InitCheckDeep - Translate isInit value to return code

! !INTERFACE:
recursive function ESMF_InitCheckDeep(isInit) result (InitCheckDeep)
!
! !RETURN VALUE:
        integer                                 :: InitCheckDeep
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
!EOPI
        
    ! base return code on isInit value
    if (isInit .eq. ESMF_INIT_CREATED) then
        InitCheckDeep=ESMF_SUCCESS
    else if (isInit .eq. ESMF_INIT_DELETED) then
        InitCheckDeep=ESMF_RC_OBJ_DELETED
    else
        InitCheckDeep=ESMF_RC_OBJ_NOT_CREATED
    endif       

end function ESMF_InitCheckDeep

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IMErr"
!BOPI
! !IROUTINE: ESMF_IMErr - Init Macros Error Handling for Deep classes

! !INTERFACE:
recursive function ESMF_IMErr(isInit, line, file, method, rc) result (IMErr)
!
! !RETURN VALUE:
        logical                                         :: IMErr
! !ARGUMENTS:
!       
        ESMF_INIT_TYPE, intent(in)              :: isInit
        integer, intent(in), optional                   :: line
        character(len=*), intent(in), optional          :: file
        character(len=*), intent(in), optional          :: method
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
!EOPI

    ! Initialize return code; assume routine not imlemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    IMErr=ESMF_LogFoundError(ESMF_InitCheckDeep(isInit), &
                                     msg="Bad Object", &
                                     line=line, file=file, method=method, &
                                     rcToReturn=rc)

end function ESMF_IMErr




!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InitCheckShallow"
!BOPI
! !IROUTINE: ESMF_InitCheckShallow - Translate isInit value to return code

! !INTERFACE:
recursive function ESMF_InitCheckShallow(isInit) result (InitCheckShallow)
!
! !RETURN VALUE:
        integer                                 :: InitCheckShallow
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
!EOPI
        
    ! base return code on isInit value
    if (isInit .eq. ESMF_INIT_DEFINED) then
        InitCheckShallow=ESMF_SUCCESS
    else
        InitCheckShallow=ESMF_RC_OBJ_INIT
    endif       

end function ESMF_InitCheckShallow

!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_IMErr"
!BOPI
! !IROUTINE: ESMF_IMErr - Init Macros Error Handling for shallow classes

! !INTERFACE:
recursive function ESMF_IMErrS(isInit, line, file, method, rc) result (IMErrS)
!
! !RETURN VALUE:
        logical                                         :: IMErrS
! !ARGUMENTS:
!       
        ESMF_INIT_TYPE, intent(in)              :: isInit
        integer, intent(in), optional                   :: line
        character(len=*), intent(in), optional          :: file
        character(len=*), intent(in), optional          :: method
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
!EOPI

    ! Initialize return code; assume routine not imlemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    IMErrS=ESMF_LogFoundError(ESMF_InitCheckShallow(isInit), &
                                     msg="Object not Initialized", &
                                     line=line, file=file, method=method, &
                                     rcToReturn=rc)

end function ESMF_IMErrS


!--------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_InitPrint"
!BOPI
! !IROUTINE: ESMF_InitPrint - Print initialization status of object

! !INTERFACE:
  subroutine ESMF_InitPrint (isInit, rc)

! !ARGUMENTS:
!
    ESMF_INIT_TYPE, intent(in)            :: isInit
    integer,        intent(out), optional :: rc

! !DESCRIPTION:
!
!  Print the status of the isInit flag.
!
!      The arguments are:
!      \begin{description}
!
!      \item [isInit]
!            Initialization value to print.
!      \item [{[rc]}]
!            Return code
!
!      \end{description}
!
!EOPI

    ! Initialize return code; assume routine not imlemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    select case (isInit)
    case(ESMF_INIT_UNDEFINED)
      print *, 'Object is UNDEFINED'
    case (ESMF_INIT_DEFINED)
      print *, 'Object is DEFINED'
    case (ESMF_INIT_CREATED)
      print *, 'Object has been CREATED'
    case (ESMF_INIT_DELETED)
      print *, 'Object has been DELETED'
    case default
      print *, 'Object is in unknown state'
    end select

    if (present (rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_InitPrint

end module ESMF_InitMacrosMod


