! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2016, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!==============================================================================
!
#define ESMF_FILENAME "ESMF_StateTypes.F90"
!
!     ESMF StateTypes module
      module ESMF_StateTypesMod
!
!==============================================================================
!
! This file contains the State class definitions. 
!  Other files in this directory contain the variou State class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_StateMod - Data exchange between components
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran function and subroutine 
!  interfaces to the {\tt State} class and associated data structures.
!
!
! !USES:
      use ESMF_UtilTypesMod
      use ESMF_LogErrMod
      use ESMF_BaseMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayBundleMod
      use ESMF_FieldMod
      use ESMF_FieldBundleMod
      use ESMF_RHandleMod
      use ESMF_InitMacrosMod
      use ESMF_StateItemMod
      use ESMF_StateContainerMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_State
!
!     ! State data type.

      type ESMF_State
#ifndef ESMF_SEQUENCE_BUG
#ifndef ESMF_NO_SEQUENCE
        sequence
#endif
#endif
      !private
        type(ESMF_StateClass), pointer :: statep
         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_State
      public ESMF_StateItem_Flag, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
        ESMF_STATEITEM_ARRAY, ESMF_STATEITEM_ARRAYBUNDLE, &
        ESMF_STATEITEM_ROUTEHANDLE, ESMF_STATEITEM_STATE, &
#if 0
        ESMF_STATEITEM_NAME, &
#endif
        ESMF_STATEITEM_NOTFOUND
      public ESMF_StateItemWrap
      public ESMF_StateItemConstruct
      public ESMF_StateIntent_Flag, ESMF_STATEINTENT_IMPORT, ESMF_STATEINTENT_EXPORT, &
                                   ESMF_STATEINTENT_UNSPECIFIED
#if ESMF_ENABLE_NEEDED
      public ESMF_NeededFlag, ESMF_NEEDED, &
                                   ESMF_NOTNEEDED
      public ESMF_ReadyFlag,  ESMF_READYTOWRITE, &
                                   ESMF_READYTOREAD, &
                                   ESMF_NOTREADY
      public ESMF_ReqForRestartFlag,  ESMF_REQUIRED_FOR_RESTART, &
                                   ESMF_NOTREQUIRED_FOR_RESTART
      public ESMF_ValidFlag,  ESMF_VALID, &
                                   ESMF_INVALID, &
                                   ESMF_VALIDITYUNKNOWN
#endif

      ! only public for other files in the state class (should be friend)
      public ESMF_StateClass, ESMF_StateItem, ESMF_DataHolder
#if 0
      public ESMF_STATEITEM_INDIRECT
#endif
      public ESMF_STATEITEM_UNKNOWN
      public ESMF_STATEINTENT_INVALID

      ! Public Methods
      public ESMF_DataHolderValidate
      public ESMF_StateItemValidate
      public ESMF_StateClassValidate

      ! ESMF library private methods
      public ESMF_DataHolderInit
      public ESMF_DataHolderGetInit
      public ESMF_StateItemInit
      public ESMF_StateItemGetInit
      public ESMF_StateItemPrint
      public ESMF_StateClassGetInit
      public ESMF_StateGetInit

      public operator(==), operator(/=)

!------------------------------------------------------------------------------
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id$'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

interface operator (==)
 module procedure ESMF_oteq
 module procedure ESMF_imexeq
#if ESMF_ENABLE_NEEDED
 module procedure ESMF_needeq
 module procedure ESMF_redyeq
 module procedure ESMF_valideq
#endif
end interface

interface operator (/=)
 module procedure ESMF_otne
 module procedure ESMF_imexne
#if ESMF_ENABLE_NEEDED
 module procedure ESMF_needne
 module procedure ESMF_redyne
 module procedure ESMF_validne
#endif
end interface


!==============================================================================

      contains

!==============================================================================

! functions to compare two ESMF types to see if they're the same or not

function ESMF_oteq(s1, s2)
 logical ESMF_oteq
 type(ESMF_StateItem_Flag), intent(in) :: s1, s2

 ESMF_oteq = (s1%ot == s2%ot)
end function

function ESMF_otne(s1, s2)
 logical ESMF_otne
 type(ESMF_StateItem_Flag), intent(in) :: s1, s2

 ESMF_otne = (s1%ot /= s2%ot)
end function


function ESMF_imexeq(s1, s2)
 logical ESMF_imexeq
 type(ESMF_StateIntent_Flag), intent(in) :: s1, s2

 ESMF_imexeq = (s1%state == s2%state)
end function

function ESMF_imexne(s1, s2)
 logical ESMF_imexne
 type(ESMF_StateIntent_Flag), intent(in) :: s1, s2

 ESMF_imexne = (s1%state /= s2%state)
end function


#if ESMF_ENABLE_NEEDED
function ESMF_needeq(s1, s2)
 logical ESMF_needeq
 type(ESMF_NeededFlag), intent(in) :: s1, s2

 ESMF_needeq = (s1%needed == s2%needed)
end function

function ESMF_needne(s1, s2)
 logical ESMF_needne
 type(ESMF_NeededFlag), intent(in) :: s1, s2

 ESMF_needne = (s1%needed /= s2%needed)
end function


function ESMF_redyeq(s1, s2)
 logical ESMF_redyeq
 type(ESMF_ReadyFlag), intent(in) :: s1, s2

 ESMF_redyeq = (s1%ready == s2%ready)
end function

function ESMF_redyne(s1, s2)
 logical ESMF_redyne
 type(ESMF_ReadyFlag), intent(in) :: s1, s2

 ESMF_redyne = (s1%ready /= s2%ready)
end function


function ESMF_valideq(s1, s2)
 logical ESMF_valideq
 type(ESMF_ValidFlag), intent(in) :: s1, s2

 ESMF_valideq = (s1%valid == s2%valid)
end function

function ESMF_validne(s1, s2)
 logical ESMF_validne
 type(ESMF_ValidFlag), intent(in) :: s1, s2

 ESMF_validne = (s1%valid /= s2%valid)
end function
#endif


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DataHolderValidate()"
!BOPI
! !IROUTINE: ESMF_DataHolderValidate - Validate DataHolder internals

! !INTERFACE:
  subroutine ESMF_DataHolderValidate(dh, rc)
!
! !ARGUMENTS:
    type(ESMF_DataHolder), intent(inout)              :: dh
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt dh} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[dh] 
!          Specified {\tt ESMF\_DataHolder} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_DataHolderGetInit, ESMF_DataHolderInit,dh)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DataHolderValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DataHolderInit()"
!BOPI
! !IROUTINE: ESMF_DataHolderInit - Init DataHolder internals

! !INTERFACE:
  subroutine ESMF_DataHolderInit(dh)
!
! !ARGUMENTS:
    type(ESMF_DataHolder), intent(inout)              :: dh
!         
!
! !DESCRIPTION:
!      Initialize DataHolder internals.
!
!     The arguments are:
!     \begin{description}
!     \item[dh] 
!          Specified {\tt ESMF\_DataHolder} object.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ESMF_INIT_SET_DEFINED(dh)
  end subroutine ESMF_DataHolderInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DataHolderGetInit"
!BOPI
! !IROUTINE: ESMF_DataHolderGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_DataHolderGetInit(dh) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_DataHolderGetInit   
!
! !ARGUMENTS:
      type(ESMF_DataHolder), intent(in), optional :: dh
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [dh]
!           DataHolder object.
!     \end{description}
!
!EOPI

    if (present(dh)) then
      ESMF_DataHolderGetInit = ESMF_INIT_GET(dh)
    else
      ESMF_DataHolderGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_DataHolderGetInit
!------------------------------------------------------------------------------



! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateItemValidate()"
!BOPI
! !IROUTINE: ESMF_StateItemValidate - Validate StateItem internals

! !INTERFACE:
  subroutine ESMF_StateItemValidate(si, rc)
!
! !ARGUMENTS:
    type(ESMF_StateItem), intent(inout)              :: si
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt si} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[si] 
!          Specified {\tt ESMF\_StateItem} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_SET_SHALLOW(ESMF_StateItemGetInit, ESMF_StateItemInit,si)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_StateItemValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateItemInit()"
!BOPI
! !IROUTINE: ESMF_StateItemInit - Init StateItem internals

! !INTERFACE:
  subroutine ESMF_StateItemInit(si)
!
! !ARGUMENTS:
    type(ESMF_StateItem), intent(inout)              :: si
!         
!
! !DESCRIPTION:
!      Initialize StateItem internals.
!
!     The arguments are:
!     \begin{description}
!     \item[si] 
!          Specified {\tt ESMF\_StateItem} object.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ESMF_INIT_SET_DEFINED(si)
  end subroutine ESMF_StateItemInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateItemGetInit"
!BOPI
! !IROUTINE: ESMF_StateItemGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_StateItemGetInit(si) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_StateItemGetInit   
!
! !ARGUMENTS:
      type(ESMF_StateItem), intent(in), optional :: si
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [si]
!           StateItem object.
!     \end{description}
!
!EOPI

    if (present(si)) then
      ESMF_StateItemGetInit = ESMF_INIT_GET(si)
    else
      ESMF_StateItemGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_StateItemGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassValidate()"
!BOPI
! !IROUTINE: ESMF_StateClassValidate - Validate StateClass internals

! !INTERFACE:
  subroutine ESMF_StateClassValidate(sc, rc)
!
! !ARGUMENTS:
    type(ESMF_StateClass), intent(in)              :: sc
    integer,          intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt StateClass} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[sc] 
!          Specified {\tt ESMF\_StateClass} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    localrc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_StateClassGetInit, sc, rc)
      
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_StateClassValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateClassGetInit"
!BOPI
! !IROUTINE: ESMF_StateClassGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_StateClassGetInit(sc) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_StateClassGetInit   
!
! !ARGUMENTS:
      type(ESMF_StateClass), intent(in), optional :: sc
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [sc]
!           StateClass object.
!     \end{description}
!
!EOPI

    if (present(sc)) then
      ESMF_StateClassGetInit = ESMF_INIT_GET(sc)
    else
      ESMF_StateClassGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_StateClassGetInit
!------------------------------------------------------------------------------



! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_StateGetInit"
!BOPI
! !IROUTINE: ESMF_StateGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_StateGetInit(s) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_StateGetInit   
!
! !ARGUMENTS:
      type(ESMF_State), intent(in), optional :: s
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [s]
!           State object.
!     \end{description}
!
!EOPI

    if (present(s)) then
      ESMF_StateGetInit = ESMF_INIT_GET(s)
    else
      ESMF_StateGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_StateGetInit
!------------------------------------------------------------------------------


end module ESMF_StateTypesMod

