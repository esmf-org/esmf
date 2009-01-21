! $Id: ESMF_StateTypes.F90,v 1.22.2.6 2009/01/21 21:25:25 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
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
      use ESMF_IOSpecMod
      use ESMF_VMMod
      use ESMF_ArrayMod
      use ESMF_ArrayBundleMod
      use ESMF_FieldMod
      use ESMF_FieldBundleMod
      use ESMF_RHandleMod
      use ESMF_InitMacrosMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_StateType
!     !   Enumerated value for storing Import or Export State type.
!
      type ESMF_StateType
      sequence
      !private
         integer :: state
      end type

      type(ESMF_StateType), parameter :: &
                ESMF_STATE_IMPORT   = ESMF_StateType(1), &
                ESMF_STATE_EXPORT   = ESMF_StateType(2), &
                ESMF_STATE_UNSPECIFIED = ESMF_StateType(3), &
                ESMF_STATE_INVALID  = ESMF_StateType(4)

!------------------------------------------------------------------------------
!     ! ESMF_StateItemType
!     !   Each entry in the list of states is either simply a name placeholder
!     !   or an actual data item - FieldBundle, Field, Array, or State. 
!
      type ESMF_StateItemType
      sequence
      !private
         integer :: ot
      end type

      ! keep these numbers distinct from the 30 or so esmf object types.
      type(ESMF_StateItemType), parameter :: &
                ESMF_STATEITEM_FIELD        = ESMF_StateItemType(101), &
                ESMF_STATEITEM_FIELDBUNDLE  = ESMF_StateItemType(102), &
                ESMF_STATEITEM_ARRAY        = ESMF_StateItemType(103), &
                ESMF_STATEITEM_ARRAYBUNDLE  = ESMF_StateItemType(104), &
                ESMF_STATEITEM_ROUTEHANDLE  = ESMF_StateItemType(105), &
                ESMF_STATEITEM_STATE        = ESMF_StateItemType(106), &
                ESMF_STATEITEM_NAME         = ESMF_StateItemType(107), &
                ESMF_STATEITEM_INDIRECT     = ESMF_StateItemType(108), &
                ESMF_STATEITEM_UNKNOWN      = ESMF_StateItemType(109), &
                ESMF_STATEITEM_NOTFOUND     = ESMF_StateItemType(110)

!------------------------------------------------------------------------------
!     ! ESMF_NeededFlag
!     !   For an Export State if all data which can potentially be created is
!     !   not needed, this flag can be used to mark data which does not need
!     !   to be created by the Component.
!
      type ESMF_NeededFlag
      sequence
      !private
         integer :: needed
      end type

      type(ESMF_NeededFlag), parameter :: &
                ESMF_NEEDED = ESMF_NeededFlag(1), &
                ESMF_NOTNEEDED = ESMF_NeededFlag(2)

!------------------------------------------------------------------------------
!     ! ESMF_ReadyFlag
!
      type ESMF_ReadyFlag
      sequence
      !private
         integer :: ready
      end type

      type(ESMF_ReadyFlag), parameter :: &
                ESMF_READYTOWRITE = ESMF_ReadyFlag(1), &
                ESMF_READYTOREAD = ESMF_ReadyFlag(2), &
                ESMF_NOTREADY = ESMF_ReadyFlag(3)


!------------------------------------------------------------------------------
!     ! ESMF_ReqForRestartFlag
!
      type ESMF_ReqForRestartFlag
      sequence
      !private
         integer :: required4restart
      end type

      type(ESMF_ReqForRestartFlag), parameter :: &
                ESMF_REQUIRED_FOR_RESTART = ESMF_ReqForRestartFlag(1), &
                ESMF_NOTREQUIRED_FOR_RESTART = ESMF_ReqForRestartFlag(2)


!------------------------------------------------------------------------------
!     ! ESMF_ValidFlag
!
      type ESMF_ValidFlag
      sequence
      !private
         integer :: valid
      end type

      type(ESMF_ValidFlag), parameter :: &
                ESMF_VALID = ESMF_ValidFlag(1), &
                ESMF_INVALID= ESMF_ValidFlag(2), &
                ESMF_VALIDITYUNKNOWN = ESMF_ValidFlag(3)


!------------------------------------------------------------------------------
!     ! ESMF_DataHolder
!
!     ! Make a single data type for FieldBundles, Fields, and Arrays.
!     !  The ObjectType is one level up, because this structure is not
!     !  allocated until it is actually needed.  This is a private type.

!     ! state has to be different because it's a forward reference.

      type ESMF_DataHolder
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
          type(ESMF_Field)        :: fp 
          type(ESMF_FieldBundle)  :: fbp
          type(ESMF_Array)        :: ap
          type(ESMF_ArrayBundle)  :: abp
          type(ESMF_RouteHandle)  :: rp
          type(ESMF_StateClass), pointer  :: spp
          ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_StateItem
!
!     ! Description of next Data item in list, or simply a name
!     !  which holds the place for an optional Data item.

      type ESMF_StateItem
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
        type(ESMF_DataHolder) :: datap
        type(ESMF_StateItemType) :: otype
        type(ESMF_NeededFlag) :: needed
        type(ESMF_ReadyFlag) :: ready
        type(ESMF_ValidFlag) :: valid
        type(ESMF_ReqForRestartFlag) :: reqrestart
        logical :: proxyFlag
        integer :: indirect_index
        character(len=ESMF_MAXSTR) :: namep
         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_StateClass
!
!     ! Internal State data type.

      type ESMF_StateClass
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
        type(ESMF_Base) :: base
        type(ESMF_Status) :: statestatus
        type(ESMF_StateType) :: st
        type(ESMF_NeededFlag) :: needed_default
        type(ESMF_ReadyFlag) :: ready_default
        type(ESMF_ValidFlag) :: stvalid_default
        type(ESMF_ReqForRestartFlag) :: reqrestart_default
        integer :: alloccount
        integer :: datacount
        type(ESMF_StateItem), dimension(:), pointer :: datalist
         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
!     ! ESMF_State
!
!     ! State data type.

      type ESMF_State
#ifndef ESMF_SEQUENCE_BUG
      sequence
#endif
      !private
#ifndef ESMF_NO_INITIALIZERS
        type(ESMF_StateClass), pointer :: statep => NULL()
#else
        type(ESMF_StateClass), pointer :: statep
#endif
         ESMF_INIT_DECLARE
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_State
      public ESMF_StateItemType, &
        ESMF_STATEITEM_FIELD, ESMF_STATEITEM_FIELDBUNDLE, &
        ESMF_STATEITEM_ARRAY, ESMF_STATEITEM_ARRAYBUNDLE, &
        ESMF_STATEITEM_ROUTEHANDLE, ESMF_STATEITEM_STATE, ESMF_STATEITEM_NAME, &
        ESMF_STATEITEM_NOTFOUND
      public ESMF_StateType, ESMF_STATE_IMPORT, ESMF_STATE_EXPORT, &
                                   ESMF_STATE_UNSPECIFIED
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

      ! only public for other files in the state class (should be friend)
      public ESMF_StateClass, ESMF_StateItem, ESMF_DataHolder
      public ESMF_STATEITEM_INDIRECT, ESMF_STATEITEM_UNKNOWN
      public ESMF_STATE_INVALID

      ! Public Methods
      public ESMF_DataHolderValidate
      public ESMF_StateItemValidate
      public ESMF_StateClassValidate

      ! ESMF library private methods
      public ESMF_DataHolderInit
      public ESMF_DataHolderGetInit
      public ESMF_StateItemInit
      public ESMF_StateItemGetInit
      public ESMF_StateClassGetInit
      public ESMF_StateGetInit




!------------------------------------------------------------------------------
      public operator(.eq.), operator(.ne.)
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateTypes.F90,v 1.22.2.6 2009/01/21 21:25:25 cdeluca Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

interface operator (.eq.)
 module procedure ESMF_oteq
 module procedure ESMF_imexeq
 module procedure ESMF_needeq
 module procedure ESMF_redyeq
 module procedure ESMF_valideq
end interface

interface operator (.ne.)
 module procedure ESMF_otne
 module procedure ESMF_imexne
 module procedure ESMF_needne
 module procedure ESMF_redyne
 module procedure ESMF_validne
end interface


!==============================================================================

      contains

!==============================================================================

! functions to compare two ESMF types to see if they're the same or not

function ESMF_oteq(s1, s2)
 logical ESMF_oteq
 type(ESMF_StateItemType), intent(in) :: s1, s2

 ESMF_oteq = (s1%ot .eq. s2%ot)
end function

function ESMF_otne(s1, s2)
 logical ESMF_otne
 type(ESMF_StateItemType), intent(in) :: s1, s2

 ESMF_otne = (s1%ot .ne. s2%ot)
end function


function ESMF_imexeq(s1, s2)
 logical ESMF_imexeq
 type(ESMF_StateType), intent(in) :: s1, s2

 ESMF_imexeq = (s1%state .eq. s2%state)
end function

function ESMF_imexne(s1, s2)
 logical ESMF_imexne
 type(ESMF_StateType), intent(in) :: s1, s2

 ESMF_imexne = (s1%state .ne. s2%state)
end function


function ESMF_needeq(s1, s2)
 logical ESMF_needeq
 type(ESMF_NeededFlag), intent(in) :: s1, s2

 ESMF_needeq = (s1%needed .eq. s2%needed)
end function

function ESMF_needne(s1, s2)
 logical ESMF_needne
 type(ESMF_NeededFlag), intent(in) :: s1, s2

 ESMF_needne = (s1%needed .ne. s2%needed)
end function


function ESMF_redyeq(s1, s2)
 logical ESMF_redyeq
 type(ESMF_ReadyFlag), intent(in) :: s1, s2

 ESMF_redyeq = (s1%ready .eq. s2%ready)
end function

function ESMF_redyne(s1, s2)
 logical ESMF_redyne
 type(ESMF_ReadyFlag), intent(in) :: s1, s2

 ESMF_redyne = (s1%ready .ne. s2%ready)
end function


function ESMF_valideq(s1, s2)
 logical ESMF_valideq
 type(ESMF_ValidFlag), intent(in) :: s1, s2

 ESMF_valideq = (s1%valid .eq. s2%valid)
end function

function ESMF_validne(s1, s2)
 logical ESMF_validne
 type(ESMF_ValidFlag), intent(in) :: s1, s2

 ESMF_validne = (s1%valid .ne. s2%valid)
end function



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
    ESMF_INIT_CHECK_SHALLOW(ESMF_DataHolderGetInit, ESMF_DataHolderInit,dh)

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
    ESMF_INIT_CHECK_SHALLOW(ESMF_StateItemGetInit, ESMF_StateItemInit,si)

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

