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
#define ESMF_FILENAME "ESMF_RHandle.F90"
!==============================================================================
!
! ESMF RHandle Module
module ESMF_RHandleMod
!
!==============================================================================
!
! This file contains the Fortran wrapper code for the C++ implementation of
!  the RouteHandle class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_RHandleMod
!
! !DESCRIPTION:
!
!   Fortran API wrapper of C++ implementation of RouteHandle
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod           ! ESMF utility types
  use ESMF_InitMacrosMod          ! ESMF initializer macros
  use ESMF_BaseMod                ! ESMF base class
  use ESMF_LogErrMod              ! ESMF error handling
  use ESMF_F90InterfaceMod        ! ESMF F90-C++ interface helper
  use ESMF_IOUtilMod              ! ESMF I/O utility layer
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! !  ESMF_RouteHandleType
! !  MUST STAY IN SYNC WITH C++ header file
!
  integer, parameter :: ESMF_UNINITIALIZEDHANDLE=1, &
                        ESMF_ARRAYSPARSEMATMULHANDLE=2

!------------------------------------------------------------------------------
! !  ESMF_RouteHandle
!
  type ESMF_RouteHandle
#ifndef ESMF_NO_SEQUENCE
    sequence
#endif
    private
    type(ESMF_Pointer) :: this    ! opaque pointer to C++ class data
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
! !  ESMF_DynamicMaskRoutine interfaces, typekind overloaded
!
  interface
    subroutine ESMF_DynamicMaskRoutineR8R8R8(dynMaskList, dynamicSrcMaskValue, &
      dynamicDstMaskValue, rc)
      use ESMF_UtilTypesMod
      implicit none
      type(ESMF_DynamicMaskElementR8R8R8), pointer        :: dynMaskList(:)
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)  :: rc
    end subroutine
  end interface

  type ESMF_DynamicMaskStateR8R8R8
    sequence  ! in order to safely assume first element is typeKey
    character(len=7)    :: typeKey
    procedure(ESMF_DynamicMaskRoutineR8R8R8), pointer, nopass :: routine
    logical             :: dynamicSrcMaskIsPresent
    real(ESMF_KIND_R8)  :: dynamicSrcMaskValue
    logical             :: dynamicDstMaskIsPresent
    real(ESMF_KIND_R8)  :: dynamicDstMaskValue
    logical             :: handleAllElements
  end type
  
  type ESMF_DynamicMaskStateWrpR8R8R8
    type(ESMF_DynamicMaskStateR8R8R8), pointer :: wrap
  end type

  !----

#ifndef ESMF_NO_DYNMASKOVERLOAD

  interface
    subroutine ESMF_DynamicMaskRoutineR8R8R8V(dynMaskList, dynamicSrcMaskValue,&
      dynamicDstMaskValue, rc)
      use ESMF_UtilTypesMod
      implicit none
      type(ESMF_DynamicMaskElementR8R8R8V), pointer       :: dynMaskList(:)
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R8),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)  :: rc
    end subroutine
  end interface

  type ESMF_DynamicMaskStateR8R8R8V
    sequence  ! in order to safely assume first element is typeKey
    character(len=7)    :: typeKey
    procedure(ESMF_DynamicMaskRoutineR8R8R8V), pointer, nopass :: routine
    logical             :: dynamicSrcMaskIsPresent
    real(ESMF_KIND_R8)  :: dynamicSrcMaskValue
    logical             :: dynamicDstMaskIsPresent
    real(ESMF_KIND_R8)  :: dynamicDstMaskValue
    logical             :: handleAllElements
  end type
  
  type ESMF_DynamicMaskStateWrpR8R8R8V
    type(ESMF_DynamicMaskStateR8R8R8V), pointer :: wrap
  end type

  !----

  interface
    subroutine ESMF_DynamicMaskRoutineR4R8R4(dynMaskList, dynamicSrcMaskValue, &
      dynamicDstMaskValue, rc)
      use ESMF_UtilTypesMod
      implicit none
      type(ESMF_DynamicMaskElementR4R8R4), pointer        :: dynMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)  :: rc
    end subroutine
  end interface

  type ESMF_DynamicMaskStateR4R8R4
    sequence  ! in order to safely assume first element is typeKey
    character(len=7)    :: typeKey
    procedure(ESMF_DynamicMaskRoutineR4R8R4), pointer, nopass :: routine
    logical             :: dynamicSrcMaskIsPresent
    real(ESMF_KIND_R4)  :: dynamicSrcMaskValue
    logical             :: dynamicDstMaskIsPresent
    real(ESMF_KIND_R4)  :: dynamicDstMaskValue
    logical             :: handleAllElements
  end type
  
  type ESMF_DynamicMaskStateWrpR4R8R4
    type(ESMF_DynamicMaskStateR4R8R4), pointer :: wrap
  end type
  
  !----

  interface
    subroutine ESMF_DynamicMaskRoutineR4R8R4V(dynMaskList, dynamicSrcMaskValue,&
      dynamicDstMaskValue, rc)
      use ESMF_UtilTypesMod
      implicit none
      type(ESMF_DynamicMaskElementR4R8R4V), pointer       :: dynMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)  :: rc
    end subroutine
  end interface

  type ESMF_DynamicMaskStateR4R8R4V
    sequence  ! in order to safely assume first element is typeKey
    character(len=7)    :: typeKey
    procedure(ESMF_DynamicMaskRoutineR4R8R4V), pointer, nopass :: routine
    logical             :: dynamicSrcMaskIsPresent
    real(ESMF_KIND_R4)  :: dynamicSrcMaskValue
    logical             :: dynamicDstMaskIsPresent
    real(ESMF_KIND_R4)  :: dynamicDstMaskValue
    logical             :: handleAllElements
  end type
  
  type ESMF_DynamicMaskStateWrpR4R8R4V
    type(ESMF_DynamicMaskStateR4R8R4V), pointer :: wrap
  end type

  !----

  interface
    subroutine ESMF_DynamicMaskRoutineR4R4R4(dynMaskList, dynamicSrcMaskValue, &
      dynamicDstMaskValue, rc)
      use ESMF_UtilTypesMod
      implicit none
      type(ESMF_DynamicMaskElementR4R4R4), pointer        :: dynMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)  :: rc
    end subroutine
  end interface

  type ESMF_DynamicMaskStateR4R4R4
    sequence  ! in order to safely assume first element is typeKey
    character(len=7)    :: typeKey
    procedure(ESMF_DynamicMaskRoutineR4R4R4), pointer, nopass :: routine
    logical             :: dynamicSrcMaskIsPresent
    real(ESMF_KIND_R4)  :: dynamicSrcMaskValue
    logical             :: dynamicDstMaskIsPresent
    real(ESMF_KIND_R4)  :: dynamicDstMaskValue
    logical             :: handleAllElements
  end type
  
  type ESMF_DynamicMaskStateWrpR4R4R4
    type(ESMF_DynamicMaskStateR4R4R4), pointer :: wrap
  end type
  
  !----

  interface
    subroutine ESMF_DynamicMaskRoutineR4R4R4V(dynMaskList, dynamicSrcMaskValue,&
      dynamicDstMaskValue, rc)
      use ESMF_UtilTypesMod
      implicit none
      type(ESMF_DynamicMaskElementR4R4R4V), pointer       :: dynMaskList(:)
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicSrcMaskValue
      real(ESMF_KIND_R4),            intent(in), optional :: dynamicDstMaskValue
      integer,                       intent(out)  :: rc
    end subroutine
  end interface

  type ESMF_DynamicMaskStateR4R4R4V
    sequence  ! in order to safely assume first element is typeKey
    character(len=7)    :: typeKey
    procedure(ESMF_DynamicMaskRoutineR4R4R4V), pointer, nopass :: routine
    logical             :: dynamicSrcMaskIsPresent
    real(ESMF_KIND_R4)  :: dynamicSrcMaskValue
    logical             :: dynamicDstMaskIsPresent
    real(ESMF_KIND_R4)  :: dynamicDstMaskValue
    logical             :: handleAllElements
  end type
  
  type ESMF_DynamicMaskStateWrpR4R4R4V
    type(ESMF_DynamicMaskStateR4R4R4V), pointer :: wrap
  end type

#endif

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_RouteHandle
  public ESMF_UNINITIALIZEDHANDLE, ESMF_ARRAYSPARSEMATMULHANDLE
  public ESMF_DynamicMaskRoutineR8R8R8
  public ESMF_DynamicMaskStateR8R8R8, ESMF_DynamicMaskStateWrpR8R8R8
#ifndef ESMF_NO_DYNMASKOVERLOAD
  public ESMF_DynamicMaskRoutineR8R8R8V
  public ESMF_DynamicMaskStateR8R8R8V, ESMF_DynamicMaskStateWrpR8R8R8V
  public ESMF_DynamicMaskRoutineR4R8R4
  public ESMF_DynamicMaskStateR4R8R4, ESMF_DynamicMaskStateWrpR4R8R4
  public ESMF_DynamicMaskRoutineR4R8R4V
  public ESMF_DynamicMaskStateR4R8R4V, ESMF_DynamicMaskStateWrpR4R8R4V
  public ESMF_DynamicMaskRoutineR4R4R4
  public ESMF_DynamicMaskStateR4R4R4, ESMF_DynamicMaskStateWrpR4R4R4
  public ESMF_DynamicMaskRoutineR4R4R4V
  public ESMF_DynamicMaskStateR4R4R4V, ESMF_DynamicMaskStateWrpR4R4R4V
#endif

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
  public ESMF_RouteHandleGetInit
  public ESMF_RouteHandleSetInitCreated

  public ESMF_RouteHandleCreate
  public ESMF_RouteHandleDestroy

  public ESMF_RouteHandleIsCreated
  
  public ESMF_RouteHandleRelease

  public ESMF_RouteHandlePrepXXE
  public ESMF_RouteHandleAppend
  
  public ESMF_RouteHandleGet
  public ESMF_RouteHandleSet
 
  public ESMF_RouteHandleValidate
  public ESMF_RouteHandlePrint

  public ESMF_RouteHandleWrite
  
  public ESMF_RouteHandleOptimize

  public ESMF_RouteHandleCopyThis
 
!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id$'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

  interface ESMF_RouteHandleCreate
    module procedure ESMF_RouteHandleCreateDef
    module procedure ESMF_RouteHandleCreateRH
    module procedure ESMF_RouteHandleCreateFile
  end interface

  interface ESMF_RouteHandleGet
    module procedure ESMF_RouteHandleGetP
    module procedure ESMF_RouteHandleGetI
  end interface

  interface ESMF_RouteHandleSet
    module procedure ESMF_RouteHandleSetP
    module procedure ESMF_RouteHandleSetI
  end interface

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleGetInit"
!BOPI
! !IROUTINE: ESMF_RouteHandleGetInit - Get the Init status 

! !INTERFACE:
  function ESMF_RouteHandleGetInit(d)
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_RouteHandleGetInit
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in),optional :: d
!
! !DESCRIPTION:
!   Get the init status
!
!   The arguments are:
!   \begin{description}
!   \item[d] 
!     The class to be queried 
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(d)) then
      ESMF_RouteHandleGetInit=ESMF_INIT_GET(d)
    else
      ESMF_RouteHandleGetInit=ESMF_INIT_CREATED
    endif 
  end function ESMF_RouteHandleGetInit
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_RouteHandleSetInitCreated - Set RouteHandle init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_RouteHandleSetInitCreated(rh, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)           :: rh
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!   Set init code in RouteHandle object to "CREATED".
!
!   The arguments are:
!   \begin{description}
!   \item[rh] 
!     Specified {\tt ESMF\_RouteHandle} object.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(rh)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteHandleSetInitCreated
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleCreateDef"
!BOPI
! !IROUTINE: ESMF_RouteHandleCreate - Create a new RouteHandle

! !INTERFACE:
  ! Private name; call using ESMF_RouteHandleCreate()
  function ESMF_RouteHandleCreateDef(rc)
!
! !RETURN VALUE:
    type(ESMF_RouteHandle) :: ESMF_RouteHandleCreateDef
!
! !ARGUMENTS:
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!   Allocates memory for a new {\tt ESMF\_RouteHandle} object and 
!   constructs its internals.
!
!   The arguments are:
!   \begin{description}
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_RouteHandle)  :: rhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    rhandle%this = ESMF_NULL_POINTER
    ESMF_RouteHandleCreateDef = rhandle

    ! Call C++ create code
    call c_ESMC_RouteHandleCreate(rhandle, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    ESMF_RouteHandleCreateDef = rhandle

    ESMF_INIT_SET_CREATED(ESMF_RouteHandleCreateDef)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_RouteHandleCreateDef
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleCreateRH"
!BOP
! !IROUTINE: ESMF_RouteHandleCreate - Create a new RouteHandle from RouteHandle

! !INTERFACE:
  ! Private name; call using ESMF_RouteHandleCreate()
  function ESMF_RouteHandleCreateRH(routehandle, keywordEnforcer, &
    originPetList, targetPetList, rc)
!
! !RETURN VALUE:
    type(ESMF_RouteHandle) :: ESMF_RouteHandleCreateRH
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(in),  optional :: originPetList(:)
    integer,                intent(in),  optional :: targetPetList(:)
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new {\tt ESMF\_RouteHandle} object from and existing RouteHandle.
!   The new RouteHandle can be created to function on a different petList than
!   the incoming RouteHandle.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle]
!     The RouteHandle object to be duplicated.
!   \item[{[originPetList]}]
!     \begin{sloppypar}
!     The petList on which the incoming {\tt routehandle} is defined to operate.
!     If present, then {\tt targetPetList} must also be present and of the same
!     size. The petLists are used to map origin PETs to target PETs. By 
!     convention the petLists are constructed to first list the PETs of the
!     source component, followed by the PETs of the destination component.
!     Defaults, to the petList of the current component context, meaning that 
!     the PETs in the RouteHandle are not modified.
!     \end{sloppypar}
!   \item[{[targetPetList]}]
!     \begin{sloppypar}
!     The petList on which the newly created RouteHandle is defined to operate.
!     If present, then {\tt originPetList} must also be present and of the same
!     size. The petLists are used to map origin PETs to target PETs. By 
!     convention the petLists are constructed to first list the PETs of the
!     source component, followed by the PETs of the destination component.
!     Defaults, to the petList of the current component context, meaning that 
!     the PETs in the RouteHandle are not modified.
!     \end{sloppypar}
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_RouteHandle)  :: rhandle
    type(ESMF_InterArray)   :: originPetListArg
    type(ESMF_InterArray)   :: targetPetListArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    rhandle%this = ESMF_NULL_POINTER
    ESMF_RouteHandleCreateRH = rhandle

    ! Deal with (optional) array arguments
    originPetListArg = ESMF_InterArrayCreate(originPetList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    targetPetListArg = ESMF_InterArrayCreate(targetPetList, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call C++ create code
    call c_ESMC_RouteHandleCreateRH(rhandle, routehandle, originPetListArg, &
      targetPetListArg, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    ESMF_RouteHandleCreateRH = rhandle

    ! Garbage collection
    call ESMF_InterArrayDestroy(originPetListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterArrayDestroy(targetPetListArg, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_RouteHandleCreateRH)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_RouteHandleCreateRH
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleCreateFile"
!BOP
! !IROUTINE: ESMF_RouteHandleCreate - Create a new RouteHandle from file

! !INTERFACE:
  ! Private name; call using ESMF_RouteHandleCreate()
  function ESMF_RouteHandleCreateFile(fileName, keywordEnforcer, rc)
!
! !RETURN VALUE:
    type(ESMF_RouteHandle) :: ESMF_RouteHandleCreateFile
!
! !ARGUMENTS:
    character(*),           intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Create a new {\tt ESMF\_RouteHandle} object from a file.
!
!   The arguments are:
!   \begin{description}
!   \item[fileName]
!     The name of the RouteHandle file.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_RouteHandle)  :: rhandle

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    rhandle%this = ESMF_NULL_POINTER
    ESMF_RouteHandleCreateFile = rhandle

    ! Call C++ create code
    call c_ESMC_RouteHandleCreateFile(rhandle, fileName, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    ESMF_RouteHandleCreateFile = rhandle

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_RouteHandleCreateFile)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_RouteHandleCreateFile
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleDestroy"
!BOP
! !IROUTINE: ESMF_RouteHandleDestroy - Release resources associated with a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleDestroy(routehandle, keywordEnforcer, &
    noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)          :: routehandle   
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional :: noGarbage
    integer,                intent(out),  optional :: rc
!
! !DESCRIPTION:
!   Destroys an {\tt ESMF\_RouteHandle}, releasing the resources associated
!   with the object.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     The {\tt ESMF\_RouteHandle} to be destroyed.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Logical)      :: opt_noGarbage  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variable
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    ! Set default flags
    opt_noGarbage = ESMF_FALSE
    if (present(noGarbage)) opt_noGarbage = noGarbage

    ! was handle already destroyed?
    if (routehandle%this .eq. ESMF_NULL_POINTER) then
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif 

    ! Call C++ destroy code
    call c_ESMC_RouteHandleDestroy(routehandle, opt_noGarbage, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! nullify pointer
    routehandle%this = ESMF_NULL_POINTER
    ESMF_INIT_SET_DELETED(routehandle)
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleGetP"
!BOP
! !IROUTINE: ESMF_RouteHandleGet - Get values from a RouteHandle

! !INTERFACE:
  ! Private name; call using ESMF_RouteHandleGet()
  subroutine ESMF_RouteHandleGetP(routehandle, keywordEnforcer, name, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len=*),       intent(out), optional :: name
    integer,                intent(out), optional :: rc

!
! !DESCRIPTION:
!     Returns information about an {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item[routehandle] 
!          {\tt ESMF\_RouteHandle} to be queried.
!     \item [{[name]}]
!          Name of the RouteHandle object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    if (present(name)) then
      call c_ESMC_GetName(routehandle, name, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleGetP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleGetI"
!BOPI
! !IROUTINE: ESMF_RouteHandleGet - Get values from a RouteHandle

! !INTERFACE:
  ! Private name; call using ESMF_RouteHandleGet()
  subroutine ESMF_RouteHandleGetI(routehandle, htype, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)  :: routehandle
    integer,                intent(out) :: htype
    integer,                intent(out) :: rc

!
! !DESCRIPTION:
!     Returns information about an {\tt ESMF\_RouteHandle}.
!
!     The arguments are:
!     \begin{description}
!     \item[routehandle] 
!          {\tt ESMF\_RouteHandle} to be queried.
!     \item[htype]
!          Route type.
!     \item[rc] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    call c_ESMC_RouteHandleGetType(routehandle, htype, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleGetI
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleIsCreated()"
!BOP
! !IROUTINE: ESMF_RouteHandleIsCreated - Check whether a RouteHandle object has been created

! !INTERFACE:
  function ESMF_RouteHandleIsCreated(routehandle, keywordEnforcer, rc)
! !RETURN VALUE:
    logical :: ESMF_RouteHandleIsCreated
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc

! !DESCRIPTION:
!   Return {\tt .true.} if the {\tt routehandle} has been created. Otherwise return 
!   {\tt .false.}. If an error occurs, i.e. {\tt rc /= ESMF\_SUCCESS} is 
!   returned, the return value of the function will also be {\tt .false.}.
!
! The arguments are:
!   \begin{description}
!   \item[routehandle]
!     {\tt ESMF\_RouteHandle} queried.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
  !-----------------------------------------------------------------------------    
    ESMF_RouteHandleIsCreated = .false.   ! initialize
    if (present(rc)) rc = ESMF_SUCCESS
    if (ESMF_RouteHandleGetInit(routehandle)==ESMF_INIT_CREATED) &
      ESMF_RouteHandleIsCreated = .true.
  end function
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleRelease"
!BOPI
! !IROUTINE: ESMF_RouteHandleRelease - Release all RouteHandle resources

! !INTERFACE:
  subroutine ESMF_RouteHandleRelease(routehandle, keywordEnforcer, noGarbage, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)          :: routehandle   
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),   optional :: noGarbage
    integer,                intent(out),  optional :: rc
!
! !DESCRIPTION:
!   Same as {\tt ESMF\_RouteHandleDestroy}.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     The {\tt ESMF\_RouteHandle} to be released.
!   \item[{[noGarbage]}]
!     If set to {\tt .TRUE.} the object will be fully destroyed and removed
!     from the ESMF garbage collection system. Note however that under this 
!     condition ESMF cannot protect against accessing the destroyed object 
!     through dangling aliases -- a situation which may lead to hard to debug 
!     application crashes.
! 
!     It is generally recommended to leave the {\tt noGarbage} argument
!     set to {\tt .FALSE.} (the default), and to take advantage of the ESMF 
!     garbage collection system which will prevent problems with dangling
!     aliases or incorrect sequences of destroy calls. However this level of
!     support requires that a small remnant of the object is kept in memory
!     past the destroy call. This can lead to an unexpected increase in memory
!     consumption over the course of execution in applications that use 
!     temporary ESMF objects. For situations where the repeated creation and 
!     destruction of temporary objects leads to memory issues, it is 
!     recommended to call with {\tt noGarbage} set to {\tt .TRUE.}, fully 
!     removing the entire temporary object from memory.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! check input variable
    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    call ESMF_RouteHandleDestroy(routehandle, noGarbage=noGarbage, rc=localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc))  return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteHandleRelease
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandlePrepXXE"
!BOPI
! !IROUTINE: ESMF_RouteHandlePrepXXE - Prepare RouteHandle for XXE based comms

! !INTERFACE:
  subroutine ESMF_RouteHandlePrepXXE(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout) :: routehandle
    integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!   Prepare an {\tt ESMF\_RouteHandle} to be of type ARRAYBUNDLEXXE, and 
!   ready for {\tt ESMF\_RouteHandleAppend()} calls.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     {\tt ESMF\_RouteHandle} to be prepared.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)
    
    call c_ESMC_RouteHandlePrepXXE(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandlePrepXXE
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleAppend"
!BOPI
! !IROUTINE: ESMF_RouteHandleAppend - Append XXE based RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleAppend(routehandle, appendRoutehandle, rraShift, &
    vectorLengthShift, transferflag, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout) :: routehandle
    type(ESMF_RouteHandle), intent(inout) :: appendRoutehandle
    integer, intent(in)                   :: rraShift
    integer, intent(in)                   :: vectorLengthShift
    logical, intent(in),  optional        :: transferflag
    integer, intent(out), optional        :: rc

!
! !DESCRIPTION:
!   Append the exchange pattern stored in {\tt appendRoutehandle} to the 
!   {\tt routehandle}. Optionally transfer ownership of the exchange pattern
!   stored in the incoming {\tt appendRoutehandle} to the {\tt routehandle}.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     {\tt ESMF\_RouteHandle} to be appended to.
!   \item[appendRoutehandle] 
!     {\tt ESMF\_RouteHandle} to be appended and cleared.
!   \item[{[transferflag]}] 
!     If set to {\tt .true.}, the ownership of the appended exchange will be
!     transferred to {\tt routehandle}. This means that the exchange will be 
!     released when {\tt routehandle} is released. Even when ownership of the
!     exchange pattern is transferred, {\tt appendRoutehandle} still can be used
!     as a container to reference the exchange, e.g. to append the same
!     exchange pattern multiple times. The default is {\tt .false.}.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Logical)      :: transferflagArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)
    
    if (present(transferflag)) then
      transferflagArg = transferflag
    else
      transferflagArg = ESMF_FALSE ! default
    endif

    call c_ESMC_RouteHandleAppend(routehandle, appendRoutehandle, &
      rraShift, vectorLengthShift, transferflagArg, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleAppend
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandlePrint"
!BOP
! !IROUTINE: ESMF_RouteHandlePrint - Print the contents of a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandlePrint(routehandle, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: routehandle      
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Print information about an {\tt ESMF\_RouteHandle}.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     {\tt ESMF\_RouteHandle} to print contents of.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    call ESMF_UtilIOUnitFlush (ESMF_UtilIOStdout, rc=localrc)
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_RouteHandlePrint(routehandle, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_RouteHandlePrint
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleSetP"
!BOP
! !IROUTINE: ESMF_RouteHandleSet - Set values in a RouteHandle

! !INTERFACE:
  ! Private name; call using ESMF_RouteHandleSet()
  subroutine ESMF_RouteHandleSetP(routehandle, keywordEnforcer, name, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)         :: routehandle
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    character(len = *),     intent(in),  optional :: name    
    integer,                intent(out), optional :: rc

!
! !DESCRIPTION:
!   Set an {\tt ESMF\_RouteHandle} attribute with the given value.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     {\tt ESMF\_RouteHandle} to be modified.
!   \item [{[name]}]
!     The RouteHandle name.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)
    
    if (present(name)) then
      call c_ESMC_SetName(routehandle, "RouteHandle", name, localrc)
      if (ESMF_LogFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleSetP
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleSetI"
!BOPI
! !IROUTINE: ESMF_RouteHandleSet - Set values in a RouteHandle

! !INTERFACE:
  ! Private name; call using ESMF_RouteHandleSet()
  subroutine ESMF_RouteHandleSetI(routehandle, htype, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)  :: routehandle
    integer,                intent(in)  :: htype
    integer,                intent(out) :: rc

!
! !DESCRIPTION:
!   Set an {\tt ESMF\_RouteHandle} attribute with the given value.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     {\tt ESMF\_RouteHandle} to be modified.
!   \item[htype]
!     Route type.
!   \item[rc] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)
    
    call c_ESMC_RouteHandleSetType(routehandle, htype, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    rc = ESMF_SUCCESS

  end subroutine ESMF_RouteHandleSetI
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleValidate"
!BOPI
! !IROUTINE: ESMF_RouteHandleValidate - Check internal consistency of a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleValidate(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: routehandle       
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Validates that an {\tt ESMF\_RouteHandle} is internally consistent.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     {\tt ESMF\_RouteHandle} to be queried.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    call c_ESMC_RouteHandleValidate(routehandle, localrc)   
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_RouteHandleValidate
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleWrite"
!BOP
! !IROUTINE: ESMF_RouteHandleWrite - Write the RouteHandle to file

! !INTERFACE:
  subroutine ESMF_RouteHandleWrite(routehandle, fileName, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(inout)         :: routehandle   
    character(*),           intent(in)            :: fileName
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Write the RouteHandle to file. The generated file can then be used to
!   re-create the same RouteHandle through via the 
!   {\tt ESMF\_RouteHandleCreate(fileName=...)} method.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     The {\tt ESMF\_RouteHandle} to be written.
!   \item[fileName]
!     The name of the output file to which the RouteHandle is written.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    call c_ESMC_RouteHandleWrite(routehandle, fileName, localrc)
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_RouteHandleWrite
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleOptimize"
!BOPI
! !IROUTINE: ESMF_RouteHandleOptimize - Optimization based on a RouteHandle

! !INTERFACE:
  subroutine ESMF_RouteHandleOptimize(routehandle, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)            :: routehandle      
    integer,                intent(out), optional :: rc
!
! !DESCRIPTION:
!   Optimize communications based on the information available in the
!   {\tt ESMF\_RouteHandle} object.
!
!   The arguments are:
!   \begin{description}
!   \item[routehandle] 
!     {\tt ESMF\_RouteHandle} holding the communication patter for which the
!     optimization is carried out.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ESMF_INIT_CHECK_DEEP(ESMF_RouteHandleGetInit,routehandle,rc)

    call c_ESMC_RouteHandleOptimize(routehandle, localrc)   
    if (ESMF_LogFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return values
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_RouteHandleOptimize
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_RouteHandleCopyThis()"
!BOPI
! !IROUTINE: ESMF_RouteHandleCopyThis - Copy RouteHandle this member

! !INTERFACE:
  subroutine ESMF_RouteHandleCopyThis(rhandleIn, rhandleOut, rc)
!
! !ARGUMENTS:
    type(ESMF_RouteHandle), intent(in)              :: rhandleIn
    type(ESMF_RouteHandle), intent(inout)           :: rhandleOut
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Copy RouteHandle this member. Do not set init code.
!
!     The arguments are:
!     \begin{description}
!     \item[rhandleIn] 
!          Input {\tt ESMF\_RouteHandle} object.
!     \item[rhandleOut] 
!          Output {\tt ESMF\_RouteHandle} object.
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
    
    ! Copy this member
    rhandleOut%this = rhandleIn%this

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_RouteHandleCopyThis
!------------------------------------------------------------------------------

end module ESMF_RHandleMod


!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_dynmaskcallbackr8r8r8"
recursive subroutine f_esmf_dynmaskcallbackr8r8r8(routehandle, count, &
  elementVector, countVector, totalCount, factorsVector, valuesVector, &
  vectorL, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_LogErrMod
  use ESMF_RHandleMod
  use ISO_C_BINDING
  implicit none
  ! dummy arguments
  type(ESMF_RouteHandle)        :: routehandle
  integer                       :: count
  type(C_PTR)                   :: elementVector(count)
  real(ESMF_KIND_R8), pointer   :: elementV(:)
  integer                       :: countVector(count)
  integer                       :: totalCount
  real(ESMF_KIND_R8)            :: factorsVector(totalCount)
  type(C_PTR)                   :: valuesVector(totalCount)
  real(ESMF_KIND_R8), pointer   :: value
  real(ESMF_KIND_R8), pointer   :: valueV(:)
  integer                       :: vectorL
  integer                       :: rc
  ! local variables
  integer                       :: localrc, i, ii, j, k, k_in, v
  type(ESMF_DynamicMaskStateWrpR8R8R8)   :: dynamicMaskState
  type(ESMF_DynamicMaskElementR8R8R8), pointer  :: dynamicMaskList(:)
#ifndef ESMF_NO_DYNMASKOVERLOAD
  type(ESMF_DynamicMaskStateWrpR8R8R8V)  :: dynamicMaskStateV
  type(ESMF_DynamicMaskElementR8R8R8V), pointer :: dynamicMaskListV(:)
#endif

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

#if 0
  print *, "*** made it into f_esmf_dynmaskcallbackr8r8r8(), with count=", count
#endif

  ! access the dynamicMaskState that is stored inside the Routehandle
  nullify(dynamicMaskState%wrap)
  call c_ESMC_RouteHandleGetAS(routehandle, dynamicMaskState, localrc)
  if (ESMF_LogFoundError(localrc, msg="Must provide dynamicMaskRoutine!", &
    ESMF_CONTEXT, rcToReturn=rc)) return
  
  ! look at typeKey to see what needs to be done
  if (dynamicMaskState%wrap%typeKey == "R8R8R8") then
    ! non-vector version
    ! prepare the dynamicMaskList
    if (vectorL==1) then
      ! no vectorization
      allocate(dynamicMaskList(count))
      k=1
      do i=1, count
        call C_F_POINTER(elementVector(i), dynamicMaskList(i)%dstElement)
        allocate(dynamicMaskList(i)%factor(countVector(i)))
        allocate(dynamicMaskList(i)%srcElement(countVector(i)))
        do j=1, countVector(i)
          dynamicMaskList(i)%factor(j) = factorsVector(k)
          call C_F_POINTER(valuesVector(k), value)
          dynamicMaskList(i)%srcElement(j) = value
          k = k+1
        enddo
      enddo
    else
      ! unroll the vector dimension
      allocate(dynamicMaskList(count*vectorL))
      k=1
      ii=1
      do i=1, count
        call C_F_POINTER(elementVector(i), elementV, (/vectorL/))
        k_in=k  ! need to come back to this k value several times
        do v=1, vectorL ! unrolling
          dynamicMaskList(ii)%dstElement => elementV(v)
          allocate(dynamicMaskList(ii)%factor(countVector(i)))
          allocate(dynamicMaskList(ii)%srcElement(countVector(i)))
          k=k_in  ! reset to the entrance value
          do j=1, countVector(i)
            dynamicMaskList(ii)%factor(j) = factorsVector(k)
            call C_F_POINTER(valuesVector(k), valueV, (/vectorL/))
            dynamicMaskList(ii)%srcElement(j) = valueV(v)
            k = k+1
          enddo
          ii = ii+1
        enddo
      enddo
    endif
    ! call into user provided routine to handle dynamically masked elements
    if (dynamicMaskState%wrap%dynamicSrcMaskIsPresent &
      .and. dynamicMaskState%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicSrcMaskValue=dynamicMaskState%wrap%dynamicSrcMaskValue, &
        dynamicDstMaskValue=dynamicMaskState%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else if (dynamicMaskState%wrap%dynamicSrcMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicSrcMaskValue=dynamicMaskState%wrap%dynamicSrcMaskValue, &
        rc=localrc)
    else if (dynamicMaskState%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicDstMaskValue=dynamicMaskState%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        rc=localrc)
    endif
    ! local garbage collection before error handling to prevent memory leaks
    do i=1, size(dynamicMaskList)
      deallocate(dynamicMaskList(i)%factor)
      deallocate(dynamicMaskList(i)%srcElement)
    enddo
    deallocate(dynamicMaskList)
    ! error handling of call back into user routine
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#ifndef ESMF_NO_DYNMASKOVERLOAD
  else if (dynamicMaskState%wrap%typeKey == "R8R8R8V") then
    ! vector version -> use correct variables
    nullify(dynamicMaskStateV%wrap)
    call c_ESMC_RouteHandleGetAS(routehandle, dynamicMaskStateV, localrc)
    if (ESMF_LogFoundError(localrc, msg="Must provide dynamicMaskRoutine!", &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! prepare the dynamicMaskListV
    allocate(dynamicMaskListV(count))
    k=1
    do i=1, count
      call C_F_POINTER(elementVector(i), dynamicMaskListV(i)%dstElement, &
        (/vectorL/))
      allocate(dynamicMaskListV(i)%factor(countVector(i)))
      allocate(dynamicMaskListV(i)%srcElement(countVector(i)))
      do j=1, countVector(i)
        dynamicMaskListV(i)%factor(j) = factorsVector(k)
        call C_F_POINTER(valuesVector(k), &
          dynamicMaskListV(i)%srcElement(j)%ptr, (/vectorL/))
        k = k+1
      enddo
    enddo
    ! call into user provided routine to handle dynamically masked elements
    if (dynamicMaskStateV%wrap%dynamicSrcMaskIsPresent &
      .and. dynamicMaskStateV%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicSrcMaskValue=dynamicMaskStateV%wrap%dynamicSrcMaskValue, &
        dynamicDstMaskValue=dynamicMaskStateV%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else if (dynamicMaskStateV%wrap%dynamicSrcMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicSrcMaskValue=dynamicMaskStateV%wrap%dynamicSrcMaskValue, &
        rc=localrc)
    else if (dynamicMaskStateV%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicDstMaskValue=dynamicMaskStateV%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        rc=localrc)
    endif
    ! local garbage collection before error handling to prevent memory leaks
    do i=1, count
      deallocate(dynamicMaskListV(i)%factor)
      deallocate(dynamicMaskListV(i)%srcElement)
    enddo
    deallocate(dynamicMaskListV)
    ! error handling of call back into user routine
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
#endif
  else    
    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
      msg="Inconsistency between the provided 'dynamicMaskRoutine' and "// &
      "actual data types.", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
  endif

  ! return successfully
  rc = ESMF_SUCCESS

end subroutine f_esmf_dynmaskcallbackr8r8r8
!------------------------------------------------------------------------------

#ifndef ESMF_NO_DYNMASKOVERLOAD

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_dynmaskcallbackr4r8r4"
recursive subroutine f_esmf_dynmaskcallbackr4r8r4(routehandle, count, &
  elementVector, countVector, totalCount, factorsVector, valuesVector, &
  vectorL, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_LogErrMod
  use ESMF_RHandleMod
  use ISO_C_BINDING
  implicit none
  ! dummy arguments
  type(ESMF_RouteHandle)        :: routehandle
  integer                       :: count
  type(C_PTR)                   :: elementVector(count)
  real(ESMF_KIND_R4), pointer   :: elementV(:)
  integer                       :: countVector(count)
  integer                       :: totalCount
  real(ESMF_KIND_R8)            :: factorsVector(totalCount)
  type(C_PTR)                   :: valuesVector(totalCount)
  real(ESMF_KIND_R4), pointer   :: value
  real(ESMF_KIND_R4), pointer   :: valueV(:)
  integer                       :: vectorL
  integer                       :: rc
  ! local variables
  integer                       :: localrc, i, ii, j, k, k_in, v
  type(ESMF_DynamicMaskStateWrpR4R8R4)   :: dynamicMaskState
  type(ESMF_DynamicMaskElementR4R8R4), pointer  :: dynamicMaskList(:)
  type(ESMF_DynamicMaskStateWrpR4R8R4V)  :: dynamicMaskStateV
  type(ESMF_DynamicMaskElementR4R8R4V), pointer :: dynamicMaskListV(:)

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

#if 0
  print *, "*** made it into f_esmf_dynmaskcallbackr4r8r4(), with count=", count
#endif

  ! access the dynamicMaskState that is stored inside the Routehandle
  nullify(dynamicMaskState%wrap)
  call c_ESMC_RouteHandleGetAS(routehandle, dynamicMaskState, localrc)
  if (ESMF_LogFoundError(localrc, msg="Must provide dynamicMaskRoutine!", &
    ESMF_CONTEXT, rcToReturn=rc)) return
  
  ! look at typeKey to see what needs to be done
  if (dynamicMaskState%wrap%typeKey == "R4R8R4") then
    ! non-vector version
    ! prepare the dynamicMaskList
    if (vectorL==1) then
      ! no vectorization
      allocate(dynamicMaskList(count))
      k=1
      do i=1, count
        call C_F_POINTER(elementVector(i), dynamicMaskList(i)%dstElement)
        allocate(dynamicMaskList(i)%factor(countVector(i)))
        allocate(dynamicMaskList(i)%srcElement(countVector(i)))
        do j=1, countVector(i)
          dynamicMaskList(i)%factor(j) = factorsVector(k)
          call C_F_POINTER(valuesVector(k), value)
          dynamicMaskList(i)%srcElement(j) = value
          k = k+1
        enddo
      enddo
    else
      ! unroll the vector dimension
      allocate(dynamicMaskList(count*vectorL))
      k=1
      ii=1
      do i=1, count
        call C_F_POINTER(elementVector(i), elementV, (/vectorL/))
        k_in=k  ! need to come back to this k value several times
        do v=1, vectorL ! unrolling
          dynamicMaskList(ii)%dstElement => elementV(v)
          allocate(dynamicMaskList(ii)%factor(countVector(i)))
          allocate(dynamicMaskList(ii)%srcElement(countVector(i)))
          k=k_in  ! reset to the entrance value
          do j=1, countVector(i)
            dynamicMaskList(ii)%factor(j) = factorsVector(k)
            call C_F_POINTER(valuesVector(k), valueV, (/vectorL/))
            dynamicMaskList(ii)%srcElement(j) = valueV(v)
            k = k+1
          enddo
          ii = ii+1
        enddo
      enddo
    endif
    ! call into user provided routine to handle dynamically masked elements
    if (dynamicMaskState%wrap%dynamicSrcMaskIsPresent &
      .and. dynamicMaskState%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicSrcMaskValue=dynamicMaskState%wrap%dynamicSrcMaskValue, &
        dynamicDstMaskValue=dynamicMaskState%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else if (dynamicMaskState%wrap%dynamicSrcMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicSrcMaskValue=dynamicMaskState%wrap%dynamicSrcMaskValue, &
        rc=localrc)
    else if (dynamicMaskState%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicDstMaskValue=dynamicMaskState%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        rc=localrc)
    endif
    ! local garbage collection before error handling to prevent memory leaks
    do i=1, size(dynamicMaskList)
      deallocate(dynamicMaskList(i)%factor)
      deallocate(dynamicMaskList(i)%srcElement)
    enddo
    deallocate(dynamicMaskList)
    ! error handling of call back into user routine
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  else if (dynamicMaskState%wrap%typeKey == "R4R8R4V") then
    ! vector version -> use correct variables
    nullify(dynamicMaskStateV%wrap)
    call c_ESMC_RouteHandleGetAS(routehandle, dynamicMaskStateV, localrc)
    if (ESMF_LogFoundError(localrc, msg="Must provide dynamicMaskRoutine!", &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! prepare the dynamicMaskListV
    allocate(dynamicMaskListV(count))
    k=1
    do i=1, count
      call C_F_POINTER(elementVector(i), dynamicMaskListV(i)%dstElement, &
        (/vectorL/))
      allocate(dynamicMaskListV(i)%factor(countVector(i)))
      allocate(dynamicMaskListV(i)%srcElement(countVector(i)))
      do j=1, countVector(i)
        dynamicMaskListV(i)%factor(j) = factorsVector(k)
        call C_F_POINTER(valuesVector(k), &
          dynamicMaskListV(i)%srcElement(j)%ptr, (/vectorL/))
        k = k+1
      enddo
    enddo
    ! call into user provided routine to handle dynamically masked elements
    if (dynamicMaskStateV%wrap%dynamicSrcMaskIsPresent &
      .and. dynamicMaskStateV%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicSrcMaskValue=dynamicMaskStateV%wrap%dynamicSrcMaskValue, &
        dynamicDstMaskValue=dynamicMaskStateV%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else if (dynamicMaskStateV%wrap%dynamicSrcMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicSrcMaskValue=dynamicMaskStateV%wrap%dynamicSrcMaskValue, &
        rc=localrc)
    else if (dynamicMaskStateV%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicDstMaskValue=dynamicMaskStateV%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        rc=localrc)
    endif
    ! local garbage collection before error handling to prevent memory leaks
    do i=1, count
      deallocate(dynamicMaskListV(i)%factor)
      deallocate(dynamicMaskListV(i)%srcElement)
    enddo
    deallocate(dynamicMaskListV)
    ! error handling of call back into user routine
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  else    
    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
      msg="Inconsistency between the provided 'dynamicMaskRoutine' and "// &
      "actual data types.", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
  endif

  ! return successfully
  rc = ESMF_SUCCESS

end subroutine f_esmf_dynmaskcallbackr4r8r4
!------------------------------------------------------------------------------
 

!------------------------------------------------------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "f_esmf_dynmaskcallbackr4r4r4"
recursive subroutine f_esmf_dynmaskcallbackr4r4r4(routehandle, count, &
  elementVector, countVector, totalCount, factorsVector, valuesVector, &
  vectorL, rc)
  use ESMF_UtilTypesMod      ! ESMF utility types
  use ESMF_BaseMod           ! ESMF base class
  use ESMF_LogErrMod
  use ESMF_RHandleMod
  use ISO_C_BINDING
  implicit none
  ! dummy arguments
  type(ESMF_RouteHandle)        :: routehandle
  integer                       :: count
  type(C_PTR)                   :: elementVector(count)
  real(ESMF_KIND_R4), pointer   :: elementV(:)
  integer                       :: countVector(count)
  integer                       :: totalCount
  real(ESMF_KIND_R4)            :: factorsVector(totalCount)
  type(C_PTR)                   :: valuesVector(totalCount)
  real(ESMF_KIND_R4), pointer   :: value
  real(ESMF_KIND_R4), pointer   :: valueV(:)
  integer                       :: vectorL
  integer                       :: rc
  ! local variables
  integer                       :: localrc, i, ii, j, k, k_in, v
  type(ESMF_DynamicMaskStateWrpR4R4R4)   :: dynamicMaskState
  type(ESMF_DynamicMaskElementR4R4R4), pointer  :: dynamicMaskList(:)
  type(ESMF_DynamicMaskStateWrpR4R4R4V)  :: dynamicMaskStateV
  type(ESMF_DynamicMaskElementR4R4R4V), pointer :: dynamicMaskListV(:)

  ! Initialize return code; assume routine not implemented
  rc = ESMF_RC_NOT_IMPL

#if 0
  print *, "*** made it into f_esmf_dynmaskcallbackr4r4r4(), with count=", count
#endif

  ! access the dynamicMaskState that is stored inside the Routehandle
  nullify(dynamicMaskState%wrap)
  call c_ESMC_RouteHandleGetAS(routehandle, dynamicMaskState, localrc)
  if (ESMF_LogFoundError(localrc, msg="Must provide dynamicMaskRoutine!", &
    ESMF_CONTEXT, rcToReturn=rc)) return
  
  ! look at typeKey to see what needs to be done
  if (dynamicMaskState%wrap%typeKey == "R4R4R4") then
    ! non-vector version
    ! prepare the dynamicMaskList
    if (vectorL==1) then
      ! no vectorization
      allocate(dynamicMaskList(count))
      k=1
      do i=1, count
        call C_F_POINTER(elementVector(i), dynamicMaskList(i)%dstElement)
        allocate(dynamicMaskList(i)%factor(countVector(i)))
        allocate(dynamicMaskList(i)%srcElement(countVector(i)))
        do j=1, countVector(i)
          dynamicMaskList(i)%factor(j) = factorsVector(k)
          call C_F_POINTER(valuesVector(k), value)
          dynamicMaskList(i)%srcElement(j) = value
          k = k+1
        enddo
      enddo
    else
      ! unroll the vector dimension
      allocate(dynamicMaskList(count*vectorL))
      k=1
      ii=1
      do i=1, count
        call C_F_POINTER(elementVector(i), elementV, (/vectorL/))
        k_in=k  ! need to come back to this k value several times
        do v=1, vectorL ! unrolling
          dynamicMaskList(ii)%dstElement => elementV(v)
          allocate(dynamicMaskList(ii)%factor(countVector(i)))
          allocate(dynamicMaskList(ii)%srcElement(countVector(i)))
          k=k_in  ! reset to the entrance value
          do j=1, countVector(i)
            dynamicMaskList(ii)%factor(j) = factorsVector(k)
            call C_F_POINTER(valuesVector(k), valueV, (/vectorL/))
            dynamicMaskList(ii)%srcElement(j) = valueV(v)
            k = k+1
          enddo
          ii = ii+1
        enddo
      enddo
    endif
    ! call into user provided routine to handle dynamically masked elements
    if (dynamicMaskState%wrap%dynamicSrcMaskIsPresent &
      .and. dynamicMaskState%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicSrcMaskValue=dynamicMaskState%wrap%dynamicSrcMaskValue, &
        dynamicDstMaskValue=dynamicMaskState%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else if (dynamicMaskState%wrap%dynamicSrcMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicSrcMaskValue=dynamicMaskState%wrap%dynamicSrcMaskValue, &
        rc=localrc)
    else if (dynamicMaskState%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        dynamicDstMaskValue=dynamicMaskState%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else
      call dynamicMaskState%wrap%routine(dynMaskList=dynamicMaskList, &
        rc=localrc)
    endif
    ! local garbage collection before error handling to prevent memory leaks
    do i=1, size(dynamicMaskList)
      deallocate(dynamicMaskList(i)%factor)
      deallocate(dynamicMaskList(i)%srcElement)
    enddo
    deallocate(dynamicMaskList)
    ! error handling of call back into user routine
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  else if (dynamicMaskState%wrap%typeKey == "R4R4R4V") then
    ! vector version -> use correct variables
    nullify(dynamicMaskStateV%wrap)
    call c_ESMC_RouteHandleGetAS(routehandle, dynamicMaskStateV, localrc)
    if (ESMF_LogFoundError(localrc, msg="Must provide dynamicMaskRoutine!", &
      ESMF_CONTEXT, rcToReturn=rc)) return
    ! prepare the dynamicMaskListV
    allocate(dynamicMaskListV(count))
    k=1
    do i=1, count
      call C_F_POINTER(elementVector(i), dynamicMaskListV(i)%dstElement, &
        (/vectorL/))
      allocate(dynamicMaskListV(i)%factor(countVector(i)))
      allocate(dynamicMaskListV(i)%srcElement(countVector(i)))
      do j=1, countVector(i)
        dynamicMaskListV(i)%factor(j) = factorsVector(k)
        call C_F_POINTER(valuesVector(k), &
          dynamicMaskListV(i)%srcElement(j)%ptr, (/vectorL/))
        k = k+1
      enddo
    enddo
    ! call into user provided routine to handle dynamically masked elements
    if (dynamicMaskStateV%wrap%dynamicSrcMaskIsPresent &
      .and. dynamicMaskStateV%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicSrcMaskValue=dynamicMaskStateV%wrap%dynamicSrcMaskValue, &
        dynamicDstMaskValue=dynamicMaskStateV%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else if (dynamicMaskStateV%wrap%dynamicSrcMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicSrcMaskValue=dynamicMaskStateV%wrap%dynamicSrcMaskValue, &
        rc=localrc)
    else if (dynamicMaskStateV%wrap%dynamicDstMaskIsPresent) then
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        dynamicDstMaskValue=dynamicMaskStateV%wrap%dynamicDstMaskValue, &
        rc=localrc)
    else
      call dynamicMaskStateV%wrap%routine(dynMaskList=dynamicMaskListV, &
        rc=localrc)
    endif
    ! local garbage collection before error handling to prevent memory leaks
    do i=1, count
      deallocate(dynamicMaskListV(i)%factor)
      deallocate(dynamicMaskListV(i)%srcElement)
    enddo
    deallocate(dynamicMaskListV)
    ! error handling of call back into user routine
    if (ESMF_LogFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
  else    
    call ESMF_LogSetError(rcToCheck=ESMF_RC_ARG_INCOMP, &
      msg="Inconsistency between the provided 'dynamicMaskRoutine' and "// &
      "actual data types.", &
      ESMF_CONTEXT, rcToReturn=rc)
    return
  endif

  ! return successfully
  rc = ESMF_SUCCESS

end subroutine f_esmf_dynmaskcallbackr4r4r4
!------------------------------------------------------------------------------

#endif

!------------------------------------------------------------------------------
