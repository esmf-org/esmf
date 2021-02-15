! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_DynamicMask.F90"
!==============================================================================
!
! ESMF DistGrid Module
module ESMF_DynamicMaskMod
!
!==============================================================================
!
! This file contains the DynamicMask shallow class implementation.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_DynamicMaskMod
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod           ! ESMF utility types
  use ESMF_InitMacrosMod          ! ESMF initializer macros
  use ESMF_LogErrMod              ! ESMF error handling
  use ESMF_RHandleMod
  use ESMF_F90InterfaceMod        ! ESMF F90-C++ interface helper
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private

!------------------------------------------------------------------------------
! ! ESMF_DynamicMask

  type ESMF_DynamicMask
    character(len=7)                  :: typeKey
    type(ESMF_DynamicMaskStateR8R8R8) :: dmsR8R8R8
#ifndef ESMF_NO_DYNMASKOVERLOAD
    type(ESMF_DynamicMaskStateR8R8R8V):: dmsR8R8R8V
    type(ESMF_DynamicMaskStateR4R8R4) :: dmsR4R8R4
    type(ESMF_DynamicMaskStateR4R8R4V):: dmsR4R8R4V
    type(ESMF_DynamicMaskStateR4R4R4) :: dmsR4R4R4
    type(ESMF_DynamicMaskStateR4R4R4V):: dmsR4R4R4V
#endif
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:

  public ESMF_DynamicMask, ESMF_DynamicMaskGet, ESMF_DynamicMaskGetInit
  public ESMF_DynamicMaskSetR8R8R8
#ifndef ESMF_NO_DYNMASKOVERLOAD
  public ESMF_DynamicMaskSetR8R8R8V
  public ESMF_DynamicMaskSetR4R8R4
  public ESMF_DynamicMaskSetR4R8R4V
  public ESMF_DynamicMaskSetR4R4R4
  public ESMF_DynamicMaskSetR4R4R4V
#endif


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DynamicMaskGet()"
!BOPI
! !IROUTINE: ESMF_DynamicMaskGet - Get DynamicMask
! !INTERFACE:
  subroutine ESMF_DynamicMaskGet(dynamicMask, keywordEnforcer, rc)
!
! !ARGUMENTS:
    type(ESMF_DynamicMask), intent(in)            :: dynamicMask
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    integer,                       intent(out), optional :: rc
!
! !DESCRIPTION:
!   \label{api:DynamicMaskGet}
!   Get information from a {\tt ESMF\_DynamicMask} object.
!
!   The arguments are:
!   \begin{description}
!   \item[dynamicMask]
!     DynamicMask object.
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc      ! local return code
    integer :: localdimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! ensure dynamicMask is valid
    ESMF_INIT_CHECK_SHALLOW_SHORT(ESMF_DynamicMaskGetInit, dynamicMask, rc)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DynamicMaskGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DynamicMaskSetR8R8R8()"
!BOP
! !IROUTINE: ESMF_DynamicMaskSetR8R8R8 - Set DynamicMask for R8R8R8
! !INTERFACE:
  subroutine ESMF_DynamicMaskSetR8R8R8(dynamicMask, dynamicMaskRoutine, &
    keywordEnforcer, handleAllElements, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
!
! !ARGUMENTS:
    type(ESMF_DynamicMask), intent(out)           :: dynamicMask
    procedure(ESMF_DynamicMaskRoutineR8R8R8)      :: dynamicMaskRoutine
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: handleAllElements
    real(ESMF_KIND_R8),     intent(in),  optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R8),     intent(in),  optional :: dynamicDstMaskValue
    integer,                intent(out), optional :: rc
!         
! !DESCRIPTION:
!   \label{api:DynamicMaskSetR8R8R8}
!   Set an {\tt ESMF\_DynamicMask} object suitable for 
!   destination element typekind {\tt ESMF\_TYPEKIND\_R8},
!   factor typekind {\tt ESMF\_TYPEKIND\_R8}, and
!   source element typekind {\tt ESMF\_TYPEKIND\_R8}.
!   
!   All values in {\tt dynamicMask} will be reset by this call.
!
!   See section \ref{RH:DynMask} for a general discussion of dynamic masking.
!
!   The arguments are:
!   \begin{description}
!   \item[dynamicMask] 
!     DynamicMask object.
!   \item [dynamicMaskRoutine]
!     The routine responsible for handling dynamically masked source and 
!     destination elements. See section \ref{RH:DynMask} for the precise
!     definition of the {\tt dynamicMaskRoutine} procedure interface.
!     The routine is only called on PETs where {\em at least one} interpolation 
!     element is identified for special handling.
!   \item [{[handleAllElements]}]
!     If set to {\tt .true.}, all local elements, regardless of their dynamic
!     masking status, are made available to {\tt dynamicMaskRoutine} for
!     handling. This option can be used to implement fully customized
!     interpolations based on the information provided by ESMF.
!     The default is {\tt .false.}, meaning that only elements affected by
!     dynamic masking will be handed to {\tt dynamicMaskRoutine}.
!   \item [{[dynamicSrcMaskValue]}]
!     The value for which a source element is considered dynamically
!     masked.
!     The default is to {\em not} consider any source elements as
!     dynamically masked.
!   \item [{[dynamicDstMaskValue]}]
!     The value for which a destination element is considered dynamically
!     masked.
!     The default is to {\em not} consider any destination elements as
!     dynamically masked.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: dimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! mark output as uninitialized    
    ESMF_INIT_SET_DELETED(dynamicMask)

    ! set the internals
    dynamicMask%typeKey           =   "R8R8R8"
    dynamicMask%dmsR8R8R8%typeKey =   dynamicMask%typeKey
    dynamicMask%dmsR8R8R8%routine =>  dynamicMaskRoutine
    dynamicMask%dmsR8R8R8%dynamicSrcMaskIsPresent = present(dynamicSrcMaskValue)
    if (present(dynamicSrcMaskValue)) &
      dynamicMask%dmsR8R8R8%dynamicSrcMaskValue = dynamicSrcMaskValue
    dynamicMask%dmsR8R8R8%dynamicDstMaskIsPresent = present(dynamicDstMaskValue)
    if (present(dynamicDstMaskValue)) &
      dynamicMask%dmsR8R8R8%dynamicDstMaskValue = dynamicDstMaskValue
    if (present(handleAllElements)) then
      dynamicMask%dmsR8R8R8%handleAllElements = handleAllElements
    else
      dynamicMask%dmsR8R8R8%handleAllElements = .false. ! default
    endif
    
    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(dynamicMask)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DynamicMaskSetR8R8R8
!------------------------------------------------------------------------------

#ifndef ESMF_NO_DYNMASKOVERLOAD

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DynamicMaskSetR8R8R8V()"
!BOP
! !IROUTINE: ESMF_DynamicMaskSetR8R8R8V - Set DynamicMask for R8R8R8 with vectorization
! !INTERFACE:
  subroutine ESMF_DynamicMaskSetR8R8R8V(dynamicMask, dynamicMaskRoutine, &
    keywordEnforcer, handleAllElements, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
!
! !ARGUMENTS:
    type(ESMF_DynamicMask), intent(out)           :: dynamicMask
    procedure(ESMF_DynamicMaskRoutineR8R8R8V)     :: dynamicMaskRoutine
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: handleAllElements
    real(ESMF_KIND_R8),     intent(in),  optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R8),     intent(in),  optional :: dynamicDstMaskValue
    integer,                intent(out), optional :: rc
!         
! !DESCRIPTION:
!   \label{api:DynamicMaskSetR8R8R8V}
!   Set an {\tt ESMF\_DynamicMask} object suitable for 
!   destination element typekind {\tt ESMF\_TYPEKIND\_R8},
!   factor typekind {\tt ESMF\_TYPEKIND\_R8}, and
!   source element typekind {\tt ESMF\_TYPEKIND\_R8}.
!   
!   All values in {\tt dynamicMask} will be reset by this call.
!
!   See section \ref{RH:DynMask} for a general discussion of dynamic masking.
!
!   The arguments are:
!   \begin{description}
!   \item[dynamicMask] 
!     DynamicMask object.
!   \item [dynamicMaskRoutine]
!     The routine responsible for handling dynamically masked source and 
!     destination elements. See section \ref{RH:DynMask} for the precise
!     definition of the {\tt dynamicMaskRoutine} procedure interface.
!     The routine is only called on PETs where {\em at least one} interpolation 
!     element is identified for special handling.
!   \item [{[handleAllElements]}]
!     If set to {\tt .true.}, all local elements, regardless of their dynamic
!     masking status, are made available to {\tt dynamicMaskRoutine} for
!     handling. This option can be used to implement fully customized
!     interpolations based on the information provided by ESMF.
!     The default is {\tt .false.}, meaning that only elements affected by
!     dynamic masking will be handed to {\tt dynamicMaskRoutine}.
!   \item [{[dynamicSrcMaskValue]}]
!     The value for which a source element is considered dynamically
!     masked.
!     The default is to {\em not} consider any source elements as
!     dynamically masked.
!   \item [{[dynamicDstMaskValue]}]
!     The value for which a destination element is considered dynamically
!     masked.
!     The default is to {\em not} consider any destination elements as
!     dynamically masked.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: dimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! mark output as uninitialized    
    ESMF_INIT_SET_DELETED(dynamicMask)

    ! set the internals
    dynamicMask%typeKey           =   "R8R8R8V"
    dynamicMask%dmsR8R8R8V%typeKey =   dynamicMask%typeKey
    dynamicMask%dmsR8R8R8V%routine =>  dynamicMaskRoutine
    dynamicMask%dmsR8R8R8V%dynamicSrcMaskIsPresent = present(dynamicSrcMaskValue)
    if (present(dynamicSrcMaskValue)) &
      dynamicMask%dmsR8R8R8V%dynamicSrcMaskValue = dynamicSrcMaskValue
    dynamicMask%dmsR8R8R8V%dynamicDstMaskIsPresent = present(dynamicDstMaskValue)
    if (present(dynamicDstMaskValue)) &
      dynamicMask%dmsR8R8R8V%dynamicDstMaskValue = dynamicDstMaskValue
    if (present(handleAllElements)) then
      dynamicMask%dmsR8R8R8V%handleAllElements = handleAllElements
    else
      dynamicMask%dmsR8R8R8V%handleAllElements = .false. ! default
    endif
    
    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(dynamicMask)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DynamicMaskSetR8R8R8V
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DynamicMaskSetR4R8R4()"
!BOP
! !IROUTINE: ESMF_DynamicMaskSetR4R8R4 - Set DynamicMask for R4R8R4
! !INTERFACE:
  subroutine ESMF_DynamicMaskSetR4R8R4(dynamicMask, dynamicMaskRoutine, &
    keywordEnforcer, handleAllElements, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
!
! !ARGUMENTS:
    type(ESMF_DynamicMask), intent(out)           :: dynamicMask
    procedure(ESMF_DynamicMaskRoutineR4R8R4)      :: dynamicMaskRoutine
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: handleAllElements
    real(ESMF_KIND_R4),     intent(in),  optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),     intent(in),  optional :: dynamicDstMaskValue
    integer,                intent(out), optional :: rc
!         
! !DESCRIPTION:
!   \label{api:DynamicMaskSetR4R8R4}
!   Set an {\tt ESMF\_DynamicMask} object suitable for 
!   destination element typekind {\tt ESMF\_TYPEKIND\_R4},
!   factor typekind {\tt ESMF\_TYPEKIND\_R8}, and
!   source element typekind {\tt ESMF\_TYPEKIND\_R4}.
!   
!   All values in {\tt dynamicMask} will be reset by this call.
!
!   See section \ref{RH:DynMask} for a general discussion of dynamic masking.
!
!   The arguments are:
!   \begin{description}
!   \item[dynamicMask] 
!     DynamicMask object.
!   \item [dynamicMaskRoutine]
!     The routine responsible for handling dynamically masked source and 
!     destination elements. See section \ref{RH:DynMask} for the precise
!     definition of the {\tt dynamicMaskRoutine} procedure interface.
!     The routine is only called on PETs where {\em at least one} interpolation 
!     element is identified for special handling.
!   \item [{[handleAllElements]}]
!     If set to {\tt .true.}, all local elements, regardless of their dynamic
!     masking status, are made available to {\tt dynamicMaskRoutine} for
!     handling. This option can be used to implement fully customized
!     interpolations based on the information provided by ESMF.
!     The default is {\tt .false.}, meaning that only elements affected by
!     dynamic masking will be handed to {\tt dynamicMaskRoutine}.
!   \item [{[dynamicSrcMaskValue]}]
!     The value for which a source element is considered dynamically
!     masked.
!     The default is to {\em not} consider any source elements as
!     dynamically masked.
!   \item [{[dynamicDstMaskValue]}]
!     The value for which a destination element is considered dynamically
!     masked.
!     The default is to {\em not} consider any destination elements as
!     dynamically masked.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: dimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! mark output as uninitialized    
    ESMF_INIT_SET_DELETED(dynamicMask)

    ! set the internals
    dynamicMask%typeKey           =   "R4R8R4"
    dynamicMask%dmsR4R8R4%typeKey =   dynamicMask%typeKey
    dynamicMask%dmsR4R8R4%routine =>  dynamicMaskRoutine
    dynamicMask%dmsR4R8R4%dynamicSrcMaskIsPresent = present(dynamicSrcMaskValue)
    if (present(dynamicSrcMaskValue)) &
      dynamicMask%dmsR4R8R4%dynamicSrcMaskValue = dynamicSrcMaskValue
    dynamicMask%dmsR4R8R4%dynamicDstMaskIsPresent = present(dynamicDstMaskValue)
    if (present(dynamicDstMaskValue)) &
      dynamicMask%dmsR4R8R4%dynamicDstMaskValue = dynamicDstMaskValue
    if (present(handleAllElements)) then
      dynamicMask%dmsR4R8R4%handleAllElements = handleAllElements
    else
      dynamicMask%dmsR4R8R4%handleAllElements = .false. ! default
    endif

    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(dynamicMask)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DynamicMaskSetR4R8R4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DynamicMaskSetR4R8R4V()"
!BOP
! !IROUTINE: ESMF_DynamicMaskSetR4R8R4V - Set DynamicMask for R4R8R4 with vectorization
! !INTERFACE:
  subroutine ESMF_DynamicMaskSetR4R8R4V(dynamicMask, dynamicMaskRoutine, &
    keywordEnforcer, handleAllElements, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
!
! !ARGUMENTS:
    type(ESMF_DynamicMask), intent(out)           :: dynamicMask
    procedure(ESMF_DynamicMaskRoutineR4R8R4V)     :: dynamicMaskRoutine
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: handleAllElements
    real(ESMF_KIND_R4),     intent(in),  optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),     intent(in),  optional :: dynamicDstMaskValue
    integer,                intent(out), optional :: rc
!         
! !DESCRIPTION:
!   \label{api:DynamicMaskSetR4R8R4V}
!   Set an {\tt ESMF\_DynamicMask} object suitable for 
!   destination element typekind {\tt ESMF\_TYPEKIND\_R4},
!   factor typekind {\tt ESMF\_TYPEKIND\_R8}, and
!   source element typekind {\tt ESMF\_TYPEKIND\_R4}.
!   
!   All values in {\tt dynamicMask} will be reset by this call.
!
!   See section \ref{RH:DynMask} for a general discussion of dynamic masking.
!
!   The arguments are:
!   \begin{description}
!   \item[dynamicMask] 
!     DynamicMask object.
!   \item [dynamicMaskRoutine]
!     The routine responsible for handling dynamically masked source and 
!     destination elements. See section \ref{RH:DynMask} for the precise
!     definition of the {\tt dynamicMaskRoutine} procedure interface.
!     The routine is only called on PETs where {\em at least one} interpolation 
!     element is identified for special handling.
!   \item [{[handleAllElements]}]
!     If set to {\tt .true.}, all local elements, regardless of their dynamic
!     masking status, are made available to {\tt dynamicMaskRoutine} for
!     handling. This option can be used to implement fully customized
!     interpolations based on the information provided by ESMF.
!     The default is {\tt .false.}, meaning that only elements affected by
!     dynamic masking will be handed to {\tt dynamicMaskRoutine}.
!   \item [{[dynamicSrcMaskValue]}]
!     The value for which a source element is considered dynamically
!     masked.
!     The default is to {\em not} consider any source elements as
!     dynamically masked.
!   \item [{[dynamicDstMaskValue]}]
!     The value for which a destination element is considered dynamically
!     masked.
!     The default is to {\em not} consider any destination elements as
!     dynamically masked.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: dimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! mark output as uninitialized    
    ESMF_INIT_SET_DELETED(dynamicMask)

    ! set the internals
    dynamicMask%typeKey           =   "R4R8R4V"
    dynamicMask%dmsR4R8R4V%typeKey =   dynamicMask%typeKey
    dynamicMask%dmsR4R8R4V%routine =>  dynamicMaskRoutine
    dynamicMask%dmsR4R8R4V%dynamicSrcMaskIsPresent = present(dynamicSrcMaskValue)
    if (present(dynamicSrcMaskValue)) &
      dynamicMask%dmsR4R8R4V%dynamicSrcMaskValue = dynamicSrcMaskValue
    dynamicMask%dmsR4R8R4V%dynamicDstMaskIsPresent = present(dynamicDstMaskValue)
    if (present(dynamicDstMaskValue)) &
      dynamicMask%dmsR4R8R4V%dynamicDstMaskValue = dynamicDstMaskValue
    if (present(handleAllElements)) then
      dynamicMask%dmsR4R8R4V%handleAllElements = handleAllElements
    else
      dynamicMask%dmsR4R8R4V%handleAllElements = .false. ! default
    endif
    
    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(dynamicMask)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DynamicMaskSetR4R8R4V
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DynamicMaskSetR4R4R4()"
!BOP
! !IROUTINE: ESMF_DynamicMaskSetR4R4R4 - Set DynamicMask for R4R4R4
! !INTERFACE:
  subroutine ESMF_DynamicMaskSetR4R4R4(dynamicMask, dynamicMaskRoutine, &
    keywordEnforcer, handleAllElements, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
!
! !ARGUMENTS:
    type(ESMF_DynamicMask), intent(out)           :: dynamicMask
    procedure(ESMF_DynamicMaskRoutineR4R4R4)      :: dynamicMaskRoutine
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: handleAllElements
    real(ESMF_KIND_R4),     intent(in),  optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),     intent(in),  optional :: dynamicDstMaskValue
    integer,                intent(out), optional :: rc
!         
! !DESCRIPTION:
!   \label{api:DynamicMaskSetR4R4R4}
!   Set an {\tt ESMF\_DynamicMask} object suitable for 
!   destination element typekind {\tt ESMF\_TYPEKIND\_R4},
!   factor typekind {\tt ESMF\_TYPEKIND\_R4}, and
!   source element typekind {\tt ESMF\_TYPEKIND\_R4}.
!   
!   All values in {\tt dynamicMask} will be reset by this call.
!
!   See section \ref{RH:DynMask} for a general discussion of dynamic masking.
!
!   The arguments are:
!   \begin{description}
!   \item[dynamicMask] 
!     DynamicMask object.
!   \item [dynamicMaskRoutine]
!     The routine responsible for handling dynamically masked source and 
!     destination elements. See section \ref{RH:DynMask} for the precise
!     definition of the {\tt dynamicMaskRoutine} procedure interface.
!     The routine is only called on PETs where {\em at least one} interpolation 
!     element is identified for special handling.
!   \item [{[handleAllElements]}]
!     If set to {\tt .true.}, all local elements, regardless of their dynamic
!     masking status, are made available to {\tt dynamicMaskRoutine} for
!     handling. This option can be used to implement fully customized
!     interpolations based on the information provided by ESMF.
!     The default is {\tt .false.}, meaning that only elements affected by
!     dynamic masking will be handed to {\tt dynamicMaskRoutine}.
!   \item [{[dynamicSrcMaskValue]}]
!     The value for which a source element is considered dynamically
!     masked.
!     The default is to {\em not} consider any source elements as
!     dynamically masked.
!   \item [{[dynamicDstMaskValue]}]
!     The value for which a destination element is considered dynamically
!     masked.
!     The default is to {\em not} consider any destination elements as
!     dynamically masked.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: dimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! mark output as uninitialized    
    ESMF_INIT_SET_DELETED(dynamicMask)

    ! set the internals
    dynamicMask%typeKey           =   "R4R4R4"
    dynamicMask%dmsR4R4R4%typeKey =   dynamicMask%typeKey
    dynamicMask%dmsR4R4R4%routine =>  dynamicMaskRoutine
    dynamicMask%dmsR4R4R4%dynamicSrcMaskIsPresent = present(dynamicSrcMaskValue)
    if (present(dynamicSrcMaskValue)) &
      dynamicMask%dmsR4R4R4%dynamicSrcMaskValue = dynamicSrcMaskValue
    dynamicMask%dmsR4R4R4%dynamicDstMaskIsPresent = present(dynamicDstMaskValue)
    if (present(dynamicDstMaskValue)) &
      dynamicMask%dmsR4R4R4%dynamicDstMaskValue = dynamicDstMaskValue
    if (present(handleAllElements)) then
      dynamicMask%dmsR4R4R4%handleAllElements = handleAllElements
    else
      dynamicMask%dmsR4R4R4%handleAllElements = .false. ! default
    endif

    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(dynamicMask)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DynamicMaskSetR4R4R4
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DynamicMaskSetR4R4R4V()"
!BOP
! !IROUTINE: ESMF_DynamicMaskSetR4R4R4V - Set DynamicMask for R4R4R4 with vectorization
! !INTERFACE:
  subroutine ESMF_DynamicMaskSetR4R4R4V(dynamicMask, dynamicMaskRoutine, &
    keywordEnforcer, handleAllElements, dynamicSrcMaskValue, &
    dynamicDstMaskValue, rc)
!
! !ARGUMENTS:
    type(ESMF_DynamicMask), intent(out)           :: dynamicMask
    procedure(ESMF_DynamicMaskRoutineR4R4R4V)     :: dynamicMaskRoutine
type(ESMF_KeywordEnforcer), optional:: keywordEnforcer ! must use keywords below
    logical,                intent(in),  optional :: handleAllElements
    real(ESMF_KIND_R4),     intent(in),  optional :: dynamicSrcMaskValue
    real(ESMF_KIND_R4),     intent(in),  optional :: dynamicDstMaskValue
    integer,                intent(out), optional :: rc
!         
! !DESCRIPTION:
!   \label{api:DynamicMaskSetR4R4R4V}
!   Set an {\tt ESMF\_DynamicMask} object suitable for 
!   destination element typekind {\tt ESMF\_TYPEKIND\_R4},
!   factor typekind {\tt ESMF\_TYPEKIND\_R4}, and
!   source element typekind {\tt ESMF\_TYPEKIND\_R4}.
!   
!   All values in {\tt dynamicMask} will be reset by this call.
!
!   See section \ref{RH:DynMask} for a general discussion of dynamic masking.
!
!   The arguments are:
!   \begin{description}
!   \item[dynamicMask] 
!     DynamicMask object.
!   \item [dynamicMaskRoutine]
!     The routine responsible for handling dynamically masked source and 
!     destination elements. See section \ref{RH:DynMask} for the precise
!     definition of the {\tt dynamicMaskRoutine} procedure interface.
!     The routine is only called on PETs where {\em at least one} interpolation 
!     element is identified for special handling.
!   \item [{[handleAllElements]}]
!     If set to {\tt .true.}, all local elements, regardless of their dynamic
!     masking status, are made available to {\tt dynamicMaskRoutine} for
!     handling. This option can be used to implement fully customized
!     interpolations based on the information provided by ESMF.
!     The default is {\tt .false.}, meaning that only elements affected by
!     dynamic masking will be handed to {\tt dynamicMaskRoutine}.
!   \item [{[dynamicSrcMaskValue]}]
!     The value for which a source element is considered dynamically
!     masked.
!     The default is to {\em not} consider any source elements as
!     dynamically masked.
!   \item [{[dynamicDstMaskValue]}]
!     The value for which a destination element is considered dynamically
!     masked.
!     The default is to {\em not} consider any destination elements as
!     dynamically masked.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: dimCount

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! mark output as uninitialized    
    ESMF_INIT_SET_DELETED(dynamicMask)

    ! set the internals
    dynamicMask%typeKey           =   "R4R4R4V"
    dynamicMask%dmsR4R4R4V%typeKey =   dynamicMask%typeKey
    dynamicMask%dmsR4R4R4V%routine =>  dynamicMaskRoutine
    dynamicMask%dmsR4R4R4V%dynamicSrcMaskIsPresent = present(dynamicSrcMaskValue)
    if (present(dynamicSrcMaskValue)) &
      dynamicMask%dmsR4R4R4V%dynamicSrcMaskValue = dynamicSrcMaskValue
    dynamicMask%dmsR4R4R4V%dynamicDstMaskIsPresent = present(dynamicDstMaskValue)
    if (present(dynamicDstMaskValue)) &
      dynamicMask%dmsR4R4R4V%dynamicDstMaskValue = dynamicDstMaskValue
    if (present(handleAllElements)) then
      dynamicMask%dmsR4R4R4V%handleAllElements = handleAllElements
    else
      dynamicMask%dmsR4R4R4V%handleAllElements = .false. ! default
    endif
    
    ! mark output as successfully initialized
    ESMF_INIT_SET_DEFINED(dynamicMask)

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DynamicMaskSetR4R4R4V
!------------------------------------------------------------------------------

#endif

! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DynamicMaskGetInit"
!BOPI
! !IROUTINE: ESMF_DynamicMaskGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_DynamicMaskGetInit(dynamicMask) 
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_DynamicMaskGetInit   
!
! !ARGUMENTS:
    type(ESMF_DynamicMask), intent(in), optional :: dynamicMask
!
! !DESCRIPTION:
!   Access init code.
!
!   The arguments are:
!   \begin{description}
!   \item [dynamicMask]
!     DynamicMask object.
!   \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(dynamicMask)) then
      ESMF_DynamicMaskGetInit = ESMF_INIT_GET(dynamicMask)
    else
      ESMF_DynamicMaskGetInit = ESMF_INIT_DEFINED
    endif

  end function ESMF_DynamicMaskGetInit
!------------------------------------------------------------------------------


end module ESMF_DynamicMaskMod
