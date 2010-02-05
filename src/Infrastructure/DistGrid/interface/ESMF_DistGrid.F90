! $Id: ESMF_DistGrid.F90,v 1.61.2.1 2010/02/05 19:55:14 svasquez Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2010, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_DistGrid.F90"
!==============================================================================
!
! ESMF DistGrid Module
module ESMF_DistGridMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the DistGrid class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_DistGridMod
!

!   F90 API wrapper of C++ implementation of DistGrid
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_VMMod            ! ESMF VM
  use ESMF_DELayoutMod      ! ESMF DELayout
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_DistGrid
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_DistGrid
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------

  ! type for decomp flag
  type ESMF_DecompFlag
  private
    integer :: value
  end type

  type(ESMF_DecompFlag), parameter:: &
    ESMF_DECOMP_DEFAULT   = ESMF_DecompFlag(1), &
    ESMF_DECOMP_HOMOGEN   = ESMF_DecompFlag(2), &
    ESMF_DECOMP_RESTFIRST = ESMF_DecompFlag(3), &
    ESMF_DECOMP_RESTLAST  = ESMF_DecompFlag(4), &
    ESMF_DECOMP_CYCLIC    = ESMF_DecompFlag(5)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_DistGrid
  public ESMF_DecompFlag, ESMF_DECOMP_DEFAULT, &
    ESMF_DECOMP_HOMOGEN, ESMF_DECOMP_RESTFIRST, ESMF_DECOMP_RESTLAST, &
    ESMF_DECOMP_CYCLIC
  
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_DistGridCreate
  public ESMF_DistGridDestroy
  
  public ESMF_DistGridGet
  public ESMF_DistGridMatch
  public ESMF_DistGridPrint
  public ESMF_DistGridSet
  public ESMF_DistGridValidate
  
  public ESMF_DistGridConnection

! - ESMF-internal methods:
  public ESMF_DistGridGetInit
  public ESMF_DistGridSetInitCreated
  
  
!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_DistGrid.F90,v 1.61.2.1 2010/02/05 19:55:14 svasquez Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_DistGridCreate -- Generic interface

! !INTERFACE:
  interface ESMF_DistGridCreate

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_DistGridCreateDG
    module procedure ESMF_DistGridCreateDGP
    module procedure ESMF_DistGridCreateRD
    module procedure ESMF_DistGridCreateDB
    module procedure ESMF_DistGridCreateRDFA
    module procedure ESMF_DistGridCreateDBFA      
    module procedure ESMF_DistGridCreateRDP
    module procedure ESMF_DistGridCreateDBP
    module procedure ESMF_DistGridCreateRDPFA
    module procedure ESMF_DistGridCreateDBPFA
    module procedure ESMF_DistGridCreateDBAI1D
    module procedure ESMF_DistGridCreateDBAI
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DistGridCreate} functions.   
!EOPI 
  end interface
!==============================================================================
      

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_DistGridGet -- Generic interface

! !INTERFACE:
  interface ESMF_DistGridGet

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_DistGridGetDefault
!    module procedure ESMF_DistGridGetPDe
    module procedure ESMF_DistGridGetPLocalDe
    module procedure ESMF_DistGridGetPLocalDePDim
      
! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DistGridGet} functions.   
!EOPI 
  end interface
!==============================================================================
      

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDG()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDG(distgrid, firstExtra, lastExtra, indexflag, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),          intent(in)            :: distgrid
    integer, target,              intent(in), optional  :: firstExtra(:)
    integer, target,              intent(in), optional  :: lastExtra(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDG
!
! !DESCRIPTION:
!     Create a new DistGrid from an existing DistGrid, keeping the decomposition
!     unchanged. The {\tt firstExtra} and {\tt lastExtra} arguments allow extra
!     elements to be added at the first/last edge DE in each dimension. If
!     neither {\tt firstExtra}, {\tt lastExtra}, nor {\tt indexflag} are
!     specified the method reduces to a deep copy of the incoming DistGrid
!     object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Incoming DistGrid object.
!     \item[{[firstExtra]}]
!          Extra elements on the edge of the first DEs along each dimension.
!          The default is a zero vector.
!     \item[{[lastExtra]}]
!          Extra elements on the edge of the last DEs along each dimension.
!          The default is a zero vector.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: dg           ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt) :: firstExtraArg ! helper variable
    type(ESMF_InterfaceInt) :: lastExtraArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    firstExtraArg = ESMF_InterfaceIntCreate(firstExtra, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    lastExtraArg = ESMF_InterfaceIntCreate(lastExtra, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    dg%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateDG(dg, distgrid, firstExtraArg, &
      lastExtraArg, indexflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(firstExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(lastExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDG = dg 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDG)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDG
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDGP()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDGP(distgrid, firstExtra, lastExtra, indexflag, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),          intent(in)            :: distgrid
    integer, target,              intent(in)            :: firstExtra(:,:)
    integer, target,              intent(in)            :: lastExtra(:,:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDGP
!
! !DESCRIPTION:
!     Create a new DistGrid from an existing DistGrid, keeping the decomposition
!     unchanged.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Incoming DistGrid object.
!     \item[firstExtra]
!          Extra elements on the edge of the first DEs along each dimension.
!     \item[lastExtra]
!          Extra elements on the edge of the last DEs along each dimension.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: dg           ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt) :: firstExtraArg ! helper variable
    type(ESMF_InterfaceInt) :: lastExtraArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    firstExtraArg = ESMF_InterfaceIntCreate(farray2D=firstExtra, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    lastExtraArg = ESMF_InterfaceIntCreate(farray2D=lastExtra, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    dg%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateDG(dg, distgrid, firstExtraArg, &
      lastExtraArg, indexflag, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(firstExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(lastExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDGP = dg 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDGP)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDGP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRD()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRD(minIndex, maxIndex, regDecomp, &
    decompflag, regDecompFirstExtra, regDecompLastExtra, deLabelList, &
    indexflag, connectionList, delayout, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:)
    integer,                      intent(in)            :: maxIndex(:)
    integer, target,              intent(in), optional  :: regDecomp(:)
    type(ESMF_DecompFlag), target,intent(in), optional  :: decompflag(:)
    integer, target,              intent(in), optional  :: regDecompFirstExtra(:)
    integer, target,              intent(in), optional  :: regDecompLastExtra(:)
    integer, target,              intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer, target,              intent(in), optional  :: connectionList(:,:)
    type(ESMF_DELayout),          intent(in), optional  :: delayout
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRD
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular (LR) 
!     patch with regular decomposition. A regular decomposition is of the same 
!     rank as the patch and decomposes each dimension into a fixed number of 
!     DEs. A regular decomposition of a single patch is expressed by a 
!     single {\tt regDecomp} list of DE counts in each dimension.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the patch.
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the patch.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of the
!          patch is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions. See section
!          \ref{opt:decompflag} for a list of valid decomposition flag options.
!     \item[{[regDecompFirstExtra]}]
!          Extra elements on the first DEs along each dimension in a regular
!          decomposition. The default is a zero vector.
!     \item[{[regDecompLastExtra]}]
!          Extra elements on the last DEs along each dimension in a regular
!          decomposition. The default is a zero vector.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minIndex to patch B's minIndex.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[{[delayout]}]
!          Optional {\tt ESMF\_DELayout} object to be used. By default a new
!          DELayout object will be created with the correct number of DEs. If
!          a DELayout object is specified its number of DEs must match the 
!          number indicated by {\tt regDecomp}.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt) :: minIndexArg  ! helper variable
    type(ESMF_InterfaceInt) :: maxIndexArg  ! helper variable
    type(ESMF_InterfaceInt) :: regDecompArg ! helper variable
    type(ESMF_DecompFlag), target   :: dummyDf(0)  ! satisfy C interface
    type(ESMF_DecompFlag), pointer  ::  opt_decompflag(:) ! optional arg helper
    integer                 :: len_decompflag ! helper variable
    type(ESMF_InterfaceInt) :: regDecompFirstExtraArg ! helper variable
    type(ESMF_InterfaceInt) :: regDecompLastExtraArg ! helper variable
    type(ESMF_InterfaceInt) :: deLabelListArg ! helper variable
    type(ESMF_InterfaceInt) :: connectionListArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexArg = ESMF_InterfaceIntCreate(minIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(maxIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompArg = ESMF_InterfaceIntCreate(regDecomp, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(decompflag)) then
      len_decompflag = size(decompflag)
      opt_decompflag => decompflag
    else
      len_decompflag = 0
      opt_decompflag => dummyDf
    endif
    regDecompFirstExtraArg = ESMF_InterfaceIntCreate(regDecompFirstExtra, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompLastExtraArg = ESMF_InterfaceIntCreate(regDecompLastExtra, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deLabelListArg = ESMF_InterfaceIntCreate(deLabelList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRD(distgrid, minIndexArg, maxIndexArg, &
      regDecompArg, opt_decompflag, len_decompflag, regDecompFirstExtraArg, &
      regDecompLastExtraArg, deLabelListArg, indexflag, &
      connectionListArg, delayout, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompFirstExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompLastExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deLabelListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRD = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRD)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateRD
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDB()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with DE blocks

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDB(minIndex, maxIndex, deBlockList, &
    deLabelList, indexflag, connectionList, delayout, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:)
    integer,                      intent(in)            :: maxIndex(:)
    integer,                      intent(in)            :: deBlockList(:,:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    type(ESMF_DELayout),          intent(in), optional  :: delayout
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDB
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular (LR) 
!     patch with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the patch.
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the patch.
!     \item[deBlockList]
!          List of DE-local LR blocks. The third index of {\tt deBlockList}
!          steps through the deBlock elements, which are defined by the first
!          two indices. The first index must be of size {\tt dimCount} and the 
!          second index must be of size 2. Each 2D element of {\tt deBlockList}
!          defined by the first two indices hold the following information.
!          \begin{verbatim}
!                   +---------------------------------------> 2nd index
!                   |    1               2           
!                   | 1  minIndex(1)    maxIndex(1)
!                   | 2  minIndex(2)    maxIndex(2)
!                   | .  minIndex(.)    maxIndex(.)
!                   | .
!                   v
!                  1st index
!          \end{verbatim}
!          It is required that there be no overlap between the LR segments
!          defined by deBlockList.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minIndex to patch B's minIndex.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[{[delayout]}]
!          Optional {\tt ESMF\_DELayout} object to be used. By default a new
!          DELayout object will be created with the correct number of DEs. If
!          a DELayout object is specified its number of DEs must match the 
!          number indicated by {\tt regDecomp}.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt) :: minIndexArg  ! helper variable
    type(ESMF_InterfaceInt) :: maxIndexArg  ! helper variable
    type(ESMF_InterfaceInt) :: deBlockListArg ! helper variable
    type(ESMF_InterfaceInt) :: deLabelListArg ! helper variable
    type(ESMF_InterfaceInt) :: connectionListArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexArg = ESMF_InterfaceIntCreate(minIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(maxIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deBlockListArg = ESMF_InterfaceIntCreate(farray3D=deBlockList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deLabelListArg = ESMF_InterfaceIntCreate(deLabelList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateDB(distgrid, minIndexArg, maxIndexArg, &
      deBlockListArg, deLabelListArg, indexflag, &
      connectionListArg, delayout, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deBlockListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deLabelListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDB = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDB)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDB
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRDFA()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with regular decomposition and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRDFA(minIndex, maxIndex, regDecomp, &
    decompflag, regDecompFirstExtra, regDecompLastExtra, deLabelList, &
    indexflag, connectionList, fastAxis, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:)
    integer,                      intent(in)            :: maxIndex(:)
    integer,                      intent(in), optional  :: regDecomp(:)
    type(ESMF_DecompFlag),target, intent(in), optional  :: decompflag(:)
    integer, target,              intent(in), optional  :: regDecompFirstExtra(:)
    integer, target,              intent(in), optional  :: regDecompLastExtra(:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in)            :: fastAxis
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDFA
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular (LR) 
!     patch with regular decomposition. A regular
!     decomposition is of the same rank as the patch and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     single patch is expressed by a single {\tt regDecomp} list of DE counts
!     in each dimension.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the patch.
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the patch.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of the
!          patch is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions. See section
!          \ref{opt:decompflag} for a list of valid decomposition flag options.
!     \item[{[regDecompFirstExtra]}]
!          Extra elements on the first DEs along each dimension in a regular
!          decomposition. The default is a zero vector.
!     \item[{[regDecompLastExtra]}]
!          Extra elements on the last DEs along each dimension in a regular
!          decomposition. The default is a zero vector.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minIndex to patch B's minIndex.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[fastAxis]
!          Integer value indicating along which axis fast communication is
!          requested. This hint will be used during DELayout creation.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt) :: minIndexArg  ! helper variable
    type(ESMF_InterfaceInt) :: maxIndexArg  ! helper variable
    type(ESMF_InterfaceInt) :: regDecompArg ! helper variable
    type(ESMF_DecompFlag), target   :: dummyDf(0)  ! satisfy C interface
    type(ESMF_DecompFlag), pointer  ::  opt_decompflag(:) ! optional arg helper
    integer                 :: len_decompflag ! helper variable
    type(ESMF_InterfaceInt) :: regDecompFirstExtraArg ! helper variable
    type(ESMF_InterfaceInt) :: regDecompLastExtraArg ! helper variable
    type(ESMF_InterfaceInt) :: deLabelListArg ! helper variable
    type(ESMF_InterfaceInt) :: connectionListArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexArg = ESMF_InterfaceIntCreate(minIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(maxIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompArg = ESMF_InterfaceIntCreate(regDecomp, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(decompflag)) then
      len_decompflag = size(decompflag)
      opt_decompflag => decompflag
    else
      len_decompflag = 0
      opt_decompflag => dummyDf
    endif
    regDecompFirstExtraArg = ESMF_InterfaceIntCreate(regDecompFirstExtra, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompLastExtraArg = ESMF_InterfaceIntCreate(regDecompLastExtra, &
      rc=localrc)
    deLabelListArg = ESMF_InterfaceIntCreate(deLabelList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRDFA(distgrid, minIndexArg, maxIndexArg, &
      regDecompArg, opt_decompflag, len_decompflag, regDecompFirstExtraArg, &
      regDecompLastExtraArg, deLabelListArg, indexflag, &
      connectionListArg, fastAxis, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompFirstExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompLastExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deLabelListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDFA = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDFA)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateRDFA
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBFA()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with DE blocks and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBFA(minIndex, maxIndex, &
    deBlockList, deLabelList, indexflag, connectionList, &
    fastAxis, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:)
    integer,                      intent(in)            :: maxIndex(:)
    integer,                      intent(in)            :: deBlockList(:,:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in)            :: fastAxis
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBFA
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular (LR) 
!     patch with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the patch.
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the patch.
!     \item[deBlockList]
!          List of DE-local LR blocks. The third index of {\tt deBlockList}
!          steps through the deBlock elements, which are defined by the first
!          two indices. The first index must be of size {\tt dimCount} and the 
!          second index must be of size 3. Each 2D element of {\tt deBlockList}
!          defined by the first two indices hold the following information.
!          \begin{verbatim}
!                   +---------------------------------------> 2nd index
!                   |    1               2              3
!                   | 1  minIndex(1)    maxIndex(1)   patchID
!                   | 2  minIndex(2)    maxIndex(2)   (not used)
!                   | .  minIndex(.)    maxIndex(.)   (not used)
!                   | .
!                   v
!                  1st index
!          \end{verbatim}
!          It is required that there be no overlap between the LR segments
!          defined by deBlockList.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          argument.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minIndex to patch B's minIndex.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[fastAxis]
!          Integer value indicating along which axis fast communication is
!          requested. This hint will be used during DELayout creation.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(minIndex) == size(minIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(maxIndex) == size(maxIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(deBlockList) == size(deBlockList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(deLabelList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(indexflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(connectionList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (fastaxis == fastaxis) continue
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DistGridCreateRDFA(distgrid, minIndexArg, maxIndexArg, &
!      regDecompArg, opt_decompflag, len_decompflag, deLabelListArg, indexflag, &
!      connectionListArg, fastAxis, vm, localrc)
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_DistGridCreateDBFA = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBFA)
 
    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented
 
  end function ESMF_DistGridCreateDBFA
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRDP()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from patchwork with regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRDP(minIndex, maxIndex, regDecomp,&
    decompflag, regDecompFirstExtra, regDecompLastExtra, deLabelList, &
    indexflag, connectionList, delayout, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:,:)
    integer,                      intent(in)            :: maxIndex(:,:)
    integer,                      intent(in), optional  :: regDecomp(:,:)
    type(ESMF_DecompFlag),target, intent(in), optional  :: decompflag(:,:)
    integer, target,              intent(in), optional  :: regDecompFirstExtra(:,:)
    integer, target,              intent(in), optional  :: regDecompLastExtra(:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    type(ESMF_DELayout),          intent(in), optional  :: delayout
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDP
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a patchwork of logically 
!     rectangular (LR) patches with regular decomposition. A regular
!     decomposition is of the same rank as the patch and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     patchwork of patches is expressed by a list of DE count vectors, one
!     vector for each patch. Each vector contained in the 
!     {\tt regDecomp} argument ascribes DE counts for each dimension. It is 
!     erroneous to provide more patches than there are DEs.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          The first index provides the global coordinate tuple of the lower 
!          corner of a patch. The second index indicates the patch number.
!     \item[maxIndex]
!          The first index provides the global coordinate tuple of the upper
!          corner of a patch. The second index indicates the patch number.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The second 
!          index indicates the patch number. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of each
!          patch is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions for all patches. 
!          See section \ref{opt:decompflag} for a list of valid decomposition
!          flag options. The second index indicates the patch number.
!     \item[{[regDecompFirstExtra]}]
!          Extra elements on the first DEs along each dimension in a regular
!          decomposition. The default is a zero vector. The second index 
!          indicates the patch number.
!     \item[{[regDecompLastExtra]}]
!          Extra elements on the last DEs along each dimension in a regular
!          decomposition. The default is a zero vector. The second index 
!          indicates the patch number.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          elements in the sequence as they appear following the patch index.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minIndex to patch B's minIndex.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[{[delayout]}]
!          Optional {\tt ESMF\_DELayout} object to be used. By default a new
!          DELayout object will be created with the correct number of DEs. If
!          a DELayout object is specified its number of DEs must match the 
!          number indicated by {\tt regDecomp}.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt) :: minIndexArg  ! helper variable
    type(ESMF_InterfaceInt) :: maxIndexArg  ! helper variable
    type(ESMF_InterfaceInt) :: regDecompArg ! helper variable
    type(ESMF_DecompFlag), target :: dummyDf(0,0)  ! satisfy C interface
    type(ESMF_DecompFlag), pointer::  opt_decompflag(:,:) ! optional arg helper
    integer                 :: len1_decompflag ! helper variable
    integer                 :: len2_decompflag ! helper variable
    type(ESMF_InterfaceInt) :: regDecompFirstExtraArg ! helper variable
    type(ESMF_InterfaceInt) :: regDecompLastExtraArg ! helper variable
    type(ESMF_InterfaceInt) :: deLabelListArg ! helper variable
    type(ESMF_InterfaceInt) :: connectionListArg ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexArg = ESMF_InterfaceIntCreate(farray2D=minIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(farray2D=maxIndex, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompArg = ESMF_InterfaceIntCreate(farray2D=regDecomp, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(decompflag)) then
      len1_decompflag = size(decompflag, 1)
      len2_decompflag = size(decompflag, 2)
      opt_decompflag => decompflag
    else
      len1_decompflag = 0
      len2_decompflag = 0
      opt_decompflag => dummyDf
    endif
    regDecompFirstExtraArg = &
      ESMF_InterfaceIntCreate(farray2D=regDecompFirstExtra, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompLastExtraArg = &
      ESMF_InterfaceIntCreate(farray2D=regDecompLastExtra, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deLabelListArg = ESMF_InterfaceIntCreate(deLabelList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRDP(distgrid, minIndexArg, maxIndexArg, &
      regDecompArg, opt_decompflag, len1_decompflag, len2_decompflag, &
      regDecompFirstExtraArg, regDecompLastExtraArg, deLabelListArg, &
      indexflag, connectionListArg, delayout, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompFirstExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompLastExtraArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deLabelListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDP = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDP)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateRDP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBP()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from patchwork with regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBP(minIndex, maxIndex, &
    deBlockList, deLabelList, indexflag, connectionList, &
    delayout, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:,:)
    integer,                      intent(in)            :: maxIndex(:,:)
    integer,                      intent(in)            :: deBlockList(:,:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    type(ESMF_DELayout),          intent(in), optional  :: delayout
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBP
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a patchwork of logically 
!     rectangular (LR) patches with regular decomposition. A regular
!     decomposition is of the same rank as the patch and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     patchwork of patches is expressed by a list of DE count vectors, one
!     vector for each patch. Each vector contained in the 
!     {\tt regDecomp} argument ascribes DE counts for each dimension. It is 
!     erroneous to provide more patches than there are DEs.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          The first index provides the global coordinate tuple of the lower 
!          corner of a patch. The second index indicates the patch number.
!     \item[maxIndex]
!          The first index provides the global coordinate tuple of the upper
!          corner of a patch. The second index indicates the patch number.
!     \item[deBlockList]
!          List of DE-local LR blocks. The third index of {\tt deBlockList}
!          steps through the deBlock elements, which are defined by the first
!          two indices. The first index must be of size {\tt dimCount} and the 
!          second index must be of size 3. Each 2D element of {\tt deBlockList}
!          defined by the first two indices hold the following information.
!          \begin{verbatim}
!                   +---------------------------------------> 2nd index
!                   |    1               2              3
!                   | 1  minIndex(1)    maxIndex(1)   patchID
!                   | 2  minIndex(2)    maxIndex(2)   (not used)
!                   | .  minIndex(.)    maxIndex(.)   (not used)
!                   | .
!                   v
!                  1st index
!          \end{verbatim}
!          It is required that there be no overlap between the LR segments
!          defined by deBlockList.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          elements in the sequence as they appear following the patch index.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minIndex to patch B's minIndex.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[{[delayout]}]
!          Optional {\tt ESMF\_DELayout} object to be used. By default a new
!          DELayout object will be created with the correct number of DEs. If
!          a DELayout object is specified its number of DEs must match the 
!          number indicated by {\tt regDecomp}.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(minIndex) == size(minIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(maxIndex) == size(maxIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(deBlockList) == size(deBlockList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(deLabelList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(indexflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(connectionList)) continue

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DistGridCreateRDFA(distgrid, minIndexArg, maxIndexArg, &
!      regDecompArg, opt_decompflag, len_decompflag, deLabelListArg, indexflag, &
!      connectionListArg, fastAxis, vm, localrc)
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Set return value
    ESMF_DistGridCreateDBP = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBP)
 
    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented
 
  end function ESMF_DistGridCreateDBP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRDPFA()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from patchwork with regular decomposition and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRDPFA(minIndex, maxIndex, &
    regDecomp, decompflag, deLabelList, indexflag, connectionList, &
    fastAxis, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:,:)
    integer,                      intent(in)            :: maxIndex(:,:)
    integer,                      intent(in), optional  :: regDecomp(:,:)
    type(ESMF_DecompFlag),target, intent(in), optional  :: decompflag(:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in)            :: fastAxis
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDPFA
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a patchwork of logically 
!     rectangular (LR) patches with regular decomposition. A regular
!     decomposition is of the same rank as the patch and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     patchwork of patches is expressed by a list of DE count vectors, one
!     vector for each patch. Each vector contained in the 
!     {\tt regDecomp} argument ascribes DE counts for each dimension. It is 
!     erroneous to provide more patches than there are DEs.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          The first index provides the global coordinate tuple of the lower 
!          corner of a patch. The second index indicates the patch number.
!     \item[maxIndex]
!          The first index provides the global coordinate tuple of the upper
!          corner of a patch. The second index indicates the patch number.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The second 
!          index indicates the patch number. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of each
!          patch is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions for all patches. 
!          See section \ref{opt:decompflag} for a list of valid decomposition
!          flag options. The second index indicates the patch number.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          elements in the sequence as they appear following the patch index.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minIndex to patch B's minIndex.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[fastAxis]
!          Integer value indicating along which axis fast communication is
!          requested. This hint will be used during DELayout creation.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(minIndex) == size(minIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(maxIndex) == size(maxIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(regDecomp)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(decompflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(deLabelList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(indexflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(connectionList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (fastaxis == fastaxis) continue
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DistGridCreateRDFA(distgrid, minIndexArg, maxIndexArg, &
!      regDecompArg, opt_decompflag, len_decompflag, deLabelListArg, indexflag, &
!      connectionListArg, fastAxis, vm, localrc)
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDPFA = distgrid
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDPFA)
 
    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented
 
  end function ESMF_DistGridCreateRDPFA
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBPFA()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from patchwork with DE blocks and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBPFA(minIndex, maxIndex, &
    deBlockList, deLabelList, indexflag, connectionList, &
    fastAxis, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:,:)
    integer,                      intent(in)            :: maxIndex(:,:)
    integer,                      intent(in)            :: deBlockList(:,:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in)            :: fastAxis
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBPFA
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a patchwork of logically 
!     rectangular (LR) patches with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          The first index provides the global coordinate tuple of the lower 
!          corner of a patch. The second index indicates the patch number.
!     \item[maxIndex]
!          The first index provides the global coordinate tuple of the upper
!          corner of a patch. The second index indicates the patch number.
!     \item[deBlockList]
!          List of DE-local LR blocks. The third index of {\tt deBlockList}
!          steps through the deBlock elements, which are defined by the first
!          two indices. The first index must be of size {\tt dimCount} and the 
!          second index must be of size 3. Each 2D element of {\tt deBlockList}
!          defined by the first two indices hold the following information.
!          \begin{verbatim}
!                   +---------------------------------------> 2nd index
!                   |    1               2              3
!                   | 1  minIndex(1)    maxIndex(1)   patchID
!                   | 2  minIndex(2)    maxIndex(2)   (not used)
!                   | .  minIndex(.)    maxIndex(.)   (not used)
!                   | .
!                   v
!                  1st index
!          \end{verbatim}
!          It is required that there be no overlap between the LR segments
!          defined by deBlockList.
!     \item[{[deLabelList]}]
!          List assigning DE labels to the default sequence of DEs. The default
!          sequence is given by the column major order of the {\tt regDecomp}
!          elements in the sequence as they appear following the patch index.
!     \item[{[indexflag]}]
!          Indicates whether the indices provided by the {\tt minIndex} and
!          {\tt maxIndex} arguments are to be interpreted to form a flat
!          pseudo global index space ({\tt ESMF\_INDEX\_GLOBAL}) or are to be 
!          taken as patch local ({\tt ESMF\_INDEX\_DELOCAL}), which is the default.
!     \item[{[connectionList]}]
!          List of connections between patches in index space. The second dimension
!          of {\tt connectionList} steps through the connection interface elements, 
!          defined by the first index. The first index must be of size
!          {\tt 2 x dimCount + 2}, where {\tt dimCount} is the rank of the 
!          decomposed index space. Each {\tt connectionList} element specifies
!          the connection interface in the format
!
!         {\tt (/patchIndex\_A,
!          patchIndex\_B, positionVector, orientationVector/)} where:
!          \begin{itemize}
!          \item {\tt patchIndex\_A} and {\tt patchIndex\_B} are the patch
!                index of the two connected patches respectively,
!          \item {\tt positionVector} is the vector that points from patch A's
!                minIndex to patch B's minIndex.
!          \item {\tt orientationVector} associates each dimension of patch A
!                with a dimension in patch B's index space. Negative index
!                values may be used to indicate a reversal in index orientation.
!          \end{itemize}
!     \item[fastAxis]
!          Integer value indicating along which axis fast communication is
!          requested. This hint will be used during DELayout creation.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object of the current context. Providing the
!          VM of the current context will lower the method's overhead.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(minIndex) == size(minIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(maxIndex) == size(maxIndex)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (size(deBlockList) == size(deBlockList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(deLabelList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(indexflag)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (present(connectionList)) continue

    !DUMMY TEST TO QUIET DOWN COMPILER WARNINGS
    !TODO: Remove the following dummy test when dummy argument actually used
    if (fastaxis == fastaxis) continue
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DistGridCreateRDFA(distgrid, minIndexArg, maxIndexArg, &
!      regDecompArg, opt_decompflag, len_decompflag, deLabelListArg, indexflag, &
!      connectionListArg, fastAxis, vm, localrc)
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDBPFA = distgrid
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBPFA)
 
    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented
 
  end function ESMF_DistGridCreateDBPFA
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBAI1D()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create 1D DistGrid object from user's arbitray index list

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBAI1D(arbSeqIndexList, rc)
!
! !ARGUMENTS:
    integer,    intent(in)            :: arbSeqIndexList(:)
    integer,    intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBAI1D
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} of {\tt dimCount} 1 from a PET-local list
!     of sequence indices. The PET-local size of the {\tt arbSeqIndexList}
!     argument determines the number of local elements in the created DistGrid.
!     The sequence indices must be unique across all PETs. A default
!     DELayout with 1 DE per PET across all PETs of the current VM is 
!     automatically created.
!
!     The arguments are:
!     \begin{description}
!     \item[arbSeqIndexList]
!          List of arbitrary sequence indices that reside on the local PET.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_VM)           :: vm           ! opaque pointer to VM object
    type(ESMF_InterfaceInt) :: indicesArg   ! index helper
    integer                 :: localSize(1) ! number of local indices
    integer, allocatable    :: globalSizes(:)  ! array of all sizes
    integer                 :: petCount        ! num pets
    integer, allocatable    :: deblock(:,:,:)  ! Array of sizes
    integer                 :: i, csum         ! loop variable
    integer                 :: minC(1), maxC(1)! min/max corner

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! number of local indices
    localSize(1) = size(arbSeqIndexList)

    ! gather all sizes locally
    allocate(globalSizes(petCount))
    call ESMF_VMAllGather(vm, localSize, globalSizes, 1, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up the deblocks
    allocate(deblock(1,2,petCount))
    csum = 0
    do i=1,petCount
      deblock(1,1,i) = csum + 1 ! min
      csum = csum + globalSizes(i)
      deblock(1,2,i) = csum     ! max
    enddo

    ! create fitting DistGrid
    minC(1) = deblock(1,1,1)
    maxC(1) = deblock(1,2,petCount)
    distgrid = ESMF_DistGridCreate(minC, maxC, deBlockList=deblock, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    deallocate(deblock)
    deallocate(globalSizes)
    
    ! set return value
    ESMF_DistGridCreateDBAI1D = distgrid 

    ! prepare to set local arbitrary sequence indices
    indicesArg = ESMF_InterfaceIntCreate(farray1D=arbSeqIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set local arbitrary sequence indices in DistGrid object
    ! localDe=0, collocation=1
    call c_ESMC_DistGridSetArbSeqIndex(distgrid, indicesArg, 0, 1, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    call ESMF_InterfaceIntDestroy(indicesArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBAI1D)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBAI1D
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBAI()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create (1+n)D DistGrid object from user's arbitray index list and minIndex/maxIndex

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBAI(arbSeqIndexList, arbDim, &
    minIndex, maxIndex, rc)
!
! !ARGUMENTS:
    integer,    intent(in)            :: arbSeqIndexList(:)
    integer,    intent(in)            :: arbDim
    integer,    intent(in)            :: minIndex(:)
    integer,    intent(in)            :: maxIndex(:)
    integer,    intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBAI
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} of {\tt dimCount} $1+n$, where 
!     $n=$ {\tt size(minIndex)} = {\tt size(maxIndex)}.
!
!     The resulting DistGrid will have a 1D distribution determined by the
!     PET-local {\tt arbSeqIndexList}. The PET-local size of the
!     {\tt arbSeqIndexList} argument determines the number of local elements 
!     along the arbitrarily distributed dimension in the created DistGrid. The
!     sequence indices must be unique across all PETs. The associated,
!     automatically created DELayout will have 1 DE per PET across all PETs of
!     the current VM.
!
!     In addition to the arbitrarily distributed dimension, regular DistGrid
!     dimensions can be specified in {\tt minIndex} and {\tt maxIndex}. The
!     $n$ dimensional subspace spanned by the regular dimensions is "multiplied"
!     with the arbitrary dimension on each DE, to form a $1+n$ dimensional
!     total index space described by the DistGrid object. The {\tt arbDim}
!     argument allows to specify which dimension in the resulting DistGrid
!     corresponds to the arbitrarily distributed one.
!
!     The arguments are:
!     \begin{description}
!     \item[arbSeqIndexList]
!          List of arbitrary sequence indices that reside on the local PET.
!     \item[arbDim]
!          Dimension of the arbitrary distribution.
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the tile. The 
!          arbitrary dimension is {\em not} included in this tile
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the tile. The
!          arbitrary dimension is {\em not} included in this tile
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DistGrid)     :: distgrid     ! opaque pointer to new C++ DistGrid
    type(ESMF_VM)           :: vm           ! opaque pointer to VM object
    type(ESMF_InterfaceInt) :: indicesArg   ! index helper
    integer                 :: localSize(1) ! number of local indices
    integer, allocatable    :: globalSizes(:)  ! array of all sizes
    integer                 :: petCount        ! num pets
    integer, allocatable    :: deblock(:,:,:)  ! Array of sizes
    integer                 :: i, j, jj, csum  ! loop variable
    integer, allocatable    :: minC(:), maxC(:)! min/max corner
    integer, allocatable    :: collocationPDim(:)
    integer                 :: dimCount     ! number of dimension

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! check input
    dimCount = size(minIndex)
    if (dimCount /= size(maxIndex)) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_SIZE, &
          "- size(minIndex) must match size(maxIndex)", &
          ESMF_CONTEXT, rc)
      return
    endif
    if (arbDim < 1 .or. arbDim > dimCount+1) then
      call ESMF_LogMsgSetError(ESMF_RC_ARG_VALUE, &
        "- arbDim out of range", &
        ESMF_CONTEXT, rc)
      return
    endif
    
    ! get VM and related information
    call ESMF_VMGetCurrent(vm, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    call ESMF_VMGet(vm, petCount=petCount, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! number of local indices
    localSize(1) = size(arbSeqIndexList)

    ! gather all sizes locally
    allocate(globalSizes(petCount))
    call ESMF_VMAllGather(vm, localSize, globalSizes, 1, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set up the deblocks
    allocate(deblock(dimCount+1,2,petCount))
    csum = 0
    do i=1,petCount
      deblock(arbDim,1,i) = csum + 1 ! min
      csum = csum + globalSizes(i)
      deblock(arbDim,2,i) = csum     ! max
      do j=1,dimCount+1
        if (j==arbDim) cycle
        jj=j
        if (j>arbDim) jj=jj-1
        deblock(j,1,i) = minIndex(jj) ! min
        deblock(j,2,i) = maxIndex(jj) ! max
      enddo
    enddo

    ! create fitting DistGrid
    allocate(minC(dimCount+1))
    allocate(maxC(dimCount+1))
    do i=1, dimCount+1
      minC(i) = deblock(i,1,1)
      maxC(i) = deblock(i,2,petCount)
    enddo
    distgrid = ESMF_DistGridCreate(minC, maxC, deBlockList=deblock, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    deallocate(deblock)
    deallocate(globalSizes)
    deallocate(minC)
    deallocate(maxC)
    
    ! set return value
    ESMF_DistGridCreateDBAI = distgrid 

    ! set collocations to separate arbDim from the reset
    allocate(collocationPDim(dimCount+1))
    collocationPDim = 2 ! initialize
    collocationPDim(arbDim) = 1 ! arbDim singled out as collocation "1"
    call ESMF_DistGridSet(distgrid, collocationPDim=collocationPDim, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deallocate(collocationPDim)

    ! prepare to set local arbitrary sequence indices
    indicesArg = ESMF_InterfaceIntCreate(farray1D=arbSeqIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! set local arbitrary sequence indices in DistGrid object
    ! localDe=0, collocation=1, i.e. arbDim's collocation
    call c_ESMC_DistGridSetArbSeqIndex(distgrid, indicesArg, 0, 1, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    call ESMF_InterfaceIntDestroy(indicesArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBAI)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBAI
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridDestroy()"
!BOP
! !IROUTINE: ESMF_DistGridDestroy - Destroy DistGrid object

! !INTERFACE:
  subroutine ESMF_DistGridDestroy(distgrid, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid), intent(inout)           :: distgrid
    integer,             intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!   Destroy an {\tt ESMF\_DistGrid} object.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid] 
!     {\tt ESMF\_DistGrid} object to be destroyed.
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

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridDestroy(distgrid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(distgrid)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetDefault()"
!BOP
! !IROUTINE: ESMF_DistGridGet - Get information about DistGrid object

! !INTERFACE:
  ! Private name; call using ESMF_DistGridGet()
  subroutine ESMF_DistGridGetDefault(distgrid, delayout, dimCount, patchCount, &
    minIndexPDimPPatch, maxIndexPDimPPatch, elementCountPPatch, &
    minIndexPDimPDe, maxIndexPDimPDe, elementCountPDe, patchListPDe, &
    indexCountPDimPDe, collocationPDim, regDecompFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    type(ESMF_DELayout),    intent(out), optional :: delayout
    integer,                intent(out), optional :: dimCount
    integer,                intent(out), optional :: patchCount
    integer,        target, intent(out), optional :: minIndexPDimPPatch(:,:)
    integer,        target, intent(out), optional :: maxIndexPDimPPatch(:,:)
    integer,        target, intent(out), optional :: elementCountPPatch(:)
    integer,        target, intent(out), optional :: minIndexPDimPDe(:,:)
    integer,        target, intent(out), optional :: maxIndexPDimPDe(:,:)
    integer,        target, intent(out), optional :: elementCountPDe(:)
    integer,        target, intent(out), optional :: patchListPDe(:)
    integer,        target, intent(out), optional :: indexCountPDimPDe(:,:)
    integer,        target, intent(out), optional :: collocationPDim(:)
    logical,                intent(out), optional :: regDecompFlag
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!   Get internal DistGrid information.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid] 
!     Queried {\tt ESMF\_DistGrid} object.
!   \item[{[delayout]}]
!     {\tt ESMF\_DELayout} object associated with {\tt distgrid}.
!   \item[{[dimCount]}]
!     Number of dimensions (rank) of {\tt distgrid}.
!   \item[{[patchCount]}]
!     Number of patches in {\tt distgrid}.
!   \item[{[minIndexPDimPPatch]}]
!     Lower index space corner per {\tt dim}, per {\tt patch}, with
!     {\tt size(minIndexPDimPPatch) == (/dimCount, patchCount/)}.
!   \item[{[maxIndexPDimPPatch]}]
!     Upper index space corner per {\tt dim}, per {\tt patch}, with
!     {\tt size(minIndexPDimPPatch) == (/dimCount, patchCount/)}.
!   \item[{[elementCountPPatch]}]
!     Number of elements in exclusive region per patch, with
!     {\tt size(elementCountPPatch) == (/patchCount/)}
!   \item[{[minIndexPDimPDe]}]
!     Lower index space corner per {\tt dim}, per {\tt De}, with
!     {\tt size(minIndexPDimPDe) == (/dimCount, deCount/)}.
!   \item[{[maxIndexPDimPDe]}]
!     Upper index space corner per {\tt dim}, per {\tt de}, with
!     {\tt size(minIndexPDimPDe) == (/dimCount, deCount/)}.
!   \item[{[elementCountPDe]}]
!     Number of elements in exclusive region per DE, with
!     {\tt size(elementCountPDe) == (/deCount/)}
!   \item[{[patchListPDe]}]
!     List of patch id numbers, one for each DE, with
!     {\tt size(patchListPDe) == (/deCount/)}
!   \item[{[indexCountPDimPDe]}]
!     Array of extents per {\tt dim}, per {\tt de}, with
!     {\tt size(indexCountPDimPDe) == (/dimCount, deCount/)}.
!   \item[{[collocationPDim]}]
!     List of collocation id numbers, one for each dim, with
!     {\tt size(collocationPDim) == (/dimCount/)}
!   \item[{[regDecompFlag]}]
!     Flag equal to {\tt ESMF\_TRUE} for regular decompositions
!     and equal to {\tt ESMF\_FALSE} otherwise.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc                ! local return code
    type(ESMF_InterfaceInt) :: minIndexPDimPPatchArg  ! helper variable
    type(ESMF_InterfaceInt) :: maxIndexPDimPPatchArg  ! helper variable
    type(ESMF_InterfaceInt) :: elementCountPPatchArg  ! helper variable
    type(ESMF_InterfaceInt) :: minIndexPDimPDeArg     ! helper variable
    type(ESMF_InterfaceInt) :: maxIndexPDimPDeArg     ! helper variable
    type(ESMF_InterfaceInt) :: elementCountPDeArg     ! helper variable
    type(ESMF_InterfaceInt) :: patchListPDeArg        ! helper variable
    type(ESMF_InterfaceInt) :: indexCountPDimPDeArg   ! helper variable
    type(ESMF_InterfaceInt) :: collocationPDimArg     ! helper variable
    type(ESMF_Logical)      :: regDecompFlagArg       ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    minIndexPDimPPatchArg = &
      ESMF_InterfaceIntCreate(farray2D=minIndexPDimPPatch, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexPDimPPatchArg = &
      ESMF_InterfaceIntCreate(farray2D=maxIndexPDimPPatch, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    elementCountPPatchArg = ESMF_InterfaceIntCreate(elementCountPPatch, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    minIndexPDimPDeArg = &
      ESMF_InterfaceIntCreate(farray2D=minIndexPDimPDe, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexPDimPDeArg = &
      ESMF_InterfaceIntCreate(farray2D=maxIndexPDimPDe, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    elementCountPDeArg = ESMF_InterfaceIntCreate(elementCountPDe, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    patchListPDeArg = ESMF_InterfaceIntCreate(patchListPDe, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    indexCountPDimPDeArg = ESMF_InterfaceIntCreate(farray2D=indexCountPDimPDe, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    collocationPDimArg = ESMF_InterfaceIntCreate(collocationPDim, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridGet(distgrid, dimCount, patchCount, &
      minIndexPDimPPatchArg, maxIndexPDimPPatchArg, elementCountPPatchArg, &
      minIndexPDimPDeArg, maxIndexPDimPDeArg, elementCountPDeArg, &
      patchListPDeArg, indexCountPDimPDeArg, collocationPDimArg, &
      regDecompFlagArg, delayout, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    if (present (regDecompFlag)) &
      regDecompFlag = regDecompFlagArg
    
    ! Set init code for deep C++ objects
    if (present(delayout)) then
      call ESMF_DELayoutSetInitCreated(delayout, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexPDimPPatchArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexPDimPPatchArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(elementCountPPatchArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(minIndexPDimPDeArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexPDimPDeArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(elementCountPDeArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(patchListPDeArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(indexCountPDimPDeArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(collocationPDimArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridGetDefault
!------------------------------------------------------------------------------

#ifdef PROTOCODE
! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetPDe()"
!BOPI
! !IROUTINE: ESMF_DistGridGet - Get DE local information about DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_DistGridGet()
  subroutine ESMF_DistGridGetPDe(distgrid, de, regDecompDeCoord, patch, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: de
    integer, target,        intent(out), optional :: regDecompDeCoord(:)
    integer,                intent(out), optional :: patch
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!   Get internal DistGrid information.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid]
!     Queried {\tt ESMF\_DistGrid} object.
!   \item[de]
!     DE for which information is requested. {\tt \[0,..,deCount-1\]}
!   \item[{[regDecompDeCoord]}]
!     For regular decompositions upon return this array holds the coordinate
!     tuple of the specified DE with respect to the local patch. For other
!     decompositions a run-time warning will be issued if
!     {\tt regDecompDeCoord} is requested.
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

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! call into the C++ interface, which will sort out optional arguments
!    call c_ESMC_DistGridGet(distgrid, delayout, patchCount, patchListArg, &
!      dimCount, dimExtentArg, regDecompFlag, localrc)
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    !if (present(rc)) rc = ESMF_SUCCESS   TODO: enable once implemented

  end subroutine ESMF_DistGridGetPDe
!------------------------------------------------------------------------------
#endif

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetPLocalDe()"
!BOP
! !IROUTINE: ESMF_DistGridGet - Get DE local information about DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_DistGridGet()
  subroutine ESMF_DistGridGetPLocalDe(distgrid, localDe, collocation, &
    arbSeqIndexFlag, seqIndexList, elementCount, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: localDe
    integer,                intent(in),  optional :: collocation
    logical,                intent(out), optional :: arbSeqIndexFlag
    integer,        target, intent(out), optional :: seqIndexList(:)
    integer,                intent(out), optional :: elementCount
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!   Get internal DistGrid information.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid]
!     Queried {\tt ESMF\_DistGrid} object.
!   \item[localDe]
!     Local DE for which information is requested. {\tt [0,..,localDeCount-1]}
!   \item[{[collocation]}]
!     Collocation for which information is requested. Default to first
!     collocation in {\tt collocationPDim} list.
!   \item[{[arbSeqIndexFlag]}]
!     Indicates whether collocation is associated with arbitrary sequence
!     indices.
!   \item[{[seqIndexList]}]
!     List of DistGrid patch-local sequence indices for {\tt localDe}, with
!     {\tt size(seqIndexList) == (/elementCountPDe(localDe)/)}.
!   \item[{[elementCount]}]
!     Number of elements in the localDe, i.e. identical to
!     elementCountPDe(localDe).
!   \item[{[rc]}]
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc            ! local return code
    type(ESMF_Logical)      :: arbSeqIndexFlagArg ! helper variable
    type(ESMF_InterfaceInt) :: seqIndexListArg    ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    seqIndexListArg = ESMF_InterfaceIntCreate(seqIndexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridGetPLocalDe(distgrid, localDe, collocation, &
      arbSeqIndexFlagArg, seqIndexListArg, elementCount, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! logicals
    if (present (arbSeqIndexFlag)) &
      arbSeqIndexFlag = arbSeqIndexFlagArg
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(seqIndexListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridGetPLocalDe
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetPLocalDePDim()"
!BOP
! !IROUTINE: ESMF_DistGridGet - Get DE local information for dimension about DistGrid

! !INTERFACE:
  ! Private name; call using ESMF_DistGridGet()
  subroutine ESMF_DistGridGetPLocalDePDim(distgrid, localDe, dim, indexList, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: localDe
    integer,                intent(in)            :: dim
    integer,        target, intent(out)           :: indexList(:)
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!   Get internal DistGrid information.
!
!   The arguments are:
!   \begin{description}
!   \item[distgrid] 
!     Queried {\tt ESMF\_DistGrid} object.
!   \item[localDe] 
!     Local DE for which information is requested. {\tt [0,..,localDeCount-1]}
!   \item[dim] 
!     Dimension for which information is requested. {\tt [1,..,dimCount]}
!   \item[indexList]
!     Upon return this holds the list of DistGrid patch-local indices
!     for {\tt localDe} along dimension {\tt dim}. The supplied variable 
!     must be at least of size {\tt indexCountPDimPDe(dim, de(localDe))}.
!   \item[{[rc]}] 
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: indexListArg    ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    indexListArg = ESMF_InterfaceIntCreate(indexList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridGetPLocalDePDim(distgrid, localDe, dim, indexListArg, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(indexListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridGetPLocalDePDim
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridPrint()"
!BOP
! !IROUTINE: ESMF_DistGridPrint - Print DistGrid internals

! !INTERFACE:
  subroutine ESMF_DistGridPrint(distgrid, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(in)              :: distgrid
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Prints internal information about the specified {\tt ESMF\_DistGrid} 
!     object to {\tt stdout}. \\
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, the {\tt ESMF\_IOUnitFlush()} method
!     may be used on unit 6 to get coherent output.  \\
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
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

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridPrint(distgrid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridMatch()"
!BOP
! !IROUTINE: ESMF_DistGridMatch - Check if two DistGrid objects match

! !INTERFACE:
  function ESMF_DistGridMatch(distgrid1, distgrid2, rc)
!
! !RETURN VALUE:
    logical :: ESMF_DistGridMatch
      
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(in)              :: distgrid1
    type(ESMF_DistGrid),  intent(in)              :: distgrid2
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Check if {\tt distgrid1} and {\tt distgrid2} match. Returns
!      {\tt .TRUE.} if DistGrid objects match, {\tt .FALSE.} otherwise.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid1] 
!          {\tt ESMF\_DistGrid} object.
!     \item[distgrid2] 
!          {\tt ESMF\_DistGrid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_Logical)      :: matchResult

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid1, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid2, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridMatch(distgrid1, distgrid2, matchResult, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    ESMF_DistGridMatch = matchResult
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_DistGridMatch
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSet()"
!BOPI
! !IROUTINE: ESMF_DistGridSet - Set DistGrid sequence index collocation labels

! !INTERFACE:
  subroutine ESMF_DistGridSet(distgrid, collocationPDim, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(in)              :: distgrid
    integer,              intent(in)              :: collocationPDim(:)
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set the sequence index collocation labels in {\tt distgrid}.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
!     \item[collocationPDim] 
!          List of size {\tt dimCount} specifying which dimensions are
!          covered by which sequence index. Each entry is associated with the
!          corresponding dimension. Dimensions with identical entries in the
!          {\tt collocationPDim} argument are collocated within the same
!          sequence index space. Dimensions with different entries are located
!          in orthogonal sequence index spaces.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: collocationPDimArg  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    collocationPDimArg = &
      ESMF_InterfaceIntCreate(collocationPDim, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridSet(distgrid, collocationPDimArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! garbage collection
    call ESMF_InterfaceIntDestroy(collocationPDimArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DistGridSet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridValidate()"
!BOP
! !IROUTINE: ESMF_DistGridValidate - Validate DistGrid internals

! !INTERFACE:
  subroutine ESMF_DistGridValidate(distgrid, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(in)              :: distgrid
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt distgrid} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
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
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridValidate(distgrid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DistGridValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetInit"
!BOPI
! !IROUTINE: ESMF_DistGridGetInit - Internal access routine for init code
!
! !INTERFACE:
      function ESMF_DistGridGetInit(distgrid) 
!
! !RETURN VALUE:
      ESMF_INIT_TYPE :: ESMF_DistGridGetInit   
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in), optional :: distgrid
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [distgrid]
!           DistGrid object.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(distgrid)) then
      ESMF_DistGridGetInit = ESMF_INIT_GET(distgrid)
    else
      ESMF_DistGridGetInit = ESMF_INIT_CREATED
    endif

    end function ESMF_DistGridGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_DistGridSetInitCreated - Set DistGrid init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_DistGridSetInitCreated(distgrid, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(inout)           :: distgrid
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in DistGrid object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(distgrid)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DistGridSetInitCreated
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridConnection()"
!BOP
! !IROUTINE: ESMF_DistGridConnection - Construct a DistGrid connection element
! !INTERFACE:
  subroutine ESMF_DistGridConnection(connection, patchIndexA, patchIndexB, &
    positionVector, orientationVector, repetitionVector, rc)
!
! !ARGUMENTS:
    integer,        target, intent(out)           :: connection(:)
    integer,                intent(in)            :: patchIndexA
    integer,                intent(in)            :: patchIndexB
    integer,                intent(in)            :: positionVector(:)
    integer,                intent(in),  optional :: orientationVector(:)
    integer,                intent(in),  optional :: repetitionVector(:)
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     This call helps to construct a DistGrid connection,
!     which is a simple vector of integers, out of its components.
!
!     The arguments are:
!     \begin{description}
!     \item[connection] 
!        Element to be constructed. The provided {\tt connection} must 
!        be dimensioned to hold exactly the number of integers that result from
!        the input information.
!     \item[patchIndexA] 
!        Index of one of the two patches that are to be connected.
!     \item[patchIndexB] 
!        Index of one of the two patches that are to be connected.
!     \item[positionVector] 
!        Position of patch B's minIndex with respect to patch A's minIndex.
!     \item[{[orientationVector]}]
!        Associates each dimension of patch A with a dimension in patch B's 
!        index space. Negative index values may be used to indicate a 
!        reversal in index orientation. It is erroneous to associate multiple
!        dimensions of patch A with the same index in patch B. By default
!        {\tt orientationVector = (/1,2,3,.../)}, i.e. same orientation as
!        patch A.
!     \item[{[repetitionVector]}]
!        The allowed values for each direction are 0 and 1. An entry of 1
!        indicates that this connection element will be repeated along the
!        respective dimension. A value of 0 indicates no repetition along this
!        dimension. By default {\tt repetitionVector = (/0,0,0,.../)}, i.e. no
!        repetition along any direction.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_InterfaceInt) :: connectionArg        ! helper variable
    type(ESMF_InterfaceInt) :: positionVectorArg    ! helper variable
    type(ESMF_InterfaceInt) :: orientationVectorArg ! helper variable
    type(ESMF_InterfaceInt) :: repetitionVectorArg  ! helper variable

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Deal with (optional) array arguments
    connectionArg = ESMF_InterfaceIntCreate(connection, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    positionVectorArg = ESMF_InterfaceIntCreate(positionVector, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    orientationVectorArg = ESMF_InterfaceIntCreate(orientationVector, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    repetitionVectorArg = ESMF_InterfaceIntCreate(repetitionVector, &
      rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridConnection(connectionArg, &
      patchIndexA, patchIndexB, positionVectorArg, orientationVectorArg, &
      repetitionVectorArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(connectionArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(positionVectorArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(orientationVectorArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(repetitionVectorArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridConnection
!------------------------------------------------------------------------------


end module ESMF_DistGridMod
