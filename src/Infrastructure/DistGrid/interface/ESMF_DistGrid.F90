!stG$
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research, 
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

!   F90 API wrapper of C++ implemenation of DistGrid
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
  public ESMF_DistGridPrint
  public ESMF_DistGridValidate
  
  public ESMF_Connection
  public ESMF_ConnectionTransform

! - ESMF-private methods:
  public ESMF_DistGridGetInit
  public ESMF_DistGridSetInitCreated
  
  
!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_DistGrid.F90,v 1.21 2007/07/12 19:40:28 cdeluca Exp $'

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
    module procedure ESMF_DistGridCreateRD
    module procedure ESMF_DistGridCreateDB
    module procedure ESMF_DistGridCreateRDFA
    module procedure ESMF_DistGridCreateDBFA      
    module procedure ESMF_DistGridCreateRDP
    module procedure ESMF_DistGridCreateDBP
    module procedure ESMF_DistGridCreateRDPFA
    module procedure ESMF_DistGridCreateDBPFA
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
    module procedure ESMF_DistGridGet
    module procedure ESMF_DistGridGetPDe
    module procedure ESMF_DistGridGetPDePDim
    module procedure ESMF_DistGridGetLinksPDe
      
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
#define ESMF_METHOD "ESMF_DistGridCreateRD()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object with regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  
  function ESMF_DistGridCreateRD(minIndex, maxIndex, regDecomp, &
    decompflag, deLabelList, indexflag, connectionList, connectionTransformList, &
    delayout, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:)
    integer,                      intent(in)            :: maxIndex(:)
    integer, target,              intent(in), optional  :: regDecomp(:)
    type(ESMF_DecompFlag), target,intent(in), optional  :: decompflag(:)
    integer, target,              intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer, target,              intent(in), optional  :: connectionList(:,:)
    integer, target,              intent(in), optional  :: connectionTransformList(:,:)
    type(ESMF_DELayout),          intent(in), optional  :: delayout
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRD
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular (LR) 
!     domain with regular decomposition. A regular decomposition is of the same 
!     rank as the domain and decomposes each dimension into a fixed number of 
!     DEs. A regular decomposition of a single LR domain is expressed by a 
!     single {\tt regDecomp} list of DE counts in each dimension.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the global domain.
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the global domain.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of the
!          global domain is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions. See section ?? for a 
!          list of valid decomposition flag options.
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
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status     ! local error status
    type(ESMF_DistGrid)     :: distgrid   ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt):: minIndexArg ! helper variable
    type(ESMF_InterfaceInt):: maxIndexArg ! helper variable
    type(ESMF_InterfaceInt):: regDecompArg ! helper variable
    type(ESMF_DecompFlag), target:: dummyDf(0)  ! used to satisfy the C interf.
    type(ESMF_DecompFlag), pointer::  opt_decompflag(:) ! optional arg helper
    integer                 :: len_decompflag ! helper variable
    type(ESMF_InterfaceInt):: deLabelListArg ! helper variable
    type(ESMF_InterfaceInt):: connectionListArg ! helper variable
    type(ESMF_InterfaceInt):: connectionTransformListArg ! helper variable

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexArg = ESMF_InterfaceIntCreate(minIndex, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(maxIndex, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompArg = ESMF_InterfaceIntCreate(regDecomp, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(decompflag)) then
      len_decompflag = size(decompflag)
      opt_decompflag => decompflag
    else
      len_decompflag = 0
      opt_decompflag => dummyDf
    endif
    deLabelListArg = ESMF_InterfaceIntCreate(deLabelList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionTransformListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionTransformList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRD(distgrid, minIndexArg, maxIndexArg, &
      regDecompArg, opt_decompflag, len_decompflag, deLabelListArg, indexflag, &
      connectionListArg, connectionTransformListArg, delayout, vm, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deLabelListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionTransformListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRD = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRD)
 
    ! Return successfully
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
    deLabelList, indexflag, connectionList, connectionTransformList, delayout, &
    vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:)
    integer,                      intent(in)            :: maxIndex(:)
    integer,                      intent(in)            :: deBlockList(:,:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in), optional  :: connectionTransformList(:,:)
    type(ESMF_DELayout),          intent(in), optional  :: delayout
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDB
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular (LR) 
!     domain with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the global domain.
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the global domain.
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
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status     ! local error status
    type(ESMF_DistGrid)     :: distgrid   ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt):: minIndexArg ! helper variable
    type(ESMF_InterfaceInt):: maxIndexArg ! helper variable
    type(ESMF_InterfaceInt):: deBlockListArg ! helper variable
    type(ESMF_InterfaceInt):: deLabelListArg ! helper variable
    type(ESMF_InterfaceInt):: connectionListArg ! helper variable
    type(ESMF_InterfaceInt):: connectionTransformListArg ! helper variable

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexArg = ESMF_InterfaceIntCreate(minIndex, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(maxIndex, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deBlockListArg = ESMF_InterfaceIntCreate(farray3D=deBlockList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    deLabelListArg = ESMF_InterfaceIntCreate(deLabelList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionTransformListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionTransformList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateDB(distgrid, minIndexArg, maxIndexArg, &
      deBlockListArg, deLabelListArg, indexflag, &
      connectionListArg, connectionTransformListArg, delayout, vm, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deBlockListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deLabelListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionTransformListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDB = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDB)
 
    ! Return successfully
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
  function ESMF_DistGridCreateRDFA(minIndex, maxIndex, &
    regDecomp, decompflag, deLabelList, indexflag, connectionList, &
    connectionTransformList, fastAxis, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:)
    integer,                      intent(in)            :: maxIndex(:)
    integer,                      intent(in), optional  :: regDecomp(:)
    type(ESMF_DecompFlag),target, intent(in), optional  :: decompflag(:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in), optional  :: connectionTransformList(:,:)
    integer,                      intent(in)            :: fastAxis
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDFA
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular (LR) 
!     domain with regular decomposition. A regular
!     decomposition is of the same rank as the global domain and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     single LR domain is expressed by a single {\tt regDecomp} list of DE counts
!     in each dimension.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the global domain.
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the global domain.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of the
!          global domain is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions. See section ?? for a 
!          list of valid decomposition flag options.
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
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status     ! local error status
    type(ESMF_DistGrid)     :: distgrid   ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt):: minIndexArg ! helper variable
    type(ESMF_InterfaceInt):: maxIndexArg ! helper variable
    type(ESMF_InterfaceInt):: regDecompArg ! helper variable
    type(ESMF_DecompFlag), target:: dummyDf(0)  ! used to satisfy the C interf.
    type(ESMF_DecompFlag), pointer::  opt_decompflag(:) ! optional arg helper
    integer                 :: len_decompflag ! helper variable
    type(ESMF_InterfaceInt):: deLabelListArg ! helper variable
    type(ESMF_InterfaceInt):: connectionListArg ! helper variable
    type(ESMF_InterfaceInt):: connectionTransformListArg ! helper variable

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexArg = ESMF_InterfaceIntCreate(minIndex, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(maxIndex, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompArg = ESMF_InterfaceIntCreate(regDecomp, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    if (present(decompflag)) then
      len_decompflag = size(decompflag)
      opt_decompflag => decompflag
    else
      len_decompflag = 0
      opt_decompflag => dummyDf
    endif
    deLabelListArg = ESMF_InterfaceIntCreate(deLabelList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionTransformListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionTransformList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRDFA(distgrid, minIndexArg, maxIndexArg, &
      regDecompArg, opt_decompflag, len_decompflag, deLabelListArg, indexflag, &
      connectionListArg, connectionTransformListArg, fastAxis, vm, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deLabelListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionTransformListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDFA = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDFA)
 
    ! Return successfully
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
    connectionTransformList, fastAxis, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:)
    integer,                      intent(in)            :: maxIndex(:)
    integer,                      intent(in)            :: deBlockList(:,:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in), optional  :: connectionTransformList(:,:)
    integer,                      intent(in)            :: fastAxis
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBFA
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a single logically rectangular (LR) 
!     domain with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          Global coordinate tuple of the lower corner of the global domain.
!     \item[maxIndex]
!          Global coordinate tuple of the upper corner of the global domain.
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
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    type(ESMF_DistGrid):: distgrid   ! opaque pointer to new C++ DistGrid
    
    ! Print current subroutine name
    print *, ">>ESMF_DistGridCreateDBFA<<"

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DELayoutCreateFromPetMap(delayout, petMap, len_petMap, &
!      virtualdepinflag, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDBFA = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBFA)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBFA
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRDP()"
!BOP
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from patch work with regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRDP(minIndex, maxIndex, regDecomp,&
    decompflag, deLabelList, indexflag, connectionList, connectionTransformList,&
    delayout, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:,:)
    integer,                      intent(in)            :: maxIndex(:,:)
    integer,                      intent(in), optional  :: regDecomp(:,:)
    type(ESMF_DecompFlag),target, intent(in), optional  :: decompflag(:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in), optional  :: connectionTransformList(:,:)
    type(ESMF_DELayout),          intent(in), optional  :: delayout
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDP
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a patch work of logically 
!     rectangular (LR) domains with regular decomposition. A regular
!     decomposition is of the same rank as the global domain and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     patch work of LR domains is expressed by a list of DE count vectors, one
!     vector for each patch domain. Each vector contained in the 
!     {\tt regDecomp} argument ascribes DE counts for each dimension. It is 
!     erroneous to provide more domain patches than there are DEs.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          The first index provides the global coordinate tuple of the lower 
!          corner of a global domain patch. The second index indicates the 
!          patch number.
!     \item[maxIndex]
!          The first index provides the global coordinate tuple of the upper
!          corner of a global domain patch. The second index indicates the 
!          patch number.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The second 
!          index indicates the patch number. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of each
!          patch domain is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions for all patches. 
!          See section ?? for a list of valid decomposition flag options. The 
!          second index indicates the patch number.
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
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status     ! local error status
    type(ESMF_DistGrid)     :: distgrid   ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt):: minIndexArg ! helper variable
    type(ESMF_InterfaceInt):: maxIndexArg ! helper variable
    type(ESMF_InterfaceInt):: regDecompArg ! helper variable
    type(ESMF_DecompFlag), target:: dummyDf(0,0)  ! used to satisfy the C interf.
    type(ESMF_DecompFlag), pointer::  opt_decompflag(:,:) ! optional arg helper
    integer                 :: len1_decompflag ! helper variable
    integer                 :: len2_decompflag ! helper variable
    type(ESMF_InterfaceInt):: deLabelListArg ! helper variable
    type(ESMF_InterfaceInt):: connectionListArg ! helper variable
    type(ESMF_InterfaceInt):: connectionTransformListArg ! helper variable

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with (optional) array arguments
    minIndexArg = ESMF_InterfaceIntCreate(farray2D=minIndex, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    maxIndexArg = ESMF_InterfaceIntCreate(farray2D=maxIndex, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    regDecompArg = ESMF_InterfaceIntCreate(farray2D=regDecomp, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
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
    deLabelListArg = ESMF_InterfaceIntCreate(deLabelList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    connectionTransformListArg = &
      ESMF_InterfaceIntCreate(farray2D=connectionTransformList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridCreateRDP(distgrid, minIndexArg, &
      maxIndexArg, regDecompArg, opt_decompflag, len1_decompflag, &
      len2_decompflag, deLabelListArg, indexflag, &
      connectionListArg, connectionTransformListArg, delayout, vm, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(minIndexArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(maxIndexArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(regDecompArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(deLabelListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(connectionTransformListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDP = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDP)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateRDP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBP()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from patch work with regular decomposition

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBP(minIndex, maxIndex, &
    deBlockList, deLabelList, indexflag, connectionList, &
    connectionTransformList, delayout, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:,:)
    integer,                      intent(in)            :: maxIndex(:,:)
    integer,                      intent(in)            :: deBlockList(:,:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in), optional  :: connectionTransformList(:,:)
    type(ESMF_DELayout),          intent(in), optional  :: delayout
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBP
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a patch work of logically 
!     rectangular (LR) domains with regular decomposition. A regular
!     decomposition is of the same rank as the global domain and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     patch work of LR domains is expressed by a list of DE count vectors, one
!     vector for each patch domain. Each vector contained in the 
!     {\tt regDecomp} argument ascribes DE counts for each dimension. It is 
!     erroneous to provide more domain patches than there are DEs.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          The first index provides the global coordinate tuple of the lower 
!          corner of a global domain patch. The second index indicates the 
!          patch number.
!     \item[maxIndex]
!          The first index provides the global coordinate tuple of the upper
!          corner of a global domain patch. The second index indicates the 
!          patch number.
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
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    type(ESMF_DistGrid):: distgrid   ! opaque pointer to new C++ DistGrid

    ! Print current subroutine name
    print *, ">>ESMF_DistGridCreateDBP<<"

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DELayoutCreateFromPetMap(delayout, petMap, len_petMap, &
!      virtualdepinflag, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDBP = distgrid 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBP)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBP
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateRDPFA()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from patch work with regular decomposition and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateRDPFA(minIndex, maxIndex, &
    regDecomp, decompflag, deLabelList, indexflag, connectionList, &
    connectionTransformList, fastAxis, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:,:)
    integer,                      intent(in)            :: maxIndex(:,:)
    integer,                      intent(in), optional  :: regDecomp(:,:)
    type(ESMF_DecompFlag),target, intent(in), optional  :: decompflag(:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in), optional  :: connectionTransformList(:,:)
    integer,                      intent(in)            :: fastAxis
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateRDPFA
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a patch work of logically 
!     rectangular (LR) domains with regular decomposition. A regular
!     decomposition is of the same rank as the global domain and decomposes
!     each dimension into a fixed number of DEs. A regular decomposition of a
!     patch work of LR domains is expressed by a list of DE count vectors, one
!     vector for each patch domain. Each vector contained in the 
!     {\tt regDecomp} argument ascribes DE counts for each dimension. It is 
!     erroneous to provide more domain patches than there are DEs.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          The first index provides the global coordinate tuple of the lower 
!          corner of a global domain patch. The second index indicates the 
!          patch number.
!     \item[maxIndex]
!          The first index provides the global coordinate tuple of the upper
!          corner of a global domain patch. The second index indicates the 
!          patch number.
!     \item[{[regDecomp]}]
!          List of DE counts for each dimension. The second 
!          index indicates the patch number. The default decomposition will
!          be {\tt deCount}$ \times 1 \times ... \times 1$. The value of
!          {\tt deCount} for a default DELayout equals {\tt petCount}, i.e. the
!          default decomposition will be into as many DEs as there are 
!          PETs and the distribution will be 1 DE per PET.
!     \item[{[decompflag]}]
!          List of decomposition flags indicating how each dimension of each
!          patch domain is to be divided between the DEs. The default setting
!          is {\tt ESMF\_DECOMP\_HOMOGEN} in all dimensions for all patches. 
!          See section ?? for a list of valid decomposition flag options. The 
!          second index indicates the patch number.
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
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    type(ESMF_DistGrid):: distgrid   ! opaque pointer to new C++ DistGrid

    ! Print current subroutine name
    print *, ">>ESMF_DistGridCreateRDPFA<<"

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DELayoutCreateFromPetMap(delayout, petMap, len_petMap, &
!      virtualdepinflag, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateRDPFA = distgrid
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateRDPFA)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateRDPFA
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBPFA()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create DistGrid object from patch work with DE blocks and fast axis

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBPFA(minIndex, maxIndex, &
    deBlockList, deLabelList, indexflag, connectionList, &
    connectionTransformList, fastAxis, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: minIndex(:,:)
    integer,                      intent(in)            :: maxIndex(:,:)
    integer,                      intent(in)            :: deBlockList(:,:,:)
    integer,                      intent(in), optional  :: deLabelList(:)
    type(ESMF_IndexFlag),         intent(in), optional  :: indexflag
    integer,                      intent(in), optional  :: connectionList(:,:)
    integer,                      intent(in), optional  :: connectionTransformList(:,:)
    integer,                      intent(in)            :: fastAxis
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBPFA
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DistGrid} from a patch work of logically 
!     rectangular (LR) domains with decomposition specified by {\tt deBlockList}.
!
!     The arguments are:
!     \begin{description}
!     \item[minIndex]
!          The first index provides the global coordinate tuple of the lower 
!          corner of a global domain patch. The second index indicates the 
!          patch number.
!     \item[maxIndex]
!          The first index provides the global coordinate tuple of the upper
!          corner of a global domain patch. The second index indicates the 
!          patch number.
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
!     \item[{[connectionTransformList]}]
!          List of transforms associated with patch connections defined in 
!          {\tt connectionList}. The second dimension of {\tt connectionTransformList}
!          steps through the connection transforms, defined by the first index. The
!          first index must be of size {\tt 5 + dimCount}, where {\tt dimCount}
!          is the rank of the decomposed index space. Each 
!          {\tt connectionTransformList} element specifies a connection transform 
!          by a list of integer values in the format {\tt (/connectionIndex,
!          direction, staggerSrc, staggerDst, offsetDst, signVector/)}, where
!          \begin{itemize}
!          \item {\tt connectionIndex} corresponds to the index of the connection in
!                {\tt connectionList},
!          \item {\tt direction} can be {\tt +1} to specify forward direction,
!                i.e. source patch of the transform is patch\_A and destination
!                patch is patch\_B of the corresponding connection, or {\tt -1}
!                to indicate reverse direction through the connection. The only
!                other valid {\tt direction} value is {\tt 0} which indicates a 
!                bidirectional connection with source and destination definitions
!                as in the forward case. 
!          \item {\tt staggerSrc} and {\tt staggerDst} indicate staggering location
!                in the source and destination patch interface, respectively,
!          \item {\tt offsetDst} is a vector of size {\tt dimCount} that 
!                specifies the index offset on the destination side of 
!                this connection,
!          \item {\tt signVector} is of size {\tt dimCount} with elements either
!                {\tt +1} or {\tt -1} to indicate optional sign change of vector
!                components along the respective directions.
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    type(ESMF_DistGrid):: distgrid   ! opaque pointer to new C++ DistGrid

    ! Print current subroutine name
    print *, ">>ESMF_DistGridCreateDBPFA<<"

    ! initialize return code; assume routine not implemented
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Initialize the pointer to NULL
    distgrid%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DELayoutCreateFromPetMap(delayout, petMap, len_petMap, &
!      virtualdepinflag, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DistGridCreateDBPFA = distgrid
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBPFA)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DistGridCreateDBPFA
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridCreateDBAI()"
!BOPI
! !IROUTINE: ESMF_DistGridCreate - Create 1D DistGrid object from users arbitray index

! !INTERFACE:
  ! Private name; call using ESMF_DistGridCreate()
  function ESMF_DistGridCreateDBAI(indices, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: indices(:)
    type(ESMF_VM),                intent(in)            :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DistGrid) :: ESMF_DistGridCreateDBAI
!
! !DESCRIPTION:
!     Create a 1D {\tt ESMF\_DistGrid} from a list of (global) indices that
!     live on the current pet.
!
!     The arguments are:
!     \begin{description}
!     \item[{[indices]}]
!          List of (global) indices that reside on the current pet.  These
!          may be a partition of the global index space, or may overlap.
!     \item[{[vm]}]
!          Required {\tt ESMF\_VM} object of the current context. 
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status          ! local error status
    type(ESMF_DistGrid)     :: distgrid        ! opaque pointer to new C++ DistGrid
    type(ESMF_InterfaceInt) :: indicesArg      ! index helper
    integer                 :: localSize(2)    ! number of local indices
    integer, allocatable    :: globalSizes(:)  ! array of all sizes
    integer                 :: petCount        ! num pets
    integer, allocatable    :: deblock(:,:,:)  ! Array of sizes
    integer                 :: imin, imax      ! min and max indicies,this pet
    integer                 :: i, csum         ! loop variable
    integer                 :: minC(1), maxC(1)! min/max corner

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)

    call ESMF_VMGet(vm, petCount=petCount, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! min/max of local indices
    localSize(1) = size(indices)
    imax = -huge(imax)
    imin = huge(imin)
    do i=1,localSize(1)
      if (indices(i) .le. imin) imin = indices(i)
      if (indices(i) .ge. imax) imax = indices(i)
    enddo

    allocate(globalSizes(2*petCount))


    ! Gather the sizes locally
    call ESMF_VMAllGather(vm, localSize, globalSizes, 1, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Now set up the deblocks
    allocate(deblock(1,2,petCount))


    csum = 0
    do i=1,petCount
      deblock(1,1,i) = csum + 1
      csum = csum + globalSizes(i)
      deblock(1,2,i) = csum
    enddo


    ! And now a global reduction to find min/max corners over all pets
    localSize(1) = imin
    call ESMF_VMAllReduce(vm, localSize, globalSizes, 1, &
     ESMF_MIN, rc=status)
    imin = globalSizes(1)
    localSize(1) = imax
    call ESMF_VMAllReduce(vm, localSize, globalSizes, 1, &
     ESMF_MAX, rc=status)
    imax = globalSizes(1)

    minC(1) = imin
    maxC(1) = imax
    distgrid = ESMF_DistGridCreateDB(minC, maxC, &
               deBlockList=deblock, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    deallocate(deblock)
    deallocate(globalSizes)
    
    ! Set return value
    ESMF_DistGridCreateDBAI = distgrid 

    indicesArg = ESMF_InterfaceIntCreate(farray1D=indices, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    call c_ESMC_DistGrid_StoreAbIdx(distgrid, indicesArg, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DistGridCreateDBAI)
 
    ! Return successfully
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
!     Destroy an {\tt ESMF\_DistGrid} object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          {\tt ESMF\_DistGrid} object to be destroyed.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status       ! local error status

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridDestroy(distgrid, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Mark this DistGrid as invalid
    distgrid%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(distgrid)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGet()"
!BOP
! !IROUTINE: ESMF_DistGridGet - Get information about DistGrid object

! !INTERFACE:
  subroutine ESMF_DistGridGet(distgrid, delayout, patchCount, patchList, &
    dimCount, dimExtent, regDecompFlag, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    type(ESMF_DELayout),    intent(out), optional :: delayout
    integer,                intent(out), optional :: patchCount
    integer,                intent(out), optional :: patchList(:)
    integer,                intent(out), optional :: dimCount
    integer,                intent(out), optional :: dimExtent(:,:)
    type(ESMF_Logical),     intent(out), optional :: regDecompFlag
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Get internal DistGrid information.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!        Queried {\tt ESMF\_DistGrid} object.
!     \item[{[delayout]}]
!        Upon return this holds the {\tt ESMF\_DELayout} object associated with
!        this DistGrid object.
!     \item[{[patchCount]}]
!        Upon return this holds the number of patches the DistGrid object is
!        composed of.
!     \item[{[patchList]}]
!        Upon return this holds a list of patch id numbers, one for each DE.
!     \item[{[dimCount]}]
!        Upon return this holds the number dimensions (or rank) of the
!        DistGrid object.
!     \item[{[dimExtent]}]
!        Upon return this array holds the extents for all dimensions of the
!        DE-local LR boxes for all DEs. The supplied variable must be at least
!        of size {\tt (/dimCount, deCount/)}.
!     \item[{[regDecompFlag]}]
!        Upon return this flag indicates regular decompositions by {\tt
!        ESMF\_TRUE} and other decompositions by {\tt ESMF\_FALSE}.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                     :: status         ! local error status
    type(ESMF_InterfaceInt):: patchListArg   ! helper variable
    type(ESMF_InterfaceInt):: dimExtentArg   ! helper variable

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    patchListArg = ESMF_InterfaceIntCreate(patchList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    dimExtentArg = ESMF_InterfaceIntCreate(farray2D=dimExtent, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridGet(distgrid, delayout, patchCount, patchListArg, &
      dimCount, dimExtentArg, regDecompFlag, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set init code for deep C++ objects
    if (present(delayout)) then
      call ESMF_DELayoutSetInitCreated(delayout, rc=status)
      if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! garbage collection
    call ESMF_InterfaceIntDestroy(patchListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(dimExtentArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetPDe()"
!BOPI
! !IROUTINE: ESMF_DistGridGet - Get DE local information about DistGrid

! !INTERFACE:
  subroutine ESMF_DistGridGetPDe(distgrid, de, dimExtent, regDecompDeCoord, &
    patch, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: de
    integer, target,        intent(out), optional :: dimExtent(:)
    integer, target,        intent(out), optional :: regDecompDeCoord(:)
    integer,                intent(out), optional :: patch
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Get internal DistGrid information.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!        Queried {\tt ESMF\_DistGrid} object.
!     \item[de] 
!        DE for which information is requested. {\tt \[1,..,deCount\]}
!     \item[{[dimExtent]}]
!        Upon return this number identifies the patch on which the DE is defined.
!     \item[{[regDecompDeCoord]}]
!        For regular decompositions upon return this array holds the coordinate
!        tuple of the specified DE with respect to the local patch. For other
!        decompositions a run-time warning will be issued if 
!        {\tt regDecompDeCoord} is requested.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
!    type(ESMF_DELayout):: dummy

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DELayoutGetVM(delayout, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_DistGridGetPDe
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetPDePDim()"
!BOP
! !IROUTINE: ESMF_DistGridGet - Get DE local information for dimension about DistGrid

! !INTERFACE:
  subroutine ESMF_DistGridGetPDePDim(distgrid, localDe, dim, localIndexList, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: localDe
    integer,                intent(in)            :: dim
    integer,                intent(out), optional :: localIndexList(:)
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Get internal DistGrid information.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!        Queried {\tt ESMF\_DistGrid} object.
!     \item[localDe] 
!        Local DE for which information is requested. {\tt [/1,..,localDeCount/]}
!     \item[dim] 
!        Dimension for which information is requested. {\tt [/1,..,dimCount/]}
!     \item[{[localIndexList]}]
!        Upon return this holds the list of DistGrid patch-local indices
!        for {\tt localDe} along dimension {\tt dim}. The supplied variable 
!        must be at least of size {\tt dimExtent(dim, de(localDe))}.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status               ! local error status
    type(ESMF_InterfaceInt) :: localIndexListArg    ! helper variable

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Deal with (optional) array arguments
    localIndexListArg = ESMF_InterfaceIntCreate(localIndexList, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DistGridGetPDePDim(distgrid, localDe, dim, localIndexListArg, &
      status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(localIndexListArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DistGridGetPDePDim
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridGetLinksPDe()"
!BOPI
! !IROUTINE: ESMF_DistGridGet - Get DE local information about links

! !INTERFACE:
  subroutine ESMF_DistGridGetLinksPDe(distgrid, de, staggerLoc, lVecInner, &
    uVecInner, lVecOuter, uVecOuter, linkCount, linkList, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),    intent(in)            :: distgrid
    integer,                intent(in)            :: de
    integer,                intent(in)            :: staggerLoc
    integer,                intent(in)            :: lVecInner(:)
    integer,                intent(in)            :: uVecInner(:)
    integer,                intent(in)            :: lVecOuter(:)
    integer,                intent(in)            :: uVecOuter(:)
    integer,                intent(out)           :: linkCount
    integer, target,        intent(out), optional :: linkList(:,:)
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Query an {\tt ESMF\_DistGrid} object about the index space topology
!     around a DE.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!        Queried {\tt ESMF\_DistGrid} object.
!     \item[de] 
!        Queried DE.
!     \item[staggerLoc] 
!        Queried staggering location.
!     \item[{[linkList]}]
!        Upon return this array holds {\tt linkCount} link elements. The second
!        index of {\tt linkList} steps through the link elements which are
!        defined by the first index. The first index must be of size
!        {\tt 5*dimCount + 2} and provides the link element information in the 
!        format {\tt (/minIndex, maxIndex, partnerDe, partnerStaggerLoc, 
!        partnerStartCorner, partnerEndCorner, partnerIndexOrder, 
!        signChangeVector/)}.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code
!    type(ESMF_DELayout):: dummy

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DELayoutGetVM(delayout, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_DistGridGetLinksPDe
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DistGridPrint()"
!BOP
! !IROUTINE: ESMF_DistGridPrint - Print DistGrid internals

! !INTERFACE:
  subroutine ESMF_DistGridPrint(distgrid, options, rc)
!
! !ARGUMENTS:
    type(ESMF_DistGrid),  intent(in)              :: distgrid
    character(len=*),     intent(in),   optional  :: options
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!   Prints internal information about the specified {\tt ESMF\_DistGrid} 
!   object to {\tt stdout}.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Specified {\tt ESMF\_DistGrid} object.
!     \item[{[options]}] 
!          Print options are not yet supported.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status       ! local error status

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridPrint(distgrid, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DistGridPrint
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DistGridGetInit, distgrid, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DistGridValidate(distgrid, localrc)
    
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DistGridValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
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
#define ESMF_METHOD "ESMF_Connection()"
!BOP
! !IROUTINE: ESMF_Connection - Construct a connection element
! !INTERFACE:
  subroutine ESMF_Connection(connection, patchIndexA, patchIndexB, &
    positionVector, orientationVector, rc)
!
! !ARGUMENTS:
    integer,                intent(out)           :: connection(:)
    integer,                intent(in)            :: patchIndexA
    integer,                intent(in)            :: patchIndexB
    integer,                intent(in)            :: positionVector(:)
    integer,                intent(in),  optional :: orientationVector(:)
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     This call helps to construct a {\tt connection},
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
!        dimensions of patch A with the same index in patch B.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer                 :: status     ! local error status
    type(ESMF_InterfaceInt):: connectionArg ! helper variable
    type(ESMF_InterfaceInt):: positionVectorArg ! helper variable
    type(ESMF_InterfaceInt):: orientationVectorArg ! helper variable

    ! initialize return code; assume routine not implemented
    status = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Deal with (optional) array arguments
    connectionArg = ESMF_InterfaceIntCreate(connection, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    positionVectorArg = ESMF_InterfaceIntCreate(positionVector, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    orientationVectorArg = ESMF_InterfaceIntCreate(orientationVector, &
      rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! call into the C++ interface, which will sort out optional arguments
    call c_ESMC_Connection(connectionArg, &
      patchIndexA, patchIndexB, positionVectorArg, orientationVectorArg, status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! garbage collection
    call ESMF_InterfaceIntDestroy(connectionArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(positionVectorArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(orientationVectorArg, rc=status)
    if (ESMF_LogMsgFoundError(status, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_Connection
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_ConnectionTransform()"
!BOPI
! !IROUTINE: ESMF_ConnectionTransform - Construct a connection transform element
! !INTERFACE:
  subroutine ESMF_ConnectionTransform(connectionTransform,&
    connectionIndex, direction, staggerSrc, staggerDst, indexOffsetVector, &
    signChangeVector, rc)
!
! !ARGUMENTS:
    integer,                intent(out)           :: connectionTransform(:)
    integer,                intent(in)            :: connectionIndex
    integer,                intent(in)            :: direction
    integer,                intent(in)            :: staggerSrc
    integer,                intent(in)            :: staggerDst
    integer,                intent(in),  optional :: indexOffsetVector(:)
    integer,                intent(in),  optional :: signChangeVector(:)
    integer,                intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     This call helps to construct a {\tt connectionTransform},
!     which is a simple vector of integers, out of its components.
!
!     The arguments are:
!     \begin{description}
!     \item[connectionTransform] 
!        Element to be constructed. The provided {\tt connectionTransform} 
!        must be dimensioned to hold exactly the number of integers that result
!        from the input information.
!     \item[connectionIndex] 
!        Index of the corresponding connection element in {\tt connectionList}
!     \item[direction] 
!        A {\tt +1} indicates forward direction, i.e. source patch is patch A
!        and destination patch is patch B of the corresponding connection
!        element. A value of {\tt -1} indicates reverse direction and a value of
!        {\tt 0} referes to a bidirectional transformation.
!     \item[staggerSrc] 
!        Stagger location in source patch
!     \item[staggerDst] 
!        Stagger location in destination patch
!     \item[{[indexOffsetVector]}]
!        This vector of size {\tt dimCount} specifies the index offset on the 
!        destination side of the connection that needs to be applied in addition
!        to the index transformation defined by the connection element.
!        Default is a null vector.
!     \item[{[signChangeVector]}]
!        This vector of size {\tt dimCount} specifies potential sign changes
!        of data that is aligned along certain directions. The interpretation
!        of this vector lies outside the scope of the {\tt DistGrid} class which 
!        provides storage of this information for convenience sake.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
!    call c_ESMC_DELayoutGetVM(delayout, vm, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
  end subroutine ESMF_ConnectionTransform
!------------------------------------------------------------------------------

end module ESMF_DistGridMod
