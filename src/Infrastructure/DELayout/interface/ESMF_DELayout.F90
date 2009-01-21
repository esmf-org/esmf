! $Id: ESMF_DELayout.F90,v 1.69.2.3 2009/01/21 21:25:20 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_DELayout.F90"
!==============================================================================
!
! ESMF DELayout Module
module ESMF_DELayoutMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the DELayout class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOPI
! !MODULE: ESMF_DELayoutMod
!

!   F90 API wrapper of C++ implemenation of DELayout
!
!------------------------------------------------------------------------------

! !USES:
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_VMMod            ! ESMF VM
  use ESMF_F90InterfaceMod  ! ESMF F90-C++ interface helper
  
  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
      
!------------------------------------------------------------------------------
!     ! ESMF_DELayout
!
!------------------------------------------------------------------------------

  ! F90 class type to hold pointer to C++ object
  type ESMF_DELayout
  sequence
  private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------

  ! type for service routines
  type ESMF_DELayoutServiceReply
  private
    integer :: value
  end type

  type(ESMF_DELayoutServiceReply), parameter:: &
    ESMF_DELAYOUT_SERVICE_ACCEPT  = ESMF_DELayoutServiceReply(1), &
    ESMF_DELAYOUT_SERVICE_DENY    = ESMF_DELayoutServiceReply(2)
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------

  integer(ESMF_KIND_I4), parameter:: ESMF_CWGHT_NORMAL = 50 !default

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_DELayout
  public ESMF_DELayoutServiceReply, ESMF_DELAYOUT_SERVICE_ACCEPT, &
    ESMF_DELAYOUT_SERVICE_DENY
  
!------------------------------------------------------------------------------
! !PUBLIC PARAMETERS:
      
  public ESMF_CWGHT_NORMAL

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

! - ESMF-public methods:
  public ESMF_DELayoutCreate
  public ESMF_DELayoutDestroy
  
  public ESMF_DELayoutGet
    
  public ESMF_DELayoutGetDEMatchDE
  public ESMF_DELayoutGetDEMatchPET
  
  public ESMF_DELayoutPrint
  public ESMF_DELayoutValidate
  
  public ESMF_DELayoutServiceOffer
  public ESMF_DELayoutServiceComplete
    
  public ESMF_DELayoutSerialize
  public ESMF_DELayoutDeserialize

! - ESMF-internal methods:
  public ESMF_DELayoutGetInit
  public ESMF_DELayoutSetInitCreated

! - deprecated methods
  public ESMF_DELayoutGetDeprecated
  public ESMF_DELayoutGetDELocalInfo
  


!EOPI
  
  public operator(.eq.), operator(.ne.)
  
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_DELayout.F90,v 1.69.2.3 2009/01/21 21:25:20 cdeluca Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

! -------------------------- ESMF-public method -------------------------------
!BOPI
! !IROUTINE: ESMF_DELayoutCreate -- Generic interface

! !INTERFACE:
  interface ESMF_DELayoutCreate

! !PRIVATE MEMBER FUNCTIONS:
!
    module procedure ESMF_DELayoutCreateDefault
    module procedure ESMF_DELayoutCreateFromPetMap
    module procedure ESMF_DELayoutCreateHintWeights
      
    module procedure ESMF_DELayoutCreateDeprecated

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutCreate} functions.   
!EOPI 
  end interface


! overload .eq. & .ne. for derived types

  interface operator (.eq.)
    module procedure ESMF_sreq
  end interface

  interface operator (.ne.)
    module procedure ESMF_srne
  end interface


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

contains

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!------------------------------------------------------------------------------
! function to compare two ESMF_DELayoutServiceReply to see if they're the same 

  function ESMF_sreq(sr1, sr2)
    logical ESMF_sreq
    type(ESMF_DELayoutServiceReply), intent(in) :: sr1, sr2

    ESMF_sreq = (sr1%value .eq. sr2%value)    
  end function

  function ESMF_srne(sr1, sr2)
    logical ESMF_srne
    type(ESMF_DELayoutServiceReply), intent(in) :: sr1, sr2

    ESMF_srne = (sr1%value .ne. sr2%value)
  end function
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCreateDefault()"
!BOP
! !IROUTINE: ESMF_DELayoutCreate - Create DELayout object

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCreate()
  function ESMF_DELayoutCreateDefault(deCount, deGrouping, dePinFlag, petList, &
    vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in), optional  :: deCount
    integer, target,              intent(in), optional  :: deGrouping(:)
    type(ESMF_DePinFlag),         intent(in), optional  :: dePinFlag
    integer, target,              intent(in), optional  :: petList(:)
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DELayout) :: ESMF_DELayoutCreateDefault
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DELayout} object on the basis of optionally provided
!     restrictions. By default a DELayout with deCount equal to petCount will
!     be created, each DE mapped to a single PET. However, the number of DEs
!     as well grouping of DEs and PETs can be specified via the optional
!     arguments.
!
!     The arguments are:
!     \begin{description}
!     \item[{[deCount]}]
!          Number of DEs to be provided by the created DELayout. By default
!          the number of DEs equals the number of PETs in the associated VM
!          context. Specifying a {\tt deCount} smaller than the number
!          of PETs will result in unassociated PETs.
!          This may be used to share VM resources between DELayouts within the
!          same ESMF component. Specifying a {\tt deCount} greater than the 
!          number of PETs will result in multiple DE to PET mapping.
!     \item[{[deGrouping]}]
!          This optional argument must be of size deCount. Its content assigns
!          a DE group index to each DE of the DELayout. A group index of -1 
!          indicates that the associated DE isn't member of any particular 
!          group. The significance of DE groups is that all the DEs belonging
!          to a certain group will be mapped against the {\em same} PET. This
!          does not, however, mean that DEs belonging to different DE groups 
!          must be mapped to different PETs.
!     \item[{[dePinFlag]}]
!          This flag specifies which type of resource DEs are pinned to. 
!          The default is to pin DEs to PETs. Alternatively it is
!          also possible to pin DEs to VASs. See section 
!          \ref{opt:depinflag} for a list of valid pinning options.
!     \item[{[petList]}]
!          List specifying PETs to be used by this DELayout. This can be used
!          to control the PET overlap between DELayouts within the same
!          ESMF component. It is erroneous to specify PETs that are not within 
!          the provided VM context. The default is to include all the PETs of
!          the VM.
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
    type(ESMF_DELayout)     :: delayout     ! opaque pointer to new C++ DELayout  
    type(ESMF_InterfaceInt) :: deGroupingArg
    type(ESMF_InterfaceInt) :: petListArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with optional array arguments
    deGroupingArg = ESMF_InterfaceIntCreate(deGrouping, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    petListArg = ESMF_InterfaceIntCreate(petList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark this DELayout as invalid
    delayout%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DELayoutCreateDefault(delayout, deCount, deGroupingArg, &
      dePinFlag, petListArg, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DELayoutCreateDefault = delayout 
    
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(deGroupingArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(petListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DELayoutCreateDefault)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DELayoutCreateDefault
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCreateFromPetMap()"
!BOP
! !IROUTINE: ESMF_DELayoutCreate - Create DELayout from petMap

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCreate()
  function ESMF_DELayoutCreateFromPetMap(petMap, dePinFlag, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in)            :: petMap(:)
    type(ESMF_DePinFlag),         intent(in), optional  :: dePinFlag
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DELayout) :: ESMF_DELayoutCreateFromPetMap
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DELayout} with exactly specified DE to PET mapping.
!
!     This ESMF method must be called in unison by all PETs of the VM. Calling
!     this method from a PET not part of the VM or not calling it from a PET
!     that is part of the VM will result in undefined behavior. ESMF does not
!     guard against violation of the unison requirement. The call is not
!     collective, there is no communication between PETs.
!
!     The arguments are:
!     \begin{description}
!     \item[petMap]
!          List specifying the DE-to-PET mapping. The list elements correspond 
!          to DE 0, 1, 2, ... and map against the specified PET of the VM
!          context. The size of the {\tt petMap} 
!          argument determines the number of DEs in the created DELayout. It is
!          erroneous to specify a PET identifyer that lies outside the VM 
!          context.
!     \item[{[dePinFlag]}]
!          This flag specifies which type of resource DEs are pinned to. 
!          The default is to pin DEs to PETs. Alternatively it is
!          also possible to pin DEs to VASs. See section 
!          \ref{opt:depinflag} for a list of valid pinning options.
!     \item[{[vm]}]
!          Optional {\tt ESMF\_VM} object. The VM of the current context is the
!          typical and default value.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DELayout)     :: delayout     ! opaque pointer to new C++ DELayout  
    integer                 :: len_petMap   ! number of elements in petMap

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Set arguments
    len_petMap = size(petMap)

    ! Mark this DELayout as invalid
    delayout%this = ESMF_NULL_POINTER

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DELayoutCreateFromPetMap(delayout, petMap(1), len_petMap, &
      dePinFlag, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DELayoutCreateFromPetMap = delayout 
    
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DELayoutCreateFromPetMap)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DELayoutCreateFromPetMap
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCreateHintWeights()"
!BOP
! !IROUTINE: ESMF_DELayoutCreate - Create DELayout with weight hints

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCreate()
  function ESMF_DELayoutCreateHintWeights(deCount, compWeights, commWeights, &
    deGrouping, dePinFlag, petList, vm, rc)
!
! !ARGUMENTS:
    integer,                      intent(in), optional  :: deCount
    integer,                      intent(in)            :: compWeights(:)
    integer,                      intent(in)            :: commWeights(:,:)
    integer, target,              intent(in), optional  :: deGrouping(:)
    type(ESMF_DePinFlag),         intent(in), optional  :: dePinFlag
    integer, target,              intent(in), optional  :: petList(:)
    type(ESMF_VM),                intent(in), optional  :: vm
    integer,                      intent(out),optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DELayout) :: ESMF_DELayoutCreateHintWeights
!
! !DESCRIPTION:
!     Create an {\tt ESMF\_DELayout} on the basis of computational and 
!     communication weights. In addition this call provides control over the 
!     number of DEs, DE domains, DE pinning and the PETs to 
!     map against.
!
!     The arguments are:
!     \begin{description}
!     \item[{[deCount]}]
!          Number of DEs to be provided by the created DELayout. By default
!          the number of DEs equals the number of PETs in the associated VM
!          context. Specifying a {\tt deCount} smaller than the number
!          of PETs will result in unassociated PETs.
!          This may be used to share VM resources between DELayouts within the
!          same ESMF component. Specifying a {\tt deCount} greater than the 
!          number of PETs will result in multiple DE to PET mapping.
!     \item[compWeights]
!          This argument provides the computational weight hint. The 
!          {\tt compWeights} list must contain at least {\tt deCount} elements
!          and specifies a relative meassure of the computatial weight for each
!          DE in form of an integer number. The weights are a relative measure
!          and only meaningful when compared to weights of the same DELayout.
!     \item[commWeights]
!          This argument provides the communication weight hint.
!          {\tt commWeights} is a 2D array and must contain at least 
!          {\tt deCount} elements in each dimension. The element indices 
!          correspond to the DEs of the DELayout and each element specifies a
!          relative communication weight for a DE pair. The {\tt commWeight} 
!          matrix must be symmetric and diagonal elements are ignored. The 
!          weights are a relative measure and only meaningful when compared to 
!          weights of the same DELayout.
!     \item[{[deStride]}]
!          This optional argument can be used to specify DE domains.
!          The argument holds two elements: {\tt (/interStride, intraStride/)}
!          which are used to form DE subsets from the full set 
!          {\tt\{ 0, 1, ..., deCount-1 \}} of DEs. The elements of the {\tt k}th 
!          subset are {\tt\{ k * interStride + i * intraStride \}} where {\tt i}
!          and {\tt k} start at {\tt 0}. DEs within each subset are mapped 
!          against the same PET, causing multiple DE to PET mapping. The default
!          is to generate homogeneously sized, blocked DE domains. If the number
!          of DEs is a multiple of the number of PETs the default can be 
!          expressed as {\tt deStride = (/deCount/petCount, 1/)}.
!     \item[{[dePinFlag]}]
!          This flag specifies which type of resource DEs are pinned to. 
!          The default is to pin DEs to PETs. Alternatively it is
!          also possible to pin DEs to VASs. See section 
!          \ref{opt:depinflag} for a list of valid pinning options.
!     \item[{[petList]}]
!          List specifying PETs to be used by this DELayout. This can be used
!          to control the PET overlap between DELayouts within the same
!          ESMF component. It is erroneous to specify PETs that are not within 
!          the provided VM context. The default is to include all the PETs of
!          the VM.
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
    type(ESMF_DELayout)     :: delayout     ! opaque pointer to new C++ DELayout  
    type(ESMF_InterfaceInt) :: deGroupingArg
    type(ESMF_InterfaceInt) :: petListArg

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vm, rc)
    
    ! Deal with optional array arguments
    deGroupingArg = ESMF_InterfaceIntCreate(deGrouping, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    petListArg = ESMF_InterfaceIntCreate(petList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Mark this DELayout as invalid
    delayout%this = ESMF_NULL_POINTER

  !TODO: use the correct C++ implementation once it is available

    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DELayoutCreateDefault(delayout, deCount, deGroupingArg, &
      dePinFlag, petListArg, vm, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DELayoutCreateHintWeights = delayout 
 
    ! Garbage collection
    call ESMF_InterfaceIntDestroy(deGroupingArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(petListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DELayoutCreateHintWeights)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DELayoutCreateHintWeights
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCreateDeprecated()"
!BOPI
! !IROUTINE: ESMF_DELayoutCreate - Create N-dimensional logically rectangular DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCreate()
  function ESMF_DELayoutCreateDeprecated(vmObject, deCountList, petList, &
    connectionWeightDimList, cyclicFlagDimList, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)              :: vmObject
    integer, target,    intent(in),   optional  :: deCountList(:)
    integer, target,    intent(in),   optional  :: petList(:)
    integer,            intent(in),   optional  :: connectionWeightDimList(:)
    type(ESMF_Logical), intent(in),   optional  :: cyclicFlagDimList(:)
    integer,            intent(out),  optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DELayout) :: ESMF_DELayoutCreateDeprecated
!
! !DESCRIPTION:
!     Create an N-dimensional, logically rectangular {\tt ESMF\_DELayout}.
!     Depending on the optional argument {\tt deCountList} there are two cases
!     that can be distinguished:
!     \begin{itemize}
!     \item If {\tt deCountList} is missing the method will create a 
!           1-dimensional 1:1 DE-to-PET layout with as many DEs as there 
!           are PETs in the VM.
!     \item If {\tt deCountList} is present the method will create an
!           N-dimensional layout, where N is equal to the the size of {\tt
!           deCountList}. The number of DEs will be {\tt deCountList(1)}
!           $\times$
!           {\tt deCountList(2)} $\times$ ... $\times$ {\tt deCountList(N)}.
!           The DE labeling sequence follows column major order for the
!           {\tt deCountList} argument. For example {\tt deCountList=(/2, 3/)}
!           would result in the following DE labels:
!         \begin{verbatim}
!         --------------> 2nd dimension
!         | +---+---+---+
!         | | 0 | 2 | 4 |
!         | +---+---+---+
!         | | 1 | 3 | 5 |
!         | +---+---+---+
!         |
!         v
!         1st dimension
!         \end{verbatim}
!     \end{itemize}
!
!     In either case, if the {\tt petList} argument is given and its size is 
!     equal to the number of DEs in the created {\tt ESMF\_DELayout}, it will 
!     be used to determine the DE-to-PET mapping. The list elements correspond 
!     to DE 0, 1, 2, ... and assign the specified PET to the respective DE. If 
!     {\tt petList} is not present, or is of incompatible size, a default 
!     DE-to-PET mapping will be chosen.
!
!     The {\tt connectionWeightDimList} argument, if present, must have N
!     entries which will be used to ascribe connection weights along each
!     dimension within the {\tt ESMF\_DELayout}. These weights have values from
!     0 to 100 and will be used to find the best match between an
!     {\tt ESMF\_DELayout} and the {\tt ESMF\_VM}.
!  
!     The {\tt cyclicFlagDimList} argument allows to enforce cyclic boundaries
!     in each of the dimensions of {\tt ESMF\_DELayout}. If present its size
!     must be equal to the number of DEs in the {\tt ESMF\_DELayout}. ({\it Not 
!     yet implemented feature!}) \newline
!
!     The arguments are:
!     \begin{description}
!     \item[vmObject] 
!          {\tt ESMF\_VM} object of the current component in which the 
!          {\tt ESMF\_DELayout} object shall operate.
!     \item[{[deCountList]}] 
!          List DE count in each dimension.
!     \item[{[petList]}] 
!          List specifying DE-to-PET mapping. The list elements correspond to 
!          DE 0, 1, 2, ... and assign the specified PET to the respective DE.
!     \item[{[connectionWeightDimList]}] 
!          List of connection weights along each dimension.
!     \item[{[cyclicFlagDimList]}]
!          List of flags indicating cyclic boundaries in each dimension.
!          ({\it Not yet implemented feature!})
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DELayout)     :: delayout     ! opaque pointer to new C++ DELayout
    integer                 :: len_deCountList, len_petList
    integer, pointer        :: opt_deCountList(:), opt_petList(:)
    integer, target         :: dummy(1)     ! used to satisfy the C interface...

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vmObject, rc)
    
    ! Mark this DELayout as invalid
    delayout%this = ESMF_NULL_POINTER

    ! Deal with optional array arguments
    if (present(deCountList)) then
      len_deCountList = size(deCountList)
      opt_deCountList => deCountList
    else
      len_deCountList = 0
      opt_deCountList => dummy
    endif
    if (present(petList)) then
      len_petList = size(petList)
      opt_petList => petList
    else
      len_petList = 0
      opt_petList => dummy
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutCreateND(delayout, vmObject, opt_deCountList(1), &
      len_deCountList, opt_petList(1), len_petList, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Set return value
    ESMF_DELayoutCreateDeprecated = delayout 
 
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DELayoutCreateDeprecated)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end function ESMF_DELayoutCreateDeprecated
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutDestroy()"
!BOP
! !IROUTINE: ESMF_DELayoutDestroy - Destroy DELayout object

! !INTERFACE:
  subroutine ESMF_DELayoutDestroy(delayout, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(inout)           :: delayout
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Destroy an {\tt ESMF\_DELayout} object.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          {\tt ESMF\_DELayout} object to be destroyed.
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
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DELayoutDestroy(delayout, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Mark this DELayout as invalid
    delayout%this = ESMF_NULL_POINTER

    ! Set init code
    ESMF_INIT_SET_DELETED(delayout)
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
 
  end subroutine ESMF_DELayoutDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGet()"
!BOP
! !IROUTINE: ESMF_DELayoutGet - Get DELayout internals

! !INTERFACE:
  subroutine ESMF_DELayoutGet(delayout, vm, deCount, petMap, vasMap, &
    compCapacity, commCapacity, oneToOneFlag, dePinFlag, &
    localDeCount, localDeList, vasLocalDeCount, vasLocalDeList, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),          intent(in)              :: delayout
    type(ESMF_VM),                intent(out),  optional  :: vm
    integer,                      intent(out),  optional  :: deCount
    integer, target,              intent(out),  optional  :: petMap(:)
    integer, target,              intent(out),  optional  :: vasMap(:)
    integer, target,              intent(out),  optional  :: compCapacity(:)
    integer, target,              intent(out),  optional  :: commCapacity(:,:)
    type(ESMF_Logical),           intent(out),  optional  :: oneToOneFlag
    type(ESMF_DePinFlag),         intent(out),  optional  :: dePinFlag
    integer,                      intent(out),  optional  :: localDeCount
    integer, target,              intent(out),  optional  :: localDeList(:)
    integer,                      intent(out),  optional  :: vasLocalDeCount
    integer, target,              intent(out),  optional  :: vasLocalDeList(:)
    integer,                      intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Access to DELayout information.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!        Queried {\tt ESMF\_DELayout} object.
!     \item[{[vm]}]
!        Upon return this holds the {\tt ESMF\_VM} object on which the delayout
!        is defined.
!     \item[{[deCount]}]
!        Upon return this holds the total number of DEs.
!     \item[{[petMap]}]
!        Upon return this holds the list of PETs against which the DEs are 
!        mapped. The {\tt petMap} argument must at least be of size
!        {\tt deCount}.
!     \item[{[vasMap]}]
!        Upon return this holds the list of VASs against which the DEs are 
!        mapped. The {\tt vasMap} argument must at least be of size
!        {\tt deCount}.
!     \item[{[compCapacity]}]
!        Upon return this holds a relative measure of the computational
!        capacity for each DE. The {\tt compCapacity} argument must at least be
!        of size {\tt deCount}.
!     \item[{[commCapacity]}]
!        Upon return this holds a relative measure of the communication
!        capacity for each pair of DEs. The {\tt commCapacity} argument is a
!        2D array where each dimension must at least be of size {\tt deCount}.
!     \item[{[oneToOneFlag]}]
!        Upon return this holds {\tt ESMF\_TRUE} if the specified 
!        {\tt ESMF\_DELayout} describes a 1-to-1 mapping between DEs and PETs,
!        {\tt ESMF\_FALSE} otherwise.
!     \item[{[dePinFlag]}]
!        Upon return this flag will indicate the type of DE pinning. 
!        See section \ref{opt:depinflag} for a list of valid pinning 
!        options.
!     \item[{[localDeCount]}]
!        Upon return this holds the number of DEs associated with the local PET.
!     \item[{[localDeList]}]
!        Upon return this holds the list of DEs associated with the local PET.
!        The provided argument must at least be of size {\tt localDeCount}.
!     \item[{[vasLocalDeCount]}]
!        Upon return this holds the number of DEs associated with the local VAS.
!     \item[{[vasLocalDeList]}]
!        Upon return this holds the list of DEs associated with the local VAS.
!        The provided argument must at least be of size {\tt vasLocalDeCount}.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc                ! local return code
    type(ESMF_InterfaceInt) :: petMapArg              ! helper variable
    type(ESMF_InterfaceInt) :: vasMapArg              ! helper variable
    type(ESMF_InterfaceInt) :: localDeListArg         ! helper variable
    type(ESMF_InterfaceInt) :: vasLocalDeListArg      ! helper variable
    
    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Not implemented features
    if (present(compCapacity)) then
      call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, &
          "- compCapacity query not implemented", &
          ESMF_CONTEXT, rc)
      return
    endif
    if (present(commCapacity)) then
      call ESMF_LogMsgSetError(ESMF_RC_NOT_IMPL, &
          "- commCapacity query not implemented", &
          ESMF_CONTEXT, rc)
      return
    endif
    
    ! Deal with (optional) array arguments
    petMapArg = ESMF_InterfaceIntCreate(petMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    vasMapArg = ESMF_InterfaceIntCreate(vasMap, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    localDeListArg = ESMF_InterfaceIntCreate(localDeList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    vasLocalDeListArg = ESMF_InterfaceIntCreate(vasLocalDeList, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DELayoutGet(delayout, vm, deCount, petMapArg, vasMapArg, &
      oneToOneFlag, dePinFlag, localDeCount, localDeListArg, &
      vasLocalDeCount, vasLocalDeListArg, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Set init code for deep C++ objects
    if (present(vm)) then
      call ESMF_VMSetInitCreated(vm, rc=localrc)
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif
    
    ! garbage collection
    call ESMF_InterfaceIntDestroy(petMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(vasMapArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(localDeListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    call ESMF_InterfaceIntDestroy(vasLocalDeListArg, rc=localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DELayoutGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGetDeprecated()"
!BOPI
! !IROUTINE: ESMF_DELayoutGetDeprecated - Get DELayout internals

! !INTERFACE:
  subroutine ESMF_DELayoutGetDeprecated(delayout, deCount, dimCount, localDeCount, &
    localDeList, localDe, oneToOneFlag, logRectFlag, deCountPerDim, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(out),  optional  :: deCount
    integer,              intent(out),  optional  :: dimCount
    integer,              intent(out),  optional  :: localDeCount
    integer, target,      intent(out),  optional  :: localDeList(:)
    integer,              intent(out),  optional  :: localDe
    type(ESMF_Logical),   intent(out),  optional  :: oneToOneFlag
    type(ESMF_Logical),   intent(out),  optional  :: logRectFlag
    integer, target,      intent(out),  optional  :: deCountPerDim(:)
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Get internal decomposion information.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!        Queried {\tt ESMF\_DELayout} object.
!     \item[{[deCount]}]
!        Upon return this holds the total number of DEs.
!     \item[{[dimCount]}]
!        Upon return this holds the number of dimensions in the specified 
!        {\tt ESMF\_DELayout} object's coordinate tuples.
!     \item[{[localDeCount]}]
!        Upon return this holds the number of DEs associated with the local PET.
!     \item[{[localDeList]}]
!        Upon return this holds the list of DEs associated with the local PET.
!     \item[{[localDe]}]
!        Upon return this holds the DE associated with the local PET. If the
!        specified {\tt ESMF\_DELayout} object associates more than one DE
!        with the local PET then the first local DE is returned. If there are
!        no PET-local DEs {\tt localDE} is set to "-1" and error code
!        {\tt ESMF\_RC\_CANNOT\_GET} is returned in {\tt rc}.
!     \item[{[oneToOneFlag]}]
!        Upon return this holds {\tt ESMF\_TRUE} if the specified 
!        {\tt ESMF\_DELayout} object is 1-to-1, {\tt ESMF\_FALSE} otherwise.
!     \item[{[logRectFlag]}]
!        Upon return this holds {\tt ESMF\_TRUE} if the specified 
!        {\tt ESMF\_DELayout} object is logically rectangular, {\tt ESMF\_FALSE}
!        otherwise.
!     \item[{[deCountPerDim]}]
!        If the specified {\tt ESMF\_DELayout} object is logically rectangular
!        then upon return this holds the number of DEs along each dimension.
!        Otherwise {\tt deCountPerDim} is filled with values of "-1" and
!        error code {\tt ESMF\_RC\_CANNOT\_GET} is returned in {\tt rc}.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: len_localDeList, len_deCountPerDim
    integer, pointer        :: opt_localDeList(:), opt_deCountPerDim(:)
    integer, target         :: dummy(1)     ! used to satisfy the C interface...

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Deal with optional array arguments
    if (present(localDeList)) then
      len_localDeList = size(localDeList)
      opt_localDeList => localDeList
    else
      len_localDeList = 0
      opt_localDeList => dummy
    endif
    if (present(deCountPerDim)) then
      len_deCountPerDim = size(deCountPerDim)
      opt_deCountPerDim => deCountPerDim
    else
      len_deCountPerDim = 0
      opt_deCountPerDim => dummy
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutGetDeprecated(delayout, deCount, dimCount, localDeCount, &
      opt_localDeList(1), len_localDeList, localDe, oneToOneFlag, logRectFlag, &
      opt_deCountPerDim(1), len_deCountPerDim, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DELayoutGetDeprecated
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGetDELocalInfo()"
!BOPI
! !IROUTINE: ESMF_DELayoutGetDELocalInfo - Get DE specific DELayout internals

! !INTERFACE:
  subroutine ESMF_DELayoutGetDELocalInfo(delayout, de, coord, connectionCount, &
    connectionList, connectionWeightList, pid, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(in)              :: de
    integer, target,      intent(out),  optional  :: coord(:)
    integer,              intent(out),  optional  :: connectionCount
    integer, target,      intent(out),  optional  :: connectionList(:)
    integer, target,      intent(out),  optional  :: connectionWeightList(:)
    integer,              intent(out),  optional  :: pid
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Get DE specific internal information about the decomposition.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!        Queried {\tt ESMF\_DELayout} object.
!     \item[de]
!        Queried DE id within the specified {\tt ESMF\_DELayout} object.
!     \item[{[coord]}]
!        Upon return this holds the coordinate tuple of the specified DE.
!     \item[{[connectionCount]}]
!        Upon return this holds the number of connections associated with the
!        specified DE.
!     \item[{[connectionList]}]
!        Upon return this holds the list of DEs the specified DE is connected
!        to.
!     \item[{[connectionWeightList]}]
!        Upon return this holds the list of connection weights of all the
!        connections with the specified DE.
!     \item[{[pid]}] 
!          Upon return this holds the virtual address space (VAS) index of the
!          PET that is associated with {\tt de}.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: i, len_coord, len_cde, len_cw
    integer, target         :: dummy(1)     ! used to satisfy the C interface...
    integer, pointer        :: opt_DEcoord(:), opt_DEcde(:), opt_DEcw(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Deal with optional array arguments
    if (present(coord)) then
      len_coord = size(coord)
      opt_DEcoord => coord
    else
      len_coord = 0
      opt_DEcoord => dummy
    endif
    if (present(connectionList)) then
      len_cde = size(connectionList)
      opt_DEcde => connectionList
    else
      len_cde = 0
      opt_DEcde => dummy
    endif
    if (present(connectionWeightList)) then
      len_cw = size(connectionWeightList)
      opt_DEcw => connectionWeightList
    else
      len_cw = 0
      opt_DEcw => dummy
    endif
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutGetDELocalInfo(delayout, de, opt_DEcoord(1), len_coord,&
      opt_DEcde(1), len_cde, opt_DEcw(1), len_cw, connectionCount, pid, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! C -> Fortran correction
    if (present(coord)) then
      do i = 1, len_coord
        coord(i) = coord(i) + 1
      enddo
    endif

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DELayoutGetDELocalInfo
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGetDEMatchDE()"
!BOPI
! !IROUTINE: ESMF_DELayoutGetDEMatchDE - Match virtual address spaces between DELayouts

! !INTERFACE:
  subroutine ESMF_DELayoutGetDEMatchDE(delayout, de, delayoutMatch, &
    deMatchCount, deMatchList, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(in)              :: de
    type(ESMF_DELayout),  intent(in)              :: delayoutMatch
    integer,              intent(out),  optional  :: deMatchCount
    integer, target,      intent(out),  optional  :: deMatchList(:)
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Match the virtual address space of the specified DE in a DELayout with that
!     of the DEs of a second DELayout. The use of this method is crutial when
!     dealing with decomposed data structures that were not defined in the
!     current VM context, i.e. defined in another component.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!        {\tt ESMF\_DELayout} object in which the specified DE is defined.
!     \item[de]
!        Specified DE within delayout, for which to find matching DEs in 
!        delayoutMatch,
!     \item[delayoutMatch] 
!        DELayout object in which to find DEs that match the virtual address
!        space of the specified DE.
!     \item[{[deMatchCount]}]
!        Upon return this holds the number of DEs in delayoutMatch that share
!        virtual address space with the specified DE.
!     \item[{[deMatchList]}]
!        Upon return this holds the list of DEs in delayoutMatch that share
!        virtual address space with the specified DE.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: len_deMatchList
    integer, target         :: dummy(1)     ! used to satisfy the C interface...
    integer, pointer        :: opt_deMatchList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayoutMatch, rc)
    
    ! Deal with optional array arguments
    if (present(deMatchList)) then
      len_deMatchList = size(deMatchList)
      opt_deMatchList => deMatchList
    else
      len_deMatchList = -1 ! indicate not present
      opt_deMatchList => dummy
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutGetDEMatchDE(delayout, de, delayoutMatch, &
      deMatchCount, opt_deMatchList(1), len_deMatchList, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DELayoutGetDEMatchDE
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGetDEMatchPET()"
!BOPI
! !IROUTINE: ESMF_DELayoutGetDEMatchPET - Match virtual address spaces between DELayout and VM

! !INTERFACE:
  subroutine ESMF_DELayoutGetDEMatchPET(delayout, de, vmMatch, &
    petMatchCount, petMatchList, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(in)              :: de
    type(ESMF_VM),        intent(in)              :: vmMatch
    integer,              intent(out),  optional  :: petMatchCount
    integer, target,      intent(out),  optional  :: petMatchList(:)
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Match the virtual address space of the specified DE in a DELayout with that
!     of the PETs of a VM object. The use of this method is crutial when
!     dealing with decomposed data structures that were not defined in the
!     current VM context, i.e. defined in another component.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!        {\tt ESMF\_DELayout} object in which the specified DE is defined.
!     \item[de]
!        Specified DE within delayout, for which to find matching DEs in 
!        delayoutMatch,
!     \item[vmMatch] 
!        VM object in which to find PETs that match the virtual address
!        space of the specified DE.
!     \item[{[petMatchCount]}]
!        Upon return this holds the number of PETs in vmMatch that share
!        virtual address space with the specified DE.
!     \item[{[petMatchList]}]
!        Upon return this holds the list of PETs in vmMatch that share
!        virtual address space with the specified DE.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: len_petMatchList
    integer, target         :: dummy(1)     ! used to satisfy the C interface...
    integer, pointer        :: opt_petMatchList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vmMatch, rc)
    
    ! Deal with optional array arguments
    if (present(petMatchList)) then
      len_petMatchList = size(petMatchList)
      opt_petMatchList => petMatchList
    else
      len_petMatchList = -1 ! indicate not present
      opt_petMatchList => dummy
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutGetDEMatchPET(delayout, de, vmMatch, &
      petMatchCount, opt_petMatchList(1), len_petMatchList, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DELayoutGetDEMatchPET
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutPrint()"
!BOP
! !IROUTINE: ESMF_DELayoutPrint - Print DELayout internals

! !INTERFACE:
  subroutine ESMF_DELayoutPrint(delayout, options, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    character(len=*),     intent(in),   optional  :: options
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Prints internal information about the specified {\tt ESMF\_DELayout} 
!     object to {\tt stdout}.
!
!     Note:  Many {\tt ESMF\_<class>Print} methods are implemented in C++.
!     On some platforms/compilers there is a potential issue with interleaving
!     Fortran and C++ output to {\tt stdout} such that it doesn't appear in
!     the expected order.  If this occurs, it is recommended to use the
!     standard Fortran call {\tt flush(6)} as a workaround until this issue
!     is fixed in a future release. 
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          Specified {\tt ESMF\_DELayout} object.
!     \item[{[options]}] 
!          Print options are not yet supported.
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
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutPrint(delayout, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DELayoutPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutServiceComplete()"
!BOP
! !IROUTINE: ESMF_DELayoutServiceComplete - Close service window

! !INTERFACE:
  recursive subroutine ESMF_DELayoutServiceComplete(delayout, de, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(in)              :: de
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!   The PET who's service offer was accepted for {\tt de} must use 
!   {\tt ESMF\_DELayoutServiceComplete} to close the service window.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          Specified {\tt ESMF\_DELayout} object.
!     \item[de]
!          DE for which to close service window.
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
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DELayoutServiceComplete(delayout, de, localrc)
!TODO: enable LogErr once it is thread-safe
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
      
  end subroutine ESMF_DELayoutServiceComplete
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutServiceOffer()"
!BOP
! !IROUTINE: ESMF_DELayoutServiceOffer - Offer service for a DE in DELayout

! !INTERFACE:
  recursive function ESMF_DELayoutServiceOffer(delayout, de, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(in)              :: de
    integer,              intent(out),  optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DELayoutServiceReply) :: ESMF_DELayoutServiceOffer
!
! !DESCRIPTION:
!     Offer service for a DE in the {\tt ESMF\_DELayout} object. This call
!     together with {\tt ESMF\_DELayoutServiceComplete()} provides the
!     synchronization primitives between the PETs of an ESMF multi-threaded VM
!     necessary for dynamic load balancing via a work queue approach.
!     The calling PET will
!     either receive {\tt ESMF\_DELAYOUT\_SERVICE\_ACCEPT} if the service offer
!     has been accepted by DELayout or {\tt ESMF\_DELAYOUT\_SERVICE\_DENY} if 
!     the service offer was denied. The service offer paradigm is different 
!     from a simple mutex approach in that DELayout keeps track of the number of
!     service offers issued for each DE by each PET and accepts only one PET's 
!     offer for each offer increment. This requires that all PETs use
!     {\tt ESMF\_DELayoutServiceOffer()} in unison.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          Specified {\tt ESMF\_DELayout} object.
!     \item[de]
!          DE for which service is offered by the calling PET.
!     \item[{[rc]}]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    type(ESMF_DELayoutServiceReply) :: reply

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments
    call c_ESMC_DELayoutServiceOffer(delayout, de, reply, localrc)
    ESMF_DELayoutServiceOffer = reply
!TODO: enable LogErr once it is thread-safe
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return
 
    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
      
  end function ESMF_DELayoutServiceOffer
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutValidate()"
!BOP
! !IROUTINE: ESMF_DELayoutValidate - Validate DELayout internals

! !INTERFACE:
  subroutine ESMF_DELayoutValidate(delayout, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Validates that the {\tt delayout} is internally consistent.
!      The method returns an error code if problems are found.  
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          Specified {\tt ESMF\_DELayout} object.
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
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutValidate(delayout, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_DELayoutValidate
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutVASMatch()"
!BOPI
! !IROUTINE: ESMF_DELayoutVASMatch - Match virtual address spaces

! !INTERFACE:
  subroutine ESMF_DELayoutVASMatch(delayout, de, vmMatch, &
    petMatchCount, petMatchList, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(in)              :: de
    type(ESMF_VM),        intent(in)              :: vmMatch
    integer,              intent(out),  optional  :: petMatchCount
    integer, target,      intent(out),  optional  :: petMatchList(:)
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Match the virtual address space of the specified DE in the DELayout with 
!     that of the PETs of a VM object. The use of this method is crutial when
!     dealing with decomposed data structures that were not defined in the
!     current VM context, i.e. defined in another component.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!        {\tt ESMF\_DELayout} object in which the specified DE is defined.
!     \item[de]
!        DE for which to find matching PETs.
!     \item[vmMatch] 
!        VM object in which to find PETs that match the virtual address
!        space of the specified DE.
!     \item[{[petMatchCount]}]
!        Upon return this holds the number of PETs in vmMatch that share
!        virtual address space with the specified DE.
!     \item[{[petMatchList]}]
!        Upon return this holds the list of PETs in vmMatch that share
!        virtual address space with the specified DE.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code
    integer                 :: len_petMatchList
    integer, target         :: dummy(1)     ! used to satisfy the C interface...
    integer, pointer        :: opt_petMatchList(:)

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    ESMF_INIT_CHECK_DEEP(ESMF_VMGetInit, vmMatch, rc)
    
    ! Deal with optional array arguments
    if (present(petMatchList)) then
      len_petMatchList = size(petMatchList)
      opt_petMatchList => petMatchList
    else
      len_petMatchList = 0
      opt_petMatchList => dummy
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutGetDEMatchPET(delayout, de, vmMatch, &
      petMatchCount, opt_petMatchList(1), len_petMatchList, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DELayoutVASMatch
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutSerialize"

!BOPI
! !IROUTINE: ESMF_DELayoutSerialize - Serialize delayout info into a byte stream
!
! !INTERFACE:
  subroutine ESMF_DELayoutSerialize(delayout, buffer, length, offset, rc) 
!
! !ARGUMENTS:
    type(ESMF_DELayout), intent(in) :: delayout 
    integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
    integer, intent(inout) :: length
    integer, intent(inout) :: offset
    integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes an {\tt ESMF\_DELayout} object and adds all the information needed
!      to save the information to a file or recreate the object based on this
!      information.   Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_DELayoutWrite()} and {\tt ESMF\_DELayoutRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [delayout]
!           {\tt ESMF\_DELayout} object to be serialized.
!     \item [buffer]
!           Data buffer which will hold the serialized information.
!     \item [length]
!           Current length of buffer, in bytes.  If the serialization
!           process needs more space it will allocate it and update
!           this length.
!     \item [offset]
!           Current write offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           available byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_DELayoutGetInit, delayout, rc)
    
    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutSerialize(delayout, buffer(1), length, offset, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! return successfully
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DELayoutSerialize
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutDeserialize"

!BOPI
! !IROUTINE: ESMF_DELayoutDeserialize - Deserialize a byte stream into a DELayout
!
! !INTERFACE:
  function ESMF_DELayoutDeserialize(buffer, offset, rc) 
!
! !RETURN VALUE:
    type(ESMF_DELayout) :: ESMF_DELayoutDeserialize   
!
! !ARGUMENTS:
    integer(ESMF_KIND_I4), pointer, dimension(:) :: buffer
    integer, intent(inout) :: offset
    integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Takes a byte-stream buffer and reads the information needed to
!      recreate a DELayout object.  Recursively calls the deserialize routines
!      needed to recreate the subobjects.
!      Expected to be used by {\tt ESMF\_StateReconcile()} and
!      by {\tt ESMF\_DELayoutWrite()} and {\tt ESMF\_DELayoutRead()}.
!
!     The arguments are:
!     \begin{description}
!     \item [buffer]
!           Data buffer which holds the serialized information.
!     \item [offset]
!           Current read offset in the current buffer.  This will be
!           updated by this routine and return pointing to the next
!           unread byte in the buffer.
!     \item [{[rc]}]
!           Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutDeserialize(ESMF_DELayoutDeserialize%this, buffer(1), &
      offset, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_DELayoutDeserialize)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end function ESMF_DELayoutDeserialize
!------------------------------------------------------------------------------


! -------------------------- ESMF-internal method -----------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGetInit"
!BOPI
! !IROUTINE: ESMF_DELayoutGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_DELayoutGetInit(delayout) 
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_DELayoutGetInit   
!
! !ARGUMENTS:
    type(ESMF_DELayout), intent(in), optional :: delayout
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [delayout]
!           DELayout object.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    if (present(delayout)) then
      ESMF_DELayoutGetInit = ESMF_INIT_GET(delayout)
    else
      ESMF_DELayoutGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_DELayoutGetInit
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutSetInitCreated()"
!BOPI
! !IROUTINE: ESMF_DELayoutSetInitCreated - Set DELayout init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_DELayoutSetInitCreated(delayout, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(inout)           :: delayout
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in DELayout object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          Specified {\tt ESMF\_DELayout} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer                 :: localrc      ! local return code

    ! initialize return code; assume routine not implemented
    localrc = ESMF_RC_NOT_IMPL
    if (present(rc)) rc = ESMF_RC_NOT_IMPL
    
    ! Set init code
    ESMF_INIT_SET_CREATED(delayout)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_DELayoutSetInitCreated
!------------------------------------------------------------------------------


end module ESMF_DELayoutMod
