! $Id: ESMF_DELayout.F90,v 1.30 2004/06/08 09:27:16 nscollins Exp $
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
  use ESMF_BaseTypesMod    ! ESMF base class
  use ESMF_BaseMod                          ! ESMF base class
  use ESMF_LogErrMod
  use ESMF_VMMod                            ! ESMF VM
  
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
  end type

!------------------------------------------------------------------------------

  ! type used to define DELayout data arrays
  type ESMF_I4_AP
    integer(ESMF_KIND_I4), pointer:: ap(:)
  end type

!------------------------------------------------------------------------------

  ! type used to define DELayout data arrays
  type ESMF_R4_AP
    real(ESMF_KIND_R4), pointer:: ap(:)
  end type

!------------------------------------------------------------------------------

  ! type used to define DELayout data arrays
  type ESMF_R8_AP
    real(ESMF_KIND_R8), pointer:: ap(:)
  end type

!------------------------------------------------------------------------------

  ! type to hold ESMF_DELayout data
  type ESMF_DELayoutData
    sequence
    private
    type(ESMF_Pointer):: this     ! C pointer to pointer vector
    integer:: n                   ! number of pointers in pointer vector
    integer, pointer:: len(:)     ! number of elements in each element
    type(ESMF_DataKind):: dtk     ! type and kind of data
  end type

!------------------------------------------------------------------------------

  integer(ESMF_KIND_I4), parameter:: ESMF_CWGHT_NORMAL = 50 !default

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_DELayout
  public ESMF_I4_AP
  public ESMF_R4_AP
  public ESMF_R8_AP
  public ESMF_DELayoutData
      
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
  public ESMF_DELayoutGetDE
  public ESMF_DELayoutGetDEMatch
  
  public ESMF_DELayoutPrint
      
! - ESMF-private methods:
  public ESMF_DELayoutAllGlobalReduce
  public ESMF_DELayoutCopy
  public ESMF_DELayoutExchange
  public ESMF_DELayoutGather
  public ESMF_DELayoutScatter
  
  public ESMF_DELayoutWait

  public ESMF_DELayoutDataCreate
  public ESMF_DELayoutDataDestroy

!EOPI
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DELayout.F90,v 1.30 2004/06/08 09:27:16 nscollins Exp $'

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
      module procedure ESMF_DELayoutCreateND

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutCreate} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-private method ------------------------------
!BOPI
! !IROUTINE: ESMF_DELayoutAllGlobalReduce -- Generic interface

! !INTERFACE:
      interface ESMF_DELayoutAllGlobalReduce

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_DELayoutAllGlobReduceGenI4
      module procedure ESMF_DELayoutAllGlobReduceGenR4
      module procedure ESMF_DELayoutAllGlobReduceGenR8
      module procedure ESMF_DELayoutAllGlobReduceI4
      module procedure ESMF_DELayoutAllGlobReduceR4
      module procedure ESMF_DELayoutAllGlobReduceR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutAllGlobalReduce} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-private method ------------------------------
!BOPI
! !IROUTINE: ESMF_DELayoutCopy -- Generic interface

! !INTERFACE:
      interface ESMF_DELayoutCopy

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_DELayoutCopyGeneral
      module procedure ESMF_DELayoutCopyI4
      module procedure ESMF_DELayoutCopyR4
      module procedure ESMF_DELayoutCopyR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutCopy} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-private method ------------------------------
!BOPI
! !IROUTINE: ESMF_DELayoutExchange -- Generic interface

! !INTERFACE:
      interface ESMF_DELayoutExchange

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_DELayoutExchangeGeneral
      module procedure ESMF_DELayoutExchangeI4
      module procedure ESMF_DELayoutExchangeR4
      module procedure ESMF_DELayoutExchangeR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutExchange} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-private method ------------------------------
!BOPI
! !IROUTINE: ESMF_DELayoutGather -- Generic interface

! !INTERFACE:
      interface ESMF_DELayoutGather

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_DELayoutGatherGeneral
      module procedure ESMF_DELayoutGatherI4
      module procedure ESMF_DELayoutGatherR4
      module procedure ESMF_DELayoutGatherR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutGather} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-private method ------------------------------
!BOPI
! !IROUTINE: ESMF_DELayoutScatter -- Generic interface

! !INTERFACE:
      interface ESMF_DELayoutScatter

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_DELayoutScatterGeneral
      module procedure ESMF_DELayoutScatterI4
      module procedure ESMF_DELayoutScatterR4
      module procedure ESMF_DELayoutScatterR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutScatter} functions.   
!EOPI 
      end interface

! -------------------------- ESMF-private method ------------------------------
!BOPI
! !IROUTINE: ESMF_DELayoutDataCreate -- Generic interface

! !INTERFACE:
      interface ESMF_DELayoutDataCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_DELayoutDataCreateI4
      module procedure ESMF_DELayoutDataCreateR4
      module procedure ESMF_DELayoutDataCreateR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutDataCreate} functions.   
!EOPI 
      end interface

!==============================================================================
      

contains


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCreateND()"
!BOP
! !IROUTINE: ESMF_DELayoutCreate - Create N-dimensional DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCreate()
  function ESMF_DELayoutCreateND(vm, deCountList, dePetList, &
    connectionWeightDimList, cyclicFlagDimList, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)              :: vm
    integer, target,    intent(in),   optional  :: deCountList(:)
    integer, target,    intent(in),   optional  :: dePetList(:)
    integer,            intent(in),   optional  :: connectionWeightDimList(:)
    type(ESMF_Logical), intent(in),   optional  :: cyclicFlagDimList(:)
    integer,            intent(out),  optional  :: rc
!         
! !RETURN VALUE:
    type(ESMF_DELayout) :: ESMF_DELayoutCreateND
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
!           {\tt deCountList(2)}$\times$...$\times${\tt deCountList(N)}.
!     \end{itemize}
!
!     In either case, if {\tt dePetList} is given and its size is equal to the
!     number of DEs in the created {\tt ESMF\_DELayout}, it will be used to
!     determine the DE-to-PET mapping. Otherwise a DE-to-PET mapping will be
!     determined automatically.
!
!     The {\tt connectionWeightDimList} argument, if present, must have N
!     entries which will be used to ascribe connection weights along each
!     dimension within the {\tt ESMF\_DELayout}. These weights have values from
!     0 to 100 and will be used to find the best match between an
!     {\tt ESMF\_DELayout} and the {\tt ESMF\_VM}.
!  
!     The {\tt cyclicFlagDimList} argument allows to enforce cyclic boundaries
!     in each of the dimensions of {\tt ESMF\_DELayout}. If present its size
!     must be equal to the number of DEs in the {\tt ESMF\_DELayout}.\newline
!
!     The arguments are:
!     \begin{description}
!     \item[vm] 
!          {\tt ESMF\_VM} object of the current component in which the 
!          {\tt ESMF\_DELayout} object shall operate.
!     \item[{[deCountList]}] 
!          List DE count in each dimension.
!     \item[{[dePetList]}] 
!          List specifying DE-to-PET mapping.
!     \item[{[connectionWeightDimList]}] 
!          List of connection weights along each dimension.
!     \item[{[cyclicFlagDimList]}] 
!          List of flags indicating cyclic boundaries in each dimension.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    type(ESMF_DELayout):: delayout  ! opaque pointer to new C++ DELayout
    integer :: len_deCountList, len_dePetList
    integer, pointer :: opt_deCountList(:), opt_dePetList(:)
    integer, target :: dummy(0)     ! used to satisfy the C interface...

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Initialize the pointer to NULL
    delayout%this = ESMF_NULL_POINTER

    ! Deal with optional array arguments
    ! TODO: make sure that this pointer, target stuff is a portable way of 
    ! TODO: dealing with multiple optional arrays and the F90/C++ interface.
    if (present(deCountList)) then
      len_deCountList = size(deCountList)
      opt_deCountList => deCountList
    else
      len_deCountList = 0
      opt_deCountList => dummy
    endif
    if (present(dePetList)) then
      len_dePetList = size(dePetList)
      opt_dePetList => dePetList
    else
      len_dePetList = 0
      opt_dePetList => dummy
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutCreate(delayout, vm, opt_deCountList, len_deCountList, &
      opt_dePetList, len_dePetList, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
    
    ! set return value
    ESMF_DELayoutCreateND = delayout 
 
  end function ESMF_DELayoutCreateND
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutDestroy()"
!BOP
! !IROUTINE: ESMF_DELayoutDestroy - Destroy a DELayout object

! !INTERFACE:
  subroutine ESMF_DELayoutDestroy(delayout, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
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
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutDestroy(delayout, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
  end subroutine ESMF_DELayoutDestroy
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGet()"
!BOP
! !IROUTINE: ESMF_DELayoutGet - Get internal info

! !INTERFACE:
  subroutine ESMF_DELayoutGet(delayout, deCount, dimCount, localDeCount, &
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
!     Get internal info
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[{[deCount]}]
!          Total number of DEs in layout.
!     \item[{[dimCount]}]
!          Number of dimensions in coordinate tuple.
!     \item[{[localDeCount]}]
!          Number of DEs associated with this PET instance.
!     \item[{[localDeList]}]
!          List of DEs associated with this PET instance.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: len_localDeList, len_deCountPerDim
    integer, pointer :: opt_localDeList(:), opt_deCountPerDim(:)
    integer, target :: dummy(0)     ! used to satisfy the C interface...
    
    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Deal with optional array arguments
    ! TODO: make sure that this pointer, target stuff is a portable way of 
    ! TODO: dealing with multiple optional arrays and the F90/C++ interface.
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
    call c_ESMC_DELayoutGet(delayout, deCount, dimCount, localDeCount, &
      opt_localDeList, len_localDeList, localDe, oneToOneFlag, logRectFlag, &
      opt_deCountPerDim, len_deCountPerDim, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutGet
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGetDE()"
!BOP
! !IROUTINE: ESMF_DELayoutGetDE - Get DE specific internal info

! !INTERFACE:
  subroutine ESMF_DELayoutGetDE(delayout, de, coord, connectionCount, &
    connectionList, connectionWeightList, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),  intent(in)              :: delayout
    integer,              intent(in)              :: de
    integer, target,      intent(out),  optional  :: coord(:)
    integer,              intent(out),  optional  :: connectionCount
    integer, target,      intent(out),  optional  :: connectionList(:)
    integer, target,      intent(out),  optional  :: connectionWeightList(:)
    integer,              intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Get DE specific internal info
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[de]
!          Id uniquely specifying the DE within this DELayout.
!     \item[{[coord]}]
!          Coordinate tuple of the specified DE.
!     \item[{[connectionCount]}]
!          Number of connections associated with the specified DE.
!     \item[{[connectionList]}]
!          List of DEs to which the specified DE is connected to.
!     \item[{[connectionWeightList]}]
!          List of connection weights of all the connections with the specified
!          DE.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: i, len_coord, len_cde, len_cw
    integer, target :: dummy(0)     ! used to satisfy the C interface...
    integer, pointer:: opt_DEcoord(:), opt_DEcde(:), opt_DEcw(:)

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Deal with optional array arguments
    ! TODO: make sure that this pointer, target stuff is a portable way of 
    ! TODO: dealing with multiple optional arrays and the F90/C++ interface.
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
    call c_ESMC_DELayoutGetDE(delayout, de, opt_DEcoord, len_coord, &
      opt_DEcde, len_cde, opt_DEcw, len_cw, connectionCount, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! C -> Fortran correction
    if (present(coord)) then
      do i = 1, len_coord
        coord(i) = coord(i) + 1
      enddo
    endif

  end subroutine ESMF_DELayoutGetDE
!------------------------------------------------------------------------------

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGetDEMatch()"
!BOP
! !IROUTINE: ESMF_DELayoutGetDEMatch - Match DE's between DELayouts

! !INTERFACE:
  subroutine ESMF_DELayoutGetDEMatch(delayout, de, delayoutMatch, &
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
!     Get DE specific internal info
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[de]
!          Id uniquely specifying the DE within this DELayout.
!     \item[delayoutMatch] 
!          DELayout object to find matching DEs in.
!     \item[{[deMatchCount]}]
!          Number of DEs in delayoutMatch to share virtual memory space with de.
!     \item[{[deMatchList]}]
!          List of DEs in delayoutMatch that share virtual memory space with de.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: len_deMatchList
    integer, target :: dummy(0)     ! used to satisfy the C interface...
    integer, pointer:: opt_deMatchList(:)

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Deal with optional array arguments
    ! TODO: make sure that this pointer, target stuff is a portable way of 
    ! TODO: dealing with multiple optional arrays and the F90/C++ interface.
    if (present(deMatchList)) then
      len_deMatchList = size(deMatchList)
      opt_deMatchList => deMatchList
    else
      len_deMatchList = 0
      opt_deMatchList => dummy
    endif

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutGetDEMatch(delayout, de, delayoutMatch, &
      deMatchCount, opt_deMatchList, len_deMatchList, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutGetDEMatch
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutPrint()"
!BOP
! !IROUTINE: ESMF_DELayoutPrint - Print details of DELayout object

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
!     Print details of DELayout object
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[{[options]}] 
!          Output options.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutPrint(delayout, localrc)
    
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutPrint
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutAllGlobReduceGenI4()"
!BOPI
! !IROUTINE: ESMF_DELayoutAllGlobalReduce - Reduce I4 data on a general DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutAllGlobalReduce()
  subroutine ESMF_DELayoutAllGlobReduceGenI4(delayout, srcData, &
    dstData, count, reduceFlag, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),      intent(in)              :: delayout
    type(ESMF_DELayoutData),  intent(in)              :: srcData
    integer,                  intent(out)             :: dstData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceFlag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle    
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Reduce I4 data to a single value.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements per DE to be considered.
!     \item[reduceFlag] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutAllGlobReduce(delayout, srcData, dstData, &
      count, ESMF_I4, reduceFlag, ESMF_FALSE, localrc)
      
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
  end subroutine ESMF_DELayoutAllGlobReduceGenI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutAllGlobReduceGenR4()"
!BOPI
! !IROUTINE: ESMF_DELayoutAllGlobalReduce - Reduce R4 data on a general DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutAllGlobalReduce()
  subroutine ESMF_DELayoutAllGlobReduceGenR4(delayout, srcData, &
    dstData, count, reduceFlag, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),      intent(in)              :: delayout
    type(ESMF_DELayoutData),  intent(in)              :: srcData
    real(ESMF_KIND_R4),       intent(out)             :: dstData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceFlag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Reduce R4 data to a single value
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements per DE to be considered.
!     \item[reduceFlag] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutAllGlobReduce(delayout, srcData, dstData, &
      count, ESMF_R4, reduceFlag, ESMF_FALSE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
  end subroutine ESMF_DELayoutAllGlobReduceGenR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutAllGlobReduceGenR8()"
!BOPI
! !IROUTINE: ESMF_DELayoutAllGlobalReduce - Reduce R8 data on a general DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutAllGlobalReduce()
  subroutine ESMF_DELayoutAllGlobReduceGenR8(delayout, srcData, &
    dstData, count, reduceFlag,  blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),      intent(in)              :: delayout
    type(ESMF_DELayoutData),  intent(in)              :: srcData
    real(ESMF_KIND_R8),       intent(out)             :: dstData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceFlag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Reduce R8 data to a single value
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements per DE to be considered.
!     \item[reduceFlag] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutAllGlobReduce(delayout, srcData, dstData, &
      count, ESMF_R8, reduceFlag, ESMF_FALSE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
  end subroutine ESMF_DELayoutAllGlobReduceGenR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutAllGlobReduceI4()"
!BOPI
! !IROUTINE: ESMF_DELayoutAllGlobalReduce - Reduce I4 data on a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutAllGlobalReduce()
  subroutine ESMF_DELayoutAllGlobReduceI4(delayout, srcData, &
    dstData, count, reduceFlag, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),      intent(in)              :: delayout
    integer(ESMF_KIND_I4),    intent(in)              :: srcData(:)
    integer,                  intent(out)             :: dstData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceFlag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle    
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Reduce I4 data to a single value
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements per DE to be considered.
!     \item[reduceFlag] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutAllGlobReduce(delayout, srcData, dstData, &
      count, ESMF_I4, reduceFlag, ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
  end subroutine ESMF_DELayoutAllGlobReduceI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutAllGlobReduceR4()"
!BOPI
! !IROUTINE: ESMF_DELayoutAllGlobalReduce - Reduce R4 data on a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutAllGlobalReduce()
  subroutine ESMF_DELayoutAllGlobReduceR4(delayout, srcData, &
    dstData, count, reduceFlag, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),      intent(in)              :: delayout
    real(ESMF_KIND_R4),       intent(in)              :: srcData(:)
    real(ESMF_KIND_R4),       intent(out)             :: dstData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceFlag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle    
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Reduce R4 data to a single value
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements per DE to be considered.
!     \item[reduceFlag] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutAllGlobReduce(delayout, srcData, dstData, &
      count, ESMF_R4, reduceFlag, ESMF_TRUE, localrc)
      
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
  end subroutine ESMF_DELayoutAllGlobReduceR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutAllGlobReduceR8()"
!BOPI
! !IROUTINE: ESMF_DELayoutAllGlobalReduce - Reduce R8 data on a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutAllGlobalReduce()
  subroutine ESMF_DELayoutAllGlobReduceR8(delayout, srcData, &
    dstData, count, reduceFlag, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),      intent(in)              :: delayout
    real(ESMF_KIND_R8),       intent(in)              :: srcData(:)
    real(ESMF_KIND_R8),       intent(out)             :: dstData
    integer,                  intent(in)              :: count
    type(ESMF_ReduceFlag),    intent(in)              :: reduceFlag
    type(ESMF_BlockingFlag),  intent(in),   optional  :: blockingFlag
    type(ESMF_CommHandle),    intent(out),  optional  :: commHandle    
    integer,                  intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Reduce R4 data to a single value
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements per DE to be considered.
!     \item[reduceFlag] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Call into the C++ interface, which will sort out optional arguments.
    call c_ESMC_DELayoutAllGlobReduce(delayout, srcData, dstData, &
      count, ESMF_R8, reduceFlag, ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
 
  end subroutine ESMF_DELayoutAllGlobReduceR8
!------------------------------------------------------------------------------

        
! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGatherGeneral()"
!BOPI
! !IROUTINE: ESMF_DELayoutGather - Gather data on a general DELayout

! !INTERFACE:
  subroutine ESMF_DELayoutGatherGeneral(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    type(ESMF_DELayoutData),       intent(out)           :: dstData
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Gather
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements gathered from each DE.
!     \item[root] 
!          Root DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    if (srcData%dtk == ESMF_I4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R8) blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutGather(delayout, srcData, dstData, blen, root, &
      ESMF_FALSE, localrc)
      
    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine 
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGatherI4()"
!BOPI
! !IROUTINE: ESMF_DELayoutGather - Gather I4 data on a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutGather()
  subroutine ESMF_DELayoutGatherI4(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    integer(ESMF_KIND_I4),         intent(in)            :: srcData(:)
    integer(ESMF_KIND_I4),         intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Gather
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements gathered from each DE.
!     \item[root] 
!          Root DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutGather(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutGatherI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGatherR4()"
!BOPI
! !IROUTINE: ESMF_DELayoutGather - Gather R4 data on a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutGather()
  subroutine ESMF_DELayoutGatherR4(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    real(ESMF_KIND_R4),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Gather
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements gathered from each DE.
!     \item[root] 
!          Root DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutGather(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutGatherR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutGatherR8()"
!BOPI
! !IROUTINE: ESMF_DELayoutGather - Gather R8 data on a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutGather()
  subroutine ESMF_DELayoutGatherR8(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    real(ESMF_KIND_R8),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Gather
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements gathered from each DE.
!     \item[root] 
!          Root DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutGather(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutGatherR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutScatterGeneral()"
!BOPI
! !IROUTINE: ESMF_DELayoutScatter - Scatter data on a general DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutScatter()
  subroutine ESMF_DELayoutScatterGeneral(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),        intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    type(ESMF_DELayoutData),       intent(out)           :: dstData
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Scatter
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements scattered to each DE.
!     \item[root] 
!          Root DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Determine the number of bytes that need to be copied dependent on type
    if (srcData%dtk == ESMF_I4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R8) blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutScatter(delayout, srcData, dstData, blen, root, &
      ESMF_FALSE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutScatterGeneral
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutScatterI4()"
!BOPI
! !IROUTINE: ESMF_DELayoutScatter - Scatter I4 data 

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutScatter()
  subroutine ESMF_DELayoutScatterI4(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    integer(ESMF_KIND_I4),         intent(in)            :: srcData(:)
    integer(ESMF_KIND_I4),         intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Scatter
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements scattered to each DE.
!     \item[root] 
!          Root DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutScatter(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

 end subroutine ESMF_DELayoutScatterI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutScatterR4()"
!BOPI
! !IROUTINE: ESMF_DELayoutScatter - Scatter R4 data

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutScatter()
  subroutine ESMF_DELayoutScatterR4(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    real(ESMF_KIND_R4),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Scatter
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements scattered to each DE.
!     \item[root] 
!          Root DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutScatter(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutScatterR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutScatterR8()"
!BOPI
! !IROUTINE: ESMF_DELayoutScatter - Scatter R8 data

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutScatter()
  subroutine ESMF_DELayoutScatterR8(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    real(ESMF_KIND_R8),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Scatter
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements scattered to each DE.
!     \item[root] 
!          Root DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutScatter(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutScatterR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCopyGeneral()"
!BOPI
! !IROUTINE: ESMF_DELayoutCopy - Copy data between DEs in a general DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCopy()
  subroutine ESMF_DELayoutCopyGeneral(delayout, srcData, dstData, count, &
    src, dst, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    type(ESMF_DELayoutData),       intent(out)           :: dstData
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: src
    integer,                       intent(in)            :: dst
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_CommHandle),         intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Copy data between DEs in a DELayout
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements to be copied.
!     \item[src] 
!          Source DE.
!     \item[dst] 
!          Destination DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    if (srcData%dtk == ESMF_I4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R8) blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutCopy(delayout, srcData, dstData, blen, src, dst, &
      ESMF_FALSE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutCopyGeneral

!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCopyI4()"
!BOPI
! !IROUTINE: ESMF_DELayoutCopy - Copy I4 data between DEs in a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCopy()
  subroutine ESMF_DELayoutCopyI4(delayout, srcData, dstData, count, &
    src, dst, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    integer(ESMF_KIND_I4),         intent(in)            :: srcData(:)
    integer(ESMF_KIND_I4),         intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: src
    integer,                       intent(in)            :: dst
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_CommHandle),         intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Copy data between DEs in a DELayout.  The {\tt delayout} must be
!     one-to-one with the PETs in its associated VM.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements to be copied.
!     \item[src] 
!          Source DE.
!     \item[dst] 
!          Destination DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutCopy(delayout, srcData, dstData, blen, src, dst, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutCopyI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCopyR4()"
!BOPI
! !IROUTINE: ESMF_DELayoutCopy - Copy R4 data between DEs in a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCopy()
  subroutine ESMF_DELayoutCopyR4(delayout, srcData, dstData, count, &
    src, dst, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    real(ESMF_KIND_R4),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: src
    integer,                       intent(in)            :: dst
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_CommHandle),         intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Copy data between DEs in a DELayout
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements to be copied.
!     \item[src] 
!          Source DE.
!     \item[dst] 
!          Destination DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutCopy(delayout, srcData, dstData, blen, src, dst, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutCopyR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutCopyR8()"
!BOPI
! !IROUTINE: ESMF_DELayoutCopy - Copy R8 data between DEs in a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutCopy()
  subroutine ESMF_DELayoutCopyR8(delayout, srcData, dstData, count, &
    src, dst, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    real(ESMF_KIND_R8),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: src
    integer,                       intent(in)            :: dst
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_CommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Copy data between DEs in a DELayout
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData] 
!          Source data.
!     \item[dstData] 
!          Destination data.
!     \item[count] 
!          Number of elements to be copied.
!     \item[src] 
!          Source DE.
!     \item[dst] 
!          Destination DE.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 8 ! 8 bytes

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutCopy(delayout, srcData, dstData, blen, src, dst, &
      ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutCopyR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutExchangeGeneral()"
!BOPI
! !IROUTINE: ESMF_DELayoutExchange - Exchange data on a general DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutExchange()
  subroutine ESMF_DELayoutExchangeGeneral(delayout, srcData1, srcData2, &
    dstData1, dstData2, count1, count2, de1, de2, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData1
    type(ESMF_DELayoutData),       intent(in)            :: srcData2
    type(ESMF_DELayoutData),       intent(out)           :: dstData1
    type(ESMF_DELayoutData),       intent(out)           :: dstData2
    integer,                       intent(in)            :: count1
    integer,                       intent(in)            :: count2
    integer,                       intent(in)            :: de1
    integer,                       intent(in)            :: de2
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_CommHandle),         intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Send and receive data between DEs in a DELayout
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData1]
!          Source data reference located on de1
!     \item[srcData2]
!          Source data reference located on de2
!     \item[dstData1]
!          Destination data reference located on de1
!     \item[dstData2]
!          Destination data reference located on de2
!     \item[count1]
!          Number of elements to be copied from de1 to de2
!     \item[count2]
!          Number of elements to be copied from de2 to de1
!     \item[de1]
!          Id of de1.
!     \item[de2]
!          Id of de2.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}]
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen1, blen2

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    if (srcData1%dtk == ESMF_I4) blen1 = count1 * 4 ! 4 bytes
    if (srcData1%dtk == ESMF_R4) blen1 = count1 * 4 ! 4 bytes
    if (srcData1%dtk == ESMF_R8) blen1 = count1 * 8 ! 8 bytes
    if (srcData2%dtk == ESMF_I4) blen2 = count2 * 4 ! 4 bytes
    if (srcData2%dtk == ESMF_R4) blen2 = count2 * 4 ! 4 bytes
    if (srcData2%dtk == ESMF_R8) blen2 = count2 * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutExchange(delayout, srcData1, srcData2, &
      dstData1, dstData2, blen1, blen2, de1, de2, ESMF_FALSE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutExchangeGeneral
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutExchangeI4()"
!BOPI
! !IROUTINE: ESMF_DELayoutExchange - Exchange I4 data on a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutExchange()
  subroutine ESMF_DELayoutExchangeI4(delayout, srcData1, srcData2, &
    dstData1, dstData2, count1, count2, de1, de2, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    integer(ESMF_KIND_I4),         intent(in)            :: srcData1(:)
    integer(ESMF_KIND_I4),         intent(in)            :: srcData2(:)
    integer(ESMF_KIND_I4),         intent(out)           :: dstData1(:)
    integer(ESMF_KIND_I4),         intent(out)           :: dstData2(:)
    integer,                       intent(in)            :: count1
    integer,                       intent(in)            :: count2
    integer,                       intent(in)            :: de1
    integer,                       intent(in)            :: de2
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_CommHandle),         intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Copy data between DEs in a DELayout
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData1]
!          Source data reference located on de1
!     \item[srcData2]
!          Source data reference located on de2
!     \item[dstData1]
!          Destination data reference located on de1
!     \item[dstData2]
!          Destination data reference located on de2
!     \item[count1]
!          Number of elements to be copied from de1 to de2
!     \item[count2]
!          Number of elements to be copied from de2 to de1
!     \item[de1]
!          Id of de1.
!     \item[de2]
!          Id of de2.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}]
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen1, blen2

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    blen1 = count1 * 4 ! 4 bytes
    blen2 = count2 * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutExchange(delayout, srcData1, srcData2, &
      dstData1, dstData2, blen1, blen2, de1, de2, ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutExchangeI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutExchangeR4()"
!BOPI
! !IROUTINE: ESMF_DELayoutExchange - Exchange R4 data on a one-to-one DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutExchange()
  subroutine ESMF_DELayoutExchangeR4(delayout, srcData1, srcData2, &
    dstData1, dstData2, count1, count2, de1, de2, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    real(ESMF_KIND_R4),            intent(in)            :: srcData1(:)
    real(ESMF_KIND_R4),            intent(in)            :: srcData2(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData1(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData2(:)
    integer,                       intent(in)            :: count1
    integer,                       intent(in)            :: count2
    integer,                       intent(in)            :: de1
    integer,                       intent(in)            :: de2
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_CommHandle),         intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Send data between DEs in a DELayout.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData1]
!          Source data reference located on de1
!     \item[srcData2]
!          Source data reference located on de2
!     \item[dstData1]
!          Destination data reference located on de1
!     \item[dstData2]
!          Destination data reference located on de2
!     \item[count1]
!          Number of elements to be copied from de1 to de2
!     \item[count2]
!          Number of elements to be copied from de2 to de1
!     \item[de1]
!          Id of de1.
!     \item[de2]
!          Id of de2.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}]
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen1, blen2

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    blen1 = count1 * 4 ! 4 bytes
    blen2 = count2 * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutExchange(delayout, srcData1, srcData2, &
      dstData1, dstData2, blen1, blen2, de1, de2, ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutExchangeR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutExchangeR8()"
!BOPI
! !IROUTINE: ESMF_DELayoutExchange - Exchange R8 data on a general DELayout

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutExchange()
  subroutine ESMF_DELayoutExchangeR8(delayout, srcData1, srcData2, &
    dstData1, dstData2, count1, count2, de1, de2, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),           intent(in)            :: delayout
    real(ESMF_KIND_R8),            intent(in)            :: srcData1(:)
    real(ESMF_KIND_R8),            intent(in)            :: srcData2(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData1(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData2(:)
    integer,                       intent(in)            :: count1
    integer,                       intent(in)            :: count2
    integer,                       intent(in)            :: de1
    integer,                       intent(in)            :: de2
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_CommHandle),         intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Copy data between DEs in a DELayout
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[srcData1]
!          Source data reference located on de1
!     \item[srcData2]
!          Source data reference located on de2
!     \item[dstData1]
!          Destination data reference located on de1
!     \item[dstData2]
!          Destination data reference located on de2
!     \item[count1]
!          Number of elements to be copied from de1 to de2
!     \item[count2]
!          Number of elements to be copied from de2 to de1
!     \item[de1]
!          Id of de1.
!     \item[de2]
!          Id of de2.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}]
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    integer :: blen1, blen2

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Determine the number of bytes that need to be copied dependent on type
    blen1 = count1 * 8 ! 8 bytes
    blen2 = count2 * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutExchange(delayout, srcData1, srcData2, &
      dstData1, dstData2, blen1, blen2, de1, de2, ESMF_TRUE, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutExchangeR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutWait()"
!BOPI
! !IROUTINE: ESMF_DELayoutWait - Wait for DELayout communication to finish

! !INTERFACE:
  subroutine ESMF_DELayoutWait(delayout, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayout),    intent(in)              :: delayout
    type(ESMF_CommHandle),  intent(in)              :: commHandle
    integer,                intent(out),  optional  :: rc
!         
!
! !DESCRIPTION:
!     Wait for DELayout communication to finish.
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[commHandle] 
!          Handle specifying a previous non-blocking communication request.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Use LogErr to handle return code
!    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
!      ESMF_CONTEXT, rcToReturn=rc)) return
    if (ESMF_LogMsgFoundError(localrc, "Method not implemented", &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
  end subroutine ESMF_DELayoutWait
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutDataCreateI4()"
!BOPI
! !IROUTINE: ESMF_DELayoutDataCreate - Create I4 DELayoutData

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutDataCreate()
  function ESMF_DELayoutDataCreateI4(array, rc)
!
! !ARGUMENTS:
    type(ESMF_I4_AP), intent(in)            :: array(:)
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
      type(ESMF_DELayoutData) :: ESMF_DELayoutDataCreateI4
!
! !DESCRIPTION:
!     Create I4 DELayoutData
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          ESMF\_I4\_AP array.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    type(ESMF_DELayoutData):: mydata    ! new object
    integer :: i

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Initialize the pointer to null.
    mydata%this = ESMF_NULL_POINTER
    
    ! Initialize members
    mydata%n = size(array)          ! number of pointers in the pointer array
    allocate(mydata%len(mydata%n))  
    do i=1, mydata%n
      mydata%len(i) = size(array(i)%ap)! number of elments for this pointer el.
    enddo
    mydata%dtk = ESMF_I4    !data type and kind
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutDataCreate(mydata, mydata%n, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    do i=1, mydata%n
      ! Routine which interfaces to the C++ creation routine.
      call c_ESMC_DELayoutDataAdd(mydata, array(i)%ap, i, localrc)

      ! Use LogErr to handle return code
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo

    ! set return values
    ESMF_DELayoutDataCreateI4 = mydata 
 
  end function ESMF_DELayoutDataCreateI4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutDataCreateR4()"
!BOPI
! !IROUTINE: ESMF_DELayoutDataCreate - Create R4 DELayoutData

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutDataCreate()
  function ESMF_DELayoutDataCreateR4(array, rc)
!
! !ARGUMENTS:
    type(ESMF_R4_AP), intent(in)            :: array(:)
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
      type(ESMF_DELayoutData) :: ESMF_DELayoutDataCreateR4
!
! !DESCRIPTION:
!     Create R4 DELayoutData
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          ESMF\_R4\_AP array.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    type(ESMF_DELayoutData):: mydata    ! new object
    integer :: i

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Initialize the pointer to null.
    mydata%this = ESMF_NULL_POINTER
    
    ! Initialize members
    mydata%n = size(array)          ! number of pointers in the pointer array
    allocate(mydata%len(mydata%n))  
    do i=1, mydata%n
      mydata%len(i) = size(array(i)%ap)! number of elments for this pointer el.
    enddo
    mydata%dtk = ESMF_R4    !data type and kind
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutDataCreate(mydata, mydata%n, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    do i=1, mydata%n
      ! Routine which interfaces to the C++ creation routine.
      call c_ESMC_DELayoutDataAdd(mydata, array(i)%ap, i, localrc)

      ! Use LogErr to handle return code
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo

    ! set return values
    ESMF_DELayoutDataCreateR4 = mydata 
 
  end function ESMF_DELayoutDataCreateR4
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutDataCreateR8()"
!BOPI
! !IROUTINE: ESMF_DELayoutDataCreate - Create R8 DELayoutData

! !INTERFACE:
  ! Private name; call using ESMF_DELayoutDataCreate()
  function ESMF_DELayoutDataCreateR8(array, rc)
!
! !ARGUMENTS:
    type(ESMF_R8_AP), intent(in)            :: array(:)
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
      type(ESMF_DELayoutData) :: ESMF_DELayoutDataCreateR8
!
! !DESCRIPTION:
!     Create R8 DELayoutData
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          ESMF\_R8\_AP array.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    type(ESMF_DELayoutData):: mydata    ! new object
    integer :: i

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Initialize the pointer to null.
    mydata%this = ESMF_NULL_POINTER
    
    ! Initialize members
    mydata%n = size(array)             ! number of pointers in the pointer array
    allocate(mydata%len(mydata%n))  
    do i=1, mydata%n
      mydata%len(i) = size(array(i)%ap)! number of elments for this pointer el.
    enddo
    mydata%dtk = ESMF_R8    !data type and kind
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutDataCreate(mydata, mydata%n, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    do i=1, mydata%n
      ! Routine which interfaces to the C++ creation routine.
      call c_ESMC_DELayoutDataAdd(mydata, array(i)%ap, i, localrc)

      ! Use LogErr to handle return code
      if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
        
    enddo

    ! set return values
    ESMF_DELayoutDataCreateR8 = mydata 
 
  end function ESMF_DELayoutDataCreateR8
!------------------------------------------------------------------------------


! -------------------------- ESMF-private method ------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_DELayoutDataDestroy()"
!BOPI
! !IROUTINE: ESMF_DELayoutDataDestroy - Destroy a DELayoutData object

! !INTERFACE:
  subroutine ESMF_DELayoutDataDestroy(delayoutData, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayoutData), intent(inout)         ::  delayoutData 
    integer,                 intent(out), optional ::  rc  
!         
!
! !DESCRIPTION:
!     Destroy a DELayoutData object
!
!     The arguments are:
!     \begin{description}
!     \item[delayoutData] 
!          ESMF\_DELayoutData object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_DELayoutDataDestroy(delayoutData, localrc)

    ! Use LogErr to handle return code
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

  end subroutine ESMF_DELayoutDataDestroy
!------------------------------------------------------------------------------


end module ESMF_DELayoutMod
