! $Id: ESMF_newDELayout.F90,v 1.19 2004/04/08 15:15:51 theurich Exp $
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
!
!     ESMF newDELayout Module
      module ESMF_newDELayoutMod
!
!==============================================================================
!
! This file contains the F90 wrapper code for the C++ implementation of
!  the newDELayout class.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"

!==============================================================================
!BOP
! !MODULE: ESMF_newDELayoutMod
!

!   F90 API wrapper of C++ implemenation of newDELayout
!
!------------------------------------------------------------------------------

! !USES:
      use ESMF_BaseMod                          ! ESMF base class
      use ESMF_VMMod                            ! ESMF VM
      
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
      
!------------------------------------------------------------------------------
!     ! ESMF_newDELayout
!
!     ! F90 class type to hold pointer to C++ object
      type ESMF_newDELayout
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

      ! CommHandle type for DELayout
      type ESMF_DELayoutCommHandle
        sequence
        integer:: dummy ! not yet used...
      end type

!------------------------------------------------------------------------------

      integer(ESMF_KIND_I4), parameter:: ESMF_CWGHT_NORMAL = 50 !default

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_newDELayout
      public ESMF_I4_AP
      public ESMF_R4_AP
      public ESMF_R8_AP
      public ESMF_DELayoutData
      public ESMF_DELayoutCommHandle
      
!------------------------------------------------------------------------------
! !PUBLIC PARAMETERS:
      
      public ESMF_CWGHT_NORMAL

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:

  ! - ESMF_newDELayout:
      public ESMF_newDELayoutCreate
      public ESMF_newDELayoutDestroy
      
      public ESMF_newDELayoutGet
      public ESMF_newDELayoutGetDE
      public ESMF_newDELayoutGetDEMatch
      
      public ESMF_newDELayoutPrint
      
      public ESMF_newDELayoutDataCreate
      public ESMF_newDELayoutDataDestroy

      public ESMF_newDELayoutCopy
      public ESMF_newDELayoutScatter
      public ESMF_newDELayoutGather
      public ESMF_newDELayoutAllGlobalReduce
      
      public ESMF_newDELayoutWait
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_newDELayout.F90,v 1.19 2004/04/08 15:15:51 theurich Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newDELayoutCreate -- Generic interface

! !INTERFACE:
      interface ESMF_newDELayoutCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutCreateND

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutCreate} functions.   
!EOPI 
      end interface

!==============================================================================
      
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newDELayoutDataCreate -- Generic interface

! !INTERFACE:
      interface ESMF_newDELayoutDataCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutDataCreateI4
      module procedure ESMF_newDELayoutDataCreateR4
      module procedure ESMF_newDELayoutDataCreateR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutDataCreate} functions.   
!EOPI 
      end interface

!==============================================================================

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newDELayoutCopy -- Generic interface

! !INTERFACE:
      interface ESMF_newDELayoutCopy

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutCopyGeneral
      module procedure ESMF_newDELayoutCopyI4
      module procedure ESMF_newDELayoutCopyR4
      module procedure ESMF_newDELayoutCopyR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutCopy} functions.   
!EOPI 
      end interface

!==============================================================================
      
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newDELayoutScatter -- Generic interface

! !INTERFACE:
      interface ESMF_newDELayoutScatter

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutScatterGeneral
      module procedure ESMF_newDELayoutScatterI4
      module procedure ESMF_newDELayoutScatterR4
      module procedure ESMF_newDELayoutScatterR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutScatter} functions.   
!EOPI 
      end interface

!==============================================================================
      
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newDELayoutGather -- Generic interface

! !INTERFACE:
      interface ESMF_newDELayoutGather

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutGatherGeneral
      module procedure ESMF_newDELayoutGatherI4
      module procedure ESMF_newDELayoutGatherR4
      module procedure ESMF_newDELayoutGatherR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutGather} functions.   
!EOPI 
      end interface

!==============================================================================
      
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_newDELayoutAllGlobalReduce -- Generic interface

! !INTERFACE:
      interface ESMF_newDELayoutAllGlobalReduce

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutAllGlobalReduceGeneralI4
      module procedure ESMF_newDELayoutAllGlobalReduceGeneralR4
      module procedure ESMF_newDELayoutAllGlobalReduceGeneralR8
      module procedure ESMF_newDELayoutAllGlobalReduceI4
      module procedure ESMF_newDELayoutAllGlobalReduceR4
      module procedure ESMF_newDELayoutAllGlobalReduceR8

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutAllGlobalReduce} functions.   
!EOPI 
      end interface

!==============================================================================
      

contains
      
        
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutCreate - Create N-dimensional DELayout

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutCreate()
  function ESMF_newDELayoutCreateND(vm, deCountList, dePetList, &
    connectionWeightDimList, cyclicFlagDimList, rc)
!
! !ARGUMENTS:
    type(ESMF_VM),      intent(in)            :: vm
    integer,            intent(in),  optional :: deCountList(:)
    integer,            intent(in),  optional :: dePetList(:)
    integer,            intent(in),  optional :: connectionWeightDimList(:)
    type(ESMF_Logical), intent(in),  optional :: cyclicFlagDimList(:)
    integer,            intent(out), optional :: rc
!         
! !RETURN VALUE:
      type(ESMF_newDELayout) :: ESMF_newDELayoutCreateND
!
! !DESCRIPTION:
!     Create an N-dimensional, logically rectangular DELayout. Depending on
!     the optional argument {\tt deCountList} there are two cases that can be
!     distinguished:
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
!     number of DEs in the created DELayout, it will be used to determine the
!     DE-to-PET mapping. Otherwise a DE-to-PET mapping will be determined
!     automatically
!
!     The {\tt connectionWeightDimList} argument, if present, must have N
!     entries which will be used to ascribe connection weights along a certain
!     dimension within the DELayout. These weights have values from 0 to 100 and
!     will be used to find the best match between a DELayout and the VM.
!  
!     The {\tt cyclicFlagDimList} argument allows to enforce cyclic boundaries
!     in each of the dimensions of DELayout.\newline
!
!     The arguments are:
!     \begin{description}
!     \item[vm] 
!          VM of the current component in which delayout exists.
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

    type(ESMF_newDELayout):: delayout ! opaque pointer to new C++ DELayout
    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: i, len, ndim
    integer :: dummy(0)     ! used to satisfy the C interface...
    integer, allocatable :: dummy_nDEs(:)

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_FAILURE
    endif

    ! Initialize the pointer to NULL
    delayout%this = ESMF_NULL_POINTER

    ! Deal with optional array argument
    len = 0
    if (present(dePetList)) len = size(dePetList)
    
    if (present(deCountList)) then
      ndim = size(deCountList)
      allocate(dummy_nDEs(ndim))
      do i=1, ndim
        dummy_nDEs(i) = deCountList(i)
      enddo
    else
      ndim = 1
      allocate(dummy_nDEs(1))
      dummy_nDEs(1)=0
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    if (len==0) then
      call c_ESMC_newDELayoutCreate(delayout, vm, dummy_nDEs, ndim, &
        dummy, len, status)
    else
      call c_ESMC_newDELayoutCreate(delayout, vm, dummy_nDEs, ndim, &
        dePetList, len, status)
    endif
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutCreate error"
      return
    endif
    
    deallocate(dummy_nDEs)
    
    ! set return values
    ESMF_newDELayoutCreateND = delayout 
    if (rcpresent) rc = ESMF_SUCCESS
 
  end function ESMF_newDELayoutCreateND
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDestroy - Destroy a DELayout object

! !INTERFACE:
  subroutine ESMF_newDELayoutDestroy(delayout, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)  :: delayout
    integer, intent(out), optional      :: rc  
!         
!
! !DESCRIPTION:
!     Destroy a DELayout object
!
!     The arguments are:
!     \begin{description}
!     \item[delayout] 
!          DELayout object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_FAILURE
    endif

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutDestroy(delayout, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutDestroy error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutDestroy
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGet - Get internal info

! !INTERFACE:
  subroutine ESMF_newDELayoutGet(delayout, deCount, dimCount, localDeCount, &
    localDeList, localDe, oneToOneFlag, logRectFlag, deCountPerDim, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)            :: delayout
    integer,                intent(out), optional :: deCount
    integer,                intent(out), optional :: dimCount
    integer,                intent(out), optional :: localDeCount
    integer, target,        intent(out), optional :: localDeList(:)
    integer,                intent(out), optional :: localDe
    type(ESMF_Logical),     intent(out), optional :: oneToOneFlag
    type(ESMF_Logical),     intent(out), optional :: logRectFlag
    integer, target,        intent(out), optional :: deCountPerDim(:)
    integer,                intent(out), optional :: rc  
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

    integer :: status       ! local error status
    logical :: rcpresent
    integer :: len
    integer :: len_localDeList, len_deCountPerDim
    integer, target :: dummy(0)     ! used to satisfy the C interface...
    integer, pointer :: opt_localDeList(:), opt_deCountPerDim(:)
    
    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
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

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutGet(delayout, deCount, dimCount, localDeCount, &
      opt_localDeList, len_localDeList, localDe, oneToOneFlag, logRectFlag, &
      opt_deCountPerDim, len_deCountPerDim, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutGet error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_newDELayoutGet
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGetDE - Get DE specific internal info

! !INTERFACE:
  subroutine ESMF_newDELayoutGetDE(delayout, de, coord, connectionCount, &
    connectionList, connectionWeightList, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)            :: delayout
    integer,                intent(in)            :: de
    integer, target,        intent(out), optional :: coord(:)
    integer,                intent(out), optional :: connectionCount
    integer, target,        intent(out), optional :: connectionList(:)
    integer, target,        intent(out), optional :: connectionWeightList(:)
    integer,                intent(out), optional :: rc  
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

    integer :: status       ! local error status
    logical :: rcpresent
    integer :: len_coord, len_cde, len_cw
    integer, target :: dummy(0)     ! used to satisfy the C interface...
    integer, pointer:: opt_DEcoord(:), opt_DEcde(:), opt_DEcw(:)

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
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
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutGetDE(delayout, de, opt_DEcoord, len_coord, &
      opt_DEcde, len_cde, opt_DEcw, len_cw, connectionCount, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutGet error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_newDELayoutGetDE
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGetDEMatch - Match DE's between Layouts

! !INTERFACE:
  subroutine ESMF_newDELayoutGetDEMatch(delayout, de, delayoutMatch, &
    deMatchCount, deMatchList, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)            :: delayout
    integer,                intent(in)            :: de
    type(ESMF_newDELayout), intent(in)            :: delayoutMatch
    integer,                intent(out), optional :: deMatchCount
    integer, target,        intent(out), optional :: deMatchList(:)
    integer,                intent(out), optional :: rc  
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

    integer :: status       ! local error status
    logical :: rcpresent
    integer :: len_deMatchList
    integer, target :: dummy(0)     ! used to satisfy the C interface...
    integer, pointer:: opt_deMatchList(:)

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
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
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutGetDEMatch(delayout, de, delayoutMatch, &
      deMatchCount, opt_deMatchList, len_deMatchList, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutGetDEMatch error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_newDELayoutGetDEMatch
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutPrint - Print details of DELayout object

! !INTERFACE:
  subroutine ESMF_newDELayoutPrint(delayout, options, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)      :: delayout
    character(len=*), intent(in), optional  :: options
    integer, intent(out), optional          :: rc  
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

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutPrint(delayout, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutPrint error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutPrint
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDataCreate - Create I4 DELayoutData

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutDataCreate()
  function ESMF_newDELayoutDataCreateI4(array, rc)
!
! !ARGUMENTS:
    type(ESMF_I4_AP), intent(in)            :: array(:)
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
      type(ESMF_DELayoutData) :: ESMF_newDELayoutDataCreateI4
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    type(ESMF_DELayoutData):: mydata    ! new object
    integer :: status                   ! local error status
    logical :: rcpresent
    integer :: i

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

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
    call c_ESMC_newDELayoutDataCreate(mydata, mydata%n, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutDataCreate error"
      return
    endif

    do i=1, mydata%n
      call c_ESMC_newDELayoutDataAdd(mydata, array(i)%ap, i, status)
      if (status /= ESMF_SUCCESS) then
        print *, "c_ESMC_newDELayoutDataAdd error"
        return
      endif
    enddo

    ! set return values
    ESMF_newDELayoutDataCreateI4 = mydata 
    if (rcpresent) rc = ESMF_SUCCESS
 
  end function ESMF_newDELayoutDataCreateI4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDataCreate - Create R4 DELayoutData

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutDataCreate()
  function ESMF_newDELayoutDataCreateR4(array, rc)
!
! !ARGUMENTS:
    type(ESMF_R4_AP), intent(in)            :: array(:)
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
      type(ESMF_DELayoutData) :: ESMF_newDELayoutDataCreateR4
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    type(ESMF_DELayoutData):: mydata    ! new object
    integer :: status                   ! local error status
    logical :: rcpresent
    integer :: i

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

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
    call c_ESMC_newDELayoutDataCreate(mydata, mydata%n, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutDataCreate error"
      return
    endif

    do i=1, mydata%n
      call c_ESMC_newDELayoutDataAdd(mydata, array(i)%ap, i, status)
      if (status /= ESMF_SUCCESS) then
        print *, "c_ESMC_newDELayoutDataAdd error"
        return
      endif
    enddo

    ! set return values
    ESMF_newDELayoutDataCreateR4 = mydata 
    if (rcpresent) rc = ESMF_SUCCESS
 
  end function ESMF_newDELayoutDataCreateR4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDataCreate - Create R8 DELayoutData

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutDataCreate()
  function ESMF_newDELayoutDataCreateR8(array, rc)
!
! !ARGUMENTS:
    type(ESMF_R8_AP), intent(in)            :: array(:)
    integer,          intent(out), optional :: rc
!         
! !RETURN VALUE:
      type(ESMF_DELayoutData) :: ESMF_newDELayoutDataCreateR8
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    type(ESMF_DELayoutData):: mydata    ! new object
    integer :: status                   ! local error status
    logical :: rcpresent
    integer :: i

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! Initialize the pointer to null.
    mydata%this = ESMF_NULL_POINTER
    
    ! Initialize members
    mydata%n = size(array)          ! number of pointers in the pointer array
    allocate(mydata%len(mydata%n))  
    do i=1, mydata%n
      mydata%len(i) = size(array(i)%ap)! number of elments for this pointer el.
    enddo
    mydata%dtk = ESMF_R8    !data type and kind
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutDataCreate(mydata, mydata%n, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutDataCreate error"
      return
    endif

    do i=1, mydata%n
      call c_ESMC_newDELayoutDataAdd(mydata, array(i)%ap, i, status)
      if (status /= ESMF_SUCCESS) then
        print *, "c_ESMC_newDELayoutDataAdd error"
        return
      endif
    enddo

    ! set return values
    ESMF_newDELayoutDataCreateR8 = mydata 
    if (rcpresent) rc = ESMF_SUCCESS
 
  end function ESMF_newDELayoutDataCreateR8
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDataDestroy - Destroy a DELayoutData object

! !INTERFACE:
  subroutine ESMF_newDELayoutDataDestroy(delayoutData, rc)
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutDataDestroy(delayoutData, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutDataDestroy error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutDataDestroy
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Communication Routines (General DELayout)
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutCopy - Copy data between DEs in a DELayout

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutCopy()
  subroutine ESMF_newDELayoutCopyGeneral(delayout, srcData, dstData, count, &
    src, dst, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    type(ESMF_DELayoutData),       intent(out)           :: dstData
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: src
    integer,                       intent(in)            :: dst
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    if (srcData%dtk == ESMF_I4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R8) blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutCopy(delayout, srcData, dstData, blen, src, dst, &
      ESMF_FALSE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutCopy error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutCopyGeneral
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutScatter - MPI-like Scatter

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutScatter()
  subroutine ESMF_newDELayoutScatterGeneral(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    type(ESMF_DELayoutData),       intent(out)           :: dstData
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    if (srcData%dtk == ESMF_I4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R8) blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutScatter(delayout, srcData, dstData, blen, root, &
      ESMF_FALSE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutScatter error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutScatterGeneral
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGather - MPI-like Gather

! !INTERFACE:
  subroutine ESMF_newDELayoutGatherGeneral(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    type(ESMF_DELayoutData),       intent(out)           :: dstData
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    if (srcData%dtk == ESMF_I4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R4) blen = count * 4 ! 4 bytes
    if (srcData%dtk == ESMF_R8) blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutGather(delayout, srcData, dstData, blen, root, &
      ESMF_FALSE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutGatherGeneral
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutAllGlobalReduce - Reduce I4 data to a single value

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutAllGlobalReduce()
  subroutine ESMF_newDELayoutAllGlobalReduceGeneralI4(delayout, srcData, &
    dstData, count, operation, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    integer,                       intent(out)           :: dstData
    integer,                       intent(in)            :: count
    type(ESMF_newOp),              intent(in)            :: operation
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
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
!     \item[operation] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutAllGlobalReduce(delayout, srcData, dstData, &
      count, ESMF_I4, operation, ESMF_FALSE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutAllGlobalReduce error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutAllGlobalReduceGeneralI4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutAllGlobalReduce - Reduce R4 data to a single value

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutAllGlobalReduce()
  subroutine ESMF_newDELayoutAllGlobalReduceGeneralR4(delayout, srcData, &
    dstData, count, operation, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    real(ESMF_KIND_R4),            intent(out)           :: dstData
    integer,                       intent(in)            :: count
    type(ESMF_newOp),              intent(in)            :: operation
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle
    integer,                       intent(out), optional :: rc
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
!     \item[operation] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutAllGlobalReduce(delayout, srcData, dstData, &
      count, ESMF_R4, operation, ESMF_FALSE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutAllGlobalReduce error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutAllGlobalReduceGeneralR4
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutAllGlobalReduce - Reduce R8 data to a single value

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutAllGlobalReduce()
  subroutine ESMF_newDELayoutAllGlobalReduceGeneralR8(delayout, srcData, &
    dstData, count, operation,  blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    type(ESMF_DELayoutData),       intent(in)            :: srcData
    real(ESMF_KIND_R8),            intent(out)           :: dstData
    integer,                       intent(in)            :: count
    type(ESMF_newOp),              intent(in)            :: operation
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle
    integer,                       intent(out), optional :: rc
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
!     \item[operation] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutAllGlobalReduce(delayout, srcData, dstData, &
      count, ESMF_R8, operation, ESMF_FALSE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutAllGlobalReduce error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutAllGlobalReduceGeneralR8
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! Communication Routines (1-to-1 DELayout)
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutCopy - Copy data between DEs in a DELayout

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutCopy()
  subroutine ESMF_newDELayoutCopyI4(delayout, srcData, dstData, count, &
    src, dst, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    integer(ESMF_KIND_I4),         intent(in)            :: srcData(:)
    integer(ESMF_KIND_I4),         intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: src
    integer,                       intent(in)            :: dst
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutCopy(delayout, srcData, dstData, blen, src, dst, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutCopy error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutCopyI4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutCopy - Copy data between DEs in a DELayout

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutCopy()
  subroutine ESMF_newDELayoutCopyR4(delayout, srcData, dstData, count, &
    src, dst, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    real(ESMF_KIND_R4),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: src
    integer,                       intent(in)            :: dst
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutCopy(delayout, srcData, dstData, blen, src, dst, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutCopy error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutCopyR4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutCopy - Copy data between DEs in a DELayout

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutCopy()
  subroutine ESMF_newDELayoutCopyR8(delayout, srcData, dstData, count, &
    src, dst, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    real(ESMF_KIND_R8),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: src
    integer,                       intent(in)            :: dst
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag    
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 8 ! 8 bytes

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutCopy(delayout, srcData, dstData, blen, src, dst, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutCopy error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutCopyR8
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutScatter - MPI-like Scatter

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutScatter()
  subroutine ESMF_newDELayoutScatterI4(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    integer(ESMF_KIND_I4),         intent(in)            :: srcData(:)
    integer(ESMF_KIND_I4),         intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutScatter(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutScatter error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutScatterI4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutScatter - MPI-like Scatter

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutScatter()
  subroutine ESMF_newDELayoutScatterR4(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    real(ESMF_KIND_R4),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutScatter(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutScatter error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutScatterR4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutScatter - MPI-like Scatter

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutScatter()
  subroutine ESMF_newDELayoutScatterR8(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    real(ESMF_KIND_R8),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutScatter(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutScatter error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutScatterR8
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGather - MPI-like Gather

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutGather()
  subroutine ESMF_newDELayoutGatherI4(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    integer(ESMF_KIND_I4),         intent(in)            :: srcData(:)
    integer(ESMF_KIND_I4),         intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutGather(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutGatherI4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGather - MPI-like Gather

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutGather()
  subroutine ESMF_newDELayoutGatherR4(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    real(ESMF_KIND_R4),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 4 ! 4 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutGather(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutGatherR4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGather - MPI-like Gather

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutGather()
  subroutine ESMF_newDELayoutGatherR8(delayout, srcData, dstData, count, &
    root, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    real(ESMF_KIND_R8),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData(:)
    integer,                       intent(in)            :: count
    integer,                       intent(in)            :: root
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    integer :: blen

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine the number of bytes that need to be copied dependent on type
    blen = count * 8 ! 8 bytes
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutGather(delayout, srcData, dstData, blen, root, &
      ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutGatherR8
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutAllGlobalReduce - Reduce I4 data to a single value

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutAllGlobalReduce()
  subroutine ESMF_newDELayoutAllGlobalReduceI4(delayout, srcData, &
    dstData, count, operation, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    integer(ESMF_KIND_I4),         intent(in)            :: srcData(:)
    integer,                       intent(out)           :: dstData
    integer,                       intent(in)            :: count
    type(ESMF_newOp),              intent(in)            :: operation
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
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
!     \item[operation] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutAllGlobalReduce(delayout, srcData, dstData, &
      count, ESMF_I4, operation, ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutAllGlobalReduce error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutAllGlobalReduceI4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutAllGlobalReduce - Reduce R4 data to a single value

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutAllGlobalReduce()
  subroutine ESMF_newDELayoutAllGlobalReduceR4(delayout, srcData, &
    dstData, count, operation, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    real(ESMF_KIND_R4),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R4),            intent(out)           :: dstData
    integer,                       intent(in)            :: count
    type(ESMF_newOp),              intent(in)            :: operation
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
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
!     \item[operation] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutAllGlobalReduce(delayout, srcData, dstData, &
      count, ESMF_R4, operation, ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutAllGlobalReduce error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutAllGlobalReduceR4
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutAllGlobalReduce - Reduce R8 data to a single value

! !INTERFACE:
  ! Private name; call using ESMF_newDELayoutAllGlobalReduce()
  subroutine ESMF_newDELayoutAllGlobalReduceR8(delayout, srcData, &
    dstData, count, operation, blockingFlag, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    real(ESMF_KIND_R8),            intent(in)            :: srcData(:)
    real(ESMF_KIND_R8),            intent(out)           :: dstData
    integer,                       intent(in)            :: count
    type(ESMF_newOp),              intent(in)            :: operation
    type(ESMF_BlockingFlag),       intent(in),  optional :: blockingFlag
    type(ESMF_DELayoutCommHandle), intent(out), optional :: commHandle    
    integer,                       intent(out), optional :: rc
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
!     \item[operation] 
!          Reduction operation.
!     \item[{[blockingFlag]}]
!          Flag indicating whether this call should be blocking or non-blocking.
!     \item[{[commHandle]}] 
!          Must be present for non-blocking calls, providing handle for Wait.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutAllGlobalReduce(delayout, srcData, dstData, &
      count, ESMF_R8, operation, ESMF_TRUE, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutAllGlobalReduce error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutAllGlobalReduceR8
!------------------------------------------------------------------------------




!------------------------------------------------------------------------------
! Sync for non-blocking communications
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutWait - Wait for DELayout communication to finish

! !INTERFACE:
  subroutine ESMF_newDELayoutWait(delayout, commHandle, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout),        intent(in)            :: delayout
    type(ESMF_DELayoutCommHandle), intent(in)            :: commHandle
    integer,                       intent(out), optional :: rc
!         
!
! !DESCRIPTION:
!     Wait for DELayout communication to finish
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
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutWait
!------------------------------------------------------------------------------


end module ESMF_newDELayoutMod
