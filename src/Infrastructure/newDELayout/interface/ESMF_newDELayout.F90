! $Id: ESMF_newDELayout.F90,v 1.2 2004/03/03 19:45:25 theurich Exp $
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
! !MODULE: ESMF_newDELayout - The newDELayout
!
! !DESCRIPTION:
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

      ! type to hold ESMF_DELayout data
      type ESMF_DELayoutData_I4
        sequence
        private
        type(ESMF_Pointer):: this
        integer:: n
        integer, pointer:: len(:)
      end type

!------------------------------------------------------------------------------

      integer(ESMF_KIND_I4), parameter:: ESMF_CWGHT_NORMAL = 50 !default

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_newDELayout
      public ESMF_I4_AP
      public ESMF_DELayoutData_I4
      
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
      
      public ESMF_newDELayoutUnplug
      public ESMF_newDELayoutPlug
      
      public ESMF_newDELayoutGet
      public ESMF_newDELayoutGetDE
      public ESMF_newDELayoutMyDE
      
      public ESMF_newDELayoutPrint
      
      public ESMF_newDELayoutScatter
      public ESMF_newDELayoutGather
      public ESMF_newDELayoutAllReduce

      public ESMF_newDELayoutDataCreate
      public ESMF_newDELayoutDataDestroy
!EOP
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_newDELayout.F90,v 1.2 2004/03/03 19:45:25 theurich Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutCreate -- Generic interface to create a DELayout

! !INTERFACE:
      interface ESMF_newDELayoutCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutCreateND

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutCreate} functions.   
!EOP 
      end interface

!==============================================================================
      
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDataCreate -- Generic interface create DELayoutData

! !INTERFACE:
      interface ESMF_newDELayoutDataCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutDataCreateI4

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutDataCreate} functions.   
!EOP 
      end interface

!==============================================================================

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDataDestroy -- Generic interface dest. DELayoutData

! !INTERFACE:
      interface ESMF_newDELayoutDataDestroy

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_newDELayoutDataDestroyI4

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutDataDestroy} functions.   
!EOP 
      end interface

!==============================================================================


contains
      
        
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutCreateND - Create ND DELayout

! !INTERFACE:
  function ESMF_newDELayoutCreateND(vm, nDEs, DEtoPET, cyclic, rc)
!
! !ARGUMENTS:
    type(ESMF_VM), intent(in)                 :: vm
    integer, intent(in), optional             :: nDEs(:)
    integer, intent(in), optional             :: DEtoPET(:)
    type(ESMF_Logical), intent(in), optional  :: cyclic
    integer, intent(out), optional            :: rc
!         
! !RETURN VALUE:
      type(ESMF_newDELayout) :: ESMF_newDELayoutCreateND
!
! !DESCRIPTION:
!     Create ND DELayout
!
!     The arguments are:
!     \begin{description}
!     \item[vm] 
!          ESMF_VM object
!     \item[{[nDEs]}] 
!          Array of number of DEs in each dimension
!     \item[{[DEtoPET]}] 
!          Array of DEtoPET mapping
!     \item[{[cyclic]}] 
!          Use cyclic boundaries
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    type(ESMF_newDELayout):: layout   ! opaque pointer to new C++ DELayout
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
    layout%this = ESMF_NULL_POINTER

    ! Deal with optional array argument
    len = 0
    if (present(DEtoPET)) len = size(DEtoPET)
    
    if (present(nDEs)) then
      ndim = size(nDEs)
      allocate(dummy_nDEs(ndim))
      do i=1, ndim
        dummy_nDEs(i) = nDEs(i)
      enddo
    else
      ndim = 1
      allocate(dummy_nDEs(1))
      dummy_nDEs(1)=0
    endif
    
    ! Routine which interfaces to the C++ creation routine.
    if (len==0) then
      call c_ESMC_newDELayoutCreate(layout, vm, dummy_nDEs, ndim, &
        dummy, len, cyclic, status)
    else
      call c_ESMC_newDELayoutCreate(layout, vm, dummy_nDEs, ndim, &
        DEtoPET, len, cyclic, status)
    endif
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutCreate1D error"
      return
    endif
    
    deallocate(dummy_nDEs)
    
    ! set return values
    ESMF_newDELayoutCreateND = layout 
    if (rcpresent) rc = ESMF_SUCCESS
 
  end function ESMF_newDELayoutCreateND
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDestroy - Destroy a DELayout object

! !INTERFACE:
  subroutine ESMF_newDELayoutDestroy(layout, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)  :: layout
    integer, intent(out), optional      ::  rc  
!         
!
! !DESCRIPTION:
!     Destroy a DELayout object
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
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
    call c_ESMC_newDELayoutDestroy(layout, status)
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
! !IROUTINE: ESMF_newDELayoutUnplug - Unplug a layout from its VM

! !INTERFACE:
  subroutine ESMF_newDELayoutUnplug(layout, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)  :: layout
    integer, intent(out), optional      :: rc  
!         
!
! !DESCRIPTION:
!     Unplug a layout from its VM
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
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
    call c_ESMC_newDELayoutUnplug(layout, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutUnplug error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutUnplug
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutPlug - Plug a layout into a VM

! !INTERFACE:
  subroutine ESMF_newDELayoutPlug(layout, vm, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)  :: layout
    type(ESMF_VM),          intent(in)  :: vm
    integer, intent(out), optional      :: rc  
!         
!
! !DESCRIPTION:
!     Plug a layout into a VM
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
!     \item[vm] 
!          VM opbject
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
    call c_ESMC_newDELayoutPlug(layout, vm, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutPlug error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutPlug
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGet - Get internal info

! !INTERFACE:
  subroutine ESMF_newDELayoutGet(layout, nDEs, ndim, nmyDEs, myDEs, pstate, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)            :: layout
    integer,                intent(out), optional :: nDEs
    integer,                intent(out), optional :: ndim
    integer,                intent(out), optional :: nmyDEs
    integer,                intent(out), optional :: myDEs(:)
    type(ESMF_Logical),     intent(out), optional :: pstate
    integer,                intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get internal info
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
!     \item[{[nDEs]}]
!          Total number of DEs in layout
!     \item[{[ndim]}]
!          Number of dimensions in coordinate tuple
!     \item[{[nmyDEs]}]
!          Number of DEs associated with instantiating PET
!     \item[{[myDEs]}]
!          List of DEs associated with instantiating PET
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status       ! local error status
    logical :: rcpresent
    integer :: len
    integer :: dummy(0)     ! used to satisfy the C interface...
    type(ESMF_Logical) :: pstat        ! local pstate variable

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif
    
    ! Determine whether this layout is plugged or unplugged
    call c_ESMC_newDELayoutPlugged(layout, pstat, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutPlugged error"
      return
    endif
    if (present(pstate)) pstate = pstat
    
    ! Look at the pstat variable and decide whether failure needs to be 
    ! indicated.
    if (pstat == ESMF_FALSE) then
      if (present(nmyDEs)) return
      if (present(myDEs)) return
    endif
    
    ! Deal with optional array argument
    len = 0
    if (present(myDEs)) len = size(myDEs)

    ! Routine which interfaces to the C++ creation routine.
    if (len==0) then
      call c_ESMC_newDELayoutGet(layout, nDEs, ndim, nmyDEs, dummy, len, status)
    else
      call c_ESMC_newDELayoutGet(layout, nDEs, ndim, nmyDEs, myDEs, len, status)
    endif
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
  subroutine ESMF_newDELayoutGetDE(layout, DEid, DEcoord, nDEc, DEcde, DEcw, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)            :: layout
    integer,                intent(in)            :: DEid
    integer, target,        intent(out), optional :: DEcoord(:)
    integer,                intent(out), optional :: nDEc
    integer, target,        intent(out), optional :: DEcde(:)
    integer, target,        intent(out), optional :: DEcw(:)
    integer,                intent(out), optional :: rc  
!         
!
! !DESCRIPTION:
!     Get DE specific internal info
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
!     \item[DEid]
!          DE id specifying the DE to be queried
!     \item[{[DEcoord]}]
!          Array providing coordinates of "DEid"
!     \item[{[nDEc]}]
!          Number of connections associated with "DEid"
!     \item[{[nDEc]}]
!          Array of DE's to which "DEid" is connected
!     \item[{[nDEc]}]
!          Array of "DEid"'s connection weights
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
    if (present(DEcoord)) then
      len_coord = size(DEcoord)
      opt_DEcoord => DEcoord
    else
      len_coord = 0
      opt_DEcoord => dummy
    endif
    if (present(DEcde)) then
      len_cde = size(DEcde)
      opt_DEcde => DEcde
    else
      len_cde = 0
      opt_DEcde => dummy
    endif
    if (present(DEcw)) then
      len_cw = size(DEcw)
      opt_DEcw => DEcw
    else
      len_cw = 0
      opt_DEcw => dummy
    endif
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutGetDE(layout, DEid, opt_DEcoord, len_coord, &
      opt_DEcde, len_cde, opt_DEcw, len_cw, nDEc, status)
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
! !IROUTINE: ESMF_newDELayoutMyDE - Inquire whether this is one of PET's DEs

! !INTERFACE:
  function ESMF_newDELayoutMyDE(layout, DE, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)      :: layout
    integer, intent(in)                     :: DE
    integer, intent(out), optional          :: rc
!         
! !RETURN VALUE:
      type(ESMF_Logical) :: ESMF_newDELayoutMyDE
!
! !DESCRIPTION:
!     Inquire whether this is one of PET's DEs
!
!     The arguments are:
!     \begin{description}
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    integer :: status                 ! local error status
    logical :: rcpresent
    type(ESMF_Logical) :: value

    ! Initialize return code; assume failure until success is certain       
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.  
      rc = ESMF_FAILURE
    endif

    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutMyDE(layout, DE, value, status)
    if (status /= ESMF_SUCCESS) then
      print *, "ESMF_newDELayoutMyDE error"
      return
    endif

    ! set return error status
    ESMF_newDELayoutMyDE = value
    if (rcpresent) rc = ESMF_SUCCESS
 
  end function ESMF_newDELayoutMyDE
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutPrint - Print details of DELayout object

! !INTERFACE:
  subroutine ESMF_newDELayoutPrint(layout, options, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)      :: layout
    character(len=*), intent(in), optional  :: options
    integer, intent(out), optional          :: rc  
!         
!
! !DESCRIPTION:
!     Print details of DELayout object
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
!     \item[{[options]}] 
!          Output options
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
    call c_ESMC_newDELayoutPrint(layout, status)
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
! !IROUTINE: ESMF_newDELayoutScatter - MPI-like Scatter

! !INTERFACE:
  subroutine ESMF_newDELayoutScatter(layout, vm, datain, dataout, len, root, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)      :: layout
    type(ESMF_VM), intent(in)               :: vm
    type(ESMF_DELayoutData_I4), intent(in)  :: datain
    type(ESMF_DELayoutData_I4), intent(out) :: dataout
    integer, intent(in)                     :: len, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Scatter
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
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
    call c_ESMC_newDELayoutScatter(layout, vm, datain, dataout, len, root, &
      status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutScatter error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutScatter
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutGather - MPI-like Gather

! !INTERFACE:
  subroutine ESMF_newDELayoutGather(layout, vm, datain, dataout, len, root, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)      :: layout
    type(ESMF_VM), intent(in)               :: vm
    type(ESMF_DELayoutData_I4), intent(in)  :: datain
    type(ESMF_DELayoutData_I4), intent(out) :: dataout
    integer, intent(in)                     :: len, root
    integer, intent(out), optional          :: rc
!         
!
! !DESCRIPTION:
!     MPI-like Gather
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
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
    call c_ESMC_newDELayoutGather(layout, vm, datain, dataout, len, root, &
      status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutGather error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutGather
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutAllReduce - MPI-like AllReduce

! !INTERFACE:
  subroutine ESMF_newDELayoutAllReduce(layout, vm, array, result, len, op, rc)
!
! !ARGUMENTS:
    type(ESMF_newDELayout), intent(in)      :: layout
    type(ESMF_VM), intent(in)               :: vm
    integer, intent(in)                     :: array(:), op, len
    integer, intent(out)                    :: result(:)
    integer, intent(out), optional          :: rc  
!         
!
! !DESCRIPTION:
!     MPI-like AllReduce
!
!     The arguments are:
!     \begin{description}
!     \item[layout] 
!          DELayout
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
    call c_ESMC_newDELayoutAllReduce(layout, vm, array, result, len, op, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutReduce error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutAllReduce
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_newDELayoutDataCreateI4 - Create I4 DELayoutData

! !INTERFACE:
  function ESMF_newDELayoutDataCreateI4(array, rc)
!
! !ARGUMENTS:
    type(ESMF_I4_AP), intent(in)            :: array(:)
    integer, intent(out), optional          :: rc
!         
! !RETURN VALUE:
      type(ESMF_DELayoutData_I4) :: ESMF_newDELayoutDataCreateI4
!
! !DESCRIPTION:
!     Create I4 DELayoutData
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          ESMF_I4_AP array
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n

    type(ESMF_DELayoutData_I4):: mydata ! opaque pointer to new C++ DELayout
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
    mydata%n = size(array)
    allocate(mydata%len(mydata%n))
    do i=1, mydata%n
      mydata%len(i) = size(array(i)%ap)
    enddo
    
    ! Routine which interfaces to the C++ creation routine.
    call c_ESMC_newDELayoutDataCreate(mydata, mydata%n, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutCreateData error"
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
! !IROUTINE: ESMF_newDELayoutDataDestroy - Destroy a DELayoutData object

! !INTERFACE:
  subroutine ESMF_newDELayoutDataDestroyI4(mydata, rc)
!
! !ARGUMENTS:
    type(ESMF_DELayoutData_I4)          ::  mydata 
    integer, intent(out), optional      ::  rc  
!         
!
! !DESCRIPTION:
!     Destroy a DELayout object
!
!     The arguments are:
!     \begin{description}
!     \item[mydata] 
!          ESMF_DELayoutData_I4 object
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
    call c_ESMC_newDELayoutDataDestroy(mydata, status)
    if (status /= ESMF_SUCCESS) then
      print *, "c_ESMC_newDELayoutDataDestroy error"
      return
    endif

    ! set return values
    if (rcpresent) rc = ESMF_SUCCESS
 
  end subroutine ESMF_newDELayoutDataDestroyI4
!------------------------------------------------------------------------------


end module ESMF_newDELayoutMod
