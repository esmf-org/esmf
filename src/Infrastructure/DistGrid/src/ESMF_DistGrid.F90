! $Id: ESMF_DistGrid.F90,v 1.67 2003/08/14 21:53:44 jwolfe Exp $
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
!     ESMF DistGrid Module
      module ESMF_DistGridMod
!
!==============================================================================
!
! This file contains the DistGrid class definition and all DistGrid class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF_DistGrid.h"
#include "ESMF_Macros.inc"
!==============================================================================
!BOPI
! !MODULE: ESMF_DistGridMod - contains Grid decompostion methods
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_DistGrid} class, which contains a
! collection of subgrids which constitute a single logical {\tt ESMF\_Grid}. The
! subgrids can be operated on in parallel on a multiprocessor machine. The
! {\tt ESMF\_DistGrid} class contains the mapping between the local grid 
! decompositions and the global logical {\tt ESMF\_Grid}. It contains methods to
! synchronize data values between the boundaries of subsets, and to collect
! and communicate global data values. It interacts closely with the
! {\tt ESMF\_PhysGrid} object.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_DELayoutMod
      use ESMF_LocalArrayMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!     ! ESMF_MyDE
!
!     ! Description of ESMF_MyDE

      type ESMF_MyDE
      sequence
!     private
        integer :: MyDE             ! identifier for this DE
        integer :: local_cell_count ! local (on this DE) number of cells
        integer, dimension(ESMF_MAXGRIDDIM) :: global_start
        type (ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM) :: ai_global
                                    ! local cell index in each direction using
                                    ! global indexing, covering the total domain
      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGridType
!
!     !  Description of ESMF_DistGrid. 

      type ESMF_DistGridType
      sequence
!     private
        type (ESMF_Base) :: base
        type (ESMF_DELayout) :: layout  ! TODO: should this be a pointer?
        type (ESMF_MyDE) :: MyDE       ! local DE identifiers
        integer, dimension(ESMF_MAXGRIDDIM) :: decompids
        logical :: covers_domain       ! identifier if distgrid covers
                                       ! the entire physical domain
        integer :: global_cell_count   ! global number of cells
        integer, dimension(ESMF_MAXGRIDDIM) :: global_cell_dim
                                       ! global number of cells in each
                                       ! dimension
        integer, dimension(:), pointer :: local_cell_count
                                       ! array of the numbers of cells on each
                                       ! DE
        integer :: local_cell_max      ! maximum number of cells on any DE
        integer, dimension(ESMF_MAXGRIDDIM) :: local_cell_max_dim
                                       ! maximum DE cell counts in each
                                       ! grid dimension
        integer, dimension(:,:), pointer :: global_start
        type (ESMF_AxisIndex), dimension(:,:), pointer :: ai_global
      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGrid
!
!     !  The DistGrid data structure that is passed between languages.

      type ESMF_DistGrid
      sequence
!     private
        type (ESMF_DistGridType), pointer :: ptr     ! pointer to a distgrid
                                                     ! type
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_MyDE
      public ESMF_DistGrid
      public ESMF_DistGridType
!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!
    public ESMF_DistGridCreate
    public ESMF_DistGridDestroy
    public ESMF_DistGridGetConfig
    public ESMF_DistGridSetConfig
    public ESMF_DistGridGet
    public ESMF_DistGridSet
    public ESMF_DistGridGetCounts
    public ESMF_DistGridSetCounts
    public ESMF_DistGridGetDE
    public ESMF_DistGridSetDE
    public ESMF_DistGridGetAllAxisIndex
    public ESMF_DistGridGetDELayout
    ! TODO:  combine all the get subroutines into one
    public ESMF_DistGridGetValue
    public ESMF_DistGridSetValue
    public ESMF_DistGridLocalToGlobalIndex
    public ESMF_DistGridGlobalToLocalIndex
    public ESMF_DistGridValidate
    public ESMF_DistGridPrint
 
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DistGrid.F90,v 1.67 2003/08/14 21:53:44 jwolfe Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_DistGridCreate 

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridCreateEmpty
         module procedure ESMF_DistGridCreateInternal
         module procedure ESMF_DistGridCreateIntSpec
!        module procedure ESMF_DistGridCreateCopy

! !DESCRIPTION:
!     This interface provides a single entry point for {\tt ESMF\_DistGrid} create
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOPI
! !INTERFACE:
      interface ESMF_DistGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridConstructNew
         module procedure ESMF_DistGridConstructInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct
!     a complete {\tt ESMF\_DistGrid}.
!
!EOPI
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_DistGridSetCounts

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridSetCountsInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     extent counts in a {\tt ESMF\_DistGrid}.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_DistGridSetDE

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridSetDEInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     extent counts in a {\tt ESMF\_DistGrid}.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!==============================================================================

      contains

!==============================================================================
!
! This section includes the DistGrid Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridCreateEmpty - Create a new DistGrid with no data

! !INTERFACE:
      function ESMF_DistGridCreateEmpty(name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateEmpty
!
! !ARGUMENTS:
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_DistGrid} object and constructs its
!     internals.  Returns a pointer to a new {\tt ESMF\_DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[name]] 
!          {\tt DistGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_DistGridType), pointer :: distgrid  ! Pointer to new distgrid
      integer :: status=ESMF_FAILURE                ! Error status
      logical :: rcpresent=.FALSE.                  ! Return code present

!     Initialize pointers
      nullify(distgrid)
      nullify(ESMF_DistGridCreateEmpty%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(distgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateEmpty: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstructNew(distgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateEmpty: DistGrid construct"
        return
      endif

!     Set return values.
      ESMF_DistGridCreateEmpty%ptr => distgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateEmpty

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridCreateInternal - Create a new DistGrid internally

! !INTERFACE:
      function ESMF_DistGridCreateInternal(counts, layout, periodic, name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateInternal
!
! !ARGUMENTS:
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: counts
      type (ESMF_DELayout), intent(in) :: layout
      character (len = *), intent(in), optional :: name  
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_DistGrid} object, constructs its
!     internals, and internally sets necessary attributes and values.
!     Returns a pointer to a new {\tt ESMF\_DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[counts]] 
!          Array of number of computational cells in each direction.
!     \item[[layout]]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[[name]] 
!          {\tt ESMF\_DistGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_DistGridType), pointer :: distgrid  ! Pointer to new distgrid
      integer :: status=ESMF_FAILURE                ! Error status
      logical :: rcpresent=.FALSE.                  ! Return code present
      integer :: nDEs(2)
      integer, dimension(:), allocatable :: countsPerDE1, countsPerDE2

!     Initialize pointers
      nullify(distgrid)
      nullify(ESMF_DistGridCreateInternal%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(distgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateInternal: Allocate"
        return
      endif

!     Call routine to parse problem size
      call ESMF_DELayoutGetSize(layout, nDEs(1), nDEs(2), rc)
      allocate(countsPerDE1(nDEs(1)), stat=status)
      allocate(countsPerDE2(nDEs(2)), stat=status)

      call ESMF_DELayoutParse(layout, 1, counts(1), countsPerDE1, rc)
      call ESMF_DELayoutParse(layout, 2, counts(2), countsPerDE2, rc)

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstruct(distgrid, layout, countsPerDE1, &
                                  countsPerDE2, periodic, name, rc)

!     Set return values.
      ESMF_DistGridCreateInternal%ptr => distgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridCreateIntSpec - Create a new DistGrid internally

! !INTERFACE:
      function ESMF_DistGridCreateIntSpec(countsPerDE1, countsPerDE2, &
                                          layout, periodic, name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateIntSpec
!
! !ARGUMENTS:
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
      type (ESMF_DELayout), intent(in) :: layout
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates memory for a new {\tt ESMF\_DistGrid} object, constructs its
!     internals, and internally sets necessary attributes and values.
!     Returns a pointer to a new {\tt ESMF\_DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[countsPerDE1]] 
!          Array of number of computational cells for each DE in direction 1.
!     \item[[countsPerDE2]] 
!          Array of number of computational cells for each DE in direction 2.
!     \item[[layout]]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[[name]] 
!          {\tt ESMF\_DistGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      type(ESMF_DistGridType), pointer :: distgrid  ! Pointer to new distgrid
      integer :: status=ESMF_FAILURE                ! Error status
      logical :: rcpresent=.FALSE.                  ! Return code present

!     Initialize pointers
      nullify(distgrid)
      nullify(ESMF_DistGridCreateIntSpec%ptr)

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      allocate(distgrid, stat=status)
!     If error write message and return.
!     Formal error handling will be added asap.
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridCreateInternalSpecd: Allocate"
        return
      endif

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstruct(distgrid, layout, countsPerDE1, &
                                  countsPerDE2, periodic, name, rc)

!     Set return values.
      ESMF_DistGridCreateIntSpec%ptr => distgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateIntSpec

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridDestroy - Free all resources associated with a DistGrid 

! !INTERFACE:
      subroutine ESMF_DistGridDestroy(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid) :: distgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt ESMF\_DistGrid} object previously allocated
!     via an {\tt ESMF\_DistGridCreate} routine.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          The class to be destroyed.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status                            ! Error status
      logical :: rcpresent                         ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! If already destroyed or never created, return ok
      if (.not. associated(distgrid%ptr)) then
        print *, "DistGridDestroy called on uninitialized or destroyed DistGrid"
        if(rcpresent) rc = ESMF_FAILURE   ! should this really be an error?
        return
      endif

      ! Destruct all distgrid internals and then free field memory.
      call ESMF_DistGridDestruct(distgrid, status)
      ! If error write message and return.
      if(status .ne. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridDestroy from ESMF_DistGridDestruct"
        return
      endif

      deallocate(distgrid%ptr, stat=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridDestroy: DistGrid deallocate"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridDestroy

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_DistGridConstructNew - Construct the internals of an allocated DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridConstructNew(distgrid, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid 
      character (len = *), intent(in), optional :: name  ! name
      integer, intent(out), optional :: rc               ! return code
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_DistGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Pointer to a {\tt ESMF\_DistGrid}.
!     \item[[name]] 
!          Name of the {\tt ESMF\_DistGrid}.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: status=ESMF_SUCCESS               ! Error status
      logical :: rcpresent=.FALSE.                 ! Return code present
      integer :: i                                 ! loop counter

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Set the DistGrid name if present, otherwise construct a default one
      call ESMF_SetName(distgrid%base, name, "DistGrid", status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructNew: Setname"
        return
      endif

!     Initialize distgrid contents  TODO:  move this into the derived type
!                                          as defaults
      distgrid%MyDE%MyDE = 0
      distgrid%MyDE%local_cell_count = 0
      do i = 1,ESMF_MAXGRIDDIM
        distgrid%MyDE%ai_global(i)%min = 0
        distgrid%MyDE%ai_global(i)%max = 0
        distgrid%MyDE%ai_global(i)%stride = 0
      enddo
      distgrid%covers_domain = .false.
      distgrid%global_cell_count = 0
      distgrid%local_cell_max = 0
      do i = 1,ESMF_MAXGRIDDIM
        distgrid%decompids(i) = 0
        distgrid%global_cell_dim(i) = 0
        distgrid%local_cell_max_dim(i) = 0
      enddo
      nullify(distgrid%local_cell_count)
      nullify(distgrid%global_start)
      nullify(distgrid%ai_global)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructNew

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_DistGridConstructInternal - Construct the internals of an allocated DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridConstructInternal(distgrid, layout, countsPerDE1, &
                                                countsPerDE2, periodic, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid 
      type (ESMF_DELayout), intent(in) :: layout
      integer, dimension(:) :: countsPerDE1
      integer, dimension(:) :: countsPerDE2
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt ESMF\_DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding {\tt ESMF\_DistGridDestruct}
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Pointer to a {\tt ESMF\_DistGrid}.
!     \item[[layout]]
!          {\tt ESMF\_DELayout} of {\tt ESMF\_DE}'s.
!     \item[[countsPerDE1]]
!          Array of number of computational cells per DE in first direction.
!     \item[[countsPerDE2]]
!          Array of number of computational cells per DE in second direction.
!     \item[[name]] 
!          {\tt ESMF\_DistGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOPI

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: global_cell_count
      integer :: i, nDE
      integer, dimension(ESMF_MAXGRIDDIM) :: global_cell_dim, nDEs

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Error checking for required input   TODO: complete
!     if (not(associated(layout%ptr))) then
!       status = ESMF_FAILURE
!       print *, "ERROR in ESMF_DistGridConstructInternal: unassociated layout"
!       return
!     endif
 
!     Initialize the derived type contents
      call ESMF_DistGridConstructNew(distgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: DistGrid construct"
        return
      endif

!     Fill in distgrid derived type with input or default
!     TODO:  temporary fix, also add defaults to objects
      global_cell_count = 1
      global_cell_dim(1) = 0
      global_cell_dim(2) = 0
      do i = 1,size(countsPerDE1)
        global_cell_dim(1) = global_cell_dim(1) + countsPerDE1(i)
      enddo
      if(global_cell_dim(1).ne.0) global_cell_count = global_cell_count &
                                                    * global_cell_dim(1)
      do i = 1,size(countsPerDE2)
        global_cell_dim(2) = global_cell_dim(2) + countsPerDE2(i)
      enddo
      if(global_cell_dim(2).ne.0) global_cell_count = global_cell_count &
                                                    * global_cell_dim(2)
      if(global_cell_count.le.0) then
        print *, "ERROR in ESMF_DistGridConstructInternal: global_cell_count le 0"
        return
      endif

      call ESMF_DistGridSet(distgrid, covers_domain=.true., &
                            global_cell_count=global_cell_count, &
                            global_cell_dim=global_cell_dim, rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: distgrid set"
        return
      endif

!     set the distgrid layout to the specified layout
      call ESMF_DELayoutGetSize(layout, nDEs(1), nDEs(2), status)
      nDE = nDEs(1) * nDEs(2)
      if((status .NE. ESMF_SUCCESS) .or. (nDE .le. 0)) then
        print *, "ERROR in ESMF_DistGridConstructInternal: DELayout get size"
        return
      endif
      distgrid%layout = layout

!     Allocate resources based on number of DE's
      allocate(distgrid%local_cell_count(nDE), stat=status)
      if(status .NE. 0) then
        print *, "ERROR in ESMF_DistGridConstructInternal: allocate"
        return
      endif
      allocate(distgrid%global_start(nDE,ESMF_MAXGRIDDIM), stat=status)
      if(status .NE. 0) then
        print *, "ERROR in ESMF_DistGridConstructInternal: allocate"
        return
      endif
      allocate(distgrid%ai_global(nDE,ESMF_MAXGRIDDIM), stat=status)
      if(status .NE. 0) then
        print *, "ERROR in ESMF_DistGridConstructInternal: allocate"
        return
      endif

!     Parse problem size
      call ESMF_DistGridSetCounts(distgrid, nDEs, countsPerDE1, countsPerDE2, &
                                  periodic, status)
        
!     Fill in DE derived type
      call ESMF_DistGridSetDE(distgrid, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: Set de"
        return
      endif

!     Calculate other distgrid values from DE information
!     distgrid%local_cell_max = GlobalCommMax()
!     do i = 1,ESMF_MAXGRIDDIM
!       distgrid%local_cell_max_dim(i) = GlobalCommMax()
!     enddo

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: DistGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructInternal

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_DistGridDestruct - Free any DistGrid memory allocated internally

! !INTERFACE:
      subroutine ESMF_DistGridDestruct(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid    
      integer, intent(out), optional :: rc         
!
! !DESCRIPTION:
!     ESMF routine which deallocates any space allocated by
!    {\tt  ESMF\_DistGridConstruct}, does any additional cleanup before the
!     original DistGrid object is freed.  Intended for internal ESMF
!     use only; end-users use {\tt ESMF\_DistGridDestroy}, which calls
!     {\tt ESMF\_DistGridDestruct}.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          The class to be destructed.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS: 
      integer :: status                       ! Error status
      logical :: rcpresent                    ! Return code present

!     Initialize return code
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      ! TODO: Agree that this is correct.  The Grid is passed in a layout
      !  created outside this grid (and perhaps shared amongst many grids)
      !  so it seems that it should not be destroyed here.)
      !call ESMF_DELayoutDestroy(distgrid%ptr%layout, status)
      !if(status .NE. ESMF_SUCCESS) then
      !  print *, "ERROR in ESMF_DistGridDestruct: DELayout destroy"
      !  return
      !endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGet - Get information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGet(distgrid, covers_domain, global_cell_count, &
                                  global_cell_dim, global_start, &
                                  local_cell_max, local_cell_max_dim, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      logical, intent(inout), optional :: covers_domain
      integer, intent(inout), optional :: global_cell_count
      integer, dimension(:), intent(inout), optional :: global_cell_dim
      integer, dimension(:,:), intent(inout), optional :: global_start
      integer, intent(inout), optional :: local_cell_max
      integer, dimension(:), intent(inout), optional :: local_cell_max_dim
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns information from the {\tt ESMF\_DistGrid} object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[[covers\_domain]]
!          Logical identifier if distgrid covers the entire physical domain.
!     \item[[global\_cell\_count]]
!          Global total number of cells.
!     \item[[global\_cell\_dim]]
!          Array of the global number of cells in each dimension.
!     \item[[global\_start]]
!          Array of the global starting count on each DE in each dimension,
!          dimensioned (nDEs, ESMF\_MAXGRIDDIM)
!     \item[[local\_cell\_max]]
!          Maximum number of cells on any {\tt ESMF\_DE}.
!     \item[[local\_cell\_max\_dim]]
!          Array of the maximum number of cells in each dimension on
!          any {\tt ESMF\_DE}.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: i, j, nDEs

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     if present, get information from distgrid derived type
      if(present(covers_domain)) covers_domain = distgrid%covers_domain

      if(present(global_cell_count)) &
                 global_cell_count = distgrid%global_cell_count

      if(present(global_cell_dim)) then
                 ! TODO: add check that global_cell_dim is large enough
                 !       or use the size of the array for the loop limit
        do i = 1,ESMF_MAXGRIDDIM
          global_cell_dim(i) = distgrid%global_cell_dim(i)
        enddo
      endif

      if(present(global_start)) then
                 ! TODO: add check that global_start is large enough
                 !       or use the size of the array for the loop limit
        call ESMF_DELayoutGetNumDEs(distgrid%layout, nDEs, status)
        do i = 1, nDEs
          do j = 1,ESMF_MAXGRIDDIM
            global_start(i,j) = distgrid%global_start(i,j)
          enddo
        enddo
      endif

      if(present(local_cell_max)) local_cell_max = distgrid%local_cell_max

      if(present(local_cell_max_dim)) then
                 ! TODO: add check that local_cell_max_dim is large enough
                 !       or use the size of the array for the loop limit
        do i = 1,ESMF_MAXGRIDDIM
          local_cell_max_dim(i) = distgrid%local_cell_max_dim(i)
        enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS
!
      end subroutine ESMF_DistGridGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSet - Set information about a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSet(distgrid, covers_domain, global_cell_count, &
                                  global_cell_dim, local_cell_max, &
                                  local_cell_max_dim, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      logical, intent(in), optional :: covers_domain
      integer, intent(in), optional :: global_cell_count
      integer, dimension(:), intent(in), optional :: global_cell_dim
      integer, intent(in), optional :: local_cell_max
      integer, dimension(:), intent(in), optional :: local_cell_max_dim
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Sets the {\tt ESMF\_DistGrid} object with information given.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be set.
!     \item[[covers\_domain]]
!          Logical identifier if distgrid covers the entire physical domain.
!     \item[[global\_cell\_count]]
!          Global total number of cells.
!     \item[[global\_cell\_dim]]
!          Array of the global number of cells in each dimension.
!     \item[[local\_cell\_max]]
!          Maximum number of cells on any {\tt ESMF\_DE}.
!     \item[[local\_cell\_max\_dim]]
!          Array of the maximum number of cells in each dimension on
!          any {\tt ESMF\_DE}.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: i

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     if present, set information filling in distgrid derived type
      if(present(covers_domain)) distgrid%covers_domain = covers_domain

      if(present(global_cell_count)) &
                 distgrid%global_cell_count = global_cell_count

      if(present(global_cell_dim)) then
                 ! TODO: add check that global_cell_dim is large enough
                 !       or use the size of the array for the loop limit
        do i = 1,ESMF_MAXGRIDDIM
          distgrid%global_cell_dim(i) = global_cell_dim(i)
        enddo
      endif

      if(present(local_cell_max)) distgrid%local_cell_max = local_cell_max

      if(present(local_cell_max_dim)) then
                 ! TODO: add check that local_cell_max_dim is large enough
                 !       or use the size of the array for the loop limit
        do i = 1,ESMF_MAXGRIDDIM
          distgrid%local_cell_max_dim(i) = local_cell_max_dim(i)
        enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetConfig - Get configuration information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetConfig(distgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(out) :: config   
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns the set of resources the {\tt ESMF\_DistGrid} object was configured with.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[config]
!          Configuration information.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_DistGridGetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetConfig - Set configuration information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetConfig(distgrid, config, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(in) :: config   
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Configures the {\tt ESMF\_DistGrid} object with set of resources given.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be configured.
!     \item[config]
!          Configuration information.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_DistGridSetConfig

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetValue - Get <Value> for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetValue(distgrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(out) :: value
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Returns the value of {\tt ESMF\_DistGrid} attribute <Value>.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[value]
!          Value to be retrieved.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_DistGridGetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetValue - Set <Value> for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetValue(DistGrid, value, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      integer, intent(in) :: value
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a {\tt ESMF\_DistGrid} attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[value]
!          Value to be set.         
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_DistGridSetValue

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetCounts - Get extent counts for a DistGrid for a given DE

! !INTERFACE:
      subroutine ESMF_DistGridGetCounts(distgrid, DE_id, lcell_count, &
                                        gcell_start, gcell_end, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in) :: DE_id
      integer, dimension(:), intent(inout), optional :: lcell_count
      integer, dimension(:), intent(inout), optional :: gcell_start
      integer, dimension(:), intent(inout), optional :: gcell_end
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get {\tt ESMF\_DistGrid} extent counts for a given {\tt ESMF\_DE}
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[DE\_id]]
!          Given {\tt ESMF\_DE}'s identifier.
!     \item[[lcell\_count]]
!          Array of the number of cells per dimension for this {\tt ESMF\_DE}.
!     \item[[gcell\_start]]
!          Array of the starting position, in the global decompostion, for
!          the cells per dimension for this {\t ESMF\_DE}.
!     \item[[gcell\_end]]
!          Array of the ending position, in the global decompostion, for
!          the cells per dimension for this {\tt ESMF\_DE}.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present
      integer :: i

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Retrieve extent counts for each axis from the de identifier
!     TODO:  check validity of DE_id
      if(present(lcell_count)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          lcell_count(i) = distgrid%ai_global(DE_id,i)%max &
                         - distgrid%ai_global(DE_id,i)%min + 1
        enddo
      endif
      if(present(gcell_start)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          gcell_start(i) = distgrid%ai_global(DE_id,i)%min
        enddo
      endif
      if(present(gcell_end)) then
        ! TODO:  add check for array size or use size for loop limit
        do i = 1,ESMF_MAXGRIDDIM
          gcell_end(i) = distgrid%ai_global(DE_id,i)%max
        enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetCounts

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetCountsInternal - Set extent counts for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetCountsInternal(distgrid, nDE, countsPerDE1, &
                                                countsPerDE2, periodic, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, dimension(ESMF_MAXGRIDDIM), intent(in) :: nDE
      integer, dimension(:), intent(in) :: countsPerDE1
      integer, dimension(:), intent(in) :: countsPerDE2
      type(ESMF_Logical), dimension(:), intent(in), optional :: periodic
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set {\tt ESMF\_DistGrid} extent counts
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[nDE]]
!          Array of number of {\tt ESMF\_DE}'s in the each direction.
!     \item[[countsPerDE1]]
!          Array of number of computational cells per DE in first direction.
!     \item[[countsPerDE2]]
!          Array of number of computational cells per DE in second direction.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      integer :: i, j, de
      integer :: local, thisde                       ! increment counters
      integer :: global_start, global_end            ! global counters
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Calculate number of local counts on each DE
      do j = 1,nDE(1)
        do i = 1,nDE(2)
          de = (i-1)*nDE(1) + j
          distgrid%local_cell_count(de) = countsPerDE1(j)*countsPerDE2(i)
        enddo
      enddo

!     Set extent counts for each axis from the number of de's and number
!     of cells
!     First in the 1 decomposition

      global_start = 1
      global_end = 0
!     if (periodic(1).eq.ESMF_TF_TRUE) then
!       global_start = countsPerDE1(nDE(1)) + 1
!       global_end   = countsPerDE1(nDE(1))
!     endif
      
      do j = 1,nDE(1)
        global_end = global_end + countsPerDE1(j)
        do i = 1,nDE(2)
          de = (i-1)*nDE(1) + j
          distgrid%global_start(de,1) = global_start - 1
          distgrid%ai_global(de,1)%min = global_start
          distgrid%ai_global(de,1)%max = global_end
        enddo
        global_start = global_end + 1
      enddo
      do j = 1,nDE(1)
        do i = 1,nDE(2)
          de = (i-1)*nDE(1) + j
          distgrid%ai_global(de,1)%stride = global_end
          if (present(periodic)) then
            if (periodic(1).eq.ESMF_TF_TRUE) &
            distgrid%ai_global(de,1)%stride = global_end + countsPerDE1(1) &
                                            + countsPerDE1(nDE(1))
          endif
        enddo
      enddo

!     Then the 2 decomposition
      global_start = 1
      global_end = 0
      if (present(periodic)) then
        if (periodic(2).eq.ESMF_TF_TRUE) then
          global_start = countsPerDE2(nDE(2)) + 1
          global_end   = countsPerDE2(nDE(2))
        endif
      endif

      do j = 1,nDE(2)
        global_end = global_end + countsPerDE2(j)
        do i = 1,nDE(1)
          de = (j-1)*nDE(1) + i
          distgrid%global_start(de,2) = global_start - 1
          distgrid%ai_global(de,2)%min = global_start
          distgrid%ai_global(de,2)%max = global_end
        enddo
        global_start = global_end + 1
      enddo
      do j = 1,nDE(2)
        do i = 1,nDE(1)
          de = (j-1)*nDE(1) + i
          distgrid%ai_global(de,2)%stride = global_end
          if (present(periodic)) then
            if (periodic(2).eq.ESMF_TF_TRUE) &
            distgrid%ai_global(de,2)%stride = global_end + countsPerDE2(1) &
                                            + countsPerDE2(nDE(2))
          endif
        enddo
      enddo

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetCountsInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetDE - Get DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetDE(distgrid, MyDE, local_cell_count, &
                                    global_start, ai_global, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(inout), optional :: MyDE
      integer, intent(inout), optional :: local_cell_count
      integer, dimension(:,:), intent(inout), optional :: global_start
      type(ESMF_AxisIndex), dimension(ESMF_MAXGRIDDIM), intent(inout), &
                        optional :: ai_global
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Class to be modified.
!     \item[[MyDE]]
!          Identifier for this {\tt ESMF\_DE}.
!     \item[[local\_cell\_count]]
!          Local (on this {\tt ESMF\_DE}) number of cells.
!     \item[[global\_start]]
!          Global index of starting count for cells.
!     \item[[ai\_global]]
!          Axis indices for cells on this DE.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     if present, get information from distgrid derived type
      if(present(MyDE)) MyDE = distgrid%myDE%MyDE

      if(present(local_cell_count)) &
                 local_cell_count = distgrid%myDE%local_cell_count

!jw      if(present(global_start)) global_start = distgrid%myDE%ai_global%min

      if(present(ai_global)) ai_global = distgrid%myDE%ai_global
! TODO:  how to query for parts of an Index type

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetDE

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetDEInternal - Set DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetDEInternal(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Set a {\tt ESMF\_DistGrid} attribute with the given value.
!     May be multiple routines, one per attribute.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present
      integer :: DE_id, local_cell_count, local_count_dim
      integer :: i

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_DELayoutGetDEid(distgrid%layout, DE_id, status)
      if((status .NE. ESMF_SUCCESS) .or. (DE_id .lt. 0)) then
        print *, "ERROR in ESMF_DistGridSetDEInternal: layout get DEid"
        return
      endif
      DE_id = DE_id + 1    ! TODO:  have to add one to go from C

      distgrid%MyDE%MyDE = DE_id
!     TODO: need to create the following with ESMFArrayCreate before doing this
!     distgrid%DEids(DE_id) = DE_id        ! need to add capability for this
!                                          ! not to be true

      local_cell_count = 1
      do i=1,ESMF_MAXGRIDDIM
        distgrid%MyDE%ai_global(i) = distgrid%ai_global(DE_id,i)
        distgrid%MyDE%global_start(i) = distgrid%global_start(DE_id,i)
        local_count_dim = distgrid%ai_global(DE_id,i)%max &
                        - distgrid%ai_global(DE_id,i)%min + 1
        if (local_count_dim.ne.0) local_cell_count = local_cell_count &
                                                   * local_count_dim
      enddo
      distgrid%MyDE%local_cell_count = local_cell_count

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetDEInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetAllAxisIndex - Get array of AxisIndices for DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetAllAxisIndex(distgrid, AI, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      type(ESMF_AxisIndex), dimension(:,:), pointer :: AI
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a {\tt ESMF\_DistGrid} attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Class to be modified.
!     \item[[AI]]
!          Array of {\tt AxisIndices} corresponding to the {\tt DistGrid}.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     get information from distgrid derived type
      AI => distgrid%ai_global

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetAllAxisIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetDELayout - Get pointer to a DELayout for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetDELayout(distgrid, layout, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      type(ESMF_DELayout) :: layout
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a DistGrid attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Class to be modified.
!     \item[[layout]]
!          The {\tt ESMF\_DELayout} corresponding to the {\tt ESMF\_DistGrid}.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     get information from distgrid derived type
      layout = distgrid%layout

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetDELayout

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_DistGridGlobalToLocalIndex(distgrid, global1D, local1D, &
                                                 global2D, local2D, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), intent(in) :: distgrid
      integer(ESMF_IKIND_I4), dimension(:), optional, intent(in) :: global1D
      integer(ESMF_IKIND_I4), dimension(:), optional, intent(out) :: local1D
      integer(ESMF_IKIND_I4), dimension(:,:), optional, intent(in) :: global2D
      integer(ESMF_IKIND_I4), dimension(:,:), optional, intent(out) :: local2D
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Translates global indexing to local indexing for a {\tt ESMF\_DistGrid}
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be used.
!     \item[[global1D]]
!          One-dimensional Array of global identifiers to be translated.
!          Infers translating between positions in memory.
!     \item[[local1D]]
!          One-dimensional Array of local identifiers corresponding to
!          global identifiers.
!     \item[[global2D]]
!          Two-dimensional Array of global identifiers to be translated.
!          Infers translating between indices in ij space.
!     \item[[local2D]]
!          Two-dimensional Array of local identifiers corresponding to
!          global identifiers.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present
      integer :: i, base, l1, r1, l2, r2

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     memory translation here
      if(present(global1D)) then

!     make sure local array is present as well
        if(.not. present(local1D)) then
          print *, "ERROR in ESMF_DistGridGlobalToLocal: local array not ", &
                   "present"
          return
        endif
!     make sure array lengths are the same
        if(size(global1D) .NE. size(local1D)) then
          print *, "ERROR in ESMF_DistGridGlobalToLocal: array lengths not ", &
                   "equal"
          return
        endif

!     the following code works only for grid where the global data is
!     organized (indexed) by DE  !TODO add coding for other cases
!     TODO: decide where enumerator for grid organization should be
!     TODO: this assumes exclusive indexing for local cells - total too?
!jw        base = distgrid%MyDE%global_start
        do i = 1, size(global1D)
          local1D(i) = global1D(i) - base
        enddo
  
      endif

!     index translation here
      if(present(global2D)) then

!     make sure local array is present as well
        if(.not. present(local2D)) then
          print *, "ERROR in ESMF_DistGridGlobalToLocal: global array not ", &
                   "present"
          return
        endif
!     make sure array lengths are the same
        if(size(global2D) .NE. size(local2D)) then
          print *, "ERROR in ESMF_DistGridGlobalToLocal: array lengths not ", &
                   "equal"
          return
        endif

        l1 = distgrid%MyDE%ai_global(1)%min
        r1 = distgrid%MyDE%ai_global(1)%max
        l2 = distgrid%MyDE%ai_global(2)%min
        r2 = distgrid%MyDE%ai_global(2)%max
        do i = 1, size(global2D,1)
          if(global2D(i,1).ge.l1 .and. global2D(i,1).le.r1 .and. &
             global2D(i,2).ge.l2 .and. global2D(i,2).le.r2 ) then
            local2D(i,1) = global2D(i,1) - l1
            local2D(i,2) = global2D(i,2) - l2
          else
            local2D(i,1) = -1    ! TODO:  make an ESMF_NOTFOUND to use instead of -1
            local2D(i,2) = -1
          endif
        enddo
  
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGlobalToLocalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridLocalToGlobalIndex - translate local indexing to global

! !INTERFACE:
      subroutine ESMF_DistGridLocalToGlobalIndex(distgrid, local1D, global1D, &
                                                 local2D, global2D, &
                                                 localAI1D, globalAI1D, &
                                                 localAI2D, globalAI2D, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType), intent(in) :: distgrid
      integer(ESMF_IKIND_I4), dimension(:), optional, intent(in) :: local1D
      integer(ESMF_IKIND_I4), dimension(:), optional, intent(out) :: global1D
      integer(ESMF_IKIND_I4), dimension(:,:), optional, intent(in) :: local2D
      integer(ESMF_IKIND_I4), dimension(:,:), optional, intent(out) :: global2D
      type(ESMF_AxisIndex), dimension(:), optional, intent(in) ::  localAI1D
      type(ESMF_AxisIndex), dimension(:), optional, intent(out) :: globalAI1D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(in) ::  localAI2D
      type(ESMF_AxisIndex), dimension(:,:), optional, intent(out) :: globalAI2D
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Translates local indexing to global indexing for a {\tt ESMF\_DistGrid}
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be used.
!     \item[[local1D]]
!          One-dimensional Array of local identifiers to be translated.
!          Infers translating between positions in memory.
!     \item[[global1D]]
!          One-dimensional Array of global identifiers corresponding to
!          local identifiers.
!     \item[[local2D]]
!          Two-dimensional Array of local identifiers to be translated.
!          Infers translating between indices in ij space.
!     \item[[global2D]]
!          Two-dimensional Array of global identifiers corresponding to
!          local identifiers.
!     \item[{[localAI1D]}]
!          One-dimensional array of local AxisIndices to be translated.
!     \item[{[globalAI1D]}]
!          One-dimensional array of global AxisIndices corresponding to local AIs.
!     \item[{[localAI2D]}]
!          Two-dimensional array of local AxisIndices to be translated.
!     \item[{[globalAI2D]}]
!          Two-dimensional array of global AxisIndices corresponding to local AIs.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present
      integer :: i, j,  base, local_count

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     1-D memory translation here
      if(present(local1D)) then
!       make sure global array is present as well
        if(.not. present(global1D)) then
          print *, "ERROR in ESMF_DistGridLocalToGlobal: global array not ", &
                   "present"
          return
        endif
!       make sure array lengths are the same
        if(size(global1D) .NE. size(local1D)) then
          print *, "ERROR in ESMF_DistGridLocalToGlobal: array lengths not ", &
                   "equal"
          return
        endif
!       the following code works only for grid where the global data is
!       organized (indexed) by DE  !TODO add coding for other cases
!       TODO: decide where enumerator for grid organization should be
!       TODO: this assumes exclusive indexing for local cells - total too?
        base = 0
        if (distgrid%MyDE%MyDE.ne.1) then
          do i = 1,distgrid%MyDE%MyDE-1
            base = base + distgrid%local_cell_count(i)
          enddo
        endif
        do i = 1, size(local1D)
          global1D(i) = local1D(i) + base
        enddo
      endif

!     2-D index translation here
      if(present(local2D)) then
!       make sure global array is present as well
        if(.not. present(global2D)) then
          print *, "ERROR in ESMF_DistGridLocalToGlobal: global array not ", &
                   "present"
          return
        endif
!       make sure array lengths are the same
        if(size(global2D) .NE. size(local2D)) then
          print *, "ERROR in ESMF_DistGridLocalToGlobal: array lengths not ", &
                   "equal"
          return
        endif
!jw        base = distgrid%MyDE%global_start
        do i = 1, size(local2D,1)
          global2D(i,1) = local2D(i,1) + base
          global2D(i,2) = local2D(i,2) + base
        enddo
      endif

!     1-D AxisIndex translation here
      if(present(localAI1D)) then
!       make sure global AI array is present as well
        if(.not. present(globalAI1D)) then
          print *, "ERROR in ESMF_DistGridLocalToGlobal: global array not ", &
                   "present"
          return
        endif
!       make sure array lengths are the same
        if(size(globalAI1D) .NE. size(localAI1D)) then
          print *, "ERROR in ESMF_DistGridLocalToGlobal: array lengths not ", &
                   "equal"
          return
        endif
        local_count = distgrid%MyDE%ai_global(1)%max &
                    - distgrid%MyDE%ai_global(1)%min + 1
        do i = 1, size(localAI1D)
          globalAI1D(i)%min = localAI1D(i)%min + distgrid%MyDE%global_start(1)
          globalAI1D(i)%max = localAI1D(i)%max + distgrid%MyDE%global_start(1)
          globalAI1D(i)%stride = localAI1D(i)%stride - local_count &
                               + distgrid%MyDE%ai_global(1)%stride
        enddo
      endif

!     2-D AxisIndex translation here
      if(present(localAI2D)) then
!       make sure global ai array is present as well
        if(.not. present(globalAI2D)) then
          print *, "ERROR in ESMF_DistGridLocalToGlobal: global array not ", &
                   "present"
          return
        endif
!       make sure array lengths are the same
        if(size(globalAI2D) .NE. size(localAI2D)) then
          print *, "ERROR in ESMF_DistGridLocalToGlobal: array lengths not ", &
                   "equal"
          return
        endif
        do j = 1, size(localAI2D,2)
          do i = 1, size(localAI2D,1)
            local_count = distgrid%ai_global(i,j)%max &
                        - distgrid%ai_global(i,j)%min + 1
            globalAI2D(i,j)%min = localAI2D(i,j)%min + distgrid%global_start(i,j)
            globalAI2D(i,j)%max = localAI2D(i,j)%max + distgrid%global_start(i,j)
            globalAI2D(i,j)%stride = localAI2D(i,j)%stride - local_count &
                                   + distgrid%ai_global(i,j)%stride
          enddo
        enddo
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridLocalToGlobalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridValidate - Check internal consistency of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridValidate(distgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid       
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that a {\tt ESMF\_DistGrid} is internally consistent.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[[opt]]
!          Validation options.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end subroutine ESMF_DistGridValidate

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridPrint - Print the contents of a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridPrint(distgrid, opt, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid      
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about a {\tt ESMF\_DistGrid}.  
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[[opt]]
!          Print ptions that control the type of information and level of 
!          detail.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  SSSn.n, GGGn.n
 
      integer :: i, j
      type(ESMF_DistGridType) , pointer :: dg

      print *, 'DistGrid Print:'

      dg => distgrid%ptr


      !Print the global axis indicies per DE
      if (associated(dg%ai_global)) then
      do i = 1, size(dg%ai_global, 1)
         print *, '   DE:', i
         do j = 1, size(dg%ai_global, 2)
            print *, 'min:', dg%ai_global(i,j)%min, 'max:', &
            dg%ai_global(i,j)%max
         enddo
      end do
      else
      print *, 'ai_global array not associated'
      endif



      end subroutine ESMF_DistGridPrint

!------------------------------------------------------------------------------

      end module ESMF_DistGridMod
