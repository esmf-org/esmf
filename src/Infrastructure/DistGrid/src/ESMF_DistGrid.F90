! $Id: ESMF_DistGrid.F90,v 1.24 2003/01/16 18:16:03 jwolfe Exp $
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
!BOP
! !MODULE: ESMF_DistGridMod - contains Grid decompostion methods
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt DistGrid} class, which contains a
! collection of subgrids which constitute a single logical {\tt Grid}. The
! subgrids can be operated on in parallel on a multiprocessor machine. The
! {\tt DistGrid} class contains the mapping between the local grid 
! decompositions and the global logical {\tt Grid}. It contains methods to
! synchronize data values between the boundaries of subsets, and to collect
! and communicate global data values. It interacts closely with the
! {\tt PhysGrid} object.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_ArrayMod
      use ESMF_BaseMod
      use ESMF_LayoutMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_DistGridConfig
!
!     ! Description of ESMF_DistGridConfig

      type ESMF_DistGridConfig
      sequence
      private
        integer :: dummy
!       < insert other class members here >
      end type

!------------------------------------------------------------------------------
!     ! ESMF_Axis
!
!     ! Description of ESMF_Axis

      type ESMF_Axis
      sequence
!     private
        integer :: start
        integer :: end
        integer :: size
      end type

!------------------------------------------------------------------------------
!     ! ESMF_Decomp
!
!     ! Description of ESMF_Decomp

      type ESMF_Decomp
      sequence
      private
        type (ESMF_Axis) :: DEdir1         ! axis decomposition in 1st dir
        type (ESMF_Axis) :: DEdir2         ! axis decomposition in 2nd dir
        logical ::  periodic_dir1          ! periodic boundary in 1st dir
        logical ::  periodic_dir2          ! periodic boundary in 2nd dir
        integer :: num_masks               ! number of decomposition masks
        type (ESMF_Array) :: mask          ! decomposition masks
      end type

!------------------------------------------------------------------------------
!     ! ESMF_MyDE
!
!     ! Description of ESMF_MyDE

      type ESMF_MyDE
      sequence
!     private
        integer :: MyDE     ! identifier for this DE
        integer :: MyDEx    ! identifier for this DE's position in the 1st dir
                            ! decomposition
        integer :: MyDEy    ! identifier for this DE's position in the 2nd dir
                            ! decomposition
        integer :: DE_E     ! identifier for DE to the east
        integer :: DE_W     ! identifier for DE to the west
        integer :: DE_N     ! identifier for DE to the north
        integer :: DE_S     ! identifier for DE to the south
        integer :: DE_NE    ! identifier for DE to the northeast
        integer :: DE_NW    ! identifier for DE to the northwest
        integer :: DE_SE    ! identifier for DE to the southeast
        integer :: DE_SW    ! identifier for DE to the southwest
        integer :: lsize    ! local (on this DE) number of cells
        integer :: gstart   ! global index of starting count
                            ! currently as the constant that should be added
                            ! to local index  TODO: really an array?
        type (ESMF_Axis) :: n_dir1  ! local cell count in 1st dir, in
                                    ! global index
        type (ESMF_Axis) :: n_dir2  ! local cell count in 2nd dir, in
                                    ! global index
      end type

!------------------------------------------------------------------------------
!     !  ESMF_DistGridType
!
!     !  Description of ESMF_DistGrid. 
!     !  WARNING!  this is not complete -- there is no halo functionality

      type ESMF_DistGridType
      sequence
!     private
        type (ESMF_Base) :: base
!       type (ESMF_Layout), dimension(:), pointer :: layouts
        type (ESMF_Layout) :: layout
        type (ESMF_Decomp) :: decomp       ! DE decomposition object
        type (ESMF_MyDE) :: MyDE           ! local DE identifiers
        integer :: gsize                   ! global number of cells
        integer :: gsize_dir1              ! global number of cells in 1st dir
        integer :: gsize_dir2              ! global number of cells in 2nd dir
        type (ESMF_Array) :: DEids         ! array of all DE identifiers
        type (ESMF_Array) :: DEx
        type (ESMF_Array) :: DEy
        integer, dimension(:), pointer :: start1
        integer, dimension(:), pointer :: end1
        integer, dimension(:), pointer :: size1
        integer, dimension(:), pointer :: start2
        integer, dimension(:), pointer :: end2
        integer, dimension(:), pointer :: size2
        integer, dimension(:), pointer :: gstart
        integer :: maxsize_dir1            ! maximum DE cell count in 1st dir
        integer :: maxsize_dir2            ! maximum DE cell count in 2nd dir
        logical :: covers_domain_dir1      ! identifiers if distgrid covers
        logical :: covers_domain_dir2      ! the entire physical domain
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
      public ESMF_DistGridConfig
      public ESMF_MyDE
      public ESMF_Axis
      public ESMF_Decomp
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
    public ESMF_DistGridGetInfo
    public ESMF_DistGridSetInfo
    public ESMF_DistGridGetCounts
    public ESMF_DistGridSetCounts
    public ESMF_DistGridGetDE
    public ESMF_DistGridSetDE
    public ESMF_DistGridSetDecomp
    public ESMF_DistGridGetValue
    public ESMF_DistGridSetValue
    public ESMF_DistGridLocalToGlobalIndex
    public ESMF_DistGridGlobalToLocalIndex
    public ESMF_DistGridValidate
    public ESMF_DistGridPrint
 
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DistGrid.F90,v 1.24 2003/01/16 18:16:03 jwolfe Exp $'

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
!        module procedure ESMF_DistGridCreateCopy

! !DESCRIPTION:
!     This interface provides a single entry point for DistGrid create
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_DistGridConstruct

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridConstructNew
         module procedure ESMF_DistGridConstructInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that construct
!     a complete {\tt DistGrid}.
!
!EOP
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
!     extent counts in a {\tt DistGrid}.
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
!     extent counts in a {\tt DistGrid}.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_DistGridSetDecomp

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_DistGridSetDecompInternal

! !DESCRIPTION:
!     This interface provides a single entry point for methods that set
!     extent counts in a {\tt DistGrid}.
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
!     Allocates memory for a new {\tt DistGrid} object and constructs its
!     internals.  Returns a pointer to a new {\tt DistGrid}.
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
      function ESMF_DistGridCreateInternal(nDE_i, nDE_j, i_max, j_max, &
                                           layout, name, rc)
!
! !RETURN VALUE:
      type(ESMF_DistGrid) :: ESMF_DistGridCreateInternal
!
! !ARGUMENTS:
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      type (ESMF_Layout), intent(in), optional :: layout
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               

! !DESCRIPTION:
!     Allocates memory for a new {\tt DistGrid} object, constructs its
!     internals, and internally sets necessary attributes and values.
!     Returns a pointer to a new {\tt DistGrid}.
!
!     The arguments are:
!     \begin{description}
!     \item[[nDE\_i]] 
!          Number of distributed elements (DE's) in the 1st direction.
!     \item[[nDE\_j]] 
!          Number of distributed elements (DE's) in the 2nd direction.
!     \item[[i\_max]] 
!          Global number of computation cells in the 1st direction.
!     \item[[j\_max]] 
!          Global number of computation cells in the 2nd direction.
!     \item[[layout]]
!          Layout of DE's.
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

!     Call construction method to allocate and initialize grid internals.
      call ESMF_DistGridConstructInternal(distgrid, nDE_i, nDE_j, i_max, &
                                          j_max, layout, name, rc)

!     Set return values.
      ESMF_DistGridCreateInternal%ptr => distgrid
      if(rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DistGridCreateInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridDestroy - Free all resources associated with a DistGrid 

! !INTERFACE:
      subroutine ESMF_DistGridDestroy(distgrid, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid   
      integer, intent(out), optional :: rc        
!
! !DESCRIPTION:
!     Destroys a {\tt DistGrid} object previously allocated
!     via an {\tt ESMF\_DistGridCreate routine}.
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

      integer :: status=ESMF_SUCCESS               ! Error status
      logical :: rcpresent=.FALSE.                 ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_LayoutDestroy(distgrid%ptr%layout, status)

      end subroutine ESMF_DistGridDestroy

!------------------------------------------------------------------------------
!BOP
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
!     allocated {\tt DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF\_DistGridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Pointer to a {\tt DistGrid}.
!     \item[[name]] 
!          Name of the {\tt DistGrid}.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_SUCCESS               ! Error status
      logical :: rcpresent=.FALSE.                 ! Return code present
      character (len = ESMF_MAXSTR) :: defaultname ! default distgrid name

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Set the DistGrid name if present, otherwise construct a default one
      if(present(name)) then
        call ESMF_SetName(distgrid%base, name, status)
      else
        defaultname = "default_distgrid_name"
        call ESMF_SetName(distgrid%base, defaultname, status)
      endif
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructNew: Setname"
        return
      endif

!     Initialize distgrid contents
      distgrid%decomp%DEdir1%start = 0
      distgrid%decomp%DEdir1%end = 0
      distgrid%decomp%DEdir1%size = 0
      distgrid%decomp%DEdir2%start = 0
      distgrid%decomp%DEdir2%end = 0
      distgrid%decomp%DEdir2%size = 0
      distgrid%decomp%periodic_dir1 = .false.
      distgrid%decomp%periodic_dir2 = .false.
      distgrid%decomp%num_masks = 0
      distgrid%MyDE%MyDE = 0
      distgrid%MyDE%MyDEx = 0
      distgrid%MyDE%MyDEy = 0
      distgrid%MyDE%DE_E = 0
      distgrid%MyDE%DE_W = 0
      distgrid%MyDE%DE_N = 0
      distgrid%MyDE%DE_S = 0
      distgrid%MyDE%DE_NE = 0
      distgrid%MyDE%DE_NW = 0
      distgrid%MyDE%DE_SE = 0
      distgrid%MyDE%DE_SW = 0
      distgrid%MyDE%lsize = 0
      distgrid%MyDE%gstart = 0
      distgrid%MyDE%n_dir1%start = 0
      distgrid%MyDE%n_dir1%end = 0
      distgrid%MyDE%n_dir1%size = 0
      distgrid%MyDE%n_dir2%start = 0
      distgrid%MyDE%n_dir2%end = 0
      distgrid%MyDE%n_dir2%size = 0
      distgrid%gsize = 0
      distgrid%gsize_dir1 = 0
      distgrid%gsize_dir2 = 0
!      nullify(distgrid%start1)
!      nullify(distgrid%end1)
!      nullify(distgrid%size1)
!      nullify(distgrid%start2)
!      nullify(distgrid%end2)
!      nullify(distgrid%size2)
      nullify(distgrid%gstart)
!     nullify(distgrid%global_dir1)
!     nullify(distgrid%global_dir2)
      distgrid%maxsize_dir1 = 0
      distgrid%maxsize_dir2 = 0
      distgrid%covers_domain_dir1 = .false.
      distgrid%covers_domain_dir2 = .false.

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructNew

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridConstructInternal - Construct the internals of an allocated DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridConstructInternal(distgrid, nDE_i, nDE_j, &
                                                i_max, j_max, layout, name, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid 
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      type (ESMF_Layout), intent(in), optional :: layout
      character (len = *), intent(in), optional :: name  
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     ESMF routine which fills in the contents of an already
!     allocated {\tt DistGrid} object.  May perform additional allocations
!     as needed.  Must call the corresponding ESMF\_DistGridDestruct
!     routine to free the additional memory.  Intended for internal
!     ESMF use only; end-users use {\tt ESMF\_DistGridCreate}, which calls
!     {\tt ESMF\_DistGridConstruct}. 
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Pointer to a {\tt DistGrid}.
!     \item[[nDE\_i]] 
!          Number of distributed elements (DE's) in the 1st direction.
!     \item[[nDE\_j]] 
!          Number of distributed elements (DE's) in the 2nd direction.
!     \item[[i\_max]] 
!          Global number of computation cells in the 1st direction.
!     \item[[j\_max]] 
!          Global number of computation cells in the 2nd direction.
!     \item[[name]] 
!          {\tt DistGrid} name.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
! !REQUIREMENTS:  TODO
!EOP

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present
      integer :: gsize_dir1
      integer :: gsize_dir2
      integer :: gsize
      integer, dimension(:), allocatable :: PEList
      logical :: cover_domain_dir1
      logical :: cover_domain_dir2
      integer :: i, lDE_i, lDE_j

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     Initialize the derived type contents
      call ESMF_DistGridConstructNew(distgrid, name, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: DistGrid construct"
        return
      endif

!     Fill in distgrid derived type with input or default
!     TODO:  temporary fix, also add defaults to objects
      call ESMF_DistGridSetInfo(distgrid, gsize_dir1=i_max, &
                                gsize_dir2=j_max, &
                                gsize=i_max*j_max, &
                                covers_domain_dir1=.true., &
                                covers_domain_dir2=.true., rc=status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: distgrid setinfo"
        return
      endif

!     If a layout is passed in, set the distgrid layout to it.
!     Otherwise create layout with specified decomposition.
      if(present(layout)) then
        call ESMF_LayoutGetSize(layout, lDE_i, lDE_j, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_DistGridConstructInternal: Layout get size"
          return
        endif
        distgrid%layout = layout
      else
        if(nDE_i.eq.0 .or. nDE_j.eq.0) then
          print *, "ERROR in ESMF_DistGridConstructInternal: nDE_i or nDE_j"
          return
        endif
        allocate(PEList(nDE_i*nDE_j))
        do i = 1,nDE_i*nDE_j
          PEList(i) = i - 1  ! TODO:  short-term fix to go to C++
        enddo
        distgrid%layout = ESMF_LayoutCreate(nDE_i, nDE_j, PEList, &
                                            ESMF_XFAST, status)
        if(status .NE. ESMF_SUCCESS) then
          print *, "ERROR in ESMF_DistGridConstructInternal: Layout create"
          return
        endif
        lDE_i = nDE_i
        lDE_j = nDE_j
      endif

!     Allocate resources based on number of DE's
      allocate(distgrid%start1(lDE_i*lDE_j), stat=status)
      allocate(distgrid%end1(lDE_i*lDE_j), stat=status)
      allocate(distgrid%size1(lDE_i*lDE_j), stat=status)
      allocate(distgrid%start2(lDE_i*lDE_j), stat=status)
      allocate(distgrid%end2(lDE_i*lDE_j), stat=status)
      allocate(distgrid%size2(lDE_i*lDE_j), stat=status)
      allocate(distgrid%gstart(lDE_i*lDE_j), stat=status)

!jw   allocate(distgrid%DEids(lDE_i*lDE_j), stat=status)  TODO: use ESMF_ArrayCreate
!jw   allocate(distgrid%DEx(lDE_i*lDE_j), stat=status)
!jw   allocate(distgrid%DEy(lDE_i*lDE_j), stat=status)
      if(status .NE. 0) then
        print *, "ERROR in ESMF_DistGridConstructInternal: allocate"
        return
      endif

!     Set decomposition based on input numbers of processors
      call ESMF_DistGridSetDecomp(distgrid, lDE_i, lDE_j, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: distgrid set decomp"
        return
      endif

!     Parse problem size
      call ESMF_DistGridSetCounts(distgrid, lDE_i, lDE_j, i_max, j_max, &
                                  status)
        
!     Fill in DE derived type
      call ESMF_DistGridSetDE(distgrid, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: Set de"
        return
      endif

!     Calculate other distgrid values from DE information
!     distgrid%maxsize_dir1 = GlobalCommMax()
!     distgrid%maxsize_dir2 = GlobalCommMax()

      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridConstructInternal: DistGrid construct"
        return
      endif

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridConstructInternal

!------------------------------------------------------------------------------
!BOP
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
!EOP
! !REQUIREMENTS: 

!
!  code goes here
!
      end subroutine ESMF_DistGridDestruct

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetInfo - Get information from a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetInfo(distgrid, &
                                      gsize_dir1, gsize_dir2, gsize, &
                                      covers_domain_dir1, &
                                      covers_domain_dir2, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(inout), optional :: gsize_dir1
      integer, intent(inout), optional :: gsize_dir2
      integer, intent(inout), optional :: gsize
      logical, intent(inout), optional :: covers_domain_dir1
      logical, intent(inout), optional :: covers_domain_dir2
      integer, intent(out), optional :: rc              
!
! !DESCRIPTION:
!     Returns information from the DistGrid object.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be queried.
!     \item[[gsize\_dir1]]
!          Global number of cells in 1st direction.
!     \item[[gsize\_dir2]]
!          Global number of cells in 2nd direction.
!     \item[[gsize]]
!          Global total number of cells.
!     \item[[covers\_domain\_dir1]]
!          Logical identifier if distgrid covers the entire physical domain
!          in the 1st direction.
!     \item[[covers\_domain\_dir2]]
!          Logical identifier if distgrid covers the entire physical domain
!          in the 2nd direction.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     if present, get information from distgrid derived type
      if(present(gsize_dir1)) gsize_dir1 = distgrid%gsize_dir1
      if(present(gsize_dir2)) gsize_dir2 = distgrid%gsize_dir2
      if(present(gsize)) gsize = distgrid%gsize
      if(present(covers_domain_dir1)) &
                 covers_domain_dir1 = distgrid%covers_domain_dir1
      if(present(covers_domain_dir2)) &
                 covers_domain_dir2 = distgrid%covers_domain_dir2

      if(rcpresent) rc = ESMF_SUCCESS
!
      end subroutine ESMF_DistGridGetInfo

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetInfo - Set information about a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetInfo(distgrid, gsize_dir1, gsize_dir2, &
                                      gsize, covers_domain_dir1, &
                                      covers_domain_dir2, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in), optional :: gsize_dir1
      integer, intent(in), optional :: gsize_dir2
      integer, intent(in), optional :: gsize
      logical, intent(in), optional :: covers_domain_dir1
      logical, intent(in), optional :: covers_domain_dir2
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!     Sets the DistGrid object with information given.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be set.
!     \item[[gsize\_dir1]]
!          Global number of cells in 1st direction.
!     \item[[gsize\_dir2]]
!          Global number of cells in 2nd direction.
!     \item[[gsize]]
!          Global total number of cells.
!     \item[[covers\_domain\_dir1]]
!          Logical identifier if distgrid covers the entire physical domain
!          in the 1st direction.
!     \item[[covers\_domain\_dir2]]
!          Logical identifier if distgrid covers the entire physical domain
!          in the 2nd direction.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_SUCCESS              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     if present, set information filling in distgrid derived type
      if(present(gsize_dir1)) distgrid%gsize_dir1 = gsize_dir1
      if(present(gsize_dir2)) distgrid%gsize_dir2 = gsize_dir2
      if(present(gsize)) distgrid%gsize = gsize
      if(present(covers_domain_dir1)) &
                 distgrid%covers_domain_dir1 = covers_domain_dir1
      if(present(covers_domain_dir2)) &
                 distgrid%covers_domain_dir2 = covers_domain_dir2

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetInfo

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
!     Returns the set of resources the DistGrid object was configured with.
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
!     Configures the DistGrid object with set of resources given.
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
!     Returns the value of DistGrid attribute <Value>.
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
!     Set a DistGrid attribute with the given value.
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
      subroutine ESMF_DistGridGetCounts(distgrid, DE_id, &
                                        size_dir1, &
                                        global_start_dir1, &
                                        global_end_dir1, &
                                        size_dir2, &
                                        global_start_dir2, &
                                        global_end_dir2, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in) :: DE_id
      integer, intent(inout), optional :: size_dir1
      integer, intent(inout), optional :: global_start_dir1
      integer, intent(inout), optional :: global_end_dir1
      integer, intent(inout), optional :: size_dir2
      integer, intent(inout), optional :: global_start_dir2
      integer, intent(inout), optional :: global_end_dir2
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get DistGrid extent counts for a given DE
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[DE\_id]]
!          Given DE's identifier.
!     \item[[size\_dir1]]
!          Extent count in the 1st direction.
!     \item[[global\_start\_dir1]]
!          Starting extent count in the 1st direction, in global index.
!     \item[[global\_end\_dir1]]
!          Ending extent count in the 1st direction, in global index.
!     \item[[size\_dir2]]
!          Extent count in the 2nd direction.
!     \item[[global\_start\_dir2]]
!          Starting extent count in the 2nd direction, in global index.
!     \item[[global\_end\_dir2]]
!          Ending extent count in the 2nd direction, in global index.
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

!     Retrieve extent counts for each axis from the de identifier
!     TODO:  check validity of DE_id
      if(present(size_dir1)) size_dir1 = distgrid%size1(DE_id)
      if(present(size_dir2)) size_dir2 = distgrid%size2(DE_id)
      if(present(global_start_dir1)) &
                 global_start_dir1 = distgrid%start1(DE_id)
      if(present(global_end_dir1)) &
                 global_end_dir1 = distgrid%end1(DE_id)
      if(present(global_start_dir2)) &
                 global_start_dir2 = distgrid%start2(DE_id)
      if(present(global_end_dir2)) &
                 global_end_dir2 = distgrid%end2(DE_id)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGetCounts

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetCountsInternal - Set extent counts for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetCountsInternal(distgrid, nDE_i, nDE_j, &
                                                i_max, j_max, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(in) :: i_max
      integer, intent(in) :: j_max
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set DistGrid extent counts
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[nDE\_i]]
!          Number of DE's in the 1st direction.
!     \item[[nDE\_j]]
!          Number of DE's in the 2nd direction.
!     \item[[i\_max]]
!          Number of cells in the 1st direction.
!     \item[[j\_max]]
!          Number of cells in the 2nd direction.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      integer :: i, j, de                            !
      integer :: ni, nj                              ! increment counters
      integer :: global_s, global_e                  ! global counters
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     Set extent counts for each axis from the number of de's and number
!     of cells
!     TODO:  this is not quite the correct algorithm to distribute over a
!            large number of DE's along an axis -- it loads up the residual
!            to the last one instead of spreading it around.  But OK for now.
      ni = i_max/nDE_i
      global_s = 1
      global_e = 0
      do i = 1,nDE_i
        global_e = global_e + ni
        if(i.eq.nDE_i) global_e = i_max
        do j = 1,nDE_j
          de = (j-1)*nDE_j + i
          distgrid%start1(de) = global_s
          distgrid%end1(de) = global_e
          distgrid%size1(de) = global_e - global_s + 1
        enddo
        global_s = global_e + 1
      enddo
      nj = j_max/nDE_j
      global_s = 1
      global_e = 0
      do j = 1,nDE_j
        global_e = global_e + nj
        if(j.eq.nDE_j) global_e = j_max
        do i = 1,nDE_i
          de = (j-1)*nDE_j + i
          distgrid%start2(de) = global_s
          distgrid%end2(de) = global_e
          distgrid%size2(de) = global_e - global_s + 1
        enddo
        global_s = global_e + 1
      enddo
      global_s = 0
      do j = 1,nDE_j
        do i = 1,nDE_i
          de = (j-1)*nDE_j + i
          distgrid%gstart(de) = global_s
          global_s = global_s + distgrid%size1(de)*distgrid%size2(de)
        enddo
      enddo

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetCountsInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGetDE - Get DE information for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridGetDE(distgrid, MyDE, MyDEx, MyDEy, &
                                    DE_E, DE_W, DE_N, DE_S, &
                                    DE_NE, DE_NW, DE_SE, DE_SW, &
                                    lsize, gstart, &
                                    n_dir1_start, n_dir1_end, n_dir1_size, &
                                    n_dir2_start, n_dir2_end, n_dir2_size, &
                                    rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(inout), optional :: MyDE
      integer, intent(inout), optional :: MyDEx
      integer, intent(inout), optional :: MyDEy
      integer, intent(inout), optional :: DE_E
      integer, intent(inout), optional :: DE_W
      integer, intent(inout), optional :: DE_N
      integer, intent(inout), optional :: DE_S
      integer, intent(inout), optional :: DE_NE
      integer, intent(inout), optional :: DE_NW
      integer, intent(inout), optional :: DE_SE
      integer, intent(inout), optional :: DE_SW
      integer, intent(inout), optional :: lsize
      integer, intent(inout), optional :: gstart
      integer, intent(inout), optional :: n_dir1_start
      integer, intent(inout), optional :: n_dir1_end
      integer, intent(inout), optional :: n_dir1_size
      integer, intent(inout), optional :: n_dir2_start
      integer, intent(inout), optional :: n_dir2_end
      integer, intent(inout), optional :: n_dir2_size
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Get a DistGrid attribute with the given value.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid]
!          Class to be modified.
!     \item[[MyDE]]
!          Identifier for this DE.
!     \item[[MyDEx]]
!          Identifier for this DE's position in the 1st dir decomposition.
!     \item[[MyDEy]]
!          Identifier for this DE's position in the 2nd dir decomposition.
!     \item[[DE\_E]]
!          Identifier for the DE to the east of this DE.
!     \item[[DE\_W]]
!          Identifier for the DE to the west of this DE.
!     \item[[DE\_N]]
!          Identifier for the DE to the north of this DE.
!     \item[[DE\_S]]
!          Identifier for the DE to the south of this DE.
!     \item[[DE\_NE]]
!          Identifier for the DE to the northeast of this DE.
!     \item[[DE\_NW]]
!          Identifier for the DE to the northwest of this DE.
!     \item[[DE\_SE]]
!          Identifier for the DE to the southeast of this DE.
!     \item[[DE\_SW]]
!          Identifier for the DE to the southwest of this DE.
!     \item[[lsize]]
!          Local (on this DE) number of cells.
!     \item[[gstart]]
!          Global index of starting count.
!     \item[[n\_dir1\_start]]
!          Starting index of this DE in 1st dir decomposition.
!     \item[[n\_dir1\_end]] 
!          Ending index of this DE in 1st dir decomposition.
!     \item[[n\_dir1\_size]]
!          Size of the 1st dir decomposition on this DE.
!     \item[[n\_dir2\_start]]
!          Starting index of this DE in 2nd dir decomposition.
!     \item[[n\_dir2\_end]]
!          Ending index of this DE in 2nd dir decomposition.
!     \item[[n\_dir2\_size]]
!          Size of the 2nd dir decomposition on this DE.
!     \item[[rc]]
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS: 

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present
      integer :: DE_id, DEx, DEy

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_LayoutGetDEPosition(distgrid%layout, DEx, DEy, status)
      call ESMF_LayoutGetDEid(distgrid%layout, DE_id, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridGetDE: layout get position"
        return
      endif
      DE_id = DE_id + 1    ! TODO:  have to add one to go from C
      DEx = DEx + 1
      DEy = DEy + 1

!     if present, get information from physgrid derived type
      if(present(MyDE)) MyDE = distgrid%myDE%MyDE
      if(present(MyDEx)) MyDEx = distgrid%myDE%MyDEx
      if(present(MyDEy)) MyDEy = distgrid%myDE%MyDEy
      if(present(DE_E)) DE_E = distgrid%myDE%DE_E
      if(present(DE_W)) DE_W = distgrid%myDE%DE_W
      if(present(DE_N)) DE_N = distgrid%myDE%DE_N
      if(present(DE_S)) DE_S = distgrid%myDE%DE_S
      if(present(DE_NE)) DE_NE = distgrid%myDE%DE_NE
      if(present(DE_NW)) DE_NW = distgrid%myDE%DE_NW
      if(present(DE_SE)) DE_SE = distgrid%myDE%DE_SE
      if(present(DE_SW)) DE_SW = distgrid%myDE%DE_SW
      if(present(lsize)) lsize = distgrid%myDE%lsize
      if(present(gstart)) gstart = distgrid%myDE%gstart
      if(present(n_dir1_start)) n_dir1_start = distgrid%myDE%n_dir1%start
      if(present(n_dir1_end)) n_dir1_end = distgrid%myDE%n_dir1%end
      if(present(n_dir1_size)) n_dir1_size = distgrid%myDE%n_dir1%size
      if(present(n_dir2_start)) n_dir2_start = distgrid%myDE%n_dir2%start
      if(present(n_dir2_end)) n_dir2_end = distgrid%myDE%n_dir2%end
      if(present(n_dir2_size)) n_dir2_size = distgrid%myDE%n_dir2%size

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
!     Set a DistGrid attribute with the given value.
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
      integer :: DE_id, DEx, DEy

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

      call ESMF_LayoutGetDEPosition(distgrid%layout, DEx, DEy, status)
      call ESMF_LayoutGetDEid(distgrid%layout, DE_id, status)
      if(status .NE. ESMF_SUCCESS) then
        print *, "ERROR in ESMF_DistGridSetDEInternal: layout get position"
        return
      endif
      DE_id = DE_id + 1    ! TODO:  have to add one to go from C
      DEx = DEx + 1
      DEy = DEy + 1
      write(*,*) 'DE_id, DEx, DEy = ', DE_id, DEx, DEy
!     TODO:  identify neighbors

      distgrid%MyDE%MyDE = DE_id
      distgrid%MyDE%MyDEx = DEx
      distgrid%MyDE%MyDEy = DEy
!     TODO: need to create the following with ESMFArrayCreate before doing this
!     distgrid%DEids(DE_id) = DE_id        ! need to add capability for this
!                                          ! not to be true
!     distgrid%DEx(DE_id) = DEx
!     distgrid%DEy(DE_id) = DEy

      distgrid%MyDE%n_dir1%start = distgrid%start1(DE_id)
      distgrid%MyDE%n_dir1%end = distgrid%end1(DE_id)
      distgrid%MyDE%n_dir1%size = distgrid%size1(DE_id)
      distgrid%MyDE%n_dir2%start = distgrid%start2(DE_id)
      distgrid%MyDE%n_dir2%end = distgrid%end2(DE_id)
      distgrid%MyDE%n_dir2%size = distgrid%size2(DE_id)
      distgrid%MyDE%lsize = distgrid%MyDE%n_dir1%size * &
                            distgrid%MyDE%n_dir2%size
      distgrid%MyDE%gstart = distgrid%gstart(DE_id)

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetDEInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridSetDecompInternal - Set decomposition for a DistGrid

! !INTERFACE:
      subroutine ESMF_DistGridSetDecompInternal(distgrid, nDE_i, nDE_j, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGridType) :: distgrid
      integer, intent(in) :: nDE_i
      integer, intent(in) :: nDE_j
      integer, intent(out), optional :: rc            

!
! !DESCRIPTION:
!     Set a DistGrid decomposition object corresponding to the given number
!     of DE's per decomposition axis.  The number of DE's for either axis
!     may be one to set a 1-D decomposition.
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be modified.
!     \item[[nDE\_i]]
!          Number of DE's in the 1st direction.
!     \item[[nDE\_j]]
!          Number of DE's in the 2nd direction.
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

      distgrid%decomp%DEdir1%start = 1
      distgrid%decomp%DEdir1%end = nDE_i
      distgrid%decomp%DEdir1%size = nDE_i
      distgrid%decomp%DEdir2%start = 1
      distgrid%decomp%DEdir2%end = nDE_j
      distgrid%decomp%DEdir2%size = nDE_j
      distgrid%decomp%periodic_dir1 = .false.
      distgrid%decomp%periodic_dir2 = .false.
      distgrid%decomp%num_masks = 0

      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridSetDecompInternal

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridGlobalToLocalIndex - translate global indexing to local

! !INTERFACE:
      subroutine ESMF_DistGridGlobalToLocalIndex(distgrid, global, local, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      type(ESMF_Array), intent(in) :: global
      type(ESMF_Array), intent(out) :: local
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Translates global indexing to local indexing for a DistGrid
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be used.
!     \item[[global]]
!          Array of global identifiers to be translated.
!     \item[[local]]
!          Array of local identifiers corresponding to global identifiers.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
!
!  code goes here
!
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DistGridGlobalToLocalIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DistGridLocalToGlobalIndex - translate local indexing to global

! !INTERFACE:
      subroutine ESMF_DistGridLocalToGlobalIndex(distgrid, local, global, rc)
!
! !ARGUMENTS:
      type(ESMF_DistGrid), intent(in) :: distgrid
      type(ESMF_Array), intent(in) :: local
      type(ESMF_Array), intent(out) :: global
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Translates local indexing to global indexing for a DistGrid
!
!     The arguments are:
!     \begin{description}
!     \item[distgrid] 
!          Class to be used.
!     \item[[local]]
!          Array of local identifiers to be translated.
!     \item[[global]]
!          Array of global identifiers corresponding to local identifiers.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

      integer :: status=ESMF_FAILURE                 ! Error status
      logical :: rcpresent=.FALSE.                   ! Return code present

!     Initialize return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif
!
!  code goes here
!
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
!     Validates that a DistGrid is internally consistent.
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
!      Print information about a DistGrid.  
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

!
!  code goes here
!
      end subroutine ESMF_DistGridPrint

!------------------------------------------------------------------------------

      end module ESMF_DistGridMod
