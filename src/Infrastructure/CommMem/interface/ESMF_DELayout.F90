! $Id: ESMF_DELayout.F90,v 1.20 2003/06/16 20:06:06 rstaufer Exp $
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
!     ESMF DELayout module
      module ESMF_DELayoutMod
!
!==============================================================================
!
! This file contains the DELayout class definition and all DELayout
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include <ESMF.h>
!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_DELayoutMod - F90 Interface to C++ ESMC_DELayout class
!
! !DESCRIPTION: !
! The code in this file implements the Fortran interfaces to the
! {\tt ESMF\_DELayout} class and associated functions and subroutines.  
!
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      implicit none

!  TODO: move to include file and share with C++ ?
      integer, parameter :: ESMF_NOHINT=0, ESMF_XFAST=1, ESMF_YFAST=2, &
                            ESMF_ZFAST=3

      integer, parameter :: ESMF_COMMTYPE_MP=0, ESMF_COMMTYPE_SHR=2

      integer, parameter :: ESMF_SUM=0, ESMF_MIN=1, ESMF_MAX=2

      integer, parameter :: ESMF_INT=0, ESMF_LONG=1, ESMF_FLOAT=2, &
                            ESMF_DOUBLE=3

! exclusivity type used for allocating DEs within a layout to sub-layouts
! TODO:  move to ESMF_DE.F90 when created, to be symmetrical with C++ ?
      integer, parameter :: ESMF_NONEXCL=0, ESMF_EXCL=1

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_DELayout
!
!     ! DELayout data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_DELayout
      sequence
      private
        type(ESMF_Pointer) :: this       ! opaque pointer to the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_DELayout
      public ESMF_NOHINT, ESMF_XFAST, ESMF_YFAST, ESMF_ZFAST
      public ESMF_COMMTYPE_MP, ESMF_COMMTYPE_SHR
      public ESMF_SUM, ESMF_MIN, ESMF_MAX
      public ESMF_NONEXCL, ESMF_EXCL
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_DELayoutCreate, ESMF_DELayoutCreateFromParent
      public ESMF_DELayoutDestroy

      !public ESMF_DELayoutSetData
      !public ESMF_DELayoutGetData
      !public ESMF_DELayoutGet
      public ESMF_DELayoutGetSize
      public ESMF_DELayoutGetDEPosition
      public ESMF_DELayoutGetDEID, ESMF_DELayoutGetDEIDat
      public ESMF_DELayoutGetParentDEID, ESMF_DELayoutGetChildDEID
      public ESMF_DELayoutGetDEExists
      public ESMF_DELayoutGetNumDEs
      public ESMF_DELayoutSetAxisIndex
      public ESMF_DELayoutGatherArrayI, ESMF_DELayoutGatherArrayR
 
      public ESMF_DELayoutCheckpoint
      public ESMF_DELayoutRestore
      public ESMF_DELayoutWrite
      public ESMF_DELayoutRead
 
      public ESMF_DELayoutPrint

      public ESMF_DELayoutSendRecv
      public ESMF_DELayoutBcast
      public ESMF_DELayoutAllReduce
      public ESMF_DELayoutAllGatherVI, ESMF_DELayoutAllGatherVR
!EOPI

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_DELayout.F90,v 1.20 2003/06/16 20:06:06 rstaufer Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutCreate -- Generic interface to create an DELayout

! !INTERFACE:
      interface ESMF_DELayoutCreate

! !PRIVATE MEMBER FUNCTIONS:
!
      module procedure ESMF_DELayoutCreateDefault1D
      module procedure ESMF_DELayoutCreateFromParent
      module procedure ESMF_DELayoutCreateFromDEList

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_DELayoutCreate} functions.   
!EOP 
      end interface

!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the DELayout Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutCreateDefault1D - Create 1D default DELayout object

! !INTERFACE:
      function ESMF_DELayoutCreateDefault1D(rc)
!
! !RETURN VALUE:
      type(ESMF_DELayout) :: ESMF_DELayoutCreateDefault1D
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new 1D DELayout object; self-discover PEList
!
!  The return value is a new DELayout.
!    
!  The arguments are:
!  \begin{description}
! 
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
      
!     Local variables.
      type (ESMF_DELayout) :: layout   ! opaque pointer to new C++ DELayout
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Initialize the pointer to null.
      layout%this = ESMF_NULL_POINTER

!     Routine which interfaces to the C++ creation routine.
      call c_ESMC_DELayoutCreateDefault1D(layout, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "DELayout creation error"
        return
      endif

!     set return values
      ESMF_DELayoutCreateDefault1D = layout 
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DELayoutCreateDefault1D

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutCreateFromParent - Create DELayout from a given layout

! !INTERFACE:
      function ESMF_DELayoutCreateFromParent(parent, ndim, lengths, &
                                    commtypes, parent_offsets, de_indices, rc)
!
! !RETURN VALUE:
      type(ESMF_DELayout) :: ESMF_DELayoutCreateFromParent
!
! !ARGUMENTS:
      type(ESMF_DELayout), intent(inout) :: parent  ! to allocate DEs from
      integer, intent(in) :: ndim                   ! number of dimensions
      integer, intent(in) :: lengths(:)             ! number of des in each dim
      integer, intent(in) :: commtypes(:)           ! comm types in each dim
      integer, intent(in), optional :: parent_offsets(:) ! offsets from parent 
      integer, intent(in), optional :: de_indices(:)  ! parent de indices 
      integer, intent(out), optional :: rc          ! return code
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_DELayout} using a parent {\tt ESMF\_DELayout}'s {\tt ESMF\_DE}s.  
!
!  The return value is a new {\tt ESMF\_DELayout}.
!    
!  The arguments are:
!  \begin{description}
! 
!   \item[parent]
!     Parent {\tt ESMF\_DELayout}.
! 
!   \item[ndim]
!     Dimension of new {\tt ESMF\_DELayout}.
!     
!   \item[lengths]
!     Array of length {\tt ndim} that contains the number of
!     {\tt DE}s in each dimension.
!
!   \item[commtypes]
!     Array of length {\tt ndim} that contains the communication
!     type of each dimension.  Valid values are {\tt ESMF\_COMMTYPE\_SHR},
!     {\tt ESMF\_COMMTYPE\_MP}, and {\tt ESMF\_COMMTYPE\_SHR}+
!     {\tt ESMF\_COMMTYPE\_MP}.
!
!   \item[{[parent\_offsets]}]
!     Offset in each parent {\tt ESMF\_DELayout} dimension.
! 
!   \item[{[de\_indices]}]
!     Selection of {\tt DE} indices to use.
!     
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP

!     Local variables.
      type (ESMF_DELayout) :: layout   ! opaque pointer to new C++ DELayout
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize the pointer to null.
      layout%this = ESMF_NULL_POINTER

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ creation routine.
!     Take care to not reference unspecified optional arguments; this
!     seems to cause problems on the alpha.
      if (present(parent_offsets) .and. present(de_indices)) then
          call c_ESMC_DELayoutCreateFParent(layout, parent, parent_offsets, &
                                 de_indices, ndim, lengths, commtypes, status)
      else if (present(parent_offsets)) then
          call c_ESMC_DELayoutCreateFParent(layout, parent, parent_offsets, &
                                 0, ndim, lengths, commtypes, status)
      else if (present(de_indices)) then
          call c_ESMC_DELayoutCreateFParent(layout, parent, 0, &
                                 de_indices, ndim, lengths, commtypes, status)
      else 
          print *, "Error: one of parent_offsets or de_indicies must be given"
          return
      endif

      if (status .ne. ESMF_SUCCESS) then
        print *, "DELayout creation error"
        return
      endif

!     set return values
      ESMF_DELayoutCreateFromParent = layout 
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DELayoutCreateFromParent

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutCreateFromDEList - Create DELayout from a DE List

! !INTERFACE:
      function ESMF_DELayoutCreateFromDEList(delist, ndim, lengths, &
               commtypes, rc)
!
! !RETURN VALUE:
      type(ESMF_DELayout) :: ESMF_DELayoutCreateFromDEList
!
! !ARGUMENTS:
      integer, intent(in) :: delist(:)            ! list of processing elements
      integer, intent(in) :: ndim                 ! number of dimensions
      integer, intent(in) :: lengths(:)           ! number of des in each dim
      integer, intent(in) :: commtypes(:)         ! comm types in each dim
      integer, intent(out), optional :: rc        ! return code
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_DELayout} from a list of {\tt ESMF\_DE} indices.  
!
!  The return value is a new {\tt ESMF\_DELayout}.
!    
!  The arguments are:
!  \begin{description}
! 
!   \item[delist]
!     List of {\tt ESMF\_DE} indices.
! 
!   \item[ndim]
!     Dimension of new {\tt ESMF\_DELayout}.
!     
!   \item[lengths]
!     Array of length {\tt ndim} that contains the number of
!     {\tt ESMF\_DE}s in each dimension.
!
!   \item[commtypes]
!     Array of length {\tt ndim} that contains the communication
!     type of each dimension.  Valid values are {\tt ESMF\_COMMTYPE\_SHR},
!     {\tt ESMF\_COMMTYPE\_MP}, and {\tt ESMF\_COMMTYPE\_SHR}+
!     {\tt ESMF\_COMMTYPE\_MP}.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP

!     Local variables.
      type (ESMF_DELayout) :: layout   ! opaque pointer to new C++ DELayout
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize the pointer to null.
      layout%this = ESMF_NULL_POINTER

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ creation routine.
      call c_ESMC_DELayoutCreateFDE(layout, delist, ndim, &
                 lengths, commtypes, status)

      if (status .ne. ESMF_SUCCESS) then
        print *, "DELayout creation error"
        return
      endif

!     set return values
      ESMF_DELayoutCreateFromDEList = layout 
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DELayoutCreateFromDEList

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutCreateNoData

! !INTERFACE:
      function ESMF_DELayoutCreateNoData(rc)
!
! !RETURN VALUE:
      type(ESMF_DELayout) :: ESMF_DELayoutCreateNoData
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Create a new empty {\tt ESMF\_DELayout} object.
!
!  The arguments are:
!  \begin{description}
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP

!     ! Local variables
      type (ESMF_DELayout) :: layout   ! new class being created
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     ! Initialize pointer
      layout%this = ESMF_NULL_POINTER

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     ! C routine which interfaces to the C++ routine which does actual work
      !call c_ESMC_DELayoutCreateNoData(layout, status)
      !if (status .ne. ESMF_SUCCESS) then
      !  print *, "DELayout construction error"
      !  return
      !endif

!     set return values
      ESMF_DELayoutCreateNoData = layout
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DELayoutCreateNoData

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutDestroy
!
! !INTERFACE:
      subroutine ESMF_DELayoutDestroy(layout, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_DELayout}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[layout]
!       Destroy contents of this {\tt ESMF\_DELayout}.
!
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     call destroy to release resources on the C++ side
      call c_ESMC_DELayoutDestroy(layout, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "DELayout destruction error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutDestroy

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutSetData - < desc here >
!
! !INTERFACE:
      subroutine ESMF_DELayoutSetData(layout, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used only with the version of DELayoutCreate which creates an empty
!      DELayout and allows the Data to be specified later.
!
!EOP
!
! TODO: code goes here
!
!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Query for information from the layout.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGet - <desc here>
!
! !INTERFACE:
      subroutine ESMF_DELayoutGet(layout, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns information about the {\tt ESMF\_DELayout}.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the layout input are optional to facilitate this
!
!EOP
!
! TODO: code goes here
!
!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGetNumDEs - return number of DEs in layout
!
! !INTERFACE:
      subroutine ESMF_DELayoutGetNumDEs(layout, nDEs, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(out) :: nDEs
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns the number of processors in the DELayout's processor list
!
!EOP
! !REQUIREMENTS:

!       Local variables.
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ routine.
        call c_ESMC_DELayoutGetNumDEs(layout, nDEs, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_DELayoutGetNumDEs error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_DELayoutGetNumDEs

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGetSize - return the Nx, Ny counts in a layout
!
! !INTERFACE:
      subroutine ESMF_DELayoutGetSize(layout, nx, ny, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(out) :: nx, ny             
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns information about the layout.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the layout input are optional to facilitate this.
!
!EOP

!     Local variables
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutGetSize(layout, nx, ny, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutGetSize error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutGetSize

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGetDEPosition
! !INTERFACE:
      subroutine ESMF_DELayoutGetDEPosition(layout, x, y, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(out) :: x, y             
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns information about the {\tt ESMF\_DELayout}.  For queries where the caller
!      only wants a single value, specify the argument by name.
!
!EOP

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutGetDEPosition(layout, x, y, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutGetDEPosition error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutGetDEPosition

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGetDEIDat
!
! !INTERFACE:
      subroutine ESMF_DELayoutGetDEIDat(layout, x, y, id, rc)
!
! TODO: overload this with a real 3d layout version that takes z
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(in) :: x, y             
      integer, intent(out) :: id
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns information about the {\tt ESMF\_DELayout}. 
!
!EOP

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutGetDEIDat(layout, x, y, 0, id, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutGetDEIDat error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutGetDEIDat

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGetDEID
!
! !INTERFACE:
      subroutine ESMF_DELayoutGetDEID(layout, id, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(out) :: id
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!     Returns DE index id where the current execution is running.
!
!EOP
! !REQUIREMENTS:

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutGetDEID(layout, id, status)
      if (status .ne. ESMF_SUCCESS) then
        !print *, "ESMF_DELayoutGetDEID error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutGetDEID

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGetParentDEID
!
! !INTERFACE:
      subroutine ESMF_DELayoutGetParentDEID(child, childid, parent, parentid, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout), intent(in) :: child
      integer, intent(in) :: childid
      type(ESMF_DELayout), intent(in) :: parent
      integer, intent(out) :: parentid
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!     Takes a layout which was created as a child of a parent layout, and an
!     DE index id in that layout.  Also takes the parent layout, and returns
!     the corresponding parent DE index id for the same PE.
!
!EOP
! !REQUIREMENTS:

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutGetParentDEID(child, childid, parent, parentid, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutGetParentDEID error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutGetParentDEID

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGetChildDEID
!
! !INTERFACE:
      subroutine ESMF_DELayoutGetChildDEID(parent, parentid, child, childid, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout), intent(in) :: parent
      integer, intent(in) :: parentid
      type(ESMF_DELayout), intent(in) :: child
      integer, intent(out) :: childid
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!     Takes a layout which was created as a child of a parent layout, and an
!     DE index id in that layout.  Also takes the parent layout, and returns
!     the corresponding parent DE index id for the same PE.
!
!EOP
! !REQUIREMENTS:

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutGetChildDEID(parent, parentid, child, childid, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutGetChildDEID error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutGetChildDEID

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGetDEExists
!
! !INTERFACE:
      subroutine ESMF_DELayoutGetDEExists(layout, deid, other, exists, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout), intent(in) :: layout
      integer, intent(in) :: deid
      type(ESMF_DELayout), intent(in) :: other
      type(ESMF_Logical), intent(out) :: exists
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!     Takes a layout and a DE index id in that layout, plus another layout
!     which is either this layout's parent or child, and returns the logical
!     value to say whether this DE is contained in the other layout.
!
!EOP
! !REQUIREMENTS:

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutGetDEExists(layout, deid, other, exists, status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutGetDEExists error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutGetDEExists

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutSetAxisIndex
!
! !INTERFACE:
      subroutine ESMF_DELayoutSetAxisIndex(layout, global_counts, decompids, &
                                           AIPtr, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: global_counts
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:) :: AIPtr
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!    Set local/global information
!
!
!EOP
! !REQUIREMENTS:

!     Local variables.
      integer :: size_gcount           ! size of the global counts array
      integer :: size_decomp           ! size of the decompids array

      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      size_gcount = size(global_counts)
      size_decomp = size(decompids)
      call c_ESMC_DELayoutSetAxisIndex(layout, global_counts, size_gcount, &
                                       decompids, size_decomp, AIPtr, status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_DELayoutSetAxisIndex error"
          return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutSetAxisIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGatherArrayI
!
! !INTERFACE:
      subroutine ESMF_DELayoutGatherArrayI(layout, DistArray, decompids, &
                                           AIPtr, AIPtr2, GlobalArray, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: DistArray
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:) :: AIPtr
      type(ESMF_AxisIndex), dimension(:) :: AIPtr2
      integer, dimension(:), intent(out) :: GlobalArray
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!     Performs an MPI-like Array Gather to a single processor for an int array.
!
!EOP
! !REQUIREMENTS:

!       Local variables.
        integer :: size_decomp              ! size of the decompids array
        integer :: size_AI                  ! size of the axis indices arrays
        integer :: i

        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

!       initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! subtract one from location parts of indices to translate to C++
        size_AI = size(AIPtr)
        do i = 1,size_AI
          AIPtr(i)%l  = AIPtr(i)%l  - 1
          AIPtr(i)%r  = AIPtr(i)%r  - 1
          AIPtr2(i)%l = AIPtr2(i)%l - 1
          AIPtr2(i)%r = AIPtr2(i)%r - 1
        enddo

!       Routine which interfaces to the C++ routine.
        size_decomp = size(decompids)
        call c_ESMC_DELayoutGatherArrayI(layout, DistArray, decompids, &
                                         size_decomp, AIPtr, AIPtr2, &
                                         GlobalArray, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_DELayoutGatherArrayI error"
          ! Do *NOT* return before we put the +1 back.
        endif

! add one back to location parts of indices to translate from C++
        do i = 1,size_AI
          AIPtr(i)%l  = AIPtr(i)%l  + 1
          AIPtr(i)%r  = AIPtr(i)%r  + 1
          AIPtr2(i)%l = AIPtr2(i)%l + 1
          AIPtr2(i)%r = AIPtr2(i)%r + 1
        enddo

!       set return code if user specified it
        if (rcpresent) rc = status

        end subroutine ESMF_DELayoutGatherArrayI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutGatherArrayR
!
! !INTERFACE:
      subroutine ESMF_DELayoutGatherArrayR(layout, DistArray, decompids, &
                                           AIPtr, AIPtr2, GlobalArray, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      real(ESMF_IKIND_R4), dimension(:), intent(in) :: DistArray
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:) :: AIPtr
      type(ESMF_AxisIndex), dimension(:) :: AIPtr2
      real(ESMF_IKIND_R4), dimension(:), intent(out) :: GlobalArray
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!     Performs an MPI-like Array Gather to a single processor for a real array.
!
!
!EOP
! !REQUIREMENTS:

!       Local variables.
        integer :: size_decomp              ! size of the decompids array
        integer :: size_AI                  ! size of the axis indices arrays
        integer :: i

        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

!       initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! subtract one from location parts of indices to translate to C++
        size_AI = size(AIPtr)
        do i = 1,size_AI
          AIPtr(i)%l  = AIPtr(i)%l  - 1
          AIPtr(i)%r  = AIPtr(i)%r  - 1
          AIPtr2(i)%l = AIPtr2(i)%l - 1
          AIPtr2(i)%r = AIPtr2(i)%r - 1
        enddo

!       Routine which interfaces to the C++ routine.
        size_decomp = size(decompids)
        call c_ESMC_DELayoutGatherArrayR(layout, DistArray, decompids, &
                                       size_decomp, AIPtr, AIPtr2, &
                                       GlobalArray, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_DELayoutGatherArrayR error"
          ! Do *NOT* return before we put the +1 back.
        endif

! add one back to location parts of indices to translate from C++
        do i = 1,size_AI
          AIPtr(i)%l  = AIPtr(i)%l  + 1
          AIPtr(i)%r  = AIPtr(i)%r  + 1
          AIPtr2(i)%l = AIPtr2(i)%l + 1
          AIPtr2(i)%r = AIPtr2(i)%r + 1
        enddo

!       set return code if user specified it
        if (rcpresent) rc = status

        end subroutine ESMF_DELayoutGatherArrayR

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for DELayouts
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutCheckpoint
!
! !INTERFACE:
      subroutine ESMF_DELayoutCheckpoint(layout, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout):: layout 
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!      Used to save all data to disk as quickly as possible.  
!      (see Read/Write for other options).  Internally this routine uses the
!      same I/O interface as Read/Write, but the default options are to
!      select the fastest way to save data to disk.
!
!EOP
!
!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutCheckpoint


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutRestore
!
! !INTERFACE:
      function ESMF_DELayoutRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_DELayout) :: ESMF_DELayoutRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! layout name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a DELayout from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
!     Local variables.
      type (ESMF_DELayout) :: a 

      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif
!

!     ! add code here

      ESMF_DELayoutRestore = a 
 
!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DELayoutRestore


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutWrite
!
! !INTERFACE:
      subroutine ESMF_DELayoutWrite(layout, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!     Used to write data to persistent storage in a variety of formats.  
!     (see Checkpoint/Restore for quick data dumps.)  Details of I/O 
!     options specified in the IOSpec derived type. 
!
!
!EOP

!
!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
        rcpresent = .TRUE.
        rc = ESMF_FAILURE
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutWrite


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutRead
!
! !INTERFACE:
      function ESMF_DELayoutRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_DELayout) :: ESMF_DELayoutRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! layout name to read
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!     Used to read data from persistent storage in a variety of formats.
!
!
!EOP
!
!     Local variables.
      type (ESMF_DELayout) :: a

      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     ! add code here

      ESMF_DELayoutRead = a 
 
!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end function ESMF_DELayoutRead


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutPrint
!
! !INTERFACE:
      subroutine ESMF_DELayoutPrint(layout, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Print contents of a {\tt ESMF\_DELayout}.
!
!EOP

!     Local variables.
      character (len=6) :: defaultopts="brief"

      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     ! Interface to call the C++ print code
      if(present(options)) then
           call c_ESMC_DELayoutPrint(layout, options, status) 
      else
           call c_ESMC_DELayoutPrint(layout, defaultopts, status) 
      endif

      if (status .ne. ESMF_SUCCESS) then
         print *, "DELayout print error"
         return
      endif

!     set return values
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutPrint

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutAllReduce
!
! !INTERFACE:
      subroutine ESMF_DELayoutAllReduce(layout, dataArray, result, arrayLen, &
                                        op, rc)

! TODO: rename to ESMF_DELayoutAllReduceI for "integer" version ?
!
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(in) :: dataArray(:), arrayLen, op
      integer, intent(out) :: result 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Performs an MPI-like Allreduce for an integer array
!
!EOP

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutAllReduce(layout, dataArray, result, arrayLen, op, &
                                    status)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutAllReduce error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS
      end subroutine ESMF_DELayoutAllReduce

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutSendRecv
!
! !INTERFACE:
      subroutine ESMF_DELayoutSendRecv(layout, sarray, rarray, snum, &
                                      	rnum, sde_index, rde_index, rc)
!
!  TODO: rename SendRecvR for real
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      real(ESMF_IKIND_R4), intent(in) :: sarray(:), rarray(:)
      integer, intent(in) :: snum
      integer, intent(in) :: rnum
      integer, intent(in) :: sde_index, rde_index
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Performs an MPI-like send-receive for a real array.
!
!EOP

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutSendRecv(layout, sarray, rarray, snum, & 
        rnum, sde_index, rde_index, ESMF_FLOAT, rc)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutSendRecv error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_DELayoutSendRecv

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutBcast
!
! !INTERFACE:
      subroutine ESMF_DELayoutBcast(layout, array, num, &
                                    rootde_index, rc)
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      real(ESMF_IKIND_R4), intent(in) :: array
      integer, intent(in) :: num
      integer, intent(in) :: rootde_index
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Broadcasts data from a root {\tt ESMF\_DE} to all other {\tt ESMF\_DE}s in
!     the {\tt ESMF\_DELayout}.
!
!EOP

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutBcast(layout, array, num, rootde_index, &
			        ESMF_FLOAT, rc)
      if (status .ne. ESMF_SUCCESS) then
        print *, "ESMF_DELayoutBcast error"
        return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS
      end subroutine ESMF_DELayoutBcast

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutAllGatherVI - Perform an "all-gather" for int arrays
!
! !INTERFACE:
      subroutine ESMF_DELayoutAllGatherVI(layout, sndArray, sndLen, &
                                        rcvArray, rcvLen, rcvDispls, rc)
!
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      integer, intent(in) :: sndArray(:)
      integer, intent(in) :: sndLen
      integer, intent(out) :: rcvArray(:)
      integer, intent(in) :: rcvLen(:)
      integer, intent(in) :: rcvDispls(:)
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Perform an MPI-like Allgatherv for integer arrays across a layout
!
!EOP

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutAllGatherVI(layout, sndArray, sndLen, &
                                      rcvArray, rcvLen, rcvDispls, status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_DELayoutAllGatherVI error"
          return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS
      end subroutine ESMF_DELayoutAllGatherVI

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_DELayoutAllGatherVR - Perform an "all-gather" for real arrays
!
! !INTERFACE:
      subroutine ESMF_DELayoutAllGatherVR(layout, sndArray, sndLen, &
                                        rcvArray, rcvLen, rcvDispls, rc)
!
!
! !ARGUMENTS:
      type(ESMF_DELayout) :: layout
      real, intent(in) :: sndArray(:)
      integer, intent(in) :: sndLen
      real, intent(out) :: rcvArray(:)
      integer, intent(in) :: rcvLen(:)
      integer, intent(in) :: rcvDispls(:)
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Perform an MPI-like Allgatherv for real arrays across a layout.
!
!EOP

!     Local variables.
      integer :: status                ! Error status
      logical :: rcpresent             ! Return code present

!     Initialize return code; assume failure until success is certain.
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

!     Routine which interfaces to the C++ routine.
      call c_ESMC_DELayoutAllGatherVR(layout, sndArray, sndLen, &
                                      rcvArray, rcvLen, rcvDispls, status)
      if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_DELayoutAllGatherVR error"
          return
      endif

!     set return code if user specified it
      if (rcpresent) rc = ESMF_SUCCESS
      end subroutine ESMF_DELayoutAllGatherVR

      end module ESMF_DELayoutMod

