! $Id: ESMF_Layout.F90,v 1.11 2003/02/28 23:36:03 jwolfe Exp $
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
!     ESMF Layout module
      module ESMF_LayoutMod
!
!==============================================================================
!
! This file contains the Layout class definition and all Layout
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_LayoutMod - F90 Interface to C++ ESMC_Layout class
!
! !DESCRIPTION:
!
! The code in this file implements the Fortran interfaces to the
! {\tt Layout} class and associated functions and subroutines.  
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

      integer, parameter :: ESMF_SUM=0, ESMF_MIN=1, ESMF_MAX=2

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_Layout
!
!     ! Layout data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_Layout
      sequence
      private
        type(ESMF_Pointer) :: this       ! opaque pointer to the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Layout
      public ESMF_NOHINT, ESMF_XFAST, ESMF_YFAST, ESMF_ZFAST
      public ESMF_SUM, ESMF_MIN, ESMF_MAX
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_LayoutCreate
      public ESMF_LayoutDestroy
 
      !public ESMF_LayoutSetData
      !public ESMF_LayoutGetData
      !public ESMF_LayoutGet
      public ESMF_LayoutGetSize
      public ESMF_LayoutGetDEPosition
      public ESMF_LayoutGetDEid
      public ESMF_LayoutSetAxisIndex
      public ESMF_LayoutGatherArrayI
 
      public ESMF_LayoutCheckpoint
      public ESMF_LayoutRestore
      public ESMF_LayoutWrite
      public ESMF_LayoutRead
 
      public ESMF_LayoutPrint

      public ESMF_LayoutAllReduce
      public ESMF_LayoutAllGatherVI
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Layout.F90,v 1.11 2003/02/28 23:36:03 jwolfe Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_LayoutCreate -- Generic interface to create an Layout

! !INTERFACE:
     interface ESMF_LayoutCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_LayoutCreateIntDE2D
!        !module procedure ESMF_LayoutCreateNoData

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_LayoutCreate} functions.   
!
!  \begin{description}
!  \item[xxx]
!    Description of xxx.
!  \item[yyy]
!    Description of yyy.
!  \item[[zzz]]
!    Description of optional arg zzz.
!  \item[rc]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!  
!EOP 
end interface

!------------------------------------------------------------------------------


!==============================================================================

      contains

!==============================================================================


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Layout Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LayoutCreateIntDE2D - Create 2D Layout from a integer DE list

! !INTERFACE:
      function ESMF_LayoutCreateIntDE2D(nx, ny, delist, commhint, rc)
!
! !RETURN VALUE:
      type(ESMF_Layout) :: ESMF_LayoutCreateIntDE2D
!
! !ARGUMENTS:
      integer, intent(in) :: nx, ny, delist(:), commhint
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new Layout and set the decomposition characteristics.
!
!  The return value is a new Layout.
!    
!  The arguments are:
!  \begin{description}
! 
!   \item[nx]
!     Number of {\tt DE}s in the {\tt I} dimension.
! 
!   \item[ny]
!     Number of {\tt DE}s in the {\tt J} dimension.
! 
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

!       local vars
        type (ESMF_Layout) :: layout        ! opaque pointer to new C++ Layout
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Initialize the pointer to null.
        layout%this = ESMF_NULL_POINTER

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ creation routine.
        call c_ESMC_LayoutCreate(layout, nx, ny, delist, commhint, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Layout creation error"
          return
        endif

!       set return values
        ESMF_LayoutCreateIntDE2D = layout 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_LayoutCreateIntDE2D

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LayoutCreateNoData

! !INTERFACE:
      function ESMF_LayoutCreateNoData(rc)
!
! !RETURN VALUE:
      type(ESMF_Layout) :: ESMF_LayoutCreateNoData
!
! !ARGUMENTS:
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!  Create a new empty {\tt Layout} object.
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
! !REQUIREMENTS:


!       ! Local variables
        type (ESMF_Layout) :: layout        ! new class being created
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       ! Initialize pointer
        layout%this = ESMF_NULL_POINTER

!       ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! C routine which interfaces to the C++ routine which does actual work
        !call c_ESMC_LayoutCreateNoData(layout, status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "Layout construction error"
        !  return
        !endif

!       set return values
        ESMF_LayoutCreateNoData = layout
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_LayoutCreateNoData

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutDestroy(layout, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Layout}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[layout]
!       Destroy contents of this {\tt Layout}.
!
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       call Destroy to release resources on the C++ side
        call c_ESMC_LayoutDestroy(layout, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Layout destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutDestroy

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutSetData(layout, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      Used only with the version of LayoutCreate which creates an empty
!      Layout and allows the Data to be specified later.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutSetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Query for information from the layout.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutGet(layout, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns information about the layout.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the layout input are optional to facilitate this
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutGet

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutGetSize(layout, nx, ny, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, intent(out) :: nx, ny             
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns information about the layout.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the layout input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ routine.
        call c_ESMC_LayoutGetSize(layout, nx, ny, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_LayoutGetSize error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutGetSize

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutGetDEPosition(layout, x, y, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, intent(out) :: x, y             
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns information about the layout.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the layout input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ routine.
        call c_ESMC_LayoutGetDEPosition(layout, x, y, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_LayoutGetDEPosition error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutGetDEPosition

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutGetDEid(layout, id, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, intent(out) :: id
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns information about the layout.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the layout input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ routine.
        call c_ESMC_LayoutGetDEid(layout, id, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_LayoutGetDEid error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutGetDEid

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutSetAxisIndex(layout, global_counts, decompids, &
                                         AIPtr, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, dimension(:), intent(in) :: global_counts
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:) :: AIPtr
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns information about the layout.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the layout input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?
        integer :: size_gcount              ! size of the global counts array
        integer :: size_decomp              ! size of the decompids array

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ routine.
        size_gcount = size(global_counts)
        size_decomp = size(decompids)
        call c_ESMC_LayoutSetAxisIndex(layout, global_counts, size_gcount, &
                                       decompids, size_decomp, AIPtr, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_LayoutSetAxisIndex error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutSetAxisIndex

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutGatherArrayI(layout, DistArray, decompids, &
                                         AIPtr, AIPtr2, GlobalArray, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, dimension(:), intent(in) :: DistArray
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:) :: AIPtr
      type(ESMF_AxisIndex), dimension(:) :: AIPtr2
      integer, dimension(:), intent(out) :: GlobalArray
      integer, intent(out), optional :: rc             
!
! !DESCRIPTION:
!      Returns information about the layout.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the layout input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?
        integer :: size_decomp              ! size of the decompids array
        integer :: size_AI                  ! size of the axis indices arrays
        integer :: i

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
        call c_ESMC_LayoutGatherArrayI(layout, DistArray, decompids, &
                                       size_decomp, AIPtr, AIPtr2, &
                                       GlobalArray, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_LayoutGatherArrayI error"
          return
        endif

! add one back to location parts of indices to translate from C++
        do i = 1,size_AI
          AIPtr(i)%l  = AIPtr(i)%l  + 1
          AIPtr(i)%r  = AIPtr(i)%r  + 1
          AIPtr2(i)%l = AIPtr2(i)%l + 1
          AIPtr2(i)%r = AIPtr2(i)%r + 1
        enddo

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutGatherArrayI

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Layouts
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutCheckpoint(layout, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout):: layout 
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
! !REQUIREMENTS:

!
! TODO: code goes here
!
!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_LayoutRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Layout) :: ESMF_LayoutRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! layout name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Layout from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

        type (ESMF_Layout) :: a 
!
!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! add code here

        ESMF_LayoutRestore = a 
 
!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_LayoutRestore


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LayoutWrite(layout, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      type(ESMF_IOSpec), intent(in), optional :: iospec
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see Checkpoint/Restore for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LayoutWrite


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_LayoutRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Layout) :: ESMF_LayoutRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! layout name to read
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to read data from persistent storage in a variety of formats.
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

        type (ESMF_Layout) :: a

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! add code here

        ESMF_LayoutRead = a 
 
!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_LayoutRead


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_LayoutPrint(layout, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a layout.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts="brief"
       integer :: status=ESMF_FAILURE      ! local error status
       logical :: rcpresent=.FALSE.

!      Initialize return code; assume failure until success is certain
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

!      ! Interface to call the C++ print code
       !if(present(options)) then
       !    call c_ESMC_LayoutPrint(layout%this, options, status) 
       !else
       !    call c_ESMC_LayoutPrint(layout%this, defaultopts, status) 
       !endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Layout print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_LayoutPrint

!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_LayoutAllReduce(layout, dataArray, result, arrayLen, &
                                      op, rc)

! TODO: rename to ESMF_LayoutAllReduceI for "integer" version ?
!
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
      integer, intent(in) :: dataArray(:), arrayLen, op
      integer, intent(out) :: result 
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!     Performs an MPI-like Allreduce for an integer array
!
!EOP
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ routine.
        call c_ESMC_LayoutAllReduce(layout, dataArray, result, arrayLen, op, &
                                    status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_LayoutAllReduce error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS
       end subroutine ESMF_LayoutAllReduce

!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_LayoutAllGatherVI(layout, sndArray, sndLen, &
                                        rcvArray, rcvLen, rcvDispls, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Layout) :: layout
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
! !REQUIREMENTS:

!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ routine.
        call c_ESMC_LayoutAllGatherVI(layout, sndArray, sndLen, &
                                      rcvArray, rcvLen, rcvDispls, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ESMF_LayoutAllGatherVI error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS
       end subroutine ESMF_LayoutAllGatherVI

       end module ESMF_LayoutMod

