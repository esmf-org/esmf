! $Id: ESMF_Layout.F90,v 1.2 2002/12/30 22:09:37 nscollins Exp $
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
! !MODULE: ESMF_LayoutMod - Manage data layouts uniformly between F90 and C++     
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
        type(ESMF_Layout), pointer :: this       ! the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_Layout
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_LayoutCreate
      public ESMF_LayoutDestroy
 
      !public ESMF_LayoutSetData
      !public ESMF_LayoutGetData
      !public ESMF_LayoutGet
 
      public ESMF_LayoutCheckpoint
      public ESMF_LayoutRestore
      public ESMF_LayoutWrite
      public ESMF_LayoutRead
 
      public ESMF_LayoutPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Layout.F90,v 1.2 2002/12/30 22:09:37 nscollins Exp $'

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
        module procedure ESMF_LayoutCreateNew
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
! !IROUTINE: ESMF_LayoutCreateNew -- Create a new Layout specifying all options.

! !INTERFACE:
      function ESMF_LayoutCreateNew(nde_i, nde_j, nde_k, rc)
!
! !RETURN VALUE:
      type(ESMF_Layout) :: ESMF_LayoutCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: nde_i, nde_j, nde_k
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
!   \item[nde_i]
!     Number of {\tt DE}s in the {\tt I} dimension.
! 
!   \item[nde_j]
!     Number of {\tt DE}s in the {\tt J} dimension.
! 
!   \item[nde_k]
!     Number of {\tt DE}s in the {\tt K} dimension.
! 
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_Layout), pointer :: ptr   ! opaque pointer to new C++ Layout
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       Initialize the pointer to null.
        nullify(ptr)

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       Routine which interfaces to the C++ creation routine.
        !call c_ESMC_LayoutCreate(nde_i, nde_j, nde_k, status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "Layout construction error"
        !  return
        !endif

!       set return values
        ESMF_LayoutCreateNew%this => ptr 
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_LayoutCreateNew


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
        type (ESMF_Layout), pointer :: a    ! pointer to new Layout
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       ! Initialize pointer
        nullify(a)

!       ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       ! C routine which interfaces to the C++ routine which does actual work
        !call c_ESMC_LayoutCreateNoData(status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "Layout construction error"
        !  return
        !endif

!       set return values
        ESMF_LayoutCreateNoData%this => a
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
        !call c_ESMC_LayoutDestroy(layout%this, status)
        !if (status .ne. ESMF_SUCCESS) then
        !  print *, "Layout contents destruction error"
        !  return
        !endif

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
!      All the arguments after the layout input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_LayoutGet

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
        type (ESMF_Layout) :: a 

!       this is just to shut the compiler up
        type (ESMF_Layout), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_LayoutRestore = a 
 
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
        type (ESMF_Layout) :: a

!       this is just to shut the compiler up
        type (ESMF_Layout), target :: b 
        a%this => b
        nullify(a%this)

!
! TODO: add code here
!

        ESMF_LayoutRead = a 
 
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

       !if (status .ne. ESMF_SUCCESS) then
       !  print *, "Layout print error"
       !  return
       !endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_LayoutPrint


       end module ESMF_LayoutMod

