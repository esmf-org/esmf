! $Id: ESMF_Alloc.F90,v 1.2 2002/12/06 16:43:57 nscollins Exp $
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
!     ESMF Alloc Module
      module ESMF_AllocMod
!
!==============================================================================
!
! This file contains the Alloc class definition and all Alloc class 
! methods.
!
!------------------------------------------------------------------------------
! INCLUDES
#include <ESMF.h>
#include <ESMF_Data.h>
!==============================================================================
!BOP
! !MODULE: ESMF_AllocMod - One line general statement about this class
!
! !DESCRIPTION:
!
! The code in this file implements a uniform interface for allocating,
!  deallocating, validating, and printing Fortran 90 pointers, which
!  are strongly typed, from C++.  For calls from Fortran, either these
!  or the intrinsic {\tt allocate()} and {\tt deallocate()} functions
!  can be used.  These functions are heavily overloaded with all possible
!  Type/Kind/Rank data pointers.
!
!
!------------------------------------------------------------------------------
! !USES:
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
!
! !PUBLIC MEMBER FUNCTIONS:
!

! These routines are interfaces heavily overloaded with all supported fortran
!  type/kind/ranks, so there is only a single allocate and deallocate
!  entry point which C++ needs to understand.  If you are allocating
!  memory which is only going to be used in fortran, you can bypass these
!  and call the fortran intrinsics allocate() and deallocate() directly.

    public ESMF_Allocate
    public ESMF_Deallocate

! TEMP  - FIXME
    public ESMF_Allocate2DR4    ! Rank=2, Real, Kind= *4

! These routines print the dope vector information from a given pointer, 
!  and verify that a pointer is associated and valid.
    public ESMF_AllocValidate
    public ESMF_AllocPrint
 
!
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Alloc.F90,v 1.2 2002/12/06 16:43:57 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!BOP
! !INTERFACE:
      interface ESMF_Allocate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_Allocate2DR4    ! Rank=2, Real, Kind= *4
!  TODO: repeat for each type/kind/rank

! !DESCRIPTION:
!     This interface provides a single entry point for types of Allocate 
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_Deallocate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_Deallocate2DR4    ! Rank=2, Real, Kind= *4
!  TODO: repeat for each type/kind/rank

! !DESCRIPTION:
!     This interface provides a single entry point for types of Deallocate 
!     methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AllocValidate

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_AllocValidate2DR4    ! Rank=2, Real, Kind= *4
!  TODO: repeat for each type/kind/rank

! !DESCRIPTION:
!     This interface provides a single entry point for types of Array pointer
!     validate methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      interface ESMF_AllocPrint

! !PRIVATE MEMBER FUNCTIONS:
         module procedure ESMF_AllocPrint2DR4    ! Rank=2, Real, Kind= *4
!  TODO: repeat for each type/kind/rank

! !DESCRIPTION:
!     This interface provides a single entry point for types of Array pointer
!     print methods.
!
!EOP
      end interface 
!
!------------------------------------------------------------------------------


!==============================================================================

      contains

!==============================================================================
!
! This section includes all the versions of allocate and deallocate.
! TODO: this needs to be heavily macroized.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_Allocate2DR4

! !INTERFACE:
      subroutine ESMF_Allocate2DR4(f90ptr, ni, nj, rc)
!
!
! !ARGUMENTS:
      !real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr
      real*4, dimension(:,:), pointer :: f90ptr
      integer, intent(in) :: ni
      integer, intent(in) :: nj
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Allocates memory for a new array of type real, 2d, kind = *4.
!
!     The arguments are:
!     \begin{description}
!     \item[f90ptr] 
!          A Fortran 90 pointer with associated sizes already specified.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     initialize the return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     make sure the pointer isn't already associated w/ something
!     if ok, start with a null ptr
      !if (associated(f90ptr)) then
      !    print *, "ERROR in ESMF_Allocate: pointer already associated"
      !    return
      !endif
      nullify(f90ptr)

!     call the fortran intrinsic allocation function
      allocate(f90ptr(ni, nj), stat=status)
!     Formal error handling will be added asap.
      if(status .NE. 0) then   ! this is the fortran rc, not ESMF's rc
        print *, "ERROR in ESMF_Allocate: Allocation failed, status =", status
        return
      endif

!     set return value.
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_Allocate2DR4

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_Deallocate2DR4

! !INTERFACE:
      subroutine ESMF_Deallocate2DR4(f90ptr, rc)
!
!
! !ARGUMENTS:
      !real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr
      real*4, dimension(:,:), pointer :: f90ptr
      integer, intent(out), optional :: rc               
!
! !DESCRIPTION:
!     Deallocates memory for a new array of type real, 2d, kind = *4.
!
!     The arguments are:
!     \begin{description}
!     \item[f90ptr] 
!          An associated Fortran 90 pointer to be released.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP
! !REQUIREMENTS:  AAAn.n.n

      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     initialize the return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     make sure the pointer is associated w/ something
      if (associated(f90ptr) .eqv. .TRUE.) then
          print *, "ERROR in ESMF_Deallocate: pointer not associated"
          return
      endif

!     call the fortran intrinsic deallocation function
      deallocate(f90ptr, stat=status)
!     Formal error handling will be added asap.
      if(status .NE. 0) then   ! this is the fortran rc, not ESMF's rc
        print *, "ERROR in ESMF_Deallocate: Deallocation failed, status =", status
        return
      endif

!     clear the pointer and set return value.
      nullify(f90ptr)
      if(rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_Deallocate2DR4


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_AllocValidate - Check internal consistency of a F90 pointer

! !INTERFACE:
      subroutine ESMF_AllocValidate2DR4(f90ptr, opt, rc)
!
! !ARGUMENTS:
      !real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr
      real*4, dimension(:,:), pointer :: f90ptr
      character (len=*), intent(in), optional :: opt    
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!     Validates that an F90 pointer is ok.
!
!     The arguments are:
!     \begin{description}
!     \item[f90ptr] 
!          Pointer to be checked.
!     \item[[opt]]
!          Validation options.
!     \item[[rc]] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}

!EOP
! !REQUIREMENTS:  XXXn.n, YYYn.n

!
!  code goes here
!
      end subroutine ESMF_AllocValidate2DR4

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: 
!     ESMF_AllocPrint2DR4 - Print the contents of an F90 pointer

! !INTERFACE:
      subroutine ESMF_AllocPrint2DR4(f90ptr, opt, rc)
!
! !ARGUMENTS:
      !real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr
      real*4, dimension(:,:), pointer :: f90ptr
      character (len=*), intent(in) :: opt      
      integer, intent(out), optional :: rc           
!
! !DESCRIPTION:
!      Print information about an allocated F90 pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[f90ptr] 
!          Pointer to print information about.
!     \item[[opt]]
!          Print options that control the type of information and level of 
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
      end subroutine ESMF_AllocPrint2DR4

!------------------------------------------------------------------------------

      end module ESMF_AllocMod
