! $Id: ESMF_Array.F90,v 1.3 2002/11/05 23:26:33 nscollins Exp $
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
!     ESMF Array module
      module ESMF_ArrayMod
!
!==============================================================================
!
! This file contains the Array class definition and all Array
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
!------------------------------------------------------------------------------
#include "ESMF.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_ArrayMod - Manage data arrays uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt Array} class and 
!  associated functions and subroutines.  
!
! C and C++ arrays are simple pointers to memory.
! Fortran arrays contain shape and stride definitions and are strongly
! typed.  To enable interoperability between the languages the C++ code
! must be able to obtain this information from the Fortran description
! (which is called the "dope vector" in Fortran), either through a priori
! knowledge or through query.
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
!     ! ESMF_CopyFlag
!
!     ! Indicates whether a data array should be copied or referenced. 

      type ESMF_CopyFlag
      sequence
      private
        integer :: docopy
      end type

      type(ESMF_CopyFlag), parameter :: &
                            ESMF_DO_COPY = ESMF_CopyFlag(1), &
                            ESMF_NO_COPY = ESMF_CopyFlag(2)

!------------------------------------------------------------------------------
!     ! ESMF_ArraySpec
!
!     ! Data array specification, with no associated data buffer.

      type ESMF_ArraySpec
      sequence
      private
   
        integer :: rank                     ! number of dimensions
        type(ESMF_DataType) :: type         ! real/float, integer, etc enum
        type(ESMF_DataKind) :: kind         ! fortran "kind" enum/integer
        integer, dimension(ESMF_MAXDIM, 3) :: rinfo ! (lower/upper/stride) per rank

      end type

!------------------------------------------------------------------------------
!     ! ESMF_Array
!
!     ! Actual data array type.  The array specification is duplicated on 
!     ! purpose for simplicity and performance reasons.

      type ESMF_Array
      sequence
      private
        type(ESMF_Pointer) :: cstruct      ! the dope vector in the C class
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CopyFlag, ESMF_DO_COPY, ESMF_NO_COPY
      public ESMF_DataKind, ESMF_Pointer
      public ESMF_ArraySpec, ESMF_Array
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArrayCreate, ESMF_ArrayDestroy
      public ESMF_ArrayConstruct,  ESMF_ArrayDestruct
 
      public ESMF_ArraySpecCreate, ESMF_ArraySpecConstruct
      !public ESMF_ArraySpecDestroy, ESMF_ArraySpecDestruct

      public ESMF_ArraySetData, ESMF_ArrayGet
 
      public ESMF_ArrayCheckpoint
      public ESMF_ArrayRestore
      public ESMF_ArrayWrite
      public ESMF_ArrayRead
 
      public ESMF_ArrayPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Array.F90,v 1.3 2002/11/05 23:26:33 nscollins Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_ArrayCreate -- Generic interface to create an Array

! !INTERFACE:
     interface ESMF_ArrayCreate

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayCreateNew
        module procedure ESMF_ArrayCreateBySpec
!        module procedure ESMF_ArrayCreateByArray1Dr4
!        module procedure ESMF_ArrayCreateByArray1Dr8
!        module procedure ESMF_ArrayCreateByArray1Di4
!        module procedure ESMF_ArrayCreateByArray1Di8
        module procedure ESMF_ArrayCreateByArray2DR4
!        module procedure ESMF_ArrayCreateByArray2Dr8
!        module procedure ESMF_ArrayCreateByArray2Di4
!        module procedure ESMF_ArrayCreateByArray2Di8
! ...to be expanded to all types, kinds, ranks

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of ArrayCreate functions.
!EOP
      end interface

!------------------------------------------------------------------------------
! interface block for construct
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayConstruct-- Generic interface to initialize Array object

! !INTERFACE:
     interface ESMF_ArrayConstruct

! !PRIVATE MEMBER FUNCTIONS:
        module procedure ESMF_ArrayConstructNew
        module procedure ESMF_ArrayConstructBySpec
!        module procedure ESMF_ArrayConstructByArray
        module procedure ESMF_ArrayConstructByArray2DR4

! !DESCRIPTION:
! This interface provides a single entry point for the various
!  types of ArrayCreate functions.
!EOP
      end interface

!------------------------------------------------------------------------------
interface operator (.eq.)
 module procedure cfeq
end interface

!==============================================================================

      contains

!==============================================================================

! function to compare two ESMF_CopyFlags to see if they're the same

function cfeq(cf1, cf2)
 logical cfeq
 type(ESMF_CopyFlag), intent(in) :: cf1, cf2

 cfeq = (cf1%docopy .eq. cf2%docopy) 

end function


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Array Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateNew -- Create a new Array specifying all options.

! !INTERFACE:
      function ESMF_ArrayCreateNew(rank, type, kind, &
                                   lbounds, ubounds, strides, &
                                   bufaddr, copyflag, rc)
!
! !RETURN VALUE:
      type(ESMF_Array), pointer :: ESMF_ArrayCreateNew
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, dimension(:), intent(in) :: strides
      type(ESMF_Pointer), intent(in) :: bufaddr
      type(ESMF_CopyFlag), intent(in) :: copyflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new Array and set the data values. 
!
!  The return value is a new Array.
!    
!  The arguments are:
!  \begin{description}
!
!  \item[rank]
!    Array rank (dimensionality, 1D, 2D, etc).  Maximum allowed is 5D.
!
!  \item[type]
!    Array type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!
!  \item[kind]
!    Array kind.  Valid kinds include {\tt ESMF\_KIND\_4}
!     and {\tt ESMF\_KIND\_8}.
!
!  \item[lbounds]
!    The lower bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[ubounds]
!    The upper bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[strides]
!    The strides for each rank of the array. This is a 1D
!    integer array the same length as the rank.
!
!  \item[bufaddr]
!    A pointer to the start of the contents of the Array.
!
!  \item[copyflag]
!    Set to either copy the contents of the data buffer, or set a reference
!    and assume ownership of the buffer.  It must be able to be deallocated
!    by the ESMF.  Valid values are {\tt ESMF\_DO\_COPY} or 
!    {\tt ESMF\_NO\_COPY}.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_Array), pointer :: a     ! pointer to new Array
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize pointer
        nullify(a)

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       allocate space for Array object and call Construct method to initalize
        allocate(a, stat=status)
        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
          print *, "Array allocation error"
          return
        endif

        call ESMF_ArrayConstructNew(a, rank, type, kind, &
                                      lbounds, ubounds, strides, &
                                      bufaddr, copyflag, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array construction error"
          return
        endif

!       set return values
        ESMF_ArrayCreateNew => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ArrayCreateNew


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateBySpec -- Create a new Array from a spec

! !INTERFACE:
      function ESMF_ArrayCreateBySpec(spec, bufaddr, copyflag, rc)
!
! !RETURN VALUE:
      type(ESMF_Array), pointer :: ESMF_ArrayCreateBySpec
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: spec
      type(ESMF_Pointer), intent(in) :: bufaddr
      type(ESMF_CopyFlag), intent(in) :: copyflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new Array and set the data values. 
!
!  The return value is a new Array.
!    
!  The arguments are:
!  \begin{description}
!
!  \item[spec]
!    ArraySpec object.
!
!  \item[bufaddr]
!    A pointer to the start of the contents of the Array.
!
!  \item[copyflag]
!    Set to either copy the contents of the data buffer, or set a reference
!    and assume ownership of the buffer.  It must be able to be deallocated
!    by the ESMF.  Valid values are {\tt ESMF\_DO\_COPY} or 
!    {\tt ESMF\_NO\_COPY}.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_Array), pointer :: a     ! pointer to new Array
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize pointer
        nullify(a)

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       allocate space for Array object and call Construct method to initalize
        allocate(a, stat=status)
        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
          print *, "Array allocation error"
          return
        endif

        call ESMF_ArrayConstructBySpec(a, spec, bufaddr, copyflag, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array construction error"
          return
        endif

!       set return values
        ESMF_ArrayCreateBySpec => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ArrayCreateBySpec


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateByArray2DR4 - make an ESMF array from an F90 ptr

! !INTERFACE:
      function ESMF_ArrayCreateByArray2DR4(f90ptr, ni, nj, docopy, rc)
!
! !RETURN VALUE:
      type(ESMF_Array), pointer :: ESMF_ArrayCreateByArray2DR4
!
! !ARGUMENTS:
      real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr
      integer, intent(in) :: ni
      integer, intent(in) :: nj
      type(ESMF_CopyFlag), intent(in) :: docopy  ! use existing buf or copy
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing FORTRAN 90 
! array.  The flag says whether to make a copy of the data in the array, or 
! simply make a reference to the existing buffer.
!
! The function return is a pointer to an ESMF\_Array type.
!
! The arguments are:
!  \begin{description}
!  \item[f90array]
!   An existing Fortran 90 array which can be queried for info about
!    type/kind/rank/strides/bounds/base addr, instead of having to specify
!    them each individually.
!
!  \item[docopy]
!    If set to {\tt DO\_COPY}, then the contents of the input array are copied
!    to a new memory location, so when this routine returns the caller can
!    modify or delete the original array without affecting the contents of
!    this array.  If the copyflag is set to {\tt NO\_COPY}, then the caller 
!    relinquishes the memory to this array, and when the ESMF is done with
!    the array it can delete it.  The caller must no longer update the
!    contents of the array in this case.
!
!  \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!

!
!EOP
! !REQUIREMENTS:

!       local vars
        type (ESMF_Array), pointer :: a     ! pointer to new Array
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize pointer
        nullify(a)

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       allocate space for Array object and call Construct method to initalize
        allocate(a, stat=status)
        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
          print *, "Array allocation error"
          return
        endif

        call ESMF_ArrayConstructByArray2DR4(a, f90ptr, ni, nj, docopy, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array construction error"
          return
        endif

!       set return values
        ESMF_ArrayCreateByArray2DR4 => a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ArrayCreateByArray2DR4  




!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayDestroy(array, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt Array}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[array]
!       Destroy contents of this {\tt Array}.
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

!       call Destruct first, then free this memory
        call ESMF_ArrayDestruct(array, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array destruction error"
          return
        endif

        deallocate(array, stat=status)
        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
          print *, "Array allocation error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDestroy


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayConstructNew(array, rank, type, kind, &
                                        lbounds, ubounds, strides, &
                                        bufaddr, copyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array 
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, dimension(:), intent(in) :: strides
      type(ESMF_Pointer), intent(in) :: bufaddr
      type(ESMF_CopyFlag), intent(in) :: copyflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  ESMF routine to initialize the contents of a Array type.
!  The corresponding internal routine is Destruct.
!
!  The return value is a new Array.
!    
!  The arguments are:
!  \begin{description}
!
!  \item[array]
!    An allocated {\tt Array} type where the contents are uninitialized.
!
!  \item[rank]
!    Array rank (dimensionality, 1D, 2D, etc).  Maximum allowed is 5D.
!
!  \item[type]
!    Array type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!
!  \item[kind]
!    Array kind.  Valid kinds include {\tt ESMF\_KIND\_4}
!     and {\tt ESMF\_KIND\_8}.
!
!  \item[lbounds]
!    The lower bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[ubounds]
!    The upper bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[strides]
!    The strides for each rank of the array. This is a 1D
!    integer array the same length as the rank.
!
!  \item[bufaddr]
!    A pointer to the start of the contents of the Array.
!
!  \item[copyflag]
!    Set to either copy the contents of the data buffer, or set a reference
!    and assume ownership of the buffer.  It must be able to be deallocated
!    by the ESMF.  Valid values are {\tt ESMF\_DO\_COPY} or 
!    {\tt ESMF\_NO\_COPY}.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP

!       local vars
        integer :: i
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


!       extract any info needed to pass into C++ interface
      
        if (copyflag .eq. ESMF_DO_COPY) then
            ! compute array size
            ! allocate that amount of space
            ! copy contents from bufaddr into new space
        else
            ! array%base_address = bufaddr
        endif
!     
!
!       call C++ Construct routine here
!
!

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayConstructNew


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayConstructBySpec(array, spec, bufaddr, copyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array 
      type(ESMF_ArraySpec), intent(in) :: spec
      type(ESMF_Pointer), intent(in) :: bufaddr
      type(ESMF_CopyFlag), intent(in) :: copyflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      ESMF routine to initialize the contents of a Array type.
!      The corresponding internal routine is Destruct.
!
!     The arguments are:
!     \begin{description}
!
!     \item[array]
!       Uninitialized Array type to be filled in.
!
!     \item[spec]
!     \item[bufaddr]
!     \item[copyflag]
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!EOP

!
! code goes here
!
        end subroutine ESMF_ArrayConstructBySpec



!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayConstructByArray2DR4(array, f90ptr, ni, nj, copyflag, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array 
      real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr
      integer, intent(in) :: ni
      integer, intent(in) :: nj
      type(ESMF_CopyFlag), intent(in) :: copyflag
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      ESMF routine to initialize the contents of a Array type.
!      The corresponding internal routine is Destruct.
!EOP
!

      ! extract rank, shape, etc out of F90 array and call the C++
      ! interface to create the ESMF_Array class data.

      integer :: status=ESMF_FAILURE              ! Error status
      logical :: rcpresent=.FALSE.                ! Return code present

!     initialize the return code
      if(present(rc)) then
        rcpresent=.TRUE.
        rc = ESMF_FAILURE
      endif

!     make sure the pointer isn't already associated w/ something
!     if ok, start with a null ptr
      if (associated(f90ptr)) then
          print *, "ERROR in ESMF_Allocate: pointer already associated"
          return
      endif
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



        end subroutine ESMF_ArrayConstructByArray2DR4



!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayDestruct(array, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!      ESMF routine to release all resources except the Array datatype itself.
!
!EOP
!       local vars
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       get the real F90 pointer back from struct, and call deallocate
!       this will need one interface per type/kind/rank
        !deallocate(base_address, stat=status)
        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
          print *, "Array allocation error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDestruct


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArraySetData(array, dataspec, databuf, docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array 
      type(ESMF_ArraySpec), intent(in) :: dataspec
      real, dimension (:), pointer :: databuf    
      type(ESMF_CopyFlag), intent(in) :: docopy 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of ArrayCreate which creates an empty 
!      Array and allows the Data to be specified later.  Otherwise it is an 
!      error to replace the data contents associated with a Array.  
!
!EOP
! !REQUIREMENTS:

!
! code goes here
!
        end subroutine ESMF_ArraySetData


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayReorder(array, newarrayspec, newarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array 
      type(ESMF_ArraySpec), intent(in) :: newarrayspec
      type(ESMF_Array), pointer :: newarray   
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Used to alter the local memory ordering (layout) of this Array.
!      (i'm not sure this makes sense now, or that the routine should be
!      in this class.  but i'm leaving this here as a reminder that we
!      might need some low level reorder functions.  maybe the argument
!      should be another array or an arrayspec which describes what you
!      want, and the input array is what exists, and this routine can then
!      make one into the other.   is this a type of create?  or is this
!      a copy?)
!
!EOP
! !REQUIREMENTS:

!
! code goes here
!
        end subroutine ESMF_ArrayReorder


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
     function ESMF_ArraySpecCreate(rank, type, kind, &
                                   lbounds, ubounds, strides, rc)
!
! !RETURN VALUE:
     type(ESMF_ArraySpec), pointer :: ESMF_ArraySpecCreate  
!
! !ARGUMENTS:
     integer, intent(in) :: rank
     type(ESMF_DataType), intent(in) :: type
     type(ESMF_DataKind), intent(in) :: kind
     integer, dimension(:), intent(in) :: lbounds
     integer, dimension(:), intent(in) :: ubounds
     integer, dimension(:), intent(in) :: strides
     integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!  Creates a description of the data -- the type, the dimensionality, etc.  
!  This specification (basically an empty Array), can be
!  used in an ArrayCreate call with data to create a full Array.
!  The return value is a new ArraySpec.
!    
!  The arguments are:
!  \begin{description}
!
!  \item[rank]
!    Array rank (dimensionality, 1D, 2D, etc).  Maximum allowed is 5D.
!
!  \item[type]
!    Array type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!
!  \item[kind]
!    Array kind.  Valid kinds include {\tt ESMF\_KIND\_4}
!     and {\tt ESMF\_KIND\_8}.
!
!  \item[lbounds]
!    The lower bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[ubounds]
!    The upper bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[strides]
!    The strides for each rank of the array. This is a 1D
!    integer array the same length as the rank.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_ArraySpec), pointer :: as     ! pointer to new Array
        integer :: status=ESMF_FAILURE           ! local error status
        logical :: rcpresent=.FALSE.             ! did user specify rc?

!       initialize pointer
        nullify(as)

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       allocate space for ArraySpec and call Construct method to initalize
        allocate(as, stat=status)
        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
          print *, "ArraySpec allocation error"
          return
        endif

        call ESMF_ArraySpecConstruct(as, rank, type, kind, &
                                      lbounds, ubounds, strides, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "ArraySpec construction error"
          return
        endif

!       set return value
        ESMF_ArraySpecCreate => as
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ArraySpecCreate



!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArraySpecConstruct(as, rank, type, kind, &
                                         lbounds, ubounds, strides, rc)
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), pointer :: as
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, dimension(:), intent(in) :: strides
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  ESMF routine to initialize the contents of a ArraySpec type.
!  The corresponding internal routine is Destruct.
!
!  The arguments are:
!  \begin{description}
!
!  \item[as]
!    An allocated {\tt ArraySpec} type where the contents are uninitialized.
!
!  \item[rank]
!    Array rank (dimensionality, 1D, 2D, etc).  Maximum allowed is 5D.
!
!  \item[type]
!    Array type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!
!  \item[kind]
!    Array kind.  Valid kinds include {\tt ESMF\_KIND\_4}
!     and {\tt ESMF\_KIND\_8}.
!
!  \item[lbounds]
!    The lower bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[ubounds]
!    The upper bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[strides]
!    The strides for each rank of the array. This is a 1D
!    integer array the same length as the rank.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP

!       local vars
        integer :: i
        integer :: status=ESMF_FAILURE           ! local error status
        logical :: rcpresent=.FALSE.             ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


!       set arrayspec contents
      
        as%rank = rank   
        as%type = type
        as%kind = kind

        do i=1,rank
          as%rinfo(i, 1) = lbounds(i)
          as%rinfo(i, 2) = ubounds(i)
          as%rinfo(i, 3) = strides(i)
        enddo

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArraySpecConstruct



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the array.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayGet(array, rank, type, kind, &
                               lbounds, ubounds, strides, base, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array
      integer, intent(out), optional :: rank
      type(ESMF_DataType), intent(out), optional :: type
      type(ESMF_DataKind), intent(out), optional :: kind
      integer, dimension(:), intent(out), optional :: lbounds
      integer, dimension(:), intent(out), optional :: ubounds
      integer, dimension(:), intent(out), optional :: strides
      type(ESMF_Pointer), intent(out), optional :: base
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the array.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the array input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:

!
! code goes here
!
        end subroutine ESMF_ArrayGet

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Arrays
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayCheckpoint(array, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array 
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
! code goes here
!
        end subroutine ESMF_ArrayCheckpoint


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_ArrayRestore(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayRestore
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! array name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a Array from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! code goes here
!
        type (ESMF_Array) :: a

        ESMF_ArrayRestore = a 
 
        end function ESMF_ArrayRestore


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayWrite(array, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array
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
! code goes here
!
        end subroutine ESMF_ArrayWrite


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_ArrayRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayRead
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! array name to read
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
! code goes here
!
        type (ESMF_Array) :: a

        ESMF_ArrayRead = a 
 
        end function ESMF_ArrayRead


!------------------------------------------------------------------------------
!BOP
!
!
! !INTERFACE:
      subroutine ESMF_ArrayPrint(array, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Array), pointer :: array
      character (len = *), intent(in) :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a array.
!
!EOP
! !REQUIREMENTS:

!
! code goes here
!
        end subroutine ESMF_ArrayPrint

        end module ESMF_ArrayMod

