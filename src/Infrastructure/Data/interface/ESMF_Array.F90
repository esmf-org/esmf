! $Id: ESMF_Array.F90,v 1.9 2002/12/10 22:54:24 nscollins Exp $
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
      use ESMF_AllocMod
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
!     ! Array data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_Array
      sequence
      private
        type(ESMF_Array), pointer :: this       ! the C++ class data
      end type

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CopyFlag, ESMF_DO_COPY, ESMF_NO_COPY
      public ESMF_DataKind, ESMF_Pointer
      public ESMF_ArraySpec, ESMF_Array
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArrayCreate, ESMF_ArrayDestroy
 
      public ESMF_ArraySpecCreate
      !public ESMF_ArraySpecDestroy

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
      '$Id: ESMF_Array.F90,v 1.9 2002/12/10 22:54:24 nscollins Exp $'

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
!        module procedure ESMF_ArrayCreateByPtr1Dr4
!        module procedure ESMF_ArrayCreateByPtr1Dr8
!        module procedure ESMF_ArrayCreateByPtr1Di4
!        module procedure ESMF_ArrayCreateByPtr1Di8
        module procedure ESMF_ArrayCreateByPtr2DR4
!        module procedure ESMF_ArrayCreateByPtr2Dr8
!        module procedure ESMF_ArrayCreateByPtr2Di4
!        module procedure ESMF_ArrayCreateByPtr2Di8
! ...to be expanded to all types, kinds, ranks

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
      type(ESMF_Array) :: ESMF_ArrayCreateNew
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
!    Set to either copy the contents of the data array, or simply share a
!    a pointer to the same space.  Valid values are {\tt ESMF\_DO\_COPY} or 
!    {\tt ESMF\_DO\_REF}.  (TODO: check to see if these are the right names)
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


!       local vars
        type (ESMF_Array), pointer :: ptr   ! opaque pointer to new C++ Array
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       TODO: need a null pointer to assign to initialize ptr
!       ptr = NULLPTR

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        call c_ESMC_ArrayCreate(ptr, rank, type, kind, &
                                lbounds, ubounds, strides, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array construction error"
          return
        endif

!       set return values
        ESMF_ArrayCreateNew%this => ptr 
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

        call c_ESMC_ArrayConstructBySpec(a, spec, bufaddr, copyflag, status)
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
! !IROUTINE: ESMF_ArrayCreateByPtr2DR4 - make an ESMF array from an F90 ptr

! !INTERFACE:
      function ESMF_ArrayCreateByPtr2DR4(f90ptr, ni, nj, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateByPtr2DR4
!
! !ARGUMENTS:
      real (selected_real_kind(6,45)), dimension(:,:), pointer :: f90ptr
      integer, intent(in) :: ni
      integer, intent(in) :: nj
      integer, intent(out), optional :: rc  
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer, and
!  the requested sizes.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
!  \begin{description}
!  \item[f90ptr]
!   A Fortran 90 array pointer which can be queried for info about
!    type/kind/rank.  This routine will allocate data space so that when
!    it returns the pointer will be associated and the memory can be
!    filled with values.
!
!  \item[ni]
!    Number of elements in the first dimension.
!
!  \item[nj]
!    Number of elements in the second dimension.
!
!  \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!

!
!EOP
! !REQUIREMENTS:

!       local vars
        !type (ESMF_Pointer) :: ptr           ! what C++ is going to return
        type (ESMF_Array), target :: thearray ! the real array
        type (ESMF_Array), pointer :: ptr   ! what C++ is going to return
        integer :: status=ESMF_FAILURE      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       TODO: need a null pointer to assign to initialize ptr
!       ptr = NULLPTR
        ptr => thearray

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

!       set up callback
        call c_ESMC_StoreAllocFunc(ESMF_Allocate2DR4, status);

!       call create routine
        call c_ESMC_ArrayCreateByPtr2D(ptr, ni, nj, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array initial construction error"
          return
        endif

        call ESMF_Allocate(f90ptr, ni, nj, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array allocate construction error"
          return
        endif

!       set base address
        call c_ESMC_ArraySetBaseAddr(ptr%this, f90ptr(1,1), status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array base address construction error"
          return
        endif


!       return value set by c_ESMC func above
        ESMF_ArrayCreateByPtr2DR4%this => ptr%this
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ArrayCreateByPtr2DR4  




!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayDestroy(array, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
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
        call c_ESMC_ArrayDestroy(array%this, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array contents destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDestroy



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

        !! call ESMF_SetNullPointer(a%this)

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

        !!call ESMF_SetNullPointer(a%this)
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
      type(ESMF_Array) :: array
      character (len = *), intent(in), optional :: options
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
       character (len=6) :: defaultopts="brief"
       integer :: status=ESMF_FAILURE      ! local error status
       logical :: rcpresent=.FALSE.

!      Initialize return code; assume failure until success is certain
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

       if(present(options)) then
           call c_ESMC_ArrayPrint(array%this, options, status) 
       else
           call c_ESMC_ArrayPrint(array%this, defaultopts, status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Array print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_ArrayPrint


        end module ESMF_ArrayMod

