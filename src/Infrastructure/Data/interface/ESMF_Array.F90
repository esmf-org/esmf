! $Id: ESMF_Array.F90,v 1.24 2003/02/08 00:00:00 nscollins Exp $
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







! Macros for the Array class.









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
        integer, dimension(7) :: counts ! array dimension sizes
        logical :: hascounts                ! counts optional
        integer, dimension(7, 3) :: rinfo ! (lower/upper/stride) per rank
        logical :: has_rinfo                ! rinfo optional

      end type

!------------------------------------------------------------------------------
!     ! ESMF_Array
!
!     ! Array data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_Array
      sequence
      private
        type(ESMF_Pointer) :: this       ! opaque pointer to the C++ class data
      end type

!------------------------------------------------------------------------------
!     ! Internal wrapper structures for passing f90 pointers to C++ and
!     ! guaranteeing they are passed by reference on all compilers and all
!     ! platforms.  These are never seen outside this module.
!
      ! < these expand into pointer declarations >
      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI21D
 integer (ESMF_IKIND_I2),dimension(:),pointer :: I21Dptr
 end type ESMF_ArrWrapI21D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI41D
 integer (ESMF_IKIND_I4),dimension(:),pointer :: I41Dptr
 end type ESMF_ArrWrapI41D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI81D
 integer (ESMF_IKIND_I8),dimension(:),pointer :: I81Dptr
 end type ESMF_ArrWrapI81D


      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI22D
 integer (ESMF_IKIND_I2),dimension(:,:),pointer :: I22Dptr
 end type ESMF_ArrWrapI22D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI42D
 integer (ESMF_IKIND_I4),dimension(:,:),pointer :: I42Dptr
 end type ESMF_ArrWrapI42D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI82D
 integer (ESMF_IKIND_I8),dimension(:,:),pointer :: I82Dptr
 end type ESMF_ArrWrapI82D


      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI23D
 integer (ESMF_IKIND_I2),dimension(:,:,:),pointer :: I23Dptr
 end type ESMF_ArrWrapI23D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI43D
 integer (ESMF_IKIND_I4),dimension(:,:,:),pointer :: I43Dptr
 end type ESMF_ArrWrapI43D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapI83D
 integer (ESMF_IKIND_I8),dimension(:,:,:),pointer :: I83Dptr
 end type ESMF_ArrWrapI83D


      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapR41D
 real (ESMF_IKIND_R4),dimension(:),pointer :: R41Dptr
 end type ESMF_ArrWrapR41D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapR81D
 real (ESMF_IKIND_R8),dimension(:),pointer :: R81Dptr
 end type ESMF_ArrWrapR81D


      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapR42D
 real (ESMF_IKIND_R4),dimension(:,:),pointer :: R42Dptr
 end type ESMF_ArrWrapR42D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapR82D
 real (ESMF_IKIND_R8),dimension(:,:),pointer :: R82Dptr
 end type ESMF_ArrWrapR82D


      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapR43D
 real (ESMF_IKIND_R4),dimension(:,:,:),pointer :: R43Dptr
 end type ESMF_ArrWrapR43D

      ! <Created by macro - do not edit directly >
 type ESMF_ArrWrapR83D
 real (ESMF_IKIND_R8),dimension(:,:,:),pointer :: R83Dptr
 end type ESMF_ArrWrapR83D


      ! TODO: make 1 of these for every supported T/K/R

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CopyFlag, ESMF_DO_COPY, ESMF_NO_COPY
      public ESMF_DataKind, ESMF_Pointer
      public ESMF_ArraySpec, ESMF_Array
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArrayCreate
      public ESMF_ArrayDestroy
 
      public ESMF_ArraySpecCreate
      !public ESMF_ArraySpecDestroy

      public ESMF_ArraySetData, ESMF_ArrayGetData
      public ESMF_ArraySetAxisIndex, ESMF_ArrayGetAxisIndex
      public ESMF_ArrayGet
 
      public ESMF_ArrayCheckpoint
      public ESMF_ArrayRestore
      public ESMF_ArrayWrite
      public ESMF_ArrayRead
 
      public ESMF_ArrayPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Array.F90,v 1.24 2003/02/08 00:00:00 nscollins Exp $'

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
!
!        !module procedure ESMF_ArrayCreateNewNoData
        module procedure ESMF_ArrayCreateNewBuffer
!        !module procedure ESMF_ArrayCreateBySpecNoData
!        !module procedure ESMF_ArrayCreateBySpecBuffer

        module procedure ESMF_ArrayCreateByPtrI41D
        module procedure ESMF_ArrayCreateByPtrI81D
        module procedure ESMF_ArrayCreateByPtrI42D
        module procedure ESMF_ArrayCreateByPtrI82D
        module procedure ESMF_ArrayCreateByPtrR41D
        module procedure ESMF_ArrayCreateByPtrR81D
        module procedure ESMF_ArrayCreateByPtrR42D
        module procedure ESMF_ArrayCreateByPtrR82D

! ! TODO: ...to be expanded to all types, kinds, ranks

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayCreate} functions.   
!
!  There are 4 options for setting the contents of the {\tt ESMF\_Array}
!  at creation time:
!  \begin{description}
!  \item[No Data]
!    No data space is allocated.
!  \item[Allocate Space Only]
!    Data space is allocated but not initialized.  The caller can query
!    for a pointer to the start of the space to address it directly.
!  \item[Data Copy]
!    An existing Fortran array is specified and the data contents are copied
!    into new space allocated by the {\tt ESMF\_Array}.
!  \item[Data Reference]
!    An existing Fortran array is specified and the data contents reference
!    it directly.  The caller must not deallocate the space; the
!    {\tt ESMF\_Array} will free the space when it is destroyed.
!  \end{description}
!
!  If the {\tt ESMF\_Array} contains data, there are 4 options for 
!  specifying the type/kind/rank of that data:
!  \begin{description}
!  \item[List]
!    The characteristics of the {\tt ESMF\_Array} are given explicitly
!    by individual arguments to the create function.
!  \item[ArraySpec]
!    A previously created {\tt ESMF\_ArraySpec} object is given which
!    describes the characteristics.
!  \item[Fortran array]
!    An existing Fortran array is used to describe the array.
!    (Only available from the Fortran interface.)
!  \item[Fortran 90 Pointer]
!    An existing Fortran 90 array pointer is used to describe the array.
!    (Only available from the Fortran interface.)
!  \end{description}
!  
!  
!EOP 
end interface

!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_ArrayGetData -- Get an F90 pointer to the data contents

! !INTERFACE:
     interface ESMF_ArrayGetData

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_ArrayGetDataI41D
        module procedure ESMF_ArrayGetDataI81D
        module procedure ESMF_ArrayGetDataI42D
        module procedure ESMF_ArrayGetDataI82D
        module procedure ESMF_ArrayGetDataR41D
        module procedure ESMF_ArrayGetDataR81D
        module procedure ESMF_ArrayGetDataR42D
        module procedure ESMF_ArrayGetDataR82D

! ! TODO: ...to be expanded to all types, kinds, ranks

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayGetData} functions.   
!  
!EOP 
end interface

!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_ArrayDeallocateType -- Free the data contents

! !INTERFACE:
     interface ESMF_ArrayDeallocateType

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_ArrayDeallocateI41D
        module procedure ESMF_ArrayDeallocateI81D
        module procedure ESMF_ArrayDeallocateI42D
        module procedure ESMF_ArrayDeallocateI82D
        module procedure ESMF_ArrayDeallocateR41D
        module procedure ESMF_ArrayDeallocateR81D
        module procedure ESMF_ArrayDeallocateR42D
        module procedure ESMF_ArrayDeallocateR82D

! ! TODO: ...to be expanded to all types, kinds, ranks

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayDeallocateType} functions.   
!  
!EOP 
    end interface

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
interface operator (.eq.)
 module procedure cfeq
end interface
interface operator (.ne.)
 module procedure cfne
end interface

!==============================================================================

      contains

!==============================================================================

! functions to compare two ESMF_CopyFlags to see if they're the same or not

function cfeq(cf1, cf2)
 logical cfeq
 type(ESMF_CopyFlag), intent(in) :: cf1, cf2

 cfeq = (cf1%docopy .eq. cf2%docopy) 
end function

function cfne(cf1, cf2)
 logical cfne
 type(ESMF_CopyFlag), intent(in) :: cf1, cf2

 cfne = (cf1%docopy .ne. cf2%docopy) 
end function


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Array Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateNewBuffer -- Create a new Array specifying all options.

! !INTERFACE:
      function ESMF_ArrayCreateNewBuffer(rank, type, kind, &
                                   lbounds, ubounds, strides, &
                                   bufaddr, copyflag, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateNewBuffer
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
!    Array kind.  Valid kinds include {\tt ESMF\_KIND\_I4}, 
!    {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8}, 
!    {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}. 
!
!  \item[counts]
!    The number of items in each dimension of the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[[lbounds]]
!    The lower bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.  If not specified,
!    the default is 1 for each dimension.
!
!  \item[[ubounds]]
!    The upper bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.  If not specified,
!    the default is the count for each dimension.
!
!  \item[[strides]]
!    The strides for each rank of the array. This is a 1D
!    integer array the same length as the rank.  If not specified,
!    the default is the standard Fortran row-major ordering.
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
        type (ESMF_Array) :: array          ! new C++ Array
        integer :: status=-1      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       TODO: need a null pointer to assign to initialize ptr
        array%this = ESMF_NULL_POINTER

!       Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = -1
        endif

        call c_ESMC_ArrayCreate(array, rank, type, kind, &
                                lbounds, ubounds, strides, status)
        if (status .ne. 0) then
          print *, "Array construction error"
          return
        endif

!       set return values
        ESMF_ArrayCreateNewBuffer = array 
        if (rcpresent) rc = 0

        end function ESMF_ArrayCreateNewBuffer


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateBySpec -- Create a new Array from a spec

! !INTERFACE:
      function ESMF_ArrayCreateBySpec(spec, bufaddr, copyflag, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateBySpec
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
        integer :: status=-1      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       initialize pointer
!        nullify(a)

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = -1
        endif

!       allocate space for Array object and call Construct method to initalize
!        allocate(a, stat=status)
!        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
!          print *, "Array allocation error"
!          return
!        endif


!       set return values
        ESMF_ArrayCreateBySpec = a
        if (rcpresent) rc = 0

        end function ESMF_ArrayCreateBySpec

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


      ! < these expand into actual function bodies >
!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !IROUTINE: ESMF_ArrayCreateByPtrI41D - make an ESMF array from an F90 ptr

! !INTERFACE:
 function ESMF_ArrayCreateByPtrI41D(f90ptr, docopy, rc)
!
! !RETURN VALUE:
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI41D
!
! !ARGUMENTS:
 integer (ESMF_IKIND_I4), dimension(:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer which
! is already associated with memory.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank and sizes.
!
! \item[[docopy]]
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine
! allocates new space and copies the data from the pointer into the
! new array.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!

!
!EOP
! !REQUIREMENTS:

! local vars
 type (ESMF_Array) :: array ! what C++ is going to return
 integer :: status=-1 ! local error status
 integer :: i ! local variable
 logical :: rcpresent=.FALSE. ! did user specify rc?

 type (ESMF_ArrWrapI41D) :: wrap ! for passing f90 ptr to C++
 integer :: rank, lengths(1) ! size info for the array
 integer (ESMF_IKIND_I4), dimension(:), pointer :: localp ! local copy

! !TODO: need a null pointer to assign to initialize ptr
 array%this = ESMF_NULL_POINTER

! ! initialize return code; assume failure until success is certain
 if (present(rc)) then
 rcpresent = .TRUE.
 rc = -1
 endif

! ! call create routine
 do i=1, 1
 lengths(i) = size(f90ptr, i)
 enddo

 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_REAL, ESMF_KIND_I4, &
 1, lengths, status)
 if (status .ne. 0) then
 print *, "Array initial construction error"
 return
 endif

! !TODO: add code to handle copyflag. For now, default to NO_COPY
 if (docopy .eq. ESMF_DO_COPY) then
 allocate(localp( lengths(1) ), stat=status)
 if (status .ne. 0) then ! f90 status, not ESMF
 print *, "Array do_copy allocate error"
 return
 endif
 call c_ESMC_ArraySetDealloc(array, status)
 localp = f90ptr ! this needs to be a real contents copy
 else
 call c_ESMC_ArraySetNoDealloc(array, status)
 localp => f90ptr ! simply a reference to existing space
 endif


! ! set base address
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status)
 if (status .ne. 0) then
 print *, "Array base address construction error"
 return
 endif

! ! save an (uninterpreted) copy of the f90 array information
 wrap%I41Dptr => localp
 call c_ESMC_ArraySetF90Ptr(array, wrap, status)
 if (status .ne. 0) then
 print *, "Array internal info save error"
 return
 endif

! ! return value set by c_ESMC func above
 ESMF_ArrayCreateByPtrI41D = array
 if (rcpresent) rc = 0

 end function ESMF_ArrayCreateByPtrI41D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !IROUTINE: ESMF_ArrayCreateByPtrI81D - make an ESMF array from an F90 ptr

! !INTERFACE:
 function ESMF_ArrayCreateByPtrI81D(f90ptr, docopy, rc)
!
! !RETURN VALUE:
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI81D
!
! !ARGUMENTS:
 integer (ESMF_IKIND_I8), dimension(:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer which
! is already associated with memory.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank and sizes.
!
! \item[[docopy]]
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine
! allocates new space and copies the data from the pointer into the
! new array.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!

!
!EOP
! !REQUIREMENTS:

! local vars
 type (ESMF_Array) :: array ! what C++ is going to return
 integer :: status=-1 ! local error status
 integer :: i ! local variable
 logical :: rcpresent=.FALSE. ! did user specify rc?

 type (ESMF_ArrWrapI81D) :: wrap ! for passing f90 ptr to C++
 integer :: rank, lengths(1) ! size info for the array
 integer (ESMF_IKIND_I8), dimension(:), pointer :: localp ! local copy

! !TODO: need a null pointer to assign to initialize ptr
 array%this = ESMF_NULL_POINTER

! ! initialize return code; assume failure until success is certain
 if (present(rc)) then
 rcpresent = .TRUE.
 rc = -1
 endif

! ! call create routine
 do i=1, 1
 lengths(i) = size(f90ptr, i)
 enddo

 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_REAL, ESMF_KIND_I8, &
 1, lengths, status)
 if (status .ne. 0) then
 print *, "Array initial construction error"
 return
 endif

! !TODO: add code to handle copyflag. For now, default to NO_COPY
 if (docopy .eq. ESMF_DO_COPY) then
 allocate(localp( lengths(1) ), stat=status)
 if (status .ne. 0) then ! f90 status, not ESMF
 print *, "Array do_copy allocate error"
 return
 endif
 call c_ESMC_ArraySetDealloc(array, status)
 localp = f90ptr ! this needs to be a real contents copy
 else
 call c_ESMC_ArraySetNoDealloc(array, status)
 localp => f90ptr ! simply a reference to existing space
 endif


! ! set base address
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status)
 if (status .ne. 0) then
 print *, "Array base address construction error"
 return
 endif

! ! save an (uninterpreted) copy of the f90 array information
 wrap%I81Dptr => localp
 call c_ESMC_ArraySetF90Ptr(array, wrap, status)
 if (status .ne. 0) then
 print *, "Array internal info save error"
 return
 endif

! ! return value set by c_ESMC func above
 ESMF_ArrayCreateByPtrI81D = array
 if (rcpresent) rc = 0

 end function ESMF_ArrayCreateByPtrI81D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !IROUTINE: ESMF_ArrayCreateByPtrI42D - make an ESMF array from an F90 ptr

! !INTERFACE:
 function ESMF_ArrayCreateByPtrI42D(f90ptr, docopy, rc)
!
! !RETURN VALUE:
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI42D
!
! !ARGUMENTS:
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer which
! is already associated with memory.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank and sizes.
!
! \item[[docopy]]
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine
! allocates new space and copies the data from the pointer into the
! new array.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!

!
!EOP
! !REQUIREMENTS:

! local vars
 type (ESMF_Array) :: array ! what C++ is going to return
 integer :: status=-1 ! local error status
 integer :: i ! local variable
 logical :: rcpresent=.FALSE. ! did user specify rc?

 type (ESMF_ArrWrapI42D) :: wrap ! for passing f90 ptr to C++
 integer :: rank, lengths(2) ! size info for the array
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: localp ! local copy

! !TODO: need a null pointer to assign to initialize ptr
 array%this = ESMF_NULL_POINTER

! ! initialize return code; assume failure until success is certain
 if (present(rc)) then
 rcpresent = .TRUE.
 rc = -1
 endif

! ! call create routine
 do i=1, 2
 lengths(i) = size(f90ptr, i)
 enddo

 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_REAL, ESMF_KIND_I4, &
 2, lengths, status)
 if (status .ne. 0) then
 print *, "Array initial construction error"
 return
 endif

! !TODO: add code to handle copyflag. For now, default to NO_COPY
 if (docopy .eq. ESMF_DO_COPY) then
 allocate(localp( lengths(1), lengths(2) ), stat=status)
 if (status .ne. 0) then ! f90 status, not ESMF
 print *, "Array do_copy allocate error"
 return
 endif
 call c_ESMC_ArraySetDealloc(array, status)
 localp = f90ptr ! this needs to be a real contents copy
 else
 call c_ESMC_ArraySetNoDealloc(array, status)
 localp => f90ptr ! simply a reference to existing space
 endif


! ! set base address
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status)
 if (status .ne. 0) then
 print *, "Array base address construction error"
 return
 endif

! ! save an (uninterpreted) copy of the f90 array information
 wrap%I42Dptr => localp
 call c_ESMC_ArraySetF90Ptr(array, wrap, status)
 if (status .ne. 0) then
 print *, "Array internal info save error"
 return
 endif

! ! return value set by c_ESMC func above
 ESMF_ArrayCreateByPtrI42D = array
 if (rcpresent) rc = 0

 end function ESMF_ArrayCreateByPtrI42D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !IROUTINE: ESMF_ArrayCreateByPtrI82D - make an ESMF array from an F90 ptr

! !INTERFACE:
 function ESMF_ArrayCreateByPtrI82D(f90ptr, docopy, rc)
!
! !RETURN VALUE:
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI82D
!
! !ARGUMENTS:
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer which
! is already associated with memory.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank and sizes.
!
! \item[[docopy]]
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine
! allocates new space and copies the data from the pointer into the
! new array.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!

!
!EOP
! !REQUIREMENTS:

! local vars
 type (ESMF_Array) :: array ! what C++ is going to return
 integer :: status=-1 ! local error status
 integer :: i ! local variable
 logical :: rcpresent=.FALSE. ! did user specify rc?

 type (ESMF_ArrWrapI82D) :: wrap ! for passing f90 ptr to C++
 integer :: rank, lengths(2) ! size info for the array
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: localp ! local copy

! !TODO: need a null pointer to assign to initialize ptr
 array%this = ESMF_NULL_POINTER

! ! initialize return code; assume failure until success is certain
 if (present(rc)) then
 rcpresent = .TRUE.
 rc = -1
 endif

! ! call create routine
 do i=1, 2
 lengths(i) = size(f90ptr, i)
 enddo

 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_REAL, ESMF_KIND_I8, &
 2, lengths, status)
 if (status .ne. 0) then
 print *, "Array initial construction error"
 return
 endif

! !TODO: add code to handle copyflag. For now, default to NO_COPY
 if (docopy .eq. ESMF_DO_COPY) then
 allocate(localp( lengths(1), lengths(2) ), stat=status)
 if (status .ne. 0) then ! f90 status, not ESMF
 print *, "Array do_copy allocate error"
 return
 endif
 call c_ESMC_ArraySetDealloc(array, status)
 localp = f90ptr ! this needs to be a real contents copy
 else
 call c_ESMC_ArraySetNoDealloc(array, status)
 localp => f90ptr ! simply a reference to existing space
 endif


! ! set base address
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status)
 if (status .ne. 0) then
 print *, "Array base address construction error"
 return
 endif

! ! save an (uninterpreted) copy of the f90 array information
 wrap%I82Dptr => localp
 call c_ESMC_ArraySetF90Ptr(array, wrap, status)
 if (status .ne. 0) then
 print *, "Array internal info save error"
 return
 endif

! ! return value set by c_ESMC func above
 ESMF_ArrayCreateByPtrI82D = array
 if (rcpresent) rc = 0

 end function ESMF_ArrayCreateByPtrI82D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !IROUTINE: ESMF_ArrayCreateByPtrR41D - make an ESMF array from an F90 ptr

! !INTERFACE:
 function ESMF_ArrayCreateByPtrR41D(f90ptr, docopy, rc)
!
! !RETURN VALUE:
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR41D
!
! !ARGUMENTS:
 real (ESMF_IKIND_R4), dimension(:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer which
! is already associated with memory.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank and sizes.
!
! \item[[docopy]]
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine
! allocates new space and copies the data from the pointer into the
! new array.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!

!
!EOP
! !REQUIREMENTS:

! local vars
 type (ESMF_Array) :: array ! what C++ is going to return
 integer :: status=-1 ! local error status
 integer :: i ! local variable
 logical :: rcpresent=.FALSE. ! did user specify rc?

 type (ESMF_ArrWrapR41D) :: wrap ! for passing f90 ptr to C++
 integer :: rank, lengths(1) ! size info for the array
 real (ESMF_IKIND_R4), dimension(:), pointer :: localp ! local copy

! !TODO: need a null pointer to assign to initialize ptr
 array%this = ESMF_NULL_POINTER

! ! initialize return code; assume failure until success is certain
 if (present(rc)) then
 rcpresent = .TRUE.
 rc = -1
 endif

! ! call create routine
 do i=1, 1
 lengths(i) = size(f90ptr, i)
 enddo

 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_REAL, ESMF_KIND_R4, &
 1, lengths, status)
 if (status .ne. 0) then
 print *, "Array initial construction error"
 return
 endif

! !TODO: add code to handle copyflag. For now, default to NO_COPY
 if (docopy .eq. ESMF_DO_COPY) then
 allocate(localp( lengths(1) ), stat=status)
 if (status .ne. 0) then ! f90 status, not ESMF
 print *, "Array do_copy allocate error"
 return
 endif
 call c_ESMC_ArraySetDealloc(array, status)
 localp = f90ptr ! this needs to be a real contents copy
 else
 call c_ESMC_ArraySetNoDealloc(array, status)
 localp => f90ptr ! simply a reference to existing space
 endif


! ! set base address
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status)
 if (status .ne. 0) then
 print *, "Array base address construction error"
 return
 endif

! ! save an (uninterpreted) copy of the f90 array information
 wrap%R41Dptr => localp
 call c_ESMC_ArraySetF90Ptr(array, wrap, status)
 if (status .ne. 0) then
 print *, "Array internal info save error"
 return
 endif

! ! return value set by c_ESMC func above
 ESMF_ArrayCreateByPtrR41D = array
 if (rcpresent) rc = 0

 end function ESMF_ArrayCreateByPtrR41D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !IROUTINE: ESMF_ArrayCreateByPtrR81D - make an ESMF array from an F90 ptr

! !INTERFACE:
 function ESMF_ArrayCreateByPtrR81D(f90ptr, docopy, rc)
!
! !RETURN VALUE:
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR81D
!
! !ARGUMENTS:
 real (ESMF_IKIND_R8), dimension(:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer which
! is already associated with memory.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank and sizes.
!
! \item[[docopy]]
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine
! allocates new space and copies the data from the pointer into the
! new array.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!

!
!EOP
! !REQUIREMENTS:

! local vars
 type (ESMF_Array) :: array ! what C++ is going to return
 integer :: status=-1 ! local error status
 integer :: i ! local variable
 logical :: rcpresent=.FALSE. ! did user specify rc?

 type (ESMF_ArrWrapR81D) :: wrap ! for passing f90 ptr to C++
 integer :: rank, lengths(1) ! size info for the array
 real (ESMF_IKIND_R8), dimension(:), pointer :: localp ! local copy

! !TODO: need a null pointer to assign to initialize ptr
 array%this = ESMF_NULL_POINTER

! ! initialize return code; assume failure until success is certain
 if (present(rc)) then
 rcpresent = .TRUE.
 rc = -1
 endif

! ! call create routine
 do i=1, 1
 lengths(i) = size(f90ptr, i)
 enddo

 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_REAL, ESMF_KIND_R8, &
 1, lengths, status)
 if (status .ne. 0) then
 print *, "Array initial construction error"
 return
 endif

! !TODO: add code to handle copyflag. For now, default to NO_COPY
 if (docopy .eq. ESMF_DO_COPY) then
 allocate(localp( lengths(1) ), stat=status)
 if (status .ne. 0) then ! f90 status, not ESMF
 print *, "Array do_copy allocate error"
 return
 endif
 call c_ESMC_ArraySetDealloc(array, status)
 localp = f90ptr ! this needs to be a real contents copy
 else
 call c_ESMC_ArraySetNoDealloc(array, status)
 localp => f90ptr ! simply a reference to existing space
 endif


! ! set base address
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status)
 if (status .ne. 0) then
 print *, "Array base address construction error"
 return
 endif

! ! save an (uninterpreted) copy of the f90 array information
 wrap%R81Dptr => localp
 call c_ESMC_ArraySetF90Ptr(array, wrap, status)
 if (status .ne. 0) then
 print *, "Array internal info save error"
 return
 endif

! ! return value set by c_ESMC func above
 ESMF_ArrayCreateByPtrR81D = array
 if (rcpresent) rc = 0

 end function ESMF_ArrayCreateByPtrR81D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !IROUTINE: ESMF_ArrayCreateByPtrR42D - make an ESMF array from an F90 ptr

! !INTERFACE:
 function ESMF_ArrayCreateByPtrR42D(f90ptr, docopy, rc)
!
! !RETURN VALUE:
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR42D
!
! !ARGUMENTS:
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer which
! is already associated with memory.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank and sizes.
!
! \item[[docopy]]
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine
! allocates new space and copies the data from the pointer into the
! new array.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!

!
!EOP
! !REQUIREMENTS:

! local vars
 type (ESMF_Array) :: array ! what C++ is going to return
 integer :: status=-1 ! local error status
 integer :: i ! local variable
 logical :: rcpresent=.FALSE. ! did user specify rc?

 type (ESMF_ArrWrapR42D) :: wrap ! for passing f90 ptr to C++
 integer :: rank, lengths(2) ! size info for the array
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: localp ! local copy

! !TODO: need a null pointer to assign to initialize ptr
 array%this = ESMF_NULL_POINTER

! ! initialize return code; assume failure until success is certain
 if (present(rc)) then
 rcpresent = .TRUE.
 rc = -1
 endif

! ! call create routine
 do i=1, 2
 lengths(i) = size(f90ptr, i)
 enddo

 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_REAL, ESMF_KIND_R4, &
 2, lengths, status)
 if (status .ne. 0) then
 print *, "Array initial construction error"
 return
 endif

! !TODO: add code to handle copyflag. For now, default to NO_COPY
 if (docopy .eq. ESMF_DO_COPY) then
 allocate(localp( lengths(1), lengths(2) ), stat=status)
 if (status .ne. 0) then ! f90 status, not ESMF
 print *, "Array do_copy allocate error"
 return
 endif
 call c_ESMC_ArraySetDealloc(array, status)
 localp = f90ptr ! this needs to be a real contents copy
 else
 call c_ESMC_ArraySetNoDealloc(array, status)
 localp => f90ptr ! simply a reference to existing space
 endif


! ! set base address
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status)
 if (status .ne. 0) then
 print *, "Array base address construction error"
 return
 endif

! ! save an (uninterpreted) copy of the f90 array information
 wrap%R42Dptr => localp
 call c_ESMC_ArraySetF90Ptr(array, wrap, status)
 if (status .ne. 0) then
 print *, "Array internal info save error"
 return
 endif

! ! return value set by c_ESMC func above
 ESMF_ArrayCreateByPtrR42D = array
 if (rcpresent) rc = 0

 end function ESMF_ArrayCreateByPtrR42D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !IROUTINE: ESMF_ArrayCreateByPtrR82D - make an ESMF array from an F90 ptr

! !INTERFACE:
 function ESMF_ArrayCreateByPtrR82D(f90ptr, docopy, rc)
!
! !RETURN VALUE:
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR82D
!
! !ARGUMENTS:
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates an {\tt Array} based on an existing Fortran 90 pointer which
! is already associated with memory.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank and sizes.
!
! \item[[docopy]]
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine
! allocates new space and copies the data from the pointer into the
! new array.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!

!
!EOP
! !REQUIREMENTS:

! local vars
 type (ESMF_Array) :: array ! what C++ is going to return
 integer :: status=-1 ! local error status
 integer :: i ! local variable
 logical :: rcpresent=.FALSE. ! did user specify rc?

 type (ESMF_ArrWrapR82D) :: wrap ! for passing f90 ptr to C++
 integer :: rank, lengths(2) ! size info for the array
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: localp ! local copy

! !TODO: need a null pointer to assign to initialize ptr
 array%this = ESMF_NULL_POINTER

! ! initialize return code; assume failure until success is certain
 if (present(rc)) then
 rcpresent = .TRUE.
 rc = -1
 endif

! ! call create routine
 do i=1, 2
 lengths(i) = size(f90ptr, i)
 enddo

 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_REAL, ESMF_KIND_R8, &
 2, lengths, status)
 if (status .ne. 0) then
 print *, "Array initial construction error"
 return
 endif

! !TODO: add code to handle copyflag. For now, default to NO_COPY
 if (docopy .eq. ESMF_DO_COPY) then
 allocate(localp( lengths(1), lengths(2) ), stat=status)
 if (status .ne. 0) then ! f90 status, not ESMF
 print *, "Array do_copy allocate error"
 return
 endif
 call c_ESMC_ArraySetDealloc(array, status)
 localp = f90ptr ! this needs to be a real contents copy
 else
 call c_ESMC_ArraySetNoDealloc(array, status)
 localp => f90ptr ! simply a reference to existing space
 endif


! ! set base address
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status)
 if (status .ne. 0) then
 print *, "Array base address construction error"
 return
 endif

! ! save an (uninterpreted) copy of the f90 array information
 wrap%R82Dptr => localp
 call c_ESMC_ArraySetF90Ptr(array, wrap, status)
 if (status .ne. 0) then
 print *, "Array internal info save error"
 return
 endif

! ! return value set by c_ESMC func above
 ESMF_ArrayCreateByPtrR82D = array
 if (rcpresent) rc = 0

 end function ESMF_ArrayCreateByPtrR82D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


      ! < these expand into actual function bodies >
!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayGetDataI41D(array, f90ptr, docopy, rc)
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 integer (ESMF_IKIND_I4), dimension(:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
!
!EOP
! !REQUIREMENTS:

 type (ESMF_ArrWrapI41D) :: wrap ! for passing f90 ptr to C++
!
! TODO: code goes here
!
 ! check copyflag to see if we are making a reference
 ! or making a new array and a copy

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 f90ptr => wrap%I41Dptr

 end subroutine ESMF_ArrayGetDataI41D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayGetDataI81D(array, f90ptr, docopy, rc)
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 integer (ESMF_IKIND_I8), dimension(:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
!
!EOP
! !REQUIREMENTS:

 type (ESMF_ArrWrapI81D) :: wrap ! for passing f90 ptr to C++
!
! TODO: code goes here
!
 ! check copyflag to see if we are making a reference
 ! or making a new array and a copy

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 f90ptr => wrap%I81Dptr

 end subroutine ESMF_ArrayGetDataI81D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayGetDataI42D(array, f90ptr, docopy, rc)
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
!
!EOP
! !REQUIREMENTS:

 type (ESMF_ArrWrapI42D) :: wrap ! for passing f90 ptr to C++
!
! TODO: code goes here
!
 ! check copyflag to see if we are making a reference
 ! or making a new array and a copy

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 f90ptr => wrap%I42Dptr

 end subroutine ESMF_ArrayGetDataI42D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayGetDataI82D(array, f90ptr, docopy, rc)
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
!
!EOP
! !REQUIREMENTS:

 type (ESMF_ArrWrapI82D) :: wrap ! for passing f90 ptr to C++
!
! TODO: code goes here
!
 ! check copyflag to see if we are making a reference
 ! or making a new array and a copy

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 f90ptr => wrap%I82Dptr

 end subroutine ESMF_ArrayGetDataI82D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayGetDataR41D(array, f90ptr, docopy, rc)
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 real (ESMF_IKIND_R4), dimension(:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
!
!EOP
! !REQUIREMENTS:

 type (ESMF_ArrWrapR41D) :: wrap ! for passing f90 ptr to C++
!
! TODO: code goes here
!
 ! check copyflag to see if we are making a reference
 ! or making a new array and a copy

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 f90ptr => wrap%R41Dptr

 end subroutine ESMF_ArrayGetDataR41D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayGetDataR81D(array, f90ptr, docopy, rc)
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 real (ESMF_IKIND_R8), dimension(:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
!
!EOP
! !REQUIREMENTS:

 type (ESMF_ArrWrapR81D) :: wrap ! for passing f90 ptr to C++
!
! TODO: code goes here
!
 ! check copyflag to see if we are making a reference
 ! or making a new array and a copy

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 f90ptr => wrap%R81Dptr

 end subroutine ESMF_ArrayGetDataR81D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayGetDataR42D(array, f90ptr, docopy, rc)
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
!
!EOP
! !REQUIREMENTS:

 type (ESMF_ArrWrapR42D) :: wrap ! for passing f90 ptr to C++
!
! TODO: code goes here
!
 ! check copyflag to see if we are making a reference
 ! or making a new array and a copy

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 f90ptr => wrap%R42Dptr

 end subroutine ESMF_ArrayGetDataR42D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayGetDataR82D(array, f90ptr, docopy, rc)
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: f90ptr
 type(ESMF_CopyFlag), intent(in), optional :: docopy
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
!
!EOP
! !REQUIREMENTS:

 type (ESMF_ArrWrapR82D) :: wrap ! for passing f90 ptr to C++
!
! TODO: code goes here
!
 ! check copyflag to see if we are making a reference
 ! or making a new array and a copy

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 f90ptr => wrap%R82Dptr

 end subroutine ESMF_ArrayGetDataR82D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


      ! < these expand into actual function bodies >
!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayDeallocateI41D(array, wrap, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 type (ESMF_ArrWrapI41D) :: wrap
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents if Array object is responsible for cleaning up.
!
!EOP
! !REQUIREMENTS:

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 deallocate(wrap%I41Dptr)

 if (present(rc)) rc = 0

 end subroutine ESMF_ArrayDeallocateI41D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayDeallocateI81D(array, wrap, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 type (ESMF_ArrWrapI81D) :: wrap
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents if Array object is responsible for cleaning up.
!
!EOP
! !REQUIREMENTS:

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 deallocate(wrap%I81Dptr)

 if (present(rc)) rc = 0

 end subroutine ESMF_ArrayDeallocateI81D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayDeallocateI42D(array, wrap, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 type (ESMF_ArrWrapI42D) :: wrap
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents if Array object is responsible for cleaning up.
!
!EOP
! !REQUIREMENTS:

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 deallocate(wrap%I42Dptr)

 if (present(rc)) rc = 0

 end subroutine ESMF_ArrayDeallocateI42D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayDeallocateI82D(array, wrap, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 type (ESMF_ArrWrapI82D) :: wrap
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents if Array object is responsible for cleaning up.
!
!EOP
! !REQUIREMENTS:

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 deallocate(wrap%I82Dptr)

 if (present(rc)) rc = 0

 end subroutine ESMF_ArrayDeallocateI82D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayDeallocateR41D(array, wrap, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 type (ESMF_ArrWrapR41D) :: wrap
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents if Array object is responsible for cleaning up.
!
!EOP
! !REQUIREMENTS:

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 deallocate(wrap%R41Dptr)

 if (present(rc)) rc = 0

 end subroutine ESMF_ArrayDeallocateR41D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayDeallocateR81D(array, wrap, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 type (ESMF_ArrWrapR81D) :: wrap
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents if Array object is responsible for cleaning up.
!
!EOP
! !REQUIREMENTS:

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 deallocate(wrap%R81Dptr)

 if (present(rc)) rc = 0

 end subroutine ESMF_ArrayDeallocateR81D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayDeallocateR42D(array, wrap, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 type (ESMF_ArrWrapR42D) :: wrap
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents if Array object is responsible for cleaning up.
!
!EOP
! !REQUIREMENTS:

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 deallocate(wrap%R42Dptr)

 if (present(rc)) rc = 0

 end subroutine ESMF_ArrayDeallocateR42D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
! <Created by macro - do not edit directly >
!BOP
! !INTERFACE:
 subroutine ESMF_ArrayDeallocateR82D(array, wrap, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
 type(ESMF_Array) :: array
 type (ESMF_ArrWrapR82D) :: wrap
 integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents if Array object is responsible for cleaning up.
!
!EOP
! !REQUIREMENTS:

 call c_ESMC_ArrayGetF90Ptr(array, wrap, rc)
 deallocate(wrap%R82Dptr)

 if (present(rc)) rc = 0

 end subroutine ESMF_ArrayDeallocateR82D

! < end macro - do not edit directly >
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! !TODO: this may be obsolete.
!BOP
! !IROUTINE: ESMF_ArrayCreateMTPtr2DR8 - make an ESMF array from an F90 ptr

! !INTERFACE:
      function ESMF_ArrayCreateMTPtr2DR8(f90ptr, ni, nj, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateMTPtr2DR8
!
! !ARGUMENTS:
      real (ESMF_IKIND_R8), dimension(:,:), pointer :: f90ptr
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
        type (ESMF_Array) :: array          ! what C++ is going to return
        integer :: status=-1      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?

!       TODO: need a null pointer to assign to initialize ptr
        array%this = ESMF_NULL_POINTER

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = -1
        endif

!       set up callback
        call c_ESMC_StoreAllocFunc(ESMF_Allocate2DR4, status);

!       call create routine
        !call c_ESMC_ArrayCreateMTPtr2D(array, ni, nj, status)
        if (status .ne. 0) then
          print *, "Array initial construction error"
          return
        endif

        call ESMF_Allocate(f90ptr, ni, nj, status)
        if (status .ne. 0) then
          print *, "Array allocate construction error"
          return
        endif

!       set base address
        call c_ESMC_ArraySetBaseAddr(array, f90ptr(1,1), status)
        if (status .ne. 0) then
          print *, "Array base address construction error"
          return
        endif


!       return value set by c_ESMC func above
        ESMF_ArrayCreateMTPtr2DR8 = array
        if (rcpresent) rc = 0

        end function ESMF_ArrayCreateMTPtr2DR8  


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayDeallocateData(array, rc)
!
! !RETURN VALUE:
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Return an F90 pointer to the data buffer, or return an F90 pointer
!     to a new copy of the data.
!
!EOP
! !REQUIREMENTS:
      type (ESMF_ArrWrapI41D) :: wrapI41D
      type (ESMF_ArrWrapI81D) :: wrapI81D
      type (ESMF_ArrWrapI42D) :: wrapI42D
      type (ESMF_ArrWrapI82D) :: wrapI82D
      type (ESMF_ArrWrapR41D) :: wrapR41D
      type (ESMF_ArrWrapR81D) :: wrapR81D
      type (ESMF_ArrWrapR42D) :: wrapR42D
      type (ESMF_ArrWrapR82D) :: wrapR82D

!    ! TODO: call the c interfaces to get the rank, type, kind, and then
     !  call the function with the right wrapper type to get it deallocated.
      call ESMF_ArrayDeallocateType(array, wrapI81D, rc)

      end subroutine ESMF_ArrayDeallocateData




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
        integer :: status=-1      ! local error status
        logical :: rcpresent=.FALSE.        ! did user specify rc?
        logical :: needsdealloc=.FALSE.     ! do we need to free space?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = -1
        endif

!       ! TODO: document the current rule - if we did the allocate in
!       !   the case of ESMF_DO_COPY at create time, then we delete the
!       !   space.  otherwise, the user needs to destroy the array first
!       !   (we'll ignore the data) and then call deallocate themselves.

!       call Destruct first, then free this memory
        call c_ESMC_ArrayNeedsDealloc(array, needsdealloc, status)
        if (needsdealloc) then
          call ESMF_ArrayDeallocateData(array, status)
          if (status .ne. 0) then
            print *, "Array contents destruction error"
            return
          endif
        endif

        call c_ESMC_ArrayDestroy(array, status)
        if (status .ne. 0) then
          print *, "Array destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = 0

        end subroutine ESMF_ArrayDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArraySetData
!
! !INTERFACE:
      subroutine ESMF_ArraySetData(array, dataspec, databuf, docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array 
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
! TODO: code goes here
!
        end subroutine ESMF_ArraySetData

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArraySetAxisIndex
!
! !INTERFACE:
      subroutine ESMF_ArraySetAxisIndex(array, indexlist, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array 
      type(ESMF_AxisIndex), intent(in) :: indexlist
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to annotate an Array with information used to map local to global
!      indicies.  
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        ! call c routine to add index

        end subroutine ESMF_ArraySetAxisIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayGetAxisIndex
!
! !INTERFACE:
      subroutine ESMF_ArrayGetAxisIndex(array, indexlist, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array 
      type(ESMF_AxisIndex), intent(out) :: indexlist
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to retrieve the index annotation from an Array.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        ! call c routine to query index

        end subroutine ESMF_ArrayGetAxisIndex


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayReorder(array, newarrayspec, newarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array 
      type(ESMF_ArraySpec), intent(in) :: newarrayspec
      type(ESMF_Array):: newarray   
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Used to alter the local memory ordering (layout) of this Array.
!
!  !TODO: remove this note before generating user documentation
!
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
! TODO: code goes here
!
        end subroutine ESMF_ArrayReorder


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
     function ESMF_ArraySpecCreate(rank, type, kind, counts, &
                                   lbounds, ubounds, strides, rc)
!
! !RETURN VALUE:
     type(ESMF_ArraySpec), pointer :: ESMF_ArraySpecCreate  
!
! !ARGUMENTS:
     integer, intent(in) :: rank
     type(ESMF_DataType), intent(in) :: type
     type(ESMF_DataKind), intent(in) :: kind
     integer, dimension(:), intent(in) :: counts
     integer, dimension(:), intent(in), optional :: lbounds
     integer, dimension(:), intent(in), optional :: ubounds
     integer, dimension(:), intent(in), optional :: strides
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
!    Array kind.  Valid kinds include {\tt ESMF\_KIND\_I4}, 
!    {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8}, 
!    {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}. 
!
!  \item[counts]
!    The size of each dimension in the Array.  This is a 1D integer array
!    the same length as the rank.
!
!  \item[[lbounds]]
!    The lower bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.  If not specified
!    the default values are 1 for each dimension.
!
!  \item[[ubounds]]
!    The upper bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.  If not specified
!    the default values are same as the count in each dimension.
!
!  \item[[strides]]
!    The strides for each rank of the array. This is a 1D
!    integer array the same length as the rank.  If not specified
!    the default values are the same as the default Fortran array strides.
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
        integer :: status=-1           ! local error status
        logical :: rcpresent=.FALSE.             ! did user specify rc?

!       initialize pointer
        nullify(as)

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = -1
        endif

!       allocate space for ArraySpec and call Construct method to initalize
        allocate(as, stat=status)
        if (status .ne. 0) then         ! this is a fortran rc, NOT an ESMF rc
          print *, "ArraySpec allocation error"
          return
        endif

        call ESMF_ArraySpecConstruct(as, rank, type, kind, counts, &
                                      lbounds, ubounds, strides, status)
        if (status .ne. 0) then
          print *, "ArraySpec construction error"
          return
        endif

!       set return value
        ESMF_ArraySpecCreate => as
        if (rcpresent) rc = 0

        end function ESMF_ArraySpecCreate



!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArraySpecConstruct(as, rank, type, kind, counts, &
                                         lbounds, ubounds, strides, rc)
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), pointer :: as
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, dimension(:), intent(in), optional :: lbounds
      integer, dimension(:), intent(in), optional :: ubounds
      integer, dimension(:), intent(in), optional :: strides
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
!    Array kind.  Valid kinds include {\tt ESMF\_KIND\_I4}, 
!    {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8}, 
!    {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}. 
!
!  \item[counts]
!    The size of each dimension in the Array.  This is a 1D integer array
!    the same length as the rank.
!
!  \item[[lbounds]]
!    The lower bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.  If not specified
!    the default values are 1 for each dimension.
!
!  \item[[ubounds]]
!    The upper bounds for valid indices in the array.  This is a 1D
!    integer array the same length as the rank.  If not specified
!    the default values are same as the count in each dimension.
!
!  \item[[strides]]
!    The strides for each rank of the array. This is a 1D
!    integer array the same length as the rank.  If not specified
!    the default values are the same as the default Fortran array strides.
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP

!       local vars
        integer :: i
        integer :: status=-1           ! local error status
        logical :: rcpresent=.FALSE.             ! did user specify rc?

!       initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = -1
        endif


!       set arrayspec contents
      
        as%rank = rank   
        as%type = type
        as%kind = kind

        do i=1,rank
          as%counts(i) = counts(i)
          !as%rinfo(i, 1) = lbounds(i)
          !as%rinfo(i, 2) = ubounds(i)
          !as%rinfo(i, 3) = strides(i)
        enddo

        if (rcpresent) rc = 0

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
      type(ESMF_Array) :: array
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
! TODO: code goes here
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
      type(ESMF_Array):: array 
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
! TODO: code goes here
!
        type (ESMF_Array) :: a 

!       this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER

!
! TODO: add code here
!

        ESMF_ArrayRestore = a 
 
        end function ESMF_ArrayRestore


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayWrite(array, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
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
! TODO: code goes here
!
        type (ESMF_Array) :: a

!       this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER

!
! TODO: add code here
!

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
! TODO: code goes here
!
       character (len=6) :: defaultopts="brief"
       integer :: status=-1      ! local error status
       logical :: rcpresent=.FALSE.

!      Initialize return code; assume failure until success is certain
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = -1
       endif

       if(present(options)) then
           call c_ESMC_ArrayPrint(array, options, status) 
       else
           call c_ESMC_ArrayPrint(array, defaultopts, status) 
       endif

       if (status .ne. 0) then
         print *, "Array print error"
         return
       endif

!      set return values
       if (rcpresent) rc = 0

       end subroutine ESMF_ArrayPrint


        end module ESMF_ArrayMod

