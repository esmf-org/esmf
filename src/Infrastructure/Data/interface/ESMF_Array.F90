! $Id: ESMF_Array.F90,v 1.47 2003/04/14 14:51:32 nscollins Exp $
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
! ESMF Array module
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
!BOP
! !MODULE: ESMF_ArrayMod - Manage data arrays uniformly between F90 and C++
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt Array} class and
! associated functions and subroutines.
!
! C and C++ arrays are simple pointers to memory.
! Fortran arrays contain shape and stride definitions and are strongly
! typed. To enable interoperability between the languages the C++ code
! must be able to obtain this information from the Fortran description
! (which is called the "dope vector" in Fortran), either through a priori
! knowledge or through query.
!
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_IOMod
      use ESMF_AllocMod
      use ESMF_DELayoutMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
! ! ESMF_CopyFlag
!
! ! Indicates whether a data array should be copied or referenced.

      type ESMF_CopyFlag
      sequence
      private
        integer :: docopy
      end type

      type(ESMF_CopyFlag), parameter :: &
                            ESMF_DO_COPY = ESMF_CopyFlag(1), &
                            ESMF_NO_COPY = ESMF_CopyFlag(2)

!------------------------------------------------------------------------------
! ! ESMF_ArraySpec
!
! ! Data array specification, with no associated data buffer.

      type ESMF_ArraySpec
      sequence
      private

        integer :: rank ! number of dimensions
        type(ESMF_DataType) :: type ! real/float, integer, etc enum
        type(ESMF_DataKind) :: kind ! fortran "kind" enum/integer

        !integer, dimension(ESMF_MAXDIM) :: counts ! array dimension sizes
        !logical :: hascounts ! counts optional
        !integer, dimension(ESMF_MAXDIM, 3) :: rinfo ! (lower/upper/stride) per rank
        !logical :: has_rinfo ! rinfo optional
        !type(ESMF_AxisIndex), dimension(ESMF_MAXDIM) :: ai ! axis indices

      end type

!------------------------------------------------------------------------------
! ! ESMF_Array
!
! ! Array data type. All information is kept on the C++ side inside
! ! the class structure.

      type ESMF_Array
      sequence
      private
        type(ESMF_Pointer) :: this ! opaque pointer to the C++ class data
      end type

!------------------------------------------------------------------------------
! ! Internal wrapper structures for passing f90 pointers to C++ and
! ! guaranteeing they are passed by reference on all compilers and all
! ! platforms. These are never seen outside this module.
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
 type ESMF_ArrWrapI24D 
 integer (ESMF_IKIND_I2),dimension(:,:,:,:),pointer :: I24Dptr 
 end type ESMF_ArrWrapI24D 

      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapI44D 
 integer (ESMF_IKIND_I4),dimension(:,:,:,:),pointer :: I44Dptr 
 end type ESMF_ArrWrapI44D 

      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapI84D 
 integer (ESMF_IKIND_I8),dimension(:,:,:,:),pointer :: I84Dptr 
 end type ESMF_ArrWrapI84D 


      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapI25D 
 integer (ESMF_IKIND_I2),dimension(:,:,:,:,:),pointer :: I25Dptr 
 end type ESMF_ArrWrapI25D 

      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapI45D 
 integer (ESMF_IKIND_I4),dimension(:,:,:,:,:),pointer :: I45Dptr 
 end type ESMF_ArrWrapI45D 

      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapI85D 
 integer (ESMF_IKIND_I8),dimension(:,:,:,:,:),pointer :: I85Dptr 
 end type ESMF_ArrWrapI85D 


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


      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapR44D 
 real (ESMF_IKIND_R4),dimension(:,:,:,:),pointer :: R44Dptr 
 end type ESMF_ArrWrapR44D 

      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapR84D 
 real (ESMF_IKIND_R8),dimension(:,:,:,:),pointer :: R84Dptr 
 end type ESMF_ArrWrapR84D 


      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapR45D 
 real (ESMF_IKIND_R4),dimension(:,:,:,:,:),pointer :: R45Dptr 
 end type ESMF_ArrWrapR45D 

      ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrapR85D 
 real (ESMF_IKIND_R8),dimension(:,:,:,:,:),pointer :: R85Dptr 
 end type ESMF_ArrWrapR85D 


      ! TODO: add any additional type/kind/ranks here

!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CopyFlag, ESMF_DO_COPY, ESMF_NO_COPY
      public ESMF_DataKind, ESMF_Pointer
      public ESMF_ArraySpec, ESMF_Array
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArrayCreate
      public ESMF_ArrayDestroy

      public ESMF_ArraySpecInit
      public ESMF_ArraySpecGet

      public ESMF_ArraySetData, ESMF_ArrayGetData
      public ESMF_ArraySetAxisIndex, ESMF_ArrayGetAxisIndex
      public ESMF_ArrayRedist, ESMF_ArrayHalo
      public ESMF_ArrayGet, ESMF_ArrayGetName

      public ESMF_ArrayCheckpoint
      public ESMF_ArrayRestore
      public ESMF_ArrayWrite
      public ESMF_ArrayRead

      public ESMF_ArrayValidate
      public ESMF_ArrayPrint
!EOP

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Array.F90,v 1.47 2003/04/14 14:51:32 nscollins Exp $'

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
! !module procedure ESMF_ArrayCreateNewNoData
        module procedure ESMF_ArrayCreateNewBuffer
        module procedure ESMF_ArrayCreateBySpec
        module procedure ESMF_ArrayCreateBySpecNoData
! !module procedure ESMF_ArrayCreateBySpecBuffer

       ! < this expands into declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_ArrayCreateByPtrI21D 
 module procedure ESMF_ArrayCreateByPtrI41D 
 module procedure ESMF_ArrayCreateByPtrI81D 
 module procedure ESMF_ArrayCreateByPtrI22D 
 module procedure ESMF_ArrayCreateByPtrI42D 
 module procedure ESMF_ArrayCreateByPtrI82D 
 module procedure ESMF_ArrayCreateByPtrI23D 
 module procedure ESMF_ArrayCreateByPtrI43D 
 module procedure ESMF_ArrayCreateByPtrI83D 
 module procedure ESMF_ArrayCreateByPtrI24D 
 module procedure ESMF_ArrayCreateByPtrI44D 
 module procedure ESMF_ArrayCreateByPtrI84D 
 module procedure ESMF_ArrayCreateByPtrI25D 
 module procedure ESMF_ArrayCreateByPtrI45D 
 module procedure ESMF_ArrayCreateByPtrI85D 
 module procedure ESMF_ArrayCreateByPtrR41D 
 module procedure ESMF_ArrayCreateByPtrR81D 
 module procedure ESMF_ArrayCreateByPtrR42D 
 module procedure ESMF_ArrayCreateByPtrR82D 
 module procedure ESMF_ArrayCreateByPtrR43D 
 module procedure ESMF_ArrayCreateByPtrR83D 
 module procedure ESMF_ArrayCreateByPtrR44D 
 module procedure ESMF_ArrayCreateByPtrR84D 
 module procedure ESMF_ArrayCreateByPtrR45D 
 module procedure ESMF_ArrayCreateByPtrR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



! ! TODO: add any additional type/kind/ranks here

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_ArrayCreate} functions.
!
! There are 4 options for setting the contents of the {\tt ESMF\_Array}
! at creation time:
! \begin{description}
! \item[No Data]
! No data space is allocated.
! \item[Allocate Space Only]
! Data space is allocated but not initialized. The caller can query
! for a pointer to the start of the space to address it directly.
! \item[Data Copy]
! An existing Fortran array is specified and the data contents are copied
! into new space allocated by the {\tt ESMF\_Array}.
! \item[Data Reference]
! An existing Fortran array is specified and the data contents reference
! it directly. The caller must not deallocate the space; the
! {\tt ESMF\_Array} will free the space when it is destroyed.
! \end{description}
!
! If the {\tt ESMF\_Array} contains data, there are 4 options for
! specifying the type/kind/rank of that data:
! \begin{description}
! \item[List]
! The characteristics of the {\tt ESMF\_Array} are given explicitly
! by individual arguments to the create function.
! \item[ArraySpec]
! A previously created {\tt ESMF\_ArraySpec} object is given which
! describes the characteristics.
! \item[Fortran array]
! An existing Fortran array is used to describe the array.
! (Only available from the Fortran interface.)
! \item[Fortran 90 Pointer]
! An existing Fortran 90 array pointer is used to describe the array.
! (Only available from the Fortran interface.)
! \end{description}
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
      ! < this expands into declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_ArrayGetDataI21D 
 module procedure ESMF_ArrayGetDataI41D 
 module procedure ESMF_ArrayGetDataI81D 
 module procedure ESMF_ArrayGetDataI22D 
 module procedure ESMF_ArrayGetDataI42D 
 module procedure ESMF_ArrayGetDataI82D 
 module procedure ESMF_ArrayGetDataI23D 
 module procedure ESMF_ArrayGetDataI43D 
 module procedure ESMF_ArrayGetDataI83D 
 module procedure ESMF_ArrayGetDataI24D 
 module procedure ESMF_ArrayGetDataI44D 
 module procedure ESMF_ArrayGetDataI84D 
 module procedure ESMF_ArrayGetDataI25D 
 module procedure ESMF_ArrayGetDataI45D 
 module procedure ESMF_ArrayGetDataI85D 
 module procedure ESMF_ArrayGetDataR41D 
 module procedure ESMF_ArrayGetDataR81D 
 module procedure ESMF_ArrayGetDataR42D 
 module procedure ESMF_ArrayGetDataR82D 
 module procedure ESMF_ArrayGetDataR43D 
 module procedure ESMF_ArrayGetDataR83D 
 module procedure ESMF_ArrayGetDataR44D 
 module procedure ESMF_ArrayGetDataR84D 
 module procedure ESMF_ArrayGetDataR45D 
 module procedure ESMF_ArrayGetDataR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



! ! TODO: add any additional type/kind/ranks here

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_ArrayGetData} functions.
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
      ! < this expands into declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_ArrayDeallocateI21D 
 module procedure ESMF_ArrayDeallocateI41D 
 module procedure ESMF_ArrayDeallocateI81D 
 module procedure ESMF_ArrayDeallocateI22D 
 module procedure ESMF_ArrayDeallocateI42D 
 module procedure ESMF_ArrayDeallocateI82D 
 module procedure ESMF_ArrayDeallocateI23D 
 module procedure ESMF_ArrayDeallocateI43D 
 module procedure ESMF_ArrayDeallocateI83D 
 module procedure ESMF_ArrayDeallocateI24D 
 module procedure ESMF_ArrayDeallocateI44D 
 module procedure ESMF_ArrayDeallocateI84D 
 module procedure ESMF_ArrayDeallocateI25D 
 module procedure ESMF_ArrayDeallocateI45D 
 module procedure ESMF_ArrayDeallocateI85D 
 module procedure ESMF_ArrayDeallocateR41D 
 module procedure ESMF_ArrayDeallocateR81D 
 module procedure ESMF_ArrayDeallocateR42D 
 module procedure ESMF_ArrayDeallocateR82D 
 module procedure ESMF_ArrayDeallocateR43D 
 module procedure ESMF_ArrayDeallocateR83D 
 module procedure ESMF_ArrayDeallocateR44D 
 module procedure ESMF_ArrayDeallocateR84D 
 module procedure ESMF_ArrayDeallocateR45D 
 module procedure ESMF_ArrayDeallocateR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


! ! TODO: add any additional type/kind/ranks here

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_ArrayDeallocateType} functions.
!
!EOP
    end interface

!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
interface operator (.eq.)
 module procedure cfeq
 module procedure ESMF_sfeq
 module procedure ESMF_dteq
 module procedure ESMF_dkeq
 module procedure ESMF_opeq
end interface
interface operator (.ne.)
 module procedure cfne
 module procedure ESMF_sfne
 module procedure ESMF_dtne
 module procedure ESMF_dkne
 module procedure ESMF_opne
end interface

!==============================================================================

      contains

!==============================================================================

! functions to compare two ESMF_CopyFlags to see if they are the same or not

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
! Create a new Array and set the data values.
!
! The return value is a new Array.
!
! The arguments are:
! \begin{description}
!
! \item[rank]
! Array rank (dimensionality, 1D, 2D, etc). Maximum allowed is 5D.
!
! \item[type]
! Array type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
!
! \item[kind]
! Array kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
!
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
!
! \item[lbounds]
! The lower bounds for valid indices in the array. This is a 1D
! integer array the same length as the rank. If not specified,
! the default is 1 for each dimension.
!
! \item[ubounds]
! The upper bounds for valid indices in the array. This is a 1D
! integer array the same length as the rank. If not specified,
! the default is the count for each dimension.
!
! \item[strides]
! The strides for each rank of the array. This is a 1D
! integer array the same length as the rank. If not specified,
! the default is the standard Fortran row-major ordering.
!
! \item[bufaddr]
! A pointer to the start of the contents of the Array.
!
! \item[copyflag]
! Set to either copy the contents of the data array, or simply share a
! a pointer to the same space. Valid values are {\tt ESMF\_DO\_COPY} or
! {\tt ESMF\_DO\_REF}. (TODO: check to see if these are the right names)
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:


! local vars
        type (ESMF_Array) :: array ! new C++ Array
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

! TODO: need a null pointer to assign to initialize ptr
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        array%this = ESMF_NULL_POINTER

! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        call c_ESMC_ArrayCreate(array, rank, type, kind, &
                                lbounds, ubounds, strides, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array construction error"
          return
        endif

! set return values
        ESMF_ArrayCreateNewBuffer = array
        if (rcpresent) rc = ESMF_SUCCESS

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
! Create a new Array and set the data values.
!
! The return value is a new Array.
!
! The arguments are:
! \begin{description}
!
! \item[spec]
! ArraySpec object.
!
! \item[bufaddr]
! A pointer to the start of the contents of the Array.
!
! \item[copyflag]
! Set to either copy the contents of the data buffer, or set a reference
! and assume ownership of the buffer. It must be able to be deallocated
! by the ESMF. Valid values are {\tt ESMF\_DO\_COPY} or
! {\tt ESMF\_NO\_COPY}.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:


! local vars
        type (ESMF_Array) :: a ! new array object
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

! initialize pointer
        status = ESMF_FAILURE
        rcpresent = .FALSE.
! nullify(a)

! initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! allocate space for Array object and call Construct method to initalize
! allocate(a, stat=status)
! if (status .ne. 0) then ! this is a fortran rc, NOT an ESMF rc
! print *, "Array allocation error"
! return
! endif


! set return values
        ESMF_ArrayCreateBySpec = a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ArrayCreateBySpec

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateBySpecNoData -- Create a new Array from a spec

! !INTERFACE:
      function ESMF_ArrayCreateBySpecNoData(as, counts, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateBySpecNoData
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: as
      integer, intent(in), dimension(:) :: counts
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new Array and allocate space.
!
! The return value is a new Array.
!
! The arguments are:
! \begin{description}
!
! \item[as]
! ArraySpec object.
!
! \item[counts]
! Count of items in each dimension. Must be the same length as the
! rank in the {\tt ArraySpec}.
!
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        type (ESMF_Array) :: a ! new array object
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

        ! Initialize pointer
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        a%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Call proper create routine
        select case (as%rank)
          case (1)
            select case (as%type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               a = ESMF_ArrayCreateBySpecI41D(counts, rc)
              case (ESMF_DATA_REAL%dtype)
               a = ESMF_ArrayCreateBySpecR41D(counts, rc)
              case default
               print *, "unsupported type"
            end select
          case (2)
            select case (as%type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               a = ESMF_ArrayCreateBySpecI42D(counts, rc)
              case (ESMF_DATA_REAL%dtype)
               a = ESMF_ArrayCreateBySpecR42D(counts, rc)
              case default
               print *, "unsupported type"
            end select
          case (3)
            select case (as%type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               a = ESMF_ArrayCreateBySpecI43D(counts, rc)
              case (ESMF_DATA_REAL%dtype)
               a = ESMF_ArrayCreateBySpecR43D(counts, rc)
              case default
               print *, "unsupported type"
            end select
          case (4)
            select case (as%type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               a = ESMF_ArrayCreateBySpecI44D(counts, rc)
              case (ESMF_DATA_REAL%dtype)
               a = ESMF_ArrayCreateBySpecR44D(counts, rc)
              case default
               print *, "unsupported type"
            end select
          case (5)
            select case (as%type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               a = ESMF_ArrayCreateBySpecI45D(counts, rc)
              case (ESMF_DATA_REAL%dtype)
               a = ESMF_ArrayCreateBySpecR45D(counts, rc)
              case default
               print *, "unsupported type"
            end select
          case default
           print *, "unsupported rank"
        end select

        ! Set return values
        ESMF_ArrayCreateBySpecNoData = a
        if (rcpresent) rc = ESMF_SUCCESS

        end function ESMF_ArrayCreateBySpecNoData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI21D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI21D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI21D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 1 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I21Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI21D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI41D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 1 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I41Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI41D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI81D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 1 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I81Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI81D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI22D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI22D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI22D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 2 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I22Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI22D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI22D 
 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI42D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 2 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I42Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI42D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI82D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 2 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I82Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI82D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI23D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI23D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI23D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 3 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I23Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI23D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI43D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI43D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI43D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 3 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I43Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI43D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI83D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI83D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI83D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 3 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I83Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI83D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI24D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI24D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI24D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 4 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I24Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI24D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI44D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI44D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI44D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 4 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I44Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI44D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI84D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI84D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI84D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 4 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I84Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI84D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI25D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI25D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI25D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 5 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I25Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI25D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI45D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI45D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI45D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 5 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I45Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI45D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrI85D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrI85D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI85D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 5 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I85Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrI85D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrI85D 
 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR41D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 1 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R41Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR41D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR81D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 1 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R81Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR81D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR42D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 2 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R42Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR42D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR82D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 2 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
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
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R82Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR82D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrR43D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrR43D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR43D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 3 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R43Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR43D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrR83D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrR83D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR83D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 3 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R83Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR83D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrR44D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrR44D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR44D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 4 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R44Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR44D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrR84D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrR84D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR84D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 4 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R84Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR84D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrR45D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrR45D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR45D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 5 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R45Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR45D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByPtrR85D - make an ESMF array from an F90 ptr 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByPtrR85D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByPtrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DO\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR85D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 do i=1, 5 
 lengths(i) = size(f90ptr, i) 
 enddo 
 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 if (copyreq) then 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
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
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R85Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByPtrR85D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateByPtrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 




!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI21D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI21D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI21D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI21D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I21Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI21D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI41D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI41D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI41D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI41D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I41Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI41D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI81D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI81D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI81D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI81D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I81Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI81D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI22D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI22D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI22D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI22D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I22Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI22D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI42D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI42D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI42D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI42D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I42Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI42D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI82D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI82D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI82D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI82D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I82Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI82D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI23D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI23D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI23D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI23D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I23Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI23D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI43D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI43D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI43D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI43D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I43Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI43D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI83D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI83D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI83D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI83D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I83Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI83D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI24D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI24D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI24D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI24D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I24Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI24D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI44D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI44D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI44D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI44D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I44Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI44D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI84D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI84D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI84D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI84D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I84Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI84D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI25D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI25D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI25D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI25D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I2, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I25Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI25D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI45D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI45D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI45D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI45D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I4, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I45Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI45D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecI85D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecI85D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecI85D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI85D) :: wrap ! for passing f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_integer, ESMF_KIND_I8, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% I85Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecI85D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR41D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR41D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR41D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR41D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R41Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR41D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR81D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR81D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR81D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR81D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R81Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR81D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR42D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR42D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR42D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR42D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R42Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR42D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR82D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR82D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR82D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR82D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R82Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR82D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR43D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR43D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR43D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR43D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R43Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR43D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR83D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR83D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR83D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR83D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R83Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR83D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR44D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR44D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR44D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR44D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R44Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR44D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR84D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR84D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR84D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR84D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R84Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR84D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR45D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR45D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR45D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR45D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R4, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R45Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR45D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateBySpecR85D - make an ESMF array from a Spec 
 
! !INTERFACE: 
 function ESMF_ArrayCreateBySpecR85D(lengths, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateBySpecR85D 
! 
! !ARGUMENTS: 
 integer, dimension(:), intent(in) :: lengths 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on a spec and counts. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[lengths] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! local variables 
 type (ESMF_Array) :: array ! what C++ is going to return 
 integer :: i ! local variable 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR85D) :: wrap ! for passing f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
! ! call create routine 
 call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_real, ESMF_KIND_R8, & 
 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 call c_ESMC_ArraySetDealloc(array, status) 
 
 
! ! set base address 
 call c_ESMC_ArraySetBaseAddr(array, localp( 1,1,1,1,1 ), status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array base address construction error" 
 return 
 endif 
 
! ! save an (uninterpreted) copy of the f90 array information 
 wrap% R85Dptr => localp 
 call c_ESMC_ArraySetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal info save error" 
 return 
 endif 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateBySpecR85D = array 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end function ESMF_ArrayCreateBySpecR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI21D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI21D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I21Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I21Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


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
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI41D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I41Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I41Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI81D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I81Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I81Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI22D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI22D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I22Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I22Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI22D 
 
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
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI42D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I42Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I42Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI82D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I82Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I82Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI23D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI23D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I23Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I23Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI43D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI43D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I43Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I43Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI83D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI83D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I83Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I83Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI24D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI24D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I24Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I24Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI44D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI44D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I44Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I44Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI84D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI84D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I84Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I84Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI25D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI25D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I25Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I25Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI45D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI45D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I45Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I45Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataI85D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapI85D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I85Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I85Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataI85D 
 
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
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR41D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R41Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R41Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR81D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(1) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R81Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R81Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR42D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R42Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R42Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
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
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR82D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(2) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R82Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R82Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataR43D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR43D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R43Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R43Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataR83D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR83D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(3) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R83Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R83Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataR44D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR44D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R44Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R44Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataR84D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR84D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(4) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R84Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R84Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataR45D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR45D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R45Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R45Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayGetDataR85D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return an F90 pointer to the data buffer, or return an F90 pointer 
! to a new copy of the data. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: copyreq ! did user specify copy? 
 
 type (ESMF_ArrWrapR85D) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lengths(5) ! size info for the array 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
 ! initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 copyreq = .FALSE. 
 
 ! check copyflag to see if we are making a reference 
 ! or making a new array and a copy 
 if (present(docopy)) then 
 if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, lengths, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lengths(1), lengths(2), lengths(3), lengths(4), lengths(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R85Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R85Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetDataR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI21D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI21D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I21Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


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
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I41Dptr) 
 
 if (present(rc)) rc = status 
 
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
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I81Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI22D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI22D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I22Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI22D 
 
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
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I42Dptr) 
 
 if (present(rc)) rc = status 
 
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
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I82Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI23D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI23D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I23Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI43D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI43D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I43Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI83D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI83D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I83Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI24D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI24D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I24Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI44D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI44D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I44Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI84D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI84D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I84Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI25D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI25D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I25Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI45D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI45D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I45Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateI85D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapI85D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I85Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateI85D 
 
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
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R41Dptr) 
 
 if (present(rc)) rc = status 
 
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
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R81Dptr) 
 
 if (present(rc)) rc = status 
 
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
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R42Dptr) 
 
 if (present(rc)) rc = status 
 
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
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R82Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateR43D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapR43D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R43Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateR83D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapR83D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R83Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateR44D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapR44D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R44Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateR84D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapR84D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R84Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateR45D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapR45D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R45Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocateR85D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrapR85D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R85Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!! < end of automatically generated function >

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
! the requested sizes.
!
! The function return is an ESMF\_Array type.
!
! The arguments are:
! \begin{description}
! \item[f90ptr]
! A Fortran 90 array pointer which can be queried for info about
! type/kind/rank. This routine will allocate data space so that when
! it returns the pointer will be associated and the memory can be
! filled with values.
!
! \item[ni]
! Number of elements in the first dimension.
!
! \item[nj]
! Number of elements in the second dimension.
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
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

! TODO: need a null pointer to assign to initialize ptr
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        array%this = ESMF_NULL_POINTER

! initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! set up callback
        call c_ESMC_StoreAllocFunc(ESMF_Allocate2DR4, status);

! call create routine
        !call c_ESMC_ArrayCreateMTPtr2D(array, ni, nj, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array initial construction error"
          return
        endif

        call ESMF_Allocate(f90ptr, ni, nj, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array allocate construction error"
          return
        endif

! set base address
        call c_ESMC_ArraySetBaseAddr(array, f90ptr(1,1), status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array base address construction error"
          return
        endif


! return value set by c_ESMC func above
        ESMF_ArrayCreateMTPtr2DR8 = array
        if (rcpresent) rc = ESMF_SUCCESS

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
! Return an F90 pointer to the data buffer, or return an F90 pointer
! to a new copy of the data.
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

! ! TODO: call the c interfaces to get the rank, type, kind, and then
     ! call the function with the right wrapper type to get it deallocated.
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
! Releases all resources associated with this {\tt Array}.
!
! The arguments are:
! \begin{description}
!
! \item[array]
! Destroy contents of this {\tt Array}.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:

! local vars
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        logical :: needsdealloc ! do we need to free space?

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        needsdealloc = .FALSE.

! ! TODO: document the current rule - if we did the allocate in
! ! the case of ESMF_DO_COPY at create time, then we delete the
! ! space. otherwise, the user needs to destroy the array first
! ! (we will ignore the data) and then call deallocate themselves.

! call Destruct first, then free this memory
        call c_ESMC_ArrayNeedsDealloc(array, needsdealloc, status)
        if (needsdealloc) then
          call ESMF_ArrayDeallocateData(array, status)
          if (status .ne. ESMF_SUCCESS) then
            print *, "Array contents destruction error"
            return
          endif
        endif

        call c_ESMC_ArrayDestroy(array, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array destruction error"
          return
        endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

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
! Used only with the version of ArrayCreate which creates an empty
! Array and allows the Data to be specified later. Otherwise it is an
! error to replace the data contents associated with a Array.
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
      type(ESMF_AxisIndex), intent(in) :: indexlist(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to annotate an Array with information used to map local to global
! indicies.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        ! call c routine to add index
        call c_ESMC_ArraySetAxisIndex(array, indexlist, rc)

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
      type(ESMF_AxisIndex), intent(out) :: indexlist(:)
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to retrieve the index annotation from an Array.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        ! call c routine to query index
        call c_ESMC_ArrayGetAxisIndex(array, indexlist, rc)

        end subroutine ESMF_ArrayGetAxisIndex

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRedist(array, layout, rank_trans, olddecompids, &
                                  decompids, redistarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: rank_trans
      integer, dimension(:), intent(in) :: olddecompids
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_Array), intent(in) :: redistarray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to redistribute an Array.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: size_rank_trans
        integer :: size_decomp

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! call c routine to query index
        size_rank_trans = size(rank_trans)
        size_decomp = size(decompids)
        call c_ESMC_ArrayRedist(array, layout, rank_trans, size_rank_trans, &
                                olddecompids, decompids, size_decomp, &
                                redistarray, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayRedist returned error"
          return
        endif

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayRedist

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayHalo(array, layout, decompids, AI_exc, AI_tot, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to halo an Array.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: size_decomp, size_AI
        integer :: i

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! subtract one from location parts of indices to translate to C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l - 1
          AI_exc(i)%r = AI_exc(i)%r - 1
          AI_tot(i)%l = AI_tot(i)%l - 1
          AI_tot(i)%r = AI_tot(i)%r - 1
        enddo

! call c routine to halo
        size_decomp = size(decompids)
        call c_ESMC_ArrayHalo(array, layout, decompids, size_decomp, &
                              AI_exc, AI_tot, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayHalo returned error"
          return
        endif

! add one back to location parts of indices to translate from C++
        size_AI = size(AI_tot)
        do i = 1,size_AI
          AI_exc(i)%l = AI_exc(i)%l + 1
          AI_exc(i)%r = AI_exc(i)%r + 1
          AI_tot(i)%l = AI_tot(i)%l + 1
          AI_tot(i)%r = AI_tot(i)%r + 1
        enddo

! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayHalo

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
! Used to alter the local memory ordering (layout) of this Array.
!
! !TODO: remove this note before generating user documentation
!
! (i am not sure this makes sense now, or that the routine should be
! in this class. but i am leaving this here as a reminder that we
! might need some low level reorder functions. maybe the argument
! should be another array or an arrayspec which describes what you
! want, and the input array is what exists, and this routine can then
! make one into the other. is this a type of create? or is this
! a copy?)
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
     subroutine ESMF_ArraySpecInit(as, rank, type, kind, rc)
                                   !counts, lbounds, ubounds, strides, rc)
!
!
! !ARGUMENTS:
     type(ESMF_ArraySpec), intent(inout) :: as
     integer, intent(in) :: rank
     type(ESMF_DataType), intent(in) :: type
     type(ESMF_DataKind), intent(in) :: kind
     !integer, dimension(:), intent(in), optional :: counts
     !integer, dimension(:), intent(in), optional :: lbounds
     !integer, dimension(:), intent(in), optional :: ubounds
     !integer, dimension(:), intent(in), optional :: strides
     integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Creates a description of the data -- the type, the dimensionality, etc.
! This specification (basically an empty Array), can be
! used in an ArrayCreate call with data to create a full Array.
!
! The arguments are:
! \begin{description}
!
! \item[arrayspec]
! Uninitialized array spec.
!
! \item[rank]
! Array rank (dimensionality, 1D, 2D, etc). Maximum allowed is 5D.
!
! \item[type]
! Array type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
!
! \item[kind]
! Array kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
!
!% \item[counts]
!% The size of each dimension in the Array. This is a 1D integer array
!% the same length as the rank.
!%
!% \item[[lbounds]]
!% The lower bounds for valid indices in the array. This is a 1D
!% integer array the same length as the rank. If not specified
!% the default values are 1 for each dimension.
!%
!% \item[[ubounds]]
!% The upper bounds for valid indices in the array. This is a 1D
!% integer array the same length as the rank. If not specified
!% the default values are same as the count in each dimension.
!%
!% \item[[strides]]
!% The strides for each rank of the array. This is a 1D
!% integer array the same length as the rank. If not specified
!% the default values are the same as the default Fortran array strides.
!%
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

        ! Initialize pointer
        status = ESMF_FAILURE
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Set arrayspec contents

        as%rank = rank
        as%type = type
        as%kind = kind

       ! do i=1,rank
       ! as%counts(i) = counts(i)
       ! !as%rinfo(i, 1) = lbounds(i)
       ! !as%rinfo(i, 2) = ubounds(i)
       ! !as%rinfo(i, 3) = strides(i)
       ! enddo

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArraySpecInit



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! Query for information from the array.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayGet(array, rank, type, kind, lengths, &
                               lbounds, ubounds, strides, base, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      integer, intent(out), optional :: rank
      type(ESMF_DataType), intent(out), optional :: type
      type(ESMF_DataKind), intent(out), optional :: kind
      integer, dimension(:), intent(out), optional :: lengths
      integer, dimension(:), intent(out), optional :: lbounds
      integer, dimension(:), intent(out), optional :: ubounds
      integer, dimension(:), intent(out), optional :: strides
      type(ESMF_Pointer), intent(out), optional :: base
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Returns information about the array. For queries where the caller
! only wants a single value, specify the argument by name.
! All the arguments after the array input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:


      integer :: status ! Error status
      logical :: rcpresent ! Return code present
      integer :: lrank ! Local use to get rank

      ! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif


      if (present(rank)) then
         call c_ESMC_ArrayGetRank(array, rank, status)
         ! TODO: test status
      endif

      if (present(type)) then
         call c_ESMC_ArrayGetType(array, type, status)
      endif

      if (present(kind)) then
         call c_ESMC_ArrayGetKind(array, kind, status)
      endif

      if (present(lengths)) then
         call c_ESMC_ArrayGetRank(array, lrank, status)
         call c_ESMC_ArrayGetLengths(array, lrank, lengths, status)
      endif


      ! TODO: add these methods
      !integer, dimension(:), intent(out), optional :: lbounds
      !integer, dimension(:), intent(out), optional :: ubounds
      !integer, dimension(:), intent(out), optional :: strides
      !type(ESMF_Pointer), intent(out), optional :: base

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayGetName - Retrieve the name of a Array
!
! !INTERFACE:
      subroutine ESMF_ArrayGetName(array, name, rc)

!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      character (len = *), intent(out) :: name
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
! Returns the name of the array. If the array was created without
! specifying a name, the framework will have assigned it a unique one.
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status ! Error status
      logical :: rcpresent ! Return code present

! Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      ! TODO: add an interface to the C code here
      !call c_ESMC_ArrayGetName(array, name, status)
      !if(status .NE. ESMF_FAILURE) then
      ! print *, "ERROR in ESMF_ArrayGetName"
      ! return
      !endif

      name = "default array name"

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayGetName


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArraySpecGet(as, rank, type, kind, rc)
                                          !counts, lbounds, ubounds, strides, rc)
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: as
      integer, intent(out), optional :: rank
      type(ESMF_DataType), intent(out), optional :: type
      type(ESMF_DataKind), intent(out), optional :: kind
      !integer, dimension(:), intent(in), optional :: counts
      !integer, dimension(:), intent(in), optional :: lbounds
      !integer, dimension(:), intent(in), optional :: ubounds
      !integer, dimension(:), intent(in), optional :: strides
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Return information about the contents of a ArraySpec type.
!
! The arguments are:
! \begin{description}
!
! \item[as]
! An {\tt ArraySpec} object.
!
! \item[rank]
! Array rank (dimensionality, 1D, 2D, etc). Maximum allowed is 5D.
!
! \item[type]
! Array type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
!
! \item[kind]
! Array kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
!
!% \item[counts]
!% The size of each dimension in the Array. This is a 1D integer array
!% the same length as the rank.
!%
!% \item[[lbounds]]
!% The lower bounds for valid indices in the array. This is a 1D
!% integer array the same length as the rank. If not specified
!% the default values are 1 for each dimension.
!%
!% \item[[ubounds]]
!% The upper bounds for valid indices in the array. This is a 1D
!% integer array the same length as the rank. If not specified
!% the default values are same as the count in each dimension.
!%
!% \item[[strides]]
!% The strides for each rank of the array. This is a 1D
!% integer array the same length as the rank. If not specified
!% the default values are the same as the default Fortran array strides.
!%
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP

! local vars
        integer :: i
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

! initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

! get arrayspec contents

        if(present(rank)) rank = as%rank
        if(present(type)) type = as%type
        if(present(kind)) kind = as%kind

       ! do i=1,rank
       ! as%counts(i) = counts(i)
       ! !as%rinfo(i, 1) = lbounds(i)
       ! !as%rinfo(i, 2) = ubounds(i)
       ! !as%rinfo(i, 3) = strides(i)
       ! enddo

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArraySpecGet


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
! Used to save all data to disk as quickly as possible.
! (see Read/Write for other options). Internally this routine uses the
! same I/O interface as Read/Write, but the default options are to
! select the fastest way to save data to disk.
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
      character (len = *), intent(in) :: name ! array name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec ! file specs
      integer, intent(out), optional :: rc ! return code
!
! !DESCRIPTION:
! Used to reinitialize
! all data associated with a Array from the last call to Checkpoint.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_Array) :: a

! this is just to shut the compiler up
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
! Used to write data to persistent storage in a variety of formats.
! (see Checkpoint/Restore for quick data dumps.) Details of I/O
! options specified in the IOSpec derived type.
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
      character (len = *), intent(in) :: name ! array name to read
      type(ESMF_IOSpec), intent(in), optional :: iospec ! file specs
      integer, intent(out), optional :: rc ! return code
!
! !DESCRIPTION:
! Used to read data from persistent storage in a variety of formats.
!
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_Array) :: a

! this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER

!
! TODO: add code here
!

        ESMF_ArrayRead = a

        end function ESMF_ArrayRead


!------------------------------------------------------------------------------
!BOP
! !IROUTINE - ESMF_ArrayValidate - Check validity of Array object
!
! !INTERFACE:
      subroutine ESMF_ArrayValidate(array, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Routine to print information about a array.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts ! default print options
       integer :: status ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

       defaultopts = "brief"

       ! Simple validity checks
       if (array%this .eq. ESMF_NULL_POINTER) then
           print *, "Array not initialized or Destroyed"
           return
       endif

       if(present(options)) then
           !call c_ESMC_ArrayValidate(array, options, status)
       else
           !call c_ESMC_ArrayValidate(array, defaultopts, status)
       endif

       !if (status .ne. ESMF_SUCCESS) then
       ! print *, "Array validate error"
       ! return
       !endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_ArrayValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayPrint - Print contents of an Array object
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
! Routine to print information about a array.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts ! default print options
       integer :: status ! local error status
       logical :: rcpresent

       ! Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

       defaultopts = "brief"

       if(present(options)) then
           call c_ESMC_ArrayPrint(array, options, status)
       else
           call c_ESMC_ArrayPrint(array, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Array print error"
         return
       endif

! set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_ArrayPrint


        end module ESMF_ArrayMod
