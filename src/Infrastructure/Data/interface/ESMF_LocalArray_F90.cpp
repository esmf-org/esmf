! $Id: ESMF_LocalArray_F90.cpp,v 1.1 2003/07/10 18:51:27 nscollins Exp $
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
! < ignore blank lines below.  they are created by the files which
!   define various macros. >
#include "ESMF_ArrayMacros.h"
#include "ESMF_AllocMacros.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_ArrayMod - Manage data arrays uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_Array} class and 
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
      use ESMF_DELayoutMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
!     ! ESMF_CopyFlag
!
!     ! Indicates whether a data array should be copied or referenced. 
!     !  This matches an enum on the C++ side and the values must match.
!     !  Update ../include/ESMC_Array.h if you change these values.

      type ESMF_CopyFlag
      sequence
      private
        integer :: docopy
      end type

      type(ESMF_CopyFlag), parameter :: & 
                            ESMF_DATA_COPY  = ESMF_CopyFlag(1), &
                            ESMF_DATA_REF   = ESMF_CopyFlag(2), &
                            ESMF_DATA_SPACE = ESMF_CopyFlag(3)  ! private

!------------------------------------------------------------------------------
!     ! ESMF_ArrayOrigin
!
!     ! Private flag which indicates the create was initiated on the F90 side.
!     !  This matches an enum on the C++ side and the values must match.
!     !  Update ../include/ESMC_Array.h if you change these values.

      type ESMF_ArrayOrigin
      sequence
      private
        integer :: origin
      end type

      type(ESMF_ArrayOrigin), parameter :: & 
                            ESMF_FROM_FORTRAN   = ESMF_ArrayOrigin(1), &
                            ESMF_FROM_CPLUSPLUS = ESMF_ArrayOrigin(2)

!------------------------------------------------------------------------------
!     ! ESMF_DomainType
!
!     ! Indicates whether a data array should be copied or referenced. 
!     !  This matches an enum on the C++ side and the values must match.
!     !  Update ../include/ESMC_Array.h if you change these values.

      type ESMF_DomainType
      sequence
      private
        integer :: dt
      end type

      type(ESMF_DomainType), parameter :: & 
                            ESMF_DOMAIN_LOCAL         = ESMF_DomainType(1), &
                            ESMF_DOMAIN_COMPUTATIONAL = ESMF_DomainType(2), &
                            ESMF_DOMAIN_EXCLUSIVE     = ESMF_DomainType(3)

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

      end type

!------------------------------------------------------------------------------
!     ! ESMF_Array
!
!     ! Array data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_Array
      sequence
      private
        ! opaque pointer to the C++ class data
        !type(ESMF_Pointer) :: this = ESMF_NULL_POINTER
        type(ESMF_Pointer) :: this
      end type

!------------------------------------------------------------------------------
!     ! Internal wrapper structures for passing f90 pointers to C++ and
!     ! guaranteeing they are passed by reference on all compilers and all
!     ! platforms.  These are never seen outside this module.
!
      ! < these expand into defined type declarations >
ArrayAllTypeMacro()


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CopyFlag, ESMF_DATA_COPY, ESMF_DATA_REF
      public ESMF_ArraySpec, ESMF_Array
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArrayCreate
      public ESMF_ArrayDestroy
 
      public ESMF_ArraySpecInit
      public ESMF_ArraySpecGet

      public ESMF_ArraySetData, ESMF_ArrayGetData
      !public ESMF_ArraySetInfo, ESMF_ArrayGetInfo
      public ESMF_ArraySetAxisIndex, ESMF_ArrayGetAxisIndex
      public ESMF_ArrayRedist, ESMF_ArrayHalo
      public ESMF_ArrayAllGather, ESMF_ArrayGather, ESMF_ArrayScatter
      public ESMF_ArrayGet, ESMF_ArrayGetName
 
      public ESMF_ArrayF90Allocate
      public ESMF_ArrayF90Deallocate
      public ESMF_ArrayConstructF90Ptr    ! needed for C++ callback only

      public ESMF_ArrayWriteRestart
      public ESMF_ArrayReadRestart
      public ESMF_ArrayWrite
      public ESMF_ArrayRead
 
      public ESMF_ArrayValidate
      public ESMF_ArrayPrint
!EOP
      public operator(.eq.), operator(.ne.)

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_LocalArray_F90.cpp,v 1.1 2003/07/10 18:51:27 nscollins Exp $'

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
        module procedure ESMF_ArrayCreateByList      ! specify TKR
        module procedure ESMF_ArrayCreateBySpec      ! specify ArraySpec
   
        ! Plus interfaces for each T/K/R 

!EOP
        

!       ! < interfaces for each T/K/R >
! --Array--InterfaceMacro(ArrayCreateByMTArr)
!
!       ! < interfaces for each T/K/R >
! --Array--InterfaceMacro(ArrayCreateByFullArr)

       ! < interfaces for each T/K/R >
ArrayInterfaceMacro(ArrayCreateByMTPtr)

       ! < interfaces for each T/K/R >
ArrayInterfaceMacro(ArrayCreateByFullPtr)


!BOP
! !DESCRIPTION: 
! This interface provides a single (heavily overloaded) entry point for 
!  the various types of {\tt ESMF\_ArrayCreate} functions.   
!
!  There are 3 options for setting the contents of the {\tt ESMF\_Array}
!  at creation time:
!  \begin{description}
!  \item[Allocate Space Only]
!    Data space is allocated but not initialized.  The caller can query
!    for a pointer to the start of the space to address it directly.
!    The caller must not deallocate the space; the
!    {\tt ESMF\_Array} will release the space when it is destroyed.
!  \item[Data Copy]
!    An existing Fortran array is specified and the data contents are copied
!    into new space allocated by the {\tt ESMF\_Array}.
!    The caller must not deallocate the space; the
!    {\tt ESMF\_Array} will release the space when it is destroyed.
!  \item[Data Reference]
!    An existing Fortran array is specified and the data contents reference
!    it directly.  The caller is responsible for deallocating the space;
!    when the {\tt ESMF\_Array} is destroyed it will not release the space.
!  \end{description}
!
!  There are 3 options for 
!  specifying the type/kind/rank of the {\tt ESMF\_Array} data:
!  \begin{description}
!  \item[List]
!    The characteristics of the {\tt ESMF\_Array} are given explicitly
!    by individual arguments to the create function.
!  \item[ArraySpec]
!    A previously created {\tt ESMF\_ArraySpec} object is given which
!    describes the characteristics.
!  %\item[Fortran array]
!  %  An existing Fortran array is used to describe the characteristics.
!  %  (Only available from the Fortran interface.)
!  \item[Fortran 90 Pointer]
!    An associated or unassociated Fortran 90 array pointer is used to 
!    describe the array.
!    (Only available from the Fortran interface.)
!  \end{description}
!  
!  The concept of an ``empty'' {\tt ESMF\_Array} does not exist.  To make an
!  ESMF object which stores the Type/Kind/Rank information create an
!  {\tt ESMF\_ArraySpec} object which can then be used repeatedly in
!  subsequent {\tt ESMF\_Array} Create calls.
!  
end interface
!EOP 

!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_ArrayGetData -- Get an F90 pointer to the data contents

! !INTERFACE:
     interface ESMF_ArrayGetData

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
ArrayInterfaceMacro(ArrayGetData)

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_ArrayGetData} functions.   
!  
!EOP 
end interface

!------------------------------------------------------------------------------
interface operator (.eq.)
 module procedure ESMF_cfeq
end interface
interface operator (.ne.)
 module procedure ESMF_cfne
end interface

!==============================================================================

      contains

!==============================================================================

! functions to compare two ESMF_CopyFlags to see if they are the same or not

function ESMF_cfeq(cf1, cf2)
 logical ESMF_cfeq
 type(ESMF_CopyFlag), intent(in) :: cf1, cf2

 ESMF_cfeq = (cf1%docopy .eq. cf2%docopy) 
end function

function ESMF_cfne(cf1, cf2)
 logical ESMF_cfne
 type(ESMF_CopyFlag), intent(in) :: cf1, cf2

 ESMF_cfne = (cf1%docopy .ne. cf2%docopy) 
end function


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Array Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateByList -- Create an Array specifying all options.

! !INTERFACE:
      function ESMF_ArrayCreateByList(rank, type, kind, counts, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateByList
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_Array and allocate data space, which remains uninitialized.
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
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        type (ESMF_Array) :: array          ! new C++ Array
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

        status = ESMF_FAILURE
        rcpresent = .FALSE.
        array%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! TODO: should this take the counts, or not?  for now i am going to
        !  set the counts after i have created the f90 array and not here.
        call c_ESMC_ArrayCreateNoData(array, rank, type, kind, &
                                                   ESMF_FROM_FORTRAN, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array construction error"
          return
        endif

        call ESMF_ArrayConstructF90Ptr(array, counts, rank, type, kind, status)

        ! Set return values
        ESMF_ArrayCreateByList = array 
        if (rcpresent) rc = status

        end function ESMF_ArrayCreateByList


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateBySpec -- Create a new Array from an ArraySpec

! !INTERFACE:
      function ESMF_ArrayCreateBySpec(spec, counts, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateBySpec
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: spec
      integer, intent(in), dimension(:) :: counts
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_Array} and allocate data space, which remains uninitialized.
!  The return value is a new Array.
!    
!  The arguments are:
!  \begin{description}
!
!  \item[spec]
!    ArraySpec object.
!
!  \item[counts]
!    The number of items in each dimension of the array.  This is a 1D
!    integer array the same length as the rank.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:

        ! Local vars
        type (ESMF_Array) :: array          ! new C++ Array
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?
        integer :: rank
        type(ESMF_DataType) :: type
        type(ESMF_DataKind) :: kind

        status = ESMF_FAILURE
        rcpresent = .FALSE.
        array%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        call ESMF_ArraySpecGet(spec, rank, type, kind, status)
        if (status .ne. ESMF_SUCCESS) return
        
        ! Call the list function to make the array
        ESMF_ArrayCreateBySpec = ESMF_ArrayCreateByList(rank, type, kind, &
                                                            counts, status)
        if (rcpresent) rc = status

        end function ESMF_ArrayCreateBySpec


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayConstructF90Ptr - Create and add F90 ptr to array

! !INTERFACE:
     subroutine ESMF_ArrayConstructF90Ptr(array, counts, rank, type, kind, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Take a partially created {\tt ESMF\_Array} and T/K/R information and call the
!   proper subroutine to create an F90 pointer, allocate space, and set the
!   corresponding values in the {\tt ESMF\_Array} object.
!    
!  The arguments are:
!  \begin{description}
!
!  \item[array]
!    Partially created {\tt ESMF\_Array} object.  This entry point is used
!    during both the C++ and F90 create calls if we need to create an F90
!    pointer to be used later.
!
!  \item[counts]
!    The number of items in each dimension of the array.  This is a 1D
!    integer array the same length as the rank.
!
!  \item[rank]
!    Array rank.
!    This must match what is already in the array - it is here only as
!    a convienience.
!
!  \item[type]
!    Array type.
!    This must match what is already in the array - it is here only as
!    a convienience.
!
!  \item[kind]
!    Array kind. 
!    This must match what is already in the array - it is here only as
!    a convienience.
!
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?

        status = ESMF_FAILURE
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


        ! Call a T/K/R specific interface in order to create the proper
        !  type of F90 pointer, allocate the space, set the values in the
        !  Array object, and return.  (The routine this code is calling is
        !  generated by macro.)

        ! Call proper create F90 ptr routine
        select case (rank)
          case (1)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_ArrayConstructF90PtrI41D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_ArrayConstructF90PtrR41D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case (2)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_ArrayConstructF90PtrI42D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_ArrayConstructF90PtrR42D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case (3)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_ArrayConstructF90PtrI43D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_ArrayConstructF90PtrR43D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case (4)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_ArrayConstructF90PtrI44D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_ArrayConstructF90PtrR44D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case (5)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_ArrayConstructF90PtrI45D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_ArrayConstructF90PtrR45D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case default
           print *, "unsupported rank"
           return
        end select

        ! Set return code if caller specified it
        if (rcpresent) rc = status

        end subroutine ESMF_ArrayConstructF90Ptr

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

ArrayCreateByMTArrMacro(integer, I2, 1, COL1, LEN1, LOC1)

ArrayCreateByMTArrMacro(integer, I4, 1, COL1, LEN1, LOC1)

ArrayCreateByMTArrMacro(integer, I8, 1, COL1, LEN1, LOC1)

ArrayCreateByMTArrMacro(integer, I2, 2, COL2, LEN2, LOC2)

ArrayCreateByMTArrMacro(integer, I4, 2, COL2, LEN2, LOC2)

ArrayCreateByMTArrMacro(integer, I8, 2, COL2, LEN2, LOC2)

ArrayCreateByMTArrMacro(integer, I2, 3, COL3, LEN3, LOC3)

ArrayCreateByMTArrMacro(integer, I4, 3, COL3, LEN3, LOC3)

ArrayCreateByMTArrMacro(integer, I8, 3, COL3, LEN3, LOC3)

ArrayCreateByMTArrMacro(integer, I2, 4, COL4, LEN4, LOC4)

ArrayCreateByMTArrMacro(integer, I4, 4, COL4, LEN4, LOC4)

ArrayCreateByMTArrMacro(integer, I8, 4, COL4, LEN4, LOC4)

ArrayCreateByMTArrMacro(integer, I2, 5, COL5, LEN5, LOC5)

ArrayCreateByMTArrMacro(integer, I4, 5, COL5, LEN5, LOC5)

ArrayCreateByMTArrMacro(integer, I8, 5, COL5, LEN5, LOC5)

ArrayCreateByMTArrMacro(real, R4, 1, COL1, LEN1, LOC1)

ArrayCreateByMTArrMacro(real, R8, 1, COL1, LEN1, LOC1)

ArrayCreateByMTArrMacro(real, R4, 2, COL2, LEN2, LOC2)

ArrayCreateByMTArrMacro(real, R8, 2, COL2, LEN2, LOC2)

ArrayCreateByMTArrMacro(real, R4, 3, COL3, LEN3, LOC3)

ArrayCreateByMTArrMacro(real, R8, 3, COL3, LEN3, LOC3)

ArrayCreateByMTArrMacro(real, R4, 4, COL4, LEN4, LOC4)

ArrayCreateByMTArrMacro(real, R8, 4, COL4, LEN4, LOC4)

ArrayCreateByMTArrMacro(real, R4, 5, COL5, LEN5, LOC5)

ArrayCreateByMTArrMacro(real, R8, 5, COL5, LEN5, LOC5)


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

ArrayCreateByFullArrMacro(integer, I2, 1, COL1, LEN1, LOC1)

ArrayCreateByFullArrMacro(integer, I4, 1, COL1, LEN1, LOC1)

ArrayCreateByFullArrMacro(integer, I8, 1, COL1, LEN1, LOC1)

ArrayCreateByFullArrMacro(integer, I2, 2, COL2, LEN2, LOC2)

ArrayCreateByFullArrMacro(integer, I4, 2, COL2, LEN2, LOC2)

ArrayCreateByFullArrMacro(integer, I8, 2, COL2, LEN2, LOC2)

ArrayCreateByFullArrMacro(integer, I2, 3, COL3, LEN3, LOC3)

ArrayCreateByFullArrMacro(integer, I4, 3, COL3, LEN3, LOC3)

ArrayCreateByFullArrMacro(integer, I8, 3, COL3, LEN3, LOC3)

ArrayCreateByFullArrMacro(integer, I2, 4, COL4, LEN4, LOC4)

ArrayCreateByFullArrMacro(integer, I4, 4, COL4, LEN4, LOC4)

ArrayCreateByFullArrMacro(integer, I8, 4, COL4, LEN4, LOC4)

ArrayCreateByFullArrMacro(integer, I2, 5, COL5, LEN5, LOC5)

ArrayCreateByFullArrMacro(integer, I4, 5, COL5, LEN5, LOC5)

ArrayCreateByFullArrMacro(integer, I8, 5, COL5, LEN5, LOC5)

ArrayCreateByFullArrMacro(real, R4, 1, COL1, LEN1, LOC1)

ArrayCreateByFullArrMacro(real, R8, 1, COL1, LEN1, LOC1)

ArrayCreateByFullArrMacro(real, R4, 2, COL2, LEN2, LOC2)

ArrayCreateByFullArrMacro(real, R8, 2, COL2, LEN2, LOC2)

ArrayCreateByFullArrMacro(real, R4, 3, COL3, LEN3, LOC3)

ArrayCreateByFullArrMacro(real, R8, 3, COL3, LEN3, LOC3)

ArrayCreateByFullArrMacro(real, R4, 4, COL4, LEN4, LOC4)

ArrayCreateByFullArrMacro(real, R8, 4, COL4, LEN4, LOC4)

ArrayCreateByFullArrMacro(real, R4, 5, COL5, LEN5, LOC5)

ArrayCreateByFullArrMacro(real, R8, 5, COL5, LEN5, LOC5)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

ArrayCreateByMTPtrMacro(integer, I2, 1, COL1, LEN1, LOC1)

ArrayCreateByMTPtrMacro(integer, I4, 1, COL1, LEN1, LOC1)

ArrayCreateByMTPtrMacro(integer, I8, 1, COL1, LEN1, LOC1)

ArrayCreateByMTPtrMacro(integer, I2, 2, COL2, LEN2, LOC2)

ArrayCreateByMTPtrMacro(integer, I4, 2, COL2, LEN2, LOC2)

ArrayCreateByMTPtrMacro(integer, I8, 2, COL2, LEN2, LOC2)

ArrayCreateByMTPtrMacro(integer, I2, 3, COL3, LEN3, LOC3)

ArrayCreateByMTPtrMacro(integer, I4, 3, COL3, LEN3, LOC3)

ArrayCreateByMTPtrMacro(integer, I8, 3, COL3, LEN3, LOC3)

ArrayCreateByMTPtrMacro(integer, I2, 4, COL4, LEN4, LOC4)

ArrayCreateByMTPtrMacro(integer, I4, 4, COL4, LEN4, LOC4)

ArrayCreateByMTPtrMacro(integer, I8, 4, COL4, LEN4, LOC4)

ArrayCreateByMTPtrMacro(integer, I2, 5, COL5, LEN5, LOC5)

ArrayCreateByMTPtrMacro(integer, I4, 5, COL5, LEN5, LOC5)

ArrayCreateByMTPtrMacro(integer, I8, 5, COL5, LEN5, LOC5)

ArrayCreateByMTPtrMacro(real, R4, 1, COL1, LEN1, LOC1)

ArrayCreateByMTPtrMacro(real, R8, 1, COL1, LEN1, LOC1)

ArrayCreateByMTPtrMacro(real, R4, 2, COL2, LEN2, LOC2)

ArrayCreateByMTPtrMacro(real, R8, 2, COL2, LEN2, LOC2)

ArrayCreateByMTPtrMacro(real, R4, 3, COL3, LEN3, LOC3)

ArrayCreateByMTPtrMacro(real, R8, 3, COL3, LEN3, LOC3)

ArrayCreateByMTPtrMacro(real, R4, 4, COL4, LEN4, LOC4)

ArrayCreateByMTPtrMacro(real, R8, 4, COL4, LEN4, LOC4)

ArrayCreateByMTPtrMacro(real, R4, 5, COL5, LEN5, LOC5)

ArrayCreateByMTPtrMacro(real, R8, 5, COL5, LEN5, LOC5)



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

ArrayCreateByFullPtrMacro(integer, I2, 1, COL1, LEN1, LOC1)

ArrayCreateByFullPtrMacro(integer, I4, 1, COL1, LEN1, LOC1)

ArrayCreateByFullPtrMacro(integer, I8, 1, COL1, LEN1, LOC1)

ArrayCreateByFullPtrMacro(integer, I2, 2, COL2, LEN2, LOC2)

ArrayCreateByFullPtrMacro(integer, I4, 2, COL2, LEN2, LOC2)

ArrayCreateByFullPtrMacro(integer, I8, 2, COL2, LEN2, LOC2)

ArrayCreateByFullPtrMacro(integer, I2, 3, COL3, LEN3, LOC3)

ArrayCreateByFullPtrMacro(integer, I4, 3, COL3, LEN3, LOC3)

ArrayCreateByFullPtrMacro(integer, I8, 3, COL3, LEN3, LOC3)

ArrayCreateByFullPtrMacro(integer, I2, 4, COL4, LEN4, LOC4)

ArrayCreateByFullPtrMacro(integer, I4, 4, COL4, LEN4, LOC4)

ArrayCreateByFullPtrMacro(integer, I8, 4, COL4, LEN4, LOC4)

ArrayCreateByFullPtrMacro(integer, I2, 5, COL5, LEN5, LOC5)

ArrayCreateByFullPtrMacro(integer, I4, 5, COL5, LEN5, LOC5)

ArrayCreateByFullPtrMacro(integer, I8, 5, COL5, LEN5, LOC5)

ArrayCreateByFullPtrMacro(real, R4, 1, COL1, LEN1, LOC1)

ArrayCreateByFullPtrMacro(real, R8, 1, COL1, LEN1, LOC1)

ArrayCreateByFullPtrMacro(real, R4, 2, COL2, LEN2, LOC2)

ArrayCreateByFullPtrMacro(real, R8, 2, COL2, LEN2, LOC2)

ArrayCreateByFullPtrMacro(real, R4, 3, COL3, LEN3, LOC3)

ArrayCreateByFullPtrMacro(real, R8, 3, COL3, LEN3, LOC3)

ArrayCreateByFullPtrMacro(real, R4, 4, COL4, LEN4, LOC4)

ArrayCreateByFullPtrMacro(real, R8, 4, COL4, LEN4, LOC4)

ArrayCreateByFullPtrMacro(real, R4, 5, COL5, LEN5, LOC5)

ArrayCreateByFullPtrMacro(real, R8, 5, COL5, LEN5, LOC5)


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

ArrayConstructF90PtrMacro(integer, I2, 1, COL1, LEN1, LOC1)

ArrayConstructF90PtrMacro(integer, I4, 1, COL1, LEN1, LOC1)

ArrayConstructF90PtrMacro(integer, I8, 1, COL1, LEN1, LOC1)

ArrayConstructF90PtrMacro(integer, I2, 2, COL2, LEN2, LOC2)

ArrayConstructF90PtrMacro(integer, I4, 2, COL2, LEN2, LOC2)

ArrayConstructF90PtrMacro(integer, I8, 2, COL2, LEN2, LOC2)

ArrayConstructF90PtrMacro(integer, I2, 3, COL3, LEN3, LOC3)

ArrayConstructF90PtrMacro(integer, I4, 3, COL3, LEN3, LOC3)

ArrayConstructF90PtrMacro(integer, I8, 3, COL3, LEN3, LOC3)

ArrayConstructF90PtrMacro(integer, I2, 4, COL4, LEN4, LOC4)

ArrayConstructF90PtrMacro(integer, I4, 4, COL4, LEN4, LOC4)

ArrayConstructF90PtrMacro(integer, I8, 4, COL4, LEN4, LOC4)

ArrayConstructF90PtrMacro(integer, I2, 5, COL5, LEN5, LOC5)

ArrayConstructF90PtrMacro(integer, I4, 5, COL5, LEN5, LOC5)

ArrayConstructF90PtrMacro(integer, I8, 5, COL5, LEN5, LOC5)

ArrayConstructF90PtrMacro(real, R4, 1, COL1, LEN1, LOC1)

ArrayConstructF90PtrMacro(real, R8, 1, COL1, LEN1, LOC1)

ArrayConstructF90PtrMacro(real, R4, 2, COL2, LEN2, LOC2)

ArrayConstructF90PtrMacro(real, R8, 2, COL2, LEN2, LOC2)

ArrayConstructF90PtrMacro(real, R4, 3, COL3, LEN3, LOC3)

ArrayConstructF90PtrMacro(real, R8, 3, COL3, LEN3, LOC3)

ArrayConstructF90PtrMacro(real, R4, 4, COL4, LEN4, LOC4)

ArrayConstructF90PtrMacro(real, R8, 4, COL4, LEN4, LOC4)

ArrayConstructF90PtrMacro(real, R4, 5, COL5, LEN5, LOC5)

ArrayConstructF90PtrMacro(real, R8, 5, COL5, LEN5, LOC5)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

ArrayGetDataMacro(integer, I2, 1, COL1, LEN1, LOC1)

ArrayGetDataMacro(integer, I4, 1, COL1, LEN1, LOC1)

ArrayGetDataMacro(integer, I8, 1, COL1, LEN1, LOC1)

ArrayGetDataMacro(integer, I2, 2, COL2, LEN2, LOC2)

ArrayGetDataMacro(integer, I4, 2, COL2, LEN2, LOC2)

ArrayGetDataMacro(integer, I8, 2, COL2, LEN2, LOC2)

ArrayGetDataMacro(integer, I2, 3, COL3, LEN3, LOC3)

ArrayGetDataMacro(integer, I4, 3, COL3, LEN3, LOC3)

ArrayGetDataMacro(integer, I8, 3, COL3, LEN3, LOC3)

ArrayGetDataMacro(integer, I2, 4, COL4, LEN4, LOC4)

ArrayGetDataMacro(integer, I4, 4, COL4, LEN4, LOC4)

ArrayGetDataMacro(integer, I8, 4, COL4, LEN4, LOC4)

ArrayGetDataMacro(integer, I2, 5, COL5, LEN5, LOC5)

ArrayGetDataMacro(integer, I4, 5, COL5, LEN5, LOC5)

ArrayGetDataMacro(integer, I8, 5, COL5, LEN5, LOC5)

ArrayGetDataMacro(real, R4, 1, COL1, LEN1, LOC1)

ArrayGetDataMacro(real, R8, 1, COL1, LEN1, LOC1)

ArrayGetDataMacro(real, R4, 2, COL2, LEN2, LOC2)

ArrayGetDataMacro(real, R8, 2, COL2, LEN2, LOC2)

ArrayGetDataMacro(real, R4, 3, COL3, LEN3, LOC3)

ArrayGetDataMacro(real, R8, 3, COL3, LEN3, LOC3)

ArrayGetDataMacro(real, R4, 4, COL4, LEN4, LOC4)

ArrayGetDataMacro(real, R8, 4, COL4, LEN4, LOC4)

ArrayGetDataMacro(real, R4, 5, COL5, LEN5, LOC5)

ArrayGetDataMacro(real, R8, 5, COL5, LEN5, LOC5)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
      
ArrayDeallocateMacro(integer, I2, 1, COL1, LEN1, LOC1)

ArrayDeallocateMacro(integer, I4, 1, COL1, LEN1, LOC1)

ArrayDeallocateMacro(integer, I8, 1, COL1, LEN1, LOC1)

ArrayDeallocateMacro(integer, I2, 2, COL2, LEN2, LOC2)

ArrayDeallocateMacro(integer, I4, 2, COL2, LEN2, LOC2)

ArrayDeallocateMacro(integer, I8, 2, COL2, LEN2, LOC2)

ArrayDeallocateMacro(integer, I2, 3, COL3, LEN3, LOC3)

ArrayDeallocateMacro(integer, I4, 3, COL3, LEN3, LOC3)

ArrayDeallocateMacro(integer, I8, 3, COL3, LEN3, LOC3)

ArrayDeallocateMacro(integer, I2, 4, COL4, LEN4, LOC4)

ArrayDeallocateMacro(integer, I4, 4, COL4, LEN4, LOC4)

ArrayDeallocateMacro(integer, I8, 4, COL4, LEN4, LOC4)

ArrayDeallocateMacro(integer, I2, 5, COL5, LEN5, LOC5)

ArrayDeallocateMacro(integer, I4, 5, COL5, LEN5, LOC5)

ArrayDeallocateMacro(integer, I8, 5, COL5, LEN5, LOC5)

ArrayDeallocateMacro(real, R4, 1, COL1, LEN1, LOC1)

ArrayDeallocateMacro(real, R8, 1, COL1, LEN1, LOC1)

ArrayDeallocateMacro(real, R4, 2, COL2, LEN2, LOC2)

ArrayDeallocateMacro(real, R8, 2, COL2, LEN2, LOC2)

ArrayDeallocateMacro(real, R4, 3, COL3, LEN3, LOC3)

ArrayDeallocateMacro(real, R8, 3, COL3, LEN3, LOC3)

ArrayDeallocateMacro(real, R4, 4, COL4, LEN4, LOC4)

ArrayDeallocateMacro(real, R8, 4, COL4, LEN4, LOC4)

ArrayDeallocateMacro(real, R4, 5, COL5, LEN5, LOC5)

ArrayDeallocateMacro(real, R8, 5, COL5, LEN5, LOC5)

!! < end of automatically generated function >

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayDestroy(array, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_Array}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[array]
!       Destroy contents of this {\tt ESMF\_Array}.
!
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!     \end{description}
!
!  To reduce the depth of crossings of the F90/C++ boundary we first
!   query to see if we are responsible for deleting the data space.  If so,
!   first deallocate the space and then call the C++ code to release
!   the object space.  When it returns we are done and can return to the user.
!   Otherwise we would need to make a nested call back into F90 from C++ to do
!   the deallocate() during the object delete.
!
!EOP
! !REQUIREMENTS:

        ! Local vars
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?
        logical :: needsdealloc             ! do we need to free space?
        integer :: rank
        type(ESMF_DataType) :: type
        type(ESMF_DataKind) :: kind

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        needsdealloc = .FALSE.

        ! TODO: document the current rule - if we do the allocate in
        !   the case of ESMF_DATA_COPY at create time then we delete the
        !   space.  otherwise, the user needs to destroy the array 
        !   (we will ignore the data) and call deallocate themselves.

        ! Call Destruct first, then free this memory
        call c_ESMC_ArrayNeedsDealloc(array, needsdealloc, status)
        if (needsdealloc) then
          call c_ESMC_ArrayGetRank(array, rank, status)
          call c_ESMC_ArrayGetType(array, type, status)
          call c_ESMC_ArrayGetKind(array, kind, status)
          call ESMF_ArrayF90Deallocate(array, rank, type, kind, status)
          if (status .ne. ESMF_SUCCESS) then
            print *, "Array contents destruction error"
            return
          endif
          call c_ESMC_ArraySetNoDealloc(array, status)
        endif

        ! Calling deallocate first means this will not return back to F90
        !  before returning for good.
        call c_ESMC_ArrayDestroy(array, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array destruction error"
          return
        endif

!       set return code if user specified it
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
      type(ESMF_Array), intent(inout) :: array 
      type(ESMF_ArraySpec), intent(in) :: dataspec
      real, dimension (:), pointer :: databuf    
      type(ESMF_CopyFlag), intent(in) :: docopy 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of {\tt ESMF\_ArrayCreate} which creates an empty 
!      {\tt ESMF\_Array} and allows the Data to be specified later.  Otherwise it is an 
!      error to replace the data contents associated with a {\tt ESMF\_Array}.  
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
      subroutine ESMF_ArraySetAxisIndex(array, domain, indexlist, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array 
      type(ESMF_DomainType), intent(in) :: domain
      type(ESMF_AxisIndex), intent(in) :: indexlist(:)
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to annotate an {\tt ESMF\_Array} with information used to map local to global
!      indicies.  
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        ! call c routine to add index
        call c_ESMC_ArraySetAxisIndex(array, domain, indexlist, rc)

        end subroutine ESMF_ArraySetAxisIndex

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayGetAxisIndex
!
! !INTERFACE:
      subroutine ESMF_ArrayGetAxisIndex(array, domain, indexlist, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array 
      type(ESMF_DomainType), intent(in) :: domain
      type(ESMF_AxisIndex), intent(out) :: indexlist(:)
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to retrieve the index annotation from an {\tt ESMF\_Array}.
!
!EOP
! !REQUIREMENTS:

        ! call c routine to query index
        call c_ESMC_ArrayGetAxisIndex(array, domain, indexlist, rc)

        end subroutine ESMF_ArrayGetAxisIndex

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayRedist(array, layout, rank_trans, olddecompids, &
                                  decompids, redistarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: rank_trans
      integer, dimension(:), intent(in) :: olddecompids
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_Array), intent(in) :: redistarray
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to redistribute an {\tt ESMF\_Array}.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
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
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to halo an {\tt ESMF\_Array}.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
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
      subroutine ESMF_ArrayAllGather(array, layout, decompids, &
                                     AI_exc, AI_tot, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed {\tt ESMF\_Array} into a global {\tt ESMF\_Array} on all {\tt ESMF\_DE}s.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
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

! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayAllGather(array, layout, decompids, size_decomp, &
                                   AI_exc, AI_tot, array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayAllGather returned error"
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

        end subroutine ESMF_ArrayAllGather

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayGather(array, layout, decompids, &
                                     AI_exc, AI_tot, deid, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      integer, intent(in) :: deid
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed {\tt ESMF\_Array} into a global {\tt ESMF\_Array} on all {\tt ESMF\_DE}s.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
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

! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayGather(array, layout, decompids, size_decomp, &
                                   AI_exc, AI_tot, deid, array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayGather returned error"
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

        end subroutine ESMF_ArrayGather

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayScatter(array, layout, decompids, &
                                     AI_exc, AI_tot, deid, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      integer, intent(in) :: deid
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to scatter a single {\tt ESMF\_Array} into a distributed {\tt ESMF\_Array} across all {\tt ESMF\_DE}s.
!
!
!EOP
! !REQUIREMENTS:
        integer :: status         ! local error status
        logical :: rcpresent      ! did user specify rc?
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

! call c routine to allgather
        size_decomp = size(decompids)
        call c_ESMC_ArrayScatter(array, layout, decompids, size_decomp, &
                                   AI_exc, AI_tot, deid, array_out, status)

        if (status .ne. ESMF_SUCCESS) then
          print *, "c_ESMC_ArrayScatter returned error"
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

        end subroutine ESMF_ArrayScatter

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayReorder(array, newarrayspec, newarray, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array 
      type(ESMF_ArraySpec), intent(in) :: newarrayspec
      type(ESMF_Array), intent(out):: newarray   
      integer, intent(out), optional :: rc       
!
! !DESCRIPTION:
!      Used to alter the local memory ordering (layout) of this {\tt ESMF\_Array}.
!
!  !TODO: remove this note before generating user documentation
!
!      (i am not sure this makes sense now, or that the routine should be
!      in this class.  but i am leaving this here as a reminder that we
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
     subroutine ESMF_ArraySpecInit(as, rank, type, kind, rc)
!
!
! !ARGUMENTS:
     type(ESMF_ArraySpec), intent(inout) :: as
     integer, intent(in) :: rank
     type(ESMF_DataType), intent(in) :: type
     type(ESMF_DataKind), intent(in) :: kind
     integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!  Creates a description of the data -- the type, the dimensionality, etc.  
!  This specification can be
!  used in an {\tt ESMF\_ArrayCreate} call with data to create a full {\tt ESMF\_Array}.
!    
!  The arguments are:
!  \begin{description}
!
!  \item[arrayspec]
!    Uninitialized array spec.
!
!  \item[rank]
!    Array rank (dimensionality, 1D, 2D, etc).  Maximum allowed is 5D.
!
!  \item[type]
!    {\tt ESMF\_Array} type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!
!  \item[kind]
!    {\tt ESMF\_Array} kind.  Valid kinds include {\tt ESMF\_KIND\_I4}, 
!    {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8}, 
!    {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}. 
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        integer :: status                        ! local error status
        logical :: rcpresent                     ! did user specify rc?

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
      subroutine ESMF_ArrayGet(array, rank, type, kind, counts, &
                               lbounds, ubounds, strides, base, name, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      integer, intent(out), optional :: rank
      type(ESMF_DataType), intent(out), optional :: type
      type(ESMF_DataKind), intent(out), optional :: kind
      integer, dimension(:), intent(out), optional :: counts
      integer, dimension(:), intent(out), optional :: lbounds
      integer, dimension(:), intent(out), optional :: ubounds
      integer, dimension(:), intent(out), optional :: strides
      type(ESMF_Pointer), intent(out), optional :: base
      character(len=ESMF_MAXSTR), intent(out), optional :: name
      integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the {\tt ESMF\_Array}.  For queries where the caller
!      only wants a single value, specify the argument by name.
!      All the arguments after the array input are optional to facilitate this.
!
!EOP
! !REQUIREMENTS:


      integer :: status ! Error status
      logical :: rcpresent ! Return code present
      integer :: lrank  ! Local use to get rank

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

      if (present(counts)) then
         call c_ESMC_ArrayGetRank(array, lrank, status)
         call c_ESMC_ArrayGetLengths(array, lrank, counts, status)
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
!      Returns the name of the {\tt ESMF\_Array}.  If the array was created without
!      specifying a name, the framework will have assigned it a unique one.
!
!EOP
! !REQUIREMENTS: FLD1.5.1, FLD1.7.1

      integer :: status                           ! Error status
      logical :: rcpresent                        ! Return code present

!     Initialize return code; assume failure until success is certain
      status = ESMF_FAILURE
      rcpresent = .FALSE.
      if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
      endif

      ! TODO: add an interface to the C code here
      !call c_ESMC_ArrayGetName(array, name, status)
      !if(status .NE. ESMF_FAILURE) then
      !  print *, "ERROR in ESMF_ArrayGetName"
      !  return
      !endif

      name = "default array name"

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_ArrayGetName


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArraySpecGet(as, rank, type, kind, rc)
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: as
      integer, intent(out), optional :: rank
      type(ESMF_DataType), intent(out), optional :: type
      type(ESMF_DataKind), intent(out), optional :: kind
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Return information about the contents of a {\tt ESMF\_ArraySpec} type.
!
!  The arguments are:
!  \begin{description}
!
!  \item[as]
!    An {\tt ESMF\_ArraySpec} object.
!
!  \item[rank]
!    {\tt ESMF\_Array} rank (dimensionality, 1D, 2D, etc).  Maximum allowed is 5D.
!
!  \item[type]
!    {\tt ESMF\_Array} type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!
!  \item[kind]
!    {\tt ESMF\_Array} kind.  Valid kinds include {\tt ESMF\_KIND\_I4}, 
!    {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8}, 
!    {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}. 
!
!   \item[[rc]]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
!   \end{description}
!
!EOP

        ! Local vars
        integer :: i
        integer :: status                        ! local error status
        logical :: rcpresent                     ! did user specify rc?

        ! Initialize return code; assume failure until success is certain
        status = ESMF_FAILURE
        rcpresent = .FALSE.
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Get arrayspec contents
      
        if(present(rank)) rank = as%rank
        if(present(type)) type = as%type
        if(present(kind)) kind = as%kind

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArraySpecGet


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is Allocate/Deallocate for Arrays
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------ 
!!! TODO:  the interface now calls ESMF_ArrayConstructF90Ptr instead of
!!! this routine.  It maybe can go away?  and can we do something with
!!! ESMF_ArrayF90Deallocate to get rid of it as well, so the interfaces
!!! are more symmetric?
!------------------------------------------------------------------------------ 
!BOPI
! !IROUTINE:  ESMF_ArrayF90Allocate - Allocate an F90 pointer and set Array info
!
! !INTERFACE: 
     subroutine ESMF_ArrayF90Allocate(array, rank, type, kind, counts, rc)
! 
! !ARGUMENTS: 
      type(ESMF_Array), intent(inout) :: array 
      integer, intent(in) :: rank   
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts 
      integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
!     Allocate data contents for an {\tt ESMF\_Array} created from the C++ interface. 
!     The arguments are: 
!     \begin{description} 
!     \item[array]  
!          A partially created {\tt ESMF\_Array} object. 
!     \item[rank]  
!          The {\tt ESMF\_Array} rank.  
!     \item[type]  
!          The {\tt ESMF\_Array} type (integer, real/float, etc).  
!     \item[kind]  
!          The {\tt ESMF\_Array} kind (short/2, long/8, etc).  
!     \item[counts]  
!          An integer array, size {\tt rank}, of each dimension length. 
!     \item[{[rc]}]  
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
!   \end{description} 
! 
!EOPI
! !REQUIREMENTS: 
 
    integer :: status                               ! local error status 
    integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds
    integer, dimension(ESMF_MAXDIM) :: strides, offsets
    integer :: localkind, localtype

    !! local variables, expanded by macro
ArrayAllLocalVarMacro()

 
    status = ESMF_FAILURE  
    if (present(rc)) rc = ESMF_FAILURE
 
    localtype = type%dtype
    localkind = kind%dkind

    !! macros which are expanded by the preprocessor
    select case (localtype)
      case (ESMF_DATA_INTEGER%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
AllocAllocateMacro(integer, I2, 1, COL1, LEN1, LOC1)
              case (ESMF_KIND_I4%dkind)
AllocAllocateMacro(integer, I4, 1, COL1, LEN1, LOC1)
              case (ESMF_KIND_I8%dkind)
AllocAllocateMacro(integer, I8, 1, COL1, LEN1, LOC1)
              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
AllocAllocateMacro(integer, I2, 2, COL2, LEN2, LOC2)
              case (ESMF_KIND_I4%dkind)
AllocAllocateMacro(integer, I4, 2, COL2, LEN2, LOC2)
              case (ESMF_KIND_I8%dkind)
AllocAllocateMacro(integer, I8, 2, COL2, LEN2, LOC2)
              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
AllocAllocateMacro(integer, I2, 3, COL3, LEN3, LOC3)       
              case (ESMF_KIND_I4%dkind)
AllocAllocateMacro(integer, I4, 3, COL3, LEN3, LOC3)       
              case (ESMF_KIND_I8%dkind)
AllocAllocateMacro(integer, I8, 3, COL3, LEN3, LOC3)
              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
AllocAllocateMacro(integer, I2, 4, COL4, LEN4, LOC4)       
              case (ESMF_KIND_I4%dkind)
AllocAllocateMacro(integer, I4, 4, COL4, LEN4, LOC4)       
              case (ESMF_KIND_I8%dkind)
AllocAllocateMacro(integer, I8, 4, COL4, LEN4, LOC4)
              case default
            end select

          case default
        end select

       case (ESMF_DATA_REAL%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
AllocAllocateMacro(real, R4, 1, COL1, LEN1, LOC1)
              case (ESMF_KIND_R8%dkind)
AllocAllocateMacro(real, R8, 1, COL1, LEN1, LOC1)
              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
AllocAllocateMacro(real, R4, 2, COL2, LEN2, LOC2)
              case (ESMF_KIND_R8%dkind)
AllocAllocateMacro(real, R8, 2, COL2, LEN2, LOC2)
              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
AllocAllocateMacro(real, R4, 3, COL3, LEN3, LOC3)       
              case (ESMF_KIND_R8%dkind)
AllocAllocateMacro(real, R8, 3, COL3, LEN3, LOC3)
              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
AllocAllocateMacro(real, R4, 4, COL4, LEN4, LOC4)       
              case (ESMF_KIND_R8%dkind)
AllocAllocateMacro(real, R8, 4, COL4, LEN4, LOC4)
              case default
            end select

          case default
        end select
      case default
     end select

     if (present(rc)) rc = status 
 
     end subroutine ESMF_ArrayF90Allocate
 

!------------------------------------------------------------------------------ 
!BOP 
! !IROUTINE:  ESMF_ArrayF90Deallocate - Deallocate an F90 pointer 
!
! !INTERFACE: 
     subroutine ESMF_ArrayF90Deallocate(array, rank, type, kind, rc)
! 
! !ARGUMENTS: 
      type(ESMF_Array), intent(inout) :: array 
      integer :: rank   
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
!     Deallocate data contents for an {\tt ESMF\_Array} created from the C++ interface. 
!     The arguments are: 
!     \begin{description} 
!     \item[array]  
!          A partially created {\tt ESMF\_Array} object. 
!     \item[rank]  
!          The {\tt ESMF\_Array} rank.  
!     \item[type]  
!          The {\tt ESMF\_Array} type (integer, real/float, etc).  
!     \item[kind]  
!          The {\tt ESMF\_Array} kind (short/2, long/8, etc).  
!     \item[{[rc]}]  
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
!   \end{description} 
! 
!EOP 
! !REQUIREMENTS: 
 
    integer :: status                               ! local error status 
    integer :: localkind, localtype

    !! local variables, expanded by macro
ArrayAllLocalVarMacro()


    if (present(rc)) rc = ESMF_FAILURE
 
    localtype = type
    localkind = kind

    !! macros which are expanded by the preprocessor
    select case (localtype)
      case (ESMF_DATA_INTEGER%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 1, COL1, LEN1, LOC1)
              case (ESMF_KIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 1, COL1, LEN1, LOC1)
              case (ESMF_KIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 1, COL1, LEN1, LOC1)
              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 2, COL2, LEN2, LOC2)
              case (ESMF_KIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 2, COL2, LEN2, LOC2)
              case (ESMF_KIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 2, COL2, LEN2, LOC2)
              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 3, COL3, LEN3, LOC3)       
              case (ESMF_KIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 3, COL3, LEN3, LOC3)       
              case (ESMF_KIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 3, COL3, LEN3, LOC3)
              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 4, COL4, LEN4, LOC4)       
              case (ESMF_KIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 4, COL4, LEN4, LOC4)       
              case (ESMF_KIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 4, COL4, LEN4, LOC4)
              case default
            end select

          case default
        end select

       case (ESMF_DATA_REAL%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
AllocDeallocateMacro(real, R4, 1, COL1, LEN1, LOC1)
              case (ESMF_KIND_R8%dkind)
AllocDeallocateMacro(real, R8, 1, COL1, LEN1, LOC1)
              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
AllocDeallocateMacro(real, R4, 2, COL2, LEN2, LOC2)
              case (ESMF_KIND_R8%dkind)
AllocDeallocateMacro(real, R8, 2, COL2, LEN2, LOC2)
              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
AllocDeallocateMacro(real, R4, 3, COL3, LEN3, LOC3)       
              case (ESMF_KIND_R8%dkind)
AllocDeallocateMacro(real, R8, 3, COL3, LEN3, LOC3)
              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
AllocDeallocateMacro(real, R4, 4, COL4, LEN4, LOC4)       
              case (ESMF_KIND_R8%dkind)
AllocDeallocateMacro(real, R8, 4, COL4, LEN4, LOC4)
              case default
            end select

          case default
        end select
      case default
     end select

     if (status .ne. 0) then 
        print *, "ESMC_ArrayDelete: Deallocation error"
        return
      endif

     if (present(rc)) rc = ESMF_SUCCESS
 
     end subroutine ESMF_ArrayF90Deallocate
 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for Arrays
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayWriteRestart(array, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array 
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
        end subroutine ESMF_ArrayWriteRestart


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_ArrayReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! array name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize
!      all data associated with a {\tt ESMF\_Array} from the last call to WriteRestart.
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

        ESMF_ArrayReadRestart = a 
 
        end function ESMF_ArrayReadRestart


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArrayWrite(array, iospec, filename, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      type(ESMF_IOSpec), intent(in), optional :: iospec
      character(len=*), intent(in), optional :: filename
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used to write data to persistent storage in a variety of formats.  
!      (see WriteRestart/ReadRestart for quick data dumps.)  Details of I/O 
!      options specified in the IOSpec derived type. 
!
!
!EOP
! !REQUIREMENTS:

       character (len=16) :: defaultopts      ! default write options 
       character (len=16) :: defaultfile      ! default filename
       integer :: status                      ! local error status
       logical :: rcpresent        

       ! Initialize return code; assume failure until success is certain
       status = ESMF_FAILURE
       rcpresent = .FALSE.
       if (present(rc)) then
         rcpresent = .TRUE.
         rc = ESMF_FAILURE
       endif

       defaultopts = "singlefile"
       defaultfile = "datafile"

       if(present(filename)) then
           call c_ESMC_ArrayWrite(array, defaultopts, trim(filename), status) 
       else
           call c_ESMC_ArrayWrite(array, defaultopts, trim(defaultfile), status) 
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "Array write error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

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
! !IROUTINE: ESMF_ArrayValidate - Check validity of Array object
!
! !INTERFACE:
      subroutine ESMF_ArrayValidate(array, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_Array}.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts      ! default print options 
       integer :: status                     ! local error status
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
       !  print *, "Array validate error"
       !  return
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
      type(ESMF_Array), intent(in) :: array
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_Array}.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
       character (len=6) :: defaultopts      ! default print options 
       integer :: status                     ! local error status
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

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_ArrayPrint


        end module ESMF_ArrayMod

