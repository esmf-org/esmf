! $Id: ESMF_LocalArray_F90.cpp,v 1.4 2003/07/15 18:16:24 jwolfe Exp $
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
!     ESMF LocalArray module
      module ESMF_LocalArrayMod
!
!==============================================================================
!
! This file contains the LocalArray class definition and all LocalArray
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below.  they are created by the files which
!   define various macros. >
#include "ESMF_LocalArrayMacros.h"
#include "ESMF_AllocMacros.h"
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_LocalArrayMod - Manage data arrays uniformly between F90 and C++     
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_LocalArray} class and 
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
!     !  Update ../include/ESMC_LocalArray.h if you change these values.

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
!     ! ESMF_LocalArrayOrigin
!
!     ! Private flag which indicates the create was initiated on the F90 side.
!     !  This matches an enum on the C++ side and the values must match.
!     !  Update ../include/ESMC_LocalArray.h if you change these values.

      type ESMF_LocalArrayOrigin
      sequence
      private
        integer :: origin
      end type

      type(ESMF_LocalArrayOrigin), parameter :: & 
                            ESMF_FROM_FORTRAN   = ESMF_LocalArrayOrigin(1), &
                            ESMF_FROM_CPLUSPLUS = ESMF_LocalArrayOrigin(2)

!------------------------------------------------------------------------------
!     ! ESMF_DomainType
!
!     ! Indicates whether a data array should be copied or referenced. 
!     !  This matches an enum on the C++ side and the values must match.
!     !  Update ../include/ESMC_LocalArray.h if you change these values.

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
      !!private
   
        integer :: rank                     ! number of dimensions
        type(ESMF_DataType) :: type         ! real/float, integer, etc enum
        type(ESMF_DataKind) :: kind         ! fortran "kind" enum/integer

      end type

!------------------------------------------------------------------------------
!     ! ESMF_LocalArray
!
!     ! LocalArray data type.  All information is kept on the C++ side inside
!     ! the class structure.

      type ESMF_LocalArray
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
      public ESMF_CopyFlag, ESMF_DATA_COPY, ESMF_DATA_REF, ESMF_DATA_SPACE
      public ESMF_ArraySpec, ESMF_LocalArray
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_LocalArrayCreate
      public ESMF_LocalArrayDestroy
 
      public ESMF_LocalArraySpecInit
      public ESMF_LocalArraySpecGet

      public ESMF_LocalArraySetData, ESMF_LocalArrayGetData
      !public ESMF_LocalArraySetInfo, ESMF_LocalArrayGetInfo
      public ESMF_LocalArrayGet, ESMF_LocalArrayGetName
 
      public ESMF_LocalArrayF90Allocate
      public ESMF_LocalArrayF90Deallocate
      public ESMF_LocalArrConstrF90Ptr    ! needed for C++ callback only

      public ESMF_LocalArrayWriteRestart
      public ESMF_LocalArrayReadRestart
      public ESMF_LocalArrayWrite
      public ESMF_LocalArrayRead
 
      public ESMF_LocalArrayValidate
      public ESMF_LocalArrayPrint
!EOP
      public operator(.eq.), operator(.ne.)

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_LocalArray_F90.cpp,v 1.4 2003/07/15 18:16:24 jwolfe Exp $'

!==============================================================================
! 
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Generic interface to create an LocalArray

! !INTERFACE:
     interface ESMF_LocalArrayCreate

! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_LocalArrayCreateByList      ! specify TKR
        module procedure ESMF_LocalArrayCreateBySpec      ! specify ArraySpec
   
        ! Plus interfaces for each T/K/R 

!EOP
        

!       ! < interfaces for each T/K/R >
! --LocalArray--InterfaceMacro(LocalArrayCreateByMTArr)
!
!       ! < interfaces for each T/K/R >
! --LocalArray--InterfaceMacro(LocalArrayCreateByFullArr)

       ! < interfaces for each T/K/R >
LocalArrayInterfaceMacro(LocalArrayCreateByMTPtr)

       ! < interfaces for each T/K/R >
LocalArrayInterfaceMacro(LocalArrayCreateByFlPtr)


!BOP
! !DESCRIPTION: 
! This interface provides a single (heavily overloaded) entry point for 
!  the various types of {\tt ESMF\_LocalArrayCreate} functions.   
!
!  There are 3 options for setting the contents of the {\tt ESMF\_LocalArray}
!  at creation time:
!  \begin{description}
!  \item[Allocate Space Only]
!    Data space is allocated but not initialized.  The caller can query
!    for a pointer to the start of the space to address it directly.
!    The caller must not deallocate the space; the
!    {\tt ESMF\_LocalArray} will release the space when it is destroyed.
!  \item[Data Copy]
!    An existing Fortran array is specified and the data contents are copied
!    into new space allocated by the {\tt ESMF\_LocalArray}.
!    The caller must not deallocate the space; the
!    {\tt ESMF\_LocalArray} will release the space when it is destroyed.
!  \item[Data Reference]
!    An existing Fortran array is specified and the data contents reference
!    it directly.  The caller is responsible for deallocating the space;
!    when the {\tt ESMF\_LocalArray} is destroyed it will not release the space.
!  \end{description}
!
!  There are 3 options for 
!  specifying the type/kind/rank of the {\tt ESMF\_LocalArray} data:
!  \begin{description}
!  \item[List]
!    The characteristics of the {\tt ESMF\_LocalArray} are given explicitly
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
!  The concept of an ``empty'' {\tt ESMF\_LocalArray} does not exist.  To make an
!  ESMF object which stores the Type/Kind/Rank information create an
!  {\tt ESMF\_ArraySpec} object which can then be used repeatedly in
!  subsequent {\tt ESMF\_LocalArray} Create calls.
!  
end interface
!EOP 

!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_LocalArrayGetData -- Get an F90 pointer to the data contents

! !INTERFACE:
     interface ESMF_LocalArrayGetData

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
LocalArrayInterfaceMacro(LocalArrayGetData)

! !DESCRIPTION: 
! This interface provides a single entry point for the various 
!  types of {\tt ESMF\_LocalArrayGetData} functions.   
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
! This section includes the LocalArray Create and Destroy methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayCreateByList -- Create an LocalArray specifying all options.

! !INTERFACE:
      function ESMF_LocalArrayCreateByList(rank, type, kind, counts, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayCreateByList
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_LocalArray and allocate data space, which remains
!  uninitialized.  The return value is a new LocalArray.
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
        type (ESMF_LocalArray) :: array     ! new C++ LocalArray
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
          print *, "LocalArray construction error"
          return
        endif

        call ESMF_LocalArrConstrF90Ptr(array, counts, rank, type, kind, status)

        ! Set return values
        ESMF_LocalArrayCreateByList = array 
        if (rcpresent) rc = status

        end function ESMF_LocalArrayCreateByList


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayCreateBySpec -- Create a new LocalArray from an ArraySpec

! !INTERFACE:
      function ESMF_LocalArrayCreateBySpec(spec, counts, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayCreateBySpec
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: spec
      integer, intent(in), dimension(:) :: counts
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
!  uninitialized.  The return value is a new LocalArray.
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
        type (ESMF_LocalArray) :: array     ! new C++ LocalArray
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
        ESMF_LocalArrayCreateBySpec = ESMF_LocalArrayCreateByList(rank, type, &
                                      kind, counts, status)
        if (rcpresent) rc = status

        end function ESMF_LocalArrayCreateBySpec


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArrConstrF90Ptr - Create and add F90 ptr to array

! !INTERFACE:
     subroutine ESMF_LocalArrConstrF90Ptr(array, counts, rank, type, kind, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Take a partially created {\tt ESMF\_LocalArray} and T/K/R information and call
!  the proper subroutine to create an F90 pointer, allocate space, and set the
!  corresponding values in the {\tt ESMF\_LocalArray} object.
!    
!  The arguments are:
!  \begin{description}
!
!  \item[array]
!    Partially created {\tt ESMF\_LocalArray} object.  This entry point is used
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
               call ESMF_LocalArrConstrF90PtrI41D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_LocalArrConstrF90PtrR41D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case (2)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_LocalArrConstrF90PtrI42D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_LocalArrConstrF90PtrR42D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case (3)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_LocalArrConstrF90PtrI43D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_LocalArrConstrF90PtrR43D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case (4)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_LocalArrConstrF90PtrI44D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_LocalArrConstrF90PtrR44D(array, counts, rc=status)
              case default
               print *, "unsupported type"
               return
            end select
          case (5)
            select case (type%dtype)
              case (ESMF_DATA_INTEGER%dtype)
               call ESMF_LocalArrConstrF90PtrI45D(array, counts, rc=status)
              case (ESMF_DATA_REAL%dtype)
               call ESMF_LocalArrConstrF90PtrR45D(array, counts, rc=status)
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

        end subroutine ESMF_LocalArrConstrF90Ptr

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

LocalArrayCreateByMTArrMacro(integer, I2, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTArrMacro(integer, I4, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTArrMacro(integer, I8, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTArrMacro(integer, I2, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTArrMacro(integer, I4, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTArrMacro(integer, I8, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTArrMacro(integer, I2, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTArrMacro(integer, I4, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTArrMacro(integer, I8, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTArrMacro(integer, I2, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTArrMacro(integer, I4, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTArrMacro(integer, I8, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTArrMacro(integer, I2, 5, COL5, LEN5, LOC5)

LocalArrayCreateByMTArrMacro(integer, I4, 5, COL5, LEN5, LOC5)

LocalArrayCreateByMTArrMacro(integer, I8, 5, COL5, LEN5, LOC5)

LocalArrayCreateByMTArrMacro(real, R4, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTArrMacro(real, R8, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTArrMacro(real, R4, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTArrMacro(real, R8, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTArrMacro(real, R4, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTArrMacro(real, R8, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTArrMacro(real, R4, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTArrMacro(real, R8, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTArrMacro(real, R4, 5, COL5, LEN5, LOC5)

LocalArrayCreateByMTArrMacro(real, R8, 5, COL5, LEN5, LOC5)


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

LocalArrayCreateByFlArrMacro(integer, I2, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlArrMacro(integer, I4, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlArrMacro(integer, I8, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlArrMacro(integer, I2, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlArrMacro(integer, I4, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlArrMacro(integer, I8, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlArrMacro(integer, I2, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlArrMacro(integer, I4, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlArrMacro(integer, I8, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlArrMacro(integer, I2, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlArrMacro(integer, I4, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlArrMacro(integer, I8, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlArrMacro(integer, I2, 5, COL5, LEN5, LOC5)

LocalArrayCreateByFlArrMacro(integer, I4, 5, COL5, LEN5, LOC5)

LocalArrayCreateByFlArrMacro(integer, I8, 5, COL5, LEN5, LOC5)

LocalArrayCreateByFlArrMacro(real, R4, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlArrMacro(real, R8, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlArrMacro(real, R4, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlArrMacro(real, R8, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlArrMacro(real, R4, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlArrMacro(real, R8, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlArrMacro(real, R4, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlArrMacro(real, R8, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlArrMacro(real, R4, 5, COL5, LEN5, LOC5)

LocalArrayCreateByFlArrMacro(real, R8, 5, COL5, LEN5, LOC5)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

LocalArrayCreateByMTPtrMacro(integer, I2, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTPtrMacro(integer, I4, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTPtrMacro(integer, I8, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTPtrMacro(integer, I2, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTPtrMacro(integer, I4, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTPtrMacro(integer, I8, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTPtrMacro(integer, I2, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTPtrMacro(integer, I4, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTPtrMacro(integer, I8, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTPtrMacro(integer, I2, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTPtrMacro(integer, I4, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTPtrMacro(integer, I8, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTPtrMacro(integer, I2, 5, COL5, LEN5, LOC5)

LocalArrayCreateByMTPtrMacro(integer, I4, 5, COL5, LEN5, LOC5)

LocalArrayCreateByMTPtrMacro(integer, I8, 5, COL5, LEN5, LOC5)

LocalArrayCreateByMTPtrMacro(real, R4, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTPtrMacro(real, R8, 1, COL1, LEN1, LOC1)

LocalArrayCreateByMTPtrMacro(real, R4, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTPtrMacro(real, R8, 2, COL2, LEN2, LOC2)

LocalArrayCreateByMTPtrMacro(real, R4, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTPtrMacro(real, R8, 3, COL3, LEN3, LOC3)

LocalArrayCreateByMTPtrMacro(real, R4, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTPtrMacro(real, R8, 4, COL4, LEN4, LOC4)

LocalArrayCreateByMTPtrMacro(real, R4, 5, COL5, LEN5, LOC5)

LocalArrayCreateByMTPtrMacro(real, R8, 5, COL5, LEN5, LOC5)



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

LocalArrayCreateByFlPtrMacro(integer, I2, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlPtrMacro(integer, I4, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlPtrMacro(integer, I8, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlPtrMacro(integer, I2, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlPtrMacro(integer, I4, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlPtrMacro(integer, I8, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlPtrMacro(integer, I2, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlPtrMacro(integer, I4, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlPtrMacro(integer, I8, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlPtrMacro(integer, I2, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlPtrMacro(integer, I4, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlPtrMacro(integer, I8, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlPtrMacro(integer, I2, 5, COL5, LEN5, LOC5)

LocalArrayCreateByFlPtrMacro(integer, I4, 5, COL5, LEN5, LOC5)

LocalArrayCreateByFlPtrMacro(integer, I8, 5, COL5, LEN5, LOC5)

LocalArrayCreateByFlPtrMacro(real, R4, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlPtrMacro(real, R8, 1, COL1, LEN1, LOC1)

LocalArrayCreateByFlPtrMacro(real, R4, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlPtrMacro(real, R8, 2, COL2, LEN2, LOC2)

LocalArrayCreateByFlPtrMacro(real, R4, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlPtrMacro(real, R8, 3, COL3, LEN3, LOC3)

LocalArrayCreateByFlPtrMacro(real, R4, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlPtrMacro(real, R8, 4, COL4, LEN4, LOC4)

LocalArrayCreateByFlPtrMacro(real, R4, 5, COL5, LEN5, LOC5)

LocalArrayCreateByFlPtrMacro(real, R8, 5, COL5, LEN5, LOC5)


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

LocalArrConstrF90PtrMacro(integer, I2, 1, COL1, LEN1, LOC1)

LocalArrConstrF90PtrMacro(integer, I4, 1, COL1, LEN1, LOC1)

LocalArrConstrF90PtrMacro(integer, I8, 1, COL1, LEN1, LOC1)

LocalArrConstrF90PtrMacro(integer, I2, 2, COL2, LEN2, LOC2)

LocalArrConstrF90PtrMacro(integer, I4, 2, COL2, LEN2, LOC2)

LocalArrConstrF90PtrMacro(integer, I8, 2, COL2, LEN2, LOC2)

LocalArrConstrF90PtrMacro(integer, I2, 3, COL3, LEN3, LOC3)

LocalArrConstrF90PtrMacro(integer, I4, 3, COL3, LEN3, LOC3)

LocalArrConstrF90PtrMacro(integer, I8, 3, COL3, LEN3, LOC3)

LocalArrConstrF90PtrMacro(integer, I2, 4, COL4, LEN4, LOC4)

LocalArrConstrF90PtrMacro(integer, I4, 4, COL4, LEN4, LOC4)

LocalArrConstrF90PtrMacro(integer, I8, 4, COL4, LEN4, LOC4)

LocalArrConstrF90PtrMacro(integer, I2, 5, COL5, LEN5, LOC5)

LocalArrConstrF90PtrMacro(integer, I4, 5, COL5, LEN5, LOC5)

LocalArrConstrF90PtrMacro(integer, I8, 5, COL5, LEN5, LOC5)

LocalArrConstrF90PtrMacro(real, R4, 1, COL1, LEN1, LOC1)

LocalArrConstrF90PtrMacro(real, R8, 1, COL1, LEN1, LOC1)

LocalArrConstrF90PtrMacro(real, R4, 2, COL2, LEN2, LOC2)

LocalArrConstrF90PtrMacro(real, R8, 2, COL2, LEN2, LOC2)

LocalArrConstrF90PtrMacro(real, R4, 3, COL3, LEN3, LOC3)

LocalArrConstrF90PtrMacro(real, R8, 3, COL3, LEN3, LOC3)

LocalArrConstrF90PtrMacro(real, R4, 4, COL4, LEN4, LOC4)

LocalArrConstrF90PtrMacro(real, R8, 4, COL4, LEN4, LOC4)

LocalArrConstrF90PtrMacro(real, R4, 5, COL5, LEN5, LOC5)

LocalArrConstrF90PtrMacro(real, R8, 5, COL5, LEN5, LOC5)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

LocalArrayGetDataMacro(integer, I2, 1, COL1, LEN1, LOC1)

LocalArrayGetDataMacro(integer, I4, 1, COL1, LEN1, LOC1)

LocalArrayGetDataMacro(integer, I8, 1, COL1, LEN1, LOC1)

LocalArrayGetDataMacro(integer, I2, 2, COL2, LEN2, LOC2)

LocalArrayGetDataMacro(integer, I4, 2, COL2, LEN2, LOC2)

LocalArrayGetDataMacro(integer, I8, 2, COL2, LEN2, LOC2)

LocalArrayGetDataMacro(integer, I2, 3, COL3, LEN3, LOC3)

LocalArrayGetDataMacro(integer, I4, 3, COL3, LEN3, LOC3)

LocalArrayGetDataMacro(integer, I8, 3, COL3, LEN3, LOC3)

LocalArrayGetDataMacro(integer, I2, 4, COL4, LEN4, LOC4)

LocalArrayGetDataMacro(integer, I4, 4, COL4, LEN4, LOC4)

LocalArrayGetDataMacro(integer, I8, 4, COL4, LEN4, LOC4)

LocalArrayGetDataMacro(integer, I2, 5, COL5, LEN5, LOC5)

LocalArrayGetDataMacro(integer, I4, 5, COL5, LEN5, LOC5)

LocalArrayGetDataMacro(integer, I8, 5, COL5, LEN5, LOC5)

LocalArrayGetDataMacro(real, R4, 1, COL1, LEN1, LOC1)

LocalArrayGetDataMacro(real, R8, 1, COL1, LEN1, LOC1)

LocalArrayGetDataMacro(real, R4, 2, COL2, LEN2, LOC2)

LocalArrayGetDataMacro(real, R8, 2, COL2, LEN2, LOC2)

LocalArrayGetDataMacro(real, R4, 3, COL3, LEN3, LOC3)

LocalArrayGetDataMacro(real, R8, 3, COL3, LEN3, LOC3)

LocalArrayGetDataMacro(real, R4, 4, COL4, LEN4, LOC4)

LocalArrayGetDataMacro(real, R8, 4, COL4, LEN4, LOC4)

LocalArrayGetDataMacro(real, R4, 5, COL5, LEN5, LOC5)

LocalArrayGetDataMacro(real, R8, 5, COL5, LEN5, LOC5)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
      
LocalArrayDeallocateMacro(integer, I2, 1, COL1, LEN1, LOC1)

LocalArrayDeallocateMacro(integer, I4, 1, COL1, LEN1, LOC1)

LocalArrayDeallocateMacro(integer, I8, 1, COL1, LEN1, LOC1)

LocalArrayDeallocateMacro(integer, I2, 2, COL2, LEN2, LOC2)

LocalArrayDeallocateMacro(integer, I4, 2, COL2, LEN2, LOC2)

LocalArrayDeallocateMacro(integer, I8, 2, COL2, LEN2, LOC2)

LocalArrayDeallocateMacro(integer, I2, 3, COL3, LEN3, LOC3)

LocalArrayDeallocateMacro(integer, I4, 3, COL3, LEN3, LOC3)

LocalArrayDeallocateMacro(integer, I8, 3, COL3, LEN3, LOC3)

LocalArrayDeallocateMacro(integer, I2, 4, COL4, LEN4, LOC4)

LocalArrayDeallocateMacro(integer, I4, 4, COL4, LEN4, LOC4)

LocalArrayDeallocateMacro(integer, I8, 4, COL4, LEN4, LOC4)

LocalArrayDeallocateMacro(integer, I2, 5, COL5, LEN5, LOC5)

LocalArrayDeallocateMacro(integer, I4, 5, COL5, LEN5, LOC5)

LocalArrayDeallocateMacro(integer, I8, 5, COL5, LEN5, LOC5)

LocalArrayDeallocateMacro(real, R4, 1, COL1, LEN1, LOC1)

LocalArrayDeallocateMacro(real, R8, 1, COL1, LEN1, LOC1)

LocalArrayDeallocateMacro(real, R4, 2, COL2, LEN2, LOC2)

LocalArrayDeallocateMacro(real, R8, 2, COL2, LEN2, LOC2)

LocalArrayDeallocateMacro(real, R4, 3, COL3, LEN3, LOC3)

LocalArrayDeallocateMacro(real, R8, 3, COL3, LEN3, LOC3)

LocalArrayDeallocateMacro(real, R4, 4, COL4, LEN4, LOC4)

LocalArrayDeallocateMacro(real, R8, 4, COL4, LEN4, LOC4)

LocalArrayDeallocateMacro(real, R4, 5, COL5, LEN5, LOC5)

LocalArrayDeallocateMacro(real, R8, 5, COL5, LEN5, LOC5)

!! < end of automatically generated function >

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LocalArrayDestroy(array, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!
!     \item[array]
!       Destroy contents of this {\tt ESMF\_LocalArray}.
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
          call ESMF_LocalArrayF90Deallocate(array, rank, type, kind, status)
          if (status .ne. ESMF_SUCCESS) then
            print *, "LocalArray contents destruction error"
            return
          endif
          call c_ESMC_ArraySetNoDealloc(array, status)
        endif

        ! Calling deallocate first means this will not return back to F90
        !  before returning for good.
        call c_ESMC_ArrayDestroy(array, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "LocalArray destruction error"
          return
        endif

!       set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LocalArrayDestroy



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArraySetData
!
! !INTERFACE:
      subroutine ESMF_LocalArraySetData(array, dataspec, databuf, docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array 
      type(ESMF_ArraySpec), intent(in) :: dataspec
      real, dimension (:), pointer :: databuf    
      type(ESMF_CopyFlag), intent(in) :: docopy 
      integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of {\tt ESMF\_LocalArrayCreate} which
!      creates an empty {\tt ESMF\_LocalArray} and allows the Data to be
!      specified later.  Otherwise it is an error to replace the data contents
!      associated with a {\tt ESMF\_LocalArray}.  
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        end subroutine ESMF_LocalArraySetData

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
     subroutine ESMF_LocalArraySpecInit(as, rank, type, kind, rc)
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
!  This specification can be used in an {\tt ESMF\_LocalArrayCreate} call with
!  data to create a full {\tt ESMF\_LocalArray}.
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
!    {\tt ESMF\_LocalArray} type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!
!  \item[kind]
!    {\tt ESMF\_LocalArray} kind.  Valid kinds include {\tt ESMF\_KIND\_I4}, 
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

        end subroutine ESMF_LocalArraySpecInit



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the array.
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LocalArrayGet(array, rank, type, kind, counts, &
                               lbounds, ubounds, strides, base, name, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array
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
!      Returns information about the {\tt ESMF\_LocalArray}.  For queries
!      where the caller only wants a single value, specify the argument by name.
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

      end subroutine ESMF_LocalArrayGet

!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayGetName - Retrieve the name of a LocalArray
!
! !INTERFACE:
      subroutine ESMF_LocalArrayGetName(array, name, rc)

!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array
      character (len = *), intent(out) :: name
      integer, intent(out), optional :: rc

!
! !DESCRIPTION:
!      Returns the name of the {\tt ESMF\_LocalArray}.  If the array was
!      created without specifying a name, the framework will have assigned it
!      a unique one.
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
      !  print *, "ERROR in ESMF_LocalArrayGetName"
      !  return
      !endif

      name = "default array name"

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LocalArrayGetName


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LocalArraySpecGet(as, rank, type, kind, rc)
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
!    {\tt ESMF\_LocalArray} rank (dimensionality, 1D, 2D, etc).  Maximum
!    allowed is 5D.
!
!  \item[type]
!    {\tt ESMF\_LocalArray} type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!
!  \item[kind]
!    {\tt ESMF\_LocalArray} kind.  Valid kinds include {\tt ESMF\_KIND\_I4}, 
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

        end subroutine ESMF_LocalArraySpecGet


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is Allocate/Deallocate for LocalArrays
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------ 
!!! TODO:  the interface now calls ESMF_LocalArrConstrF90Ptr instead of
!!! this routine.  It maybe can go away?  and can we do something with
!!! ESMF_LocalArrayF90Deallocate to get rid of it as well, so the interfaces
!!! are more symmetric?
!------------------------------------------------------------------------------ 
!BOPI
! !IROUTINE:  ESMF_LocalArrayF90Allocate - Allocate an F90 pointer and set LocalArray info
!
! !INTERFACE: 
     subroutine ESMF_LocalArrayF90Allocate(array, rank, type, kind, counts, rc)
! 
! !ARGUMENTS: 
      type(ESMF_LocalArray), intent(inout) :: array 
      integer, intent(in) :: rank   
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts 
      integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
!     Allocate data contents for an {\tt ESMF\_LocalArray} created from the
!     C++ interface.  The arguments are: 
!     \begin{description} 
!     \item[array]  
!          A partially created {\tt ESMF\_LocalArray} object. 
!     \item[rank]  
!          The {\tt ESMF\_LocalArray} rank.  
!     \item[type]  
!          The {\tt ESMF\_LocalArray} type (integer, real/float, etc).  
!     \item[kind]  
!          The {\tt ESMF\_LocalArray} kind (short/2, long/8, etc).  
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
 
     end subroutine ESMF_LocalArrayF90Allocate
 

!------------------------------------------------------------------------------ 
!BOP 
! !IROUTINE:  ESMF_LocalArrayF90Deallocate - Deallocate an F90 pointer 
!
! !INTERFACE: 
     subroutine ESMF_LocalArrayF90Deallocate(array, rank, type, kind, rc)
! 
! !ARGUMENTS: 
      type(ESMF_LocalArray), intent(inout) :: array 
      integer :: rank   
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
!     Deallocate data contents for an {\tt ESMF\_LocalArray} created from
!     the C++ interface.  The arguments are: 
!     \begin{description} 
!     \item[array]  
!          A partially created {\tt ESMF\_LocalArray} object. 
!     \item[rank]  
!          The {\tt ESMF\_LocalArray} rank.  
!     \item[type]  
!          The {\tt ESMF\_LocalArray} type (integer, real/float, etc).  
!     \item[kind]  
!          The {\tt ESMF\_LocalArray} kind (short/2, long/8, etc).  
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
 
     end subroutine ESMF_LocalArrayF90Deallocate
 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for LocalArrays
!
!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LocalArrayWriteRestart(array, iospec, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array 
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
        end subroutine ESMF_LocalArrayWriteRestart


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_LocalArrayReadRestart(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayReadRestart
!
!
! !ARGUMENTS:
      character (len = *), intent(in) :: name              ! array name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec    ! file specs
      integer, intent(out), optional :: rc                 ! return code
!
! !DESCRIPTION:
!      Used to reinitialize all data associated with a {\tt ESMF\_LocalArray}
!      from the last call to WriteRestart.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_LocalArray) :: a 

!       this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER

!
! TODO: add code here
!

        ESMF_LocalArrayReadRestart = a 
 
        end function ESMF_LocalArrayReadRestart


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_LocalArrayWrite(array, iospec, filename, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
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
         print *, "LocalArray write error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LocalArrayWrite


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      function ESMF_LocalArrayRead(name, iospec, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayRead
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
        type (ESMF_LocalArray) :: a

!       this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER

!
! TODO: add code here
!

        ESMF_LocalArrayRead = a 
 
        end function ESMF_LocalArrayRead


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayValidate - Check validity of LocalArray object
!
! !INTERFACE:
      subroutine ESMF_LocalArrayValidate(array, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_LocalArray}.
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
           print *, "LocalArray not initialized or Destroyed"
           return 
       endif

       if(present(options)) then
           !call c_ESMC_ArrayValidate(array, options, status) 
       else
           !call c_ESMC_ArrayValidate(array, defaultopts, status) 
       endif

       !if (status .ne. ESMF_SUCCESS) then
       !  print *, "LocalArray validate error"
       !  return
       !endif

       ! Set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_LocalArrayValidate


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayPrint - Print contents of an LocalArray object
!
! !INTERFACE:
      subroutine ESMF_LocalArrayPrint(array, options, rc)
!
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array
      character (len = *), intent(in), optional :: options
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!      Routine to print information about a {\tt ESMF\_LocalArray}.
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
         print *, "LocalArray print error"
         return
       endif

!      set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_LocalArrayPrint


        end module ESMF_LocalArrayMod

