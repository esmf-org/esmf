! $Id: ESMF_LocalArray.F90,v 1.11 2003/08/15 21:33:20 nscollins Exp $
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
! ESMF LocalArray module
      module ESMF_LocalArrayMod
!
!==============================================================================
!
! This file contains the LocalArray class definition and all LocalArray
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
!------------------------------------------------------------------------------
!BOP
! !MODULE: ESMF_LocalArrayMod - Manage data arrays uniformly between F90 and C++
!
! !DESCRIPTION:
!
! The code in this file implements the {\tt ESMF\_LocalArray} class and
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
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
! ! ESMF_CopyFlag
!
! ! Indicates whether a data array should be copied or referenced.
! ! This matches an enum on the C++ side and the values must match.
! ! Update ../include/ESMC_LocalArray.h if you change these values.

      type ESMF_CopyFlag
      sequence
      private
        integer :: docopy
      end type

      type(ESMF_CopyFlag), parameter :: &
                            ESMF_DATA_COPY = ESMF_CopyFlag(1), &
                            ESMF_DATA_REF = ESMF_CopyFlag(2), &
                            ESMF_DATA_SPACE = ESMF_CopyFlag(3) ! private

!------------------------------------------------------------------------------
! ! ESMF_LocalArrayOrigin
!
! ! Private flag which indicates the create was initiated on the F90 side.
! ! This matches an enum on the C++ side and the values must match.
! ! Update ../include/ESMC_LocalArray.h if you change these values.

      type ESMF_LocalArrayOrigin
      sequence
      private
        integer :: origin
      end type

      type(ESMF_LocalArrayOrigin), parameter :: &
                            ESMF_FROM_FORTRAN = ESMF_LocalArrayOrigin(1), &
                            ESMF_FROM_CPLUSPLUS = ESMF_LocalArrayOrigin(2)

!------------------------------------------------------------------------------
! ! ESMF_DomainType
!
! ! Indicates whether a data array should be copied or referenced.
! ! This matches an enum on the C++ side and the values must match.
! ! Update ../include/ESMC_LocalArray.h if you change these values.

      type ESMF_DomainType
      sequence
      private
        integer :: dt
      end type

      type(ESMF_DomainType), parameter :: &
                            ESMF_DOMAIN_TOTAL = ESMF_DomainType(1), &
                            ESMF_DOMAIN_COMPUTATIONAL = ESMF_DomainType(2), &
                            ESMF_DOMAIN_EXCLUSIVE = ESMF_DomainType(3)

!------------------------------------------------------------------------------
! ! ESMF_ArraySpec
!
! ! Data array specification, with no associated data buffer.

      type ESMF_ArraySpec
      sequence
      !!private

        integer :: rank ! number of dimensions
        type(ESMF_DataType) :: type ! real/float, integer, etc enum
        type(ESMF_DataKind) :: kind ! fortran "kind" enum/integer

      end type

!------------------------------------------------------------------------------
! ! ESMF_LocalArray
!
! ! LocalArray data type. All information is kept on the C++ side inside
! ! the class structure.

      type ESMF_LocalArray
      sequence
      !!private
        ! opaque pointer to the C++ class data
        !type(ESMF_Pointer) :: this = ESMF_NULL_POINTER
        type(ESMF_Pointer) :: this
      end type

!------------------------------------------------------------------------------
! ! Internal wrapper structures for passing f90 pointers to C++ and
! ! guaranteeing they are passed by reference on all compilers and all
! ! platforms. These are never seen outside this module.
!
      ! < these expand into defined type declarations >

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
 
! < end macro - do not edit directly > 
 



!------------------------------------------------------------------------------
! !PUBLIC TYPES:
      public ESMF_CopyFlag, ESMF_DATA_COPY, ESMF_DATA_REF, ESMF_DATA_SPACE
      public ESMF_ArraySpec, ESMF_LocalArray
      public ESMF_DomainType
      public ESMF_DOMAIN_TOTAL, ESMF_DOMAIN_COMPUTATIONAL, ESMF_DOMAIN_EXCLUSIVE
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
      public ESMF_LocalArrConstrF90Ptr ! needed for C++ callback only

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
      '$Id: ESMF_LocalArray.F90,v 1.11 2003/08/15 21:33:20 nscollins Exp $'

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
        module procedure ESMF_LocalArrayCreateByList ! specify TKR
        module procedure ESMF_LocalArrayCreateBySpec ! specify ArraySpec

        ! Plus interfaces for each T/K/R

!EOP


! ! < interfaces for each T/K/R >
! --LocalArray--InterfaceMacro(LocalArrCreateByMTArr)
!
! ! < interfaces for each T/K/R >
! --LocalArray--InterfaceMacro(LocalArrCreateByFullArr)

       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_LocalArrCreateByMTPtrI21D 
 module procedure ESMF_LocalArrCreateByMTPtrI41D 
 module procedure ESMF_LocalArrCreateByMTPtrI81D 
 module procedure ESMF_LocalArrCreateByMTPtrI22D 
 module procedure ESMF_LocalArrCreateByMTPtrI42D 
 module procedure ESMF_LocalArrCreateByMTPtrI82D 
 module procedure ESMF_LocalArrCreateByMTPtrI23D 
 module procedure ESMF_LocalArrCreateByMTPtrI43D 
 module procedure ESMF_LocalArrCreateByMTPtrI83D 
 module procedure ESMF_LocalArrCreateByMTPtrI24D 
 module procedure ESMF_LocalArrCreateByMTPtrI44D 
 module procedure ESMF_LocalArrCreateByMTPtrI84D 
 module procedure ESMF_LocalArrCreateByMTPtrI25D 
 module procedure ESMF_LocalArrCreateByMTPtrI45D 
 module procedure ESMF_LocalArrCreateByMTPtrI85D 
 module procedure ESMF_LocalArrCreateByMTPtrR41D 
 module procedure ESMF_LocalArrCreateByMTPtrR81D 
 module procedure ESMF_LocalArrCreateByMTPtrR42D 
 module procedure ESMF_LocalArrCreateByMTPtrR82D 
 module procedure ESMF_LocalArrCreateByMTPtrR43D 
 module procedure ESMF_LocalArrCreateByMTPtrR83D 
 module procedure ESMF_LocalArrCreateByMTPtrR44D 
 module procedure ESMF_LocalArrCreateByMTPtrR84D 
 module procedure ESMF_LocalArrCreateByMTPtrR45D 
 module procedure ESMF_LocalArrCreateByMTPtrR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_LocalArrCreateByFlPtrI21D 
 module procedure ESMF_LocalArrCreateByFlPtrI41D 
 module procedure ESMF_LocalArrCreateByFlPtrI81D 
 module procedure ESMF_LocalArrCreateByFlPtrI22D 
 module procedure ESMF_LocalArrCreateByFlPtrI42D 
 module procedure ESMF_LocalArrCreateByFlPtrI82D 
 module procedure ESMF_LocalArrCreateByFlPtrI23D 
 module procedure ESMF_LocalArrCreateByFlPtrI43D 
 module procedure ESMF_LocalArrCreateByFlPtrI83D 
 module procedure ESMF_LocalArrCreateByFlPtrI24D 
 module procedure ESMF_LocalArrCreateByFlPtrI44D 
 module procedure ESMF_LocalArrCreateByFlPtrI84D 
 module procedure ESMF_LocalArrCreateByFlPtrI25D 
 module procedure ESMF_LocalArrCreateByFlPtrI45D 
 module procedure ESMF_LocalArrCreateByFlPtrI85D 
 module procedure ESMF_LocalArrCreateByFlPtrR41D 
 module procedure ESMF_LocalArrCreateByFlPtrR81D 
 module procedure ESMF_LocalArrCreateByFlPtrR42D 
 module procedure ESMF_LocalArrCreateByFlPtrR82D 
 module procedure ESMF_LocalArrCreateByFlPtrR43D 
 module procedure ESMF_LocalArrCreateByFlPtrR83D 
 module procedure ESMF_LocalArrCreateByFlPtrR44D 
 module procedure ESMF_LocalArrCreateByFlPtrR84D 
 module procedure ESMF_LocalArrCreateByFlPtrR45D 
 module procedure ESMF_LocalArrCreateByFlPtrR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!BOP
! !DESCRIPTION:
! This interface provides a single (heavily overloaded) entry point for
! the various types of {\tt ESMF\_LocalArrayCreate} functions.
!
! There are 3 options for setting the contents of the {\tt ESMF\_LocalArray}
! at creation time:
! \begin{description}
! \item[Allocate Space Only]
! Data space is allocated but not initialized. The caller can query
! for a pointer to the start of the space to address it directly.
! The caller must not deallocate the space; the
! {\tt ESMF\_LocalArray} will release the space when it is destroyed.
! \item[Data Copy]
! An existing Fortran array is specified and the data contents are copied
! into new space allocated by the {\tt ESMF\_LocalArray}.
! The caller must not deallocate the space; the
! {\tt ESMF\_LocalArray} will release the space when it is destroyed.
! \item[Data Reference]
! An existing Fortran array is specified and the data contents reference
! it directly. The caller is responsible for deallocating the space;
! when the {\tt ESMF\_LocalArray} is destroyed it will not release the space.
! \end{description}
!
! There are 3 options for
! specifying the type/kind/rank of the {\tt ESMF\_LocalArray} data:
! \begin{description}
! \item[List]
! The characteristics of the {\tt ESMF\_LocalArray} are given explicitly
! by individual arguments to the create function.
! \item[ArraySpec]
! A previously created {\tt ESMF\_ArraySpec} object is given which
! describes the characteristics.
! %\item[Fortran array]
! % An existing Fortran array is used to describe the characteristics.
! % (Only available from the Fortran interface.)
! \item[Fortran 90 Pointer]
! An associated or unassociated Fortran 90 array pointer is used to
! describe the array.
! (Only available from the Fortran interface.)
! \end{description}
!
! The concept of an ``empty'' {\tt ESMF\_LocalArray} does not exist. To make an
! ESMF object which stores the Type/Kind/Rank information create an
! {\tt ESMF\_ArraySpec} object which can then be used repeatedly in
! subsequent {\tt ESMF\_LocalArray} Create calls.
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
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_LocalArrayGetDataI21D 
 module procedure ESMF_LocalArrayGetDataI41D 
 module procedure ESMF_LocalArrayGetDataI81D 
 module procedure ESMF_LocalArrayGetDataI22D 
 module procedure ESMF_LocalArrayGetDataI42D 
 module procedure ESMF_LocalArrayGetDataI82D 
 module procedure ESMF_LocalArrayGetDataI23D 
 module procedure ESMF_LocalArrayGetDataI43D 
 module procedure ESMF_LocalArrayGetDataI83D 
 module procedure ESMF_LocalArrayGetDataI24D 
 module procedure ESMF_LocalArrayGetDataI44D 
 module procedure ESMF_LocalArrayGetDataI84D 
 module procedure ESMF_LocalArrayGetDataI25D 
 module procedure ESMF_LocalArrayGetDataI45D 
 module procedure ESMF_LocalArrayGetDataI85D 
 module procedure ESMF_LocalArrayGetDataR41D 
 module procedure ESMF_LocalArrayGetDataR81D 
 module procedure ESMF_LocalArrayGetDataR42D 
 module procedure ESMF_LocalArrayGetDataR82D 
 module procedure ESMF_LocalArrayGetDataR43D 
 module procedure ESMF_LocalArrayGetDataR83D 
 module procedure ESMF_LocalArrayGetDataR44D 
 module procedure ESMF_LocalArrayGetDataR84D 
 module procedure ESMF_LocalArrayGetDataR45D 
 module procedure ESMF_LocalArrayGetDataR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_LocalArrayGetData} functions.
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
! Create a new {\tt ESMF\_LocalArray and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
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
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        type (ESMF_LocalArray) :: array ! new C++ LocalArray
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

        status = ESMF_FAILURE
        rcpresent = .FALSE.
        array%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! TODO: should this take the counts, or not? for now i am going to
        ! set the counts after i have created the f90 array and not here.
        call c_ESMC_LocalArrayCreateNoData(array, rank, type, kind, &
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
! Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
!
! The arguments are:
! \begin{description}
!
! \item[spec]
! ArraySpec object.
!
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
!
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:

        ! Local vars
        type (ESMF_LocalArray) :: array ! new C++ LocalArray
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
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

        call ESMF_LocalArraySpecGet(spec, rank, type, kind, status)
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
! Take a partially created {\tt ESMF\_LocalArray} and T/K/R information and call
! the proper subroutine to create an F90 pointer, allocate space, and set the
! corresponding values in the {\tt ESMF\_LocalArray} object.
!
! The arguments are:
! \begin{description}
!
! \item[array]
! Partially created {\tt ESMF\_LocalArray} object. This entry point is used
! during both the C++ and F90 create calls if we need to create an F90
! pointer to be used later.
!
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
!
! \item[rank]
! Array rank.
! This must match what is already in the array - it is here only as
! a convienience.
!
! \item[type]
! Array type.
! This must match what is already in the array - it is here only as
! a convienience.
!
! \item[kind]
! Array kind.
! This must match what is already in the array - it is here only as
! a convienience.
!
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

        status = ESMF_FAILURE
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif


        ! Call a T/K/R specific interface in order to create the proper
        ! type of F90 pointer, allocate the space, set the values in the
        ! Array object, and return. (The routine this code is calling is
        ! generated by macro.)

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

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI21D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI21D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI21D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI21D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI41D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI41D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI41D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI41D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI81D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI81D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI81D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI81D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI22D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI22D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI22D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI22D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI42D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI42D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI42D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI42D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI82D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI82D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI82D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI82D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI23D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI23D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI23D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI23D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI43D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI43D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI43D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI83D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI83D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI83D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI24D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI24D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI24D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI24D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI44D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI44D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI44D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI84D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI84D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI84D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI25D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI25D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI25D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI25D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI45D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI45D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI45D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrI85D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrI85D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI85D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrI85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR41D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR41D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR41D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R4), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR41D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR81D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR81D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR81D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R8), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR81D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR42D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR42D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR42D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR42D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR82D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR82D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR82D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR82D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR43D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR43D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR43D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR83D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR83D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR83D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR44D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR44D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR44D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR84D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR84D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR84D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR45D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR45D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR45D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArrR85D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArrR85D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "LocalArray cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR85D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArrR85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI21D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI21D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI21D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI21D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI41D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI41D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI41D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI41D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI81D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI81D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI81D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI81D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI22D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI22D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI22D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI22D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI42D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI42D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI42D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI42D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI82D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI82D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI82D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI82D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI23D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI23D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI23D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI23D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI43D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI43D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI43D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI83D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI83D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI83D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI24D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI24D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI24D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI24D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI44D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI44D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI44D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI84D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI84D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI84D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI25D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI25D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI25D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI25D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI45D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI45D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI45D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrI85D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrI85D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrI85D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrI85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR41D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR41D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR41D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 real (ESMF_IKIND_R4), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR41D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR81D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR81D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR81D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 real (ESMF_IKIND_R8), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR81D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR42D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR42D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR42D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR42D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR82D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR82D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR82D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR82D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR43D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR43D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR43D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR83D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR83D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR83D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR44D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR44D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR44D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR84D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR84D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR84D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR45D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR45D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR45D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArrR85D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArrR85D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 !if (.not.allocated(f90arr)) then 
 ! print *, "Array must already be allocated" 
 ! return 
 !endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90PtrR85D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArrR85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI21D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI21D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI21D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI21D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI41D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI41D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI41D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI41D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI81D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI81D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI81D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI81D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI22D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI22D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI22D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI22D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI42D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI42D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI42D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI42D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI82D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI82D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI82D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI82D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI23D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI23D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI23D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI23D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI43D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI43D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI43D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI83D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI83D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI83D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI24D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI24D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI24D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI24D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI44D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI44D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI44D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI84D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI84D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI84D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI25D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI25D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI25D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI25D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI45D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI45D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI45D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrI85D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrI85D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI85D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrI85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR41D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR41D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR41D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR41D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR81D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR81D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR81D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR81D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR42D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR42D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR42D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR42D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR82D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR82D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR82D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR82D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR43D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR43D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR43D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR83D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR83D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR83D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR44D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR44D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR44D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR84D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR84D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR84D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR45D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR45D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR45D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtrR85D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtrR85D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type with space allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An unassociated Fortran 90 array pointer. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 if (associated(f90ptr)) then 
 print *, "Pointer cannot already be allocated" 
 return 
 endif 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR85D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtrR85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 




!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI21D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI21D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI21D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI21D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI41D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI41D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI41D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI41D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI81D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI81D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI81D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI81D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI22D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI22D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI22D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI22D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI42D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI42D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI42D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI42D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI82D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI82D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI82D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI82D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI23D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI23D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI23D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI23D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI43D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI43D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI43D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI83D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI83D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI83D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI24D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI24D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI24D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI24D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI44D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI44D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI44D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI84D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI84D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI84D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI25D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI25D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI25D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI25D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI45D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI45D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI45D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrI85D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrI85D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrI85D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrI85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR41D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR41D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR41D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR41D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR81D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR81D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR81D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(1) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR81D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR42D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR42D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR42D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR42D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR82D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR82D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR82D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(2) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR82D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR43D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR43D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR43D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR83D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR83D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(3) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR83D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR44D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR44D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR44D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR84D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR84D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(4) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR84D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR45D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR45D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR45D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtrR85D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtrR85D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_LocalArray type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
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
 
 ! Local variables 
 type (ESMF_LocalArray) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 type (ESMF_CopyFlag) :: copy ! do we copy or ref? 
 integer, dimension(5) :: counts ! per dim 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Set default for copyflag 
 if (present(docopy)) then 
 copy = docopy 
 else 
 copy = ESMF_DATA_REF 
 endif 
 
 ! TODO: will this work with a statically declared array if I take 
 ! this test out? or will the function signature not match? 
 ! Test to see if array is not already allocated, and fail if so. 
 if (.not.associated(f90ptr)) then 
 print *, "Pointer must already be associated" 
 return 
 endif 
 
 ! Get sizes from current array, although the construct routine 
 ! does not need it for an already allocated array. 
 counts = shape(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90PtrR85D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtrR85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI21D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI21D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI21D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 wrap% I21Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI41D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI41D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI41D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 wrap% I41Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI81D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI81D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI81D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 wrap% I81Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI22D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI22D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI22D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 wrap% I22Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI42D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI42D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI42D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 wrap% I42Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI82D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI82D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI82D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 wrap% I82Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI23D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI23D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI23D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 wrap% I23Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI43D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI43D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI43D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 wrap% I43Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI83D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI83D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI83D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 wrap% I83Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI24D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI24D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI24D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 wrap% I24Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI44D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI44D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI44D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 wrap% I44Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI84D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI84D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI84D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 wrap% I84Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI25D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI25D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI25D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:5) = counts(1:5) 
 strides = 0 
 offsets = 0 
 
 wrap% I25Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI45D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI45D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI45D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:5) = counts(1:5) 
 strides = 0 
 offsets = 0 
 
 wrap% I45Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrI85D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrI85D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapI85D) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:5) = counts(1:5) 
 strides = 0 
 offsets = 0 
 
 wrap% I85Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR41D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR41D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR41D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 wrap% R41Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR81D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR81D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR81D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 wrap% R81Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR42D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR42D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR42D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 wrap% R42Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR82D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR82D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR82D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 wrap% R82Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR43D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR43D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR43D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 wrap% R43Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR83D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR83D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR83D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 wrap% R83Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR44D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR44D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR44D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 wrap% R44Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR84D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR84D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR84D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 wrap% R84Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR45D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR45D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR45D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:5) = counts(1:5) 
 strides = 0 
 offsets = 0 
 
 wrap% R45Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90PtrR85D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90PtrR85D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} 
! object. (This is to save on the total number of nested crossings of the 
! F90/C++ boundary.) 
! 
! Optional args are an existing F90 pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_LocalArray} to set the values into. 
! 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
! !REQUIREMENTS: 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
 type (ESMF_ArrWrapR85D) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds 
 integer, dimension(ESMF_MAXDIM) :: strides, offsets 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_TF_FALSE 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:5) = counts(1:5) 
 strides = 0 
 offsets = 0 
 
 wrap% R85Dptr => newp 
 call c_ESMC_LocalArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90PtrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI21D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(1) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I21Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I21Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI41D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(1) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I41Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I41Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI81D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(1) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I81Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I81Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI22D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(2) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I22Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I22Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI42D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(2) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I42Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I42Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI82D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(2) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I82Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I82Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI23D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(3) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I23Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I23Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI43D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(3) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I43Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I43Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI83D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(3) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I83Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I83Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI24D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(4) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I24Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I24Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI44D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(4) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I44Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I44Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI84D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(4) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I84Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I84Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI25D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(5) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I25Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I25Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI45D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(5) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I45Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I45Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataI85D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(5) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% I85Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% I85Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR41D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(1) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R41Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R41Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR81D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(1) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R81Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R81Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR42D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(2) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R42Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R42Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR82D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(2) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R82Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R82Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR43D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(3) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R43Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R43Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR83D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(3) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R83Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R83Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR44D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(4) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R44Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R44Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR84D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(4) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R84Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R84Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR45D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(5) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R45Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R45Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetDataR85D(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
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
 integer :: rank, counts(5) ! size info for the array 
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
 if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. 
 endif 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_LocalArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap% R85Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap% R85Dptr 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetDataR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI21D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI21D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I21Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI41D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI41D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I41Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI81D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI81D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I81Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI22D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI22D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I22Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI42D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI42D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I42Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI82D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI82D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I82Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI23D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI23D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I23Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI43D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI43D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I43Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI83D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI83D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I83Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI24D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI24D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I24Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI44D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI44D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I44Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI84D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI84D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I84Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI25D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI25D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I25Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI45D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI45D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I45Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateI85D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapI85D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% I85Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR41D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR41D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R41Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR81D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR81D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R81Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR42D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR42D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R42Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR82D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR82D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R82Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR43D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR43D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R43Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR83D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR83D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R83Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR44D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR44D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R44Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR84D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR84D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R84Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR45D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR45D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R45Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocateR85D(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrapR85D) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if LocalArray object is responsible for cleaning up. 
! 
!EOP 
! !REQUIREMENTS: 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap% R85Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocateR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


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
! Releases all resources associated with this {\tt ESMF\_LocalArray}.
!
! The arguments are:
! \begin{description}
!
! \item[array]
! Destroy contents of this {\tt ESMF\_LocalArray}.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
! To reduce the depth of crossings of the F90/C++ boundary we first
! query to see if we are responsible for deleting the data space. If so,
! first deallocate the space and then call the C++ code to release
! the object space. When it returns we are done and can return to the user.
! Otherwise we would need to make a nested call back into F90 from C++ to do
! the deallocate() during the object delete.
!
!EOP
! !REQUIREMENTS:

        ! Local vars
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        logical :: needsdealloc ! do we need to free space?
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
        ! the case of ESMF_DATA_COPY at create time then we delete the
        ! space. otherwise, the user needs to destroy the array
        ! (we will ignore the data) and call deallocate themselves.

        ! Call Destruct first, then free this memory
        call c_ESMC_LocalArrayNeedsDealloc(array, needsdealloc, status)
        if (needsdealloc) then
          call c_ESMC_LocalArrayGetRank(array, rank, status)
          call c_ESMC_LocalArrayGetType(array, type, status)
          call c_ESMC_LocalArrayGetKind(array, kind, status)
          call ESMF_LocalArrayF90Deallocate(array, rank, type, kind, status)
          if (status .ne. ESMF_SUCCESS) then
            print *, "LocalArray contents destruction error"
            return
          endif
          call c_ESMC_LocalArraySetNoDealloc(array, status)
        endif

        ! Calling deallocate first means this will not return back to F90
        ! before returning for good.
        call c_ESMC_LocalArrayDestroy(array, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "LocalArray destruction error"
          return
        endif

! set return code if user specified it
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
! Used only with the version of {\tt ESMF\_LocalArrayCreate} which
! creates an empty {\tt ESMF\_LocalArray} and allows the Data to be
! specified later. Otherwise it is an error to replace the data contents
! associated with a {\tt ESMF\_LocalArray}.
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
! Creates a description of the data -- the type, the dimensionality, etc.
! This specification can be used in an {\tt ESMF\_LocalArrayCreate} call with
! data to create a full {\tt ESMF\_LocalArray}.
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
! {\tt ESMF\_LocalArray} type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
!
! \item[kind]
! {\tt ESMF\_LocalArray} kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
!
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
! Returns information about the {\tt ESMF\_LocalArray}. For queries
! where the caller only wants a single value, specify the argument by name.
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
         call c_ESMC_LocalArrayGetRank(array, rank, status)
         ! TODO: test status
      endif

      if (present(type)) then
         call c_ESMC_LocalArrayGetType(array, type, status)
      endif

      if (present(kind)) then
         call c_ESMC_LocalArrayGetKind(array, kind, status)
      endif

      if (present(counts)) then
         call c_ESMC_LocalArrayGetRank(array, lrank, status)
         call c_ESMC_LocalArrayGetLengths(array, lrank, counts, status)
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
! Returns the name of the {\tt ESMF\_LocalArray}. If the array was
! created without specifying a name, the framework will have assigned it
! a unique one.
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
      !call c_ESMC_LocalArrayGetName(array, name, status)
      !if(status .NE. ESMF_FAILURE) then
      ! print *, "ERROR in ESMF_LocalArrayGetName"
      ! return
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
! Return information about the contents of a {\tt ESMF\_ArraySpec} type.
!
! The arguments are:
! \begin{description}
!
! \item[as]
! An {\tt ESMF\_ArraySpec} object.
!
! \item[rank]
! {\tt ESMF\_LocalArray} rank (dimensionality, 1D, 2D, etc). Maximum
! allowed is 5D.
!
! \item[type]
! {\tt ESMF\_LocalArray} type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
!
! \item[kind]
! {\tt ESMF\_LocalArray} kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
!
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP

        ! Local vars
        integer :: i
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?

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
!!! TODO: the interface now calls ESMF_LocalArrConstrF90Ptr instead of
!!! this routine. It maybe can go away? and can we do something with
!!! ESMF_LocalArrayF90Deallocate to get rid of it as well, so the interfaces
!!! are more symmetric?
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_LocalArrayF90Allocate - Allocate an F90 pointer and set LocalArray info
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
! Allocate data contents for an {\tt ESMF\_LocalArray} created from the
! C++ interface. The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt ESMF\_LocalArray} object.
! \item[rank]
! The {\tt ESMF\_LocalArray} rank.
! \item[type]
! The {\tt ESMF\_LocalArray} type (integer, real/float, etc).
! \item[kind]
! The {\tt ESMF\_LocalArray} kind (short/2, long/8, etc).
! \item[counts]
! An integer array, size {\tt rank}, of each dimension length.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
! !REQUIREMENTS:

    integer :: status ! local error status
    integer, dimension(ESMF_MAXDIM) :: lbounds, ubounds
    integer, dimension(ESMF_MAXDIM) :: strides, offsets
    integer :: localkind, localtype

    !! local variables, expanded by macro
! <Created by macro - do not edit directly > 
 type(ESMF_ArrWrapI21D) :: localI21D 
 type(ESMF_ArrWrapI41D) :: localI41D 
 type(ESMF_ArrWrapI81D) :: localI81D 
 
 type(ESMF_ArrWrapI22D) :: localI22D 
 type(ESMF_ArrWrapI42D) :: localI42D 
 type(ESMF_ArrWrapI82D) :: localI82D 
 
 type(ESMF_ArrWrapI23D) :: localI23D 
 type(ESMF_ArrWrapI43D) :: localI43D 
 type(ESMF_ArrWrapI83D) :: localI83D 
 
 type(ESMF_ArrWrapI24D) :: localI24D 
 type(ESMF_ArrWrapI44D) :: localI44D 
 type(ESMF_ArrWrapI84D) :: localI84D 
 
 type(ESMF_ArrWrapI25D) :: localI25D 
 type(ESMF_ArrWrapI45D) :: localI45D 
 type(ESMF_ArrWrapI85D) :: localI85D 
 
 type(ESMF_ArrWrapR41D) :: localR41D 
 type(ESMF_ArrWrapR81D) :: localR81D 
 
 type(ESMF_ArrWrapR42D) :: localR42D 
 type(ESMF_ArrWrapR82D) :: localR82D 
 
 type(ESMF_ArrWrapR43D) :: localR43D 
 type(ESMF_ArrWrapR83D) :: localR83D 
 
 type(ESMF_ArrWrapR44D) :: localR44D 
 type(ESMF_ArrWrapR84D) :: localR84D 
 
 type(ESMF_ArrWrapR45D) :: localR45D 
 type(ESMF_ArrWrapR85D) :: localR85D 
 



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
! <Created by macro - do not edit directly > 
 allocate(localI21D%I21Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI21D, & 
 localI21D%I21Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI41D%I41Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI41D, & 
 localI41D%I41Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI81D%I81Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI81D, & 
 localI81D%I81Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI22D%I22Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI22D, & 
 localI22D%I22Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI42D%I42Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI42D, & 
 localI42D%I42Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI82D%I82Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI82D, & 
 localI82D%I82Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI23D%I23Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI23D, & 
 localI23D%I23Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI43D%I43Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI43D, & 
 localI43D%I43Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI83D%I83Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI83D, & 
 localI83D%I83Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI24D%I24Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI24D, & 
 localI24D%I24Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI44D%I44Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI44D, & 
 localI44D%I44Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(localI84D%I84Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localI84D, & 
 localI84D%I84Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case default
        end select

       case (ESMF_DATA_REAL%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(localR41D%R41Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localR41D, & 
 localR41D%R41Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(localR81D%R81Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:1) = counts(1:1) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localR81D, & 
 localR81D%R81Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(localR42D%R42Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localR42D, & 
 localR42D%R42Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(localR82D%R82Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:2) = counts(1:2) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localR82D, & 
 localR82D%R82Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(localR43D%R43Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localR43D, & 
 localR43D%R43Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(localR83D%R83Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:3) = counts(1:3) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localR83D, & 
 localR83D%R83Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(localR44D%R44Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localR44D, & 
 localR44D%R44Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_KIND_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(localR84D%R84Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever 
 ! and set them in the array object. For now, used fixed values. 
 lbounds = 1 
 ubounds = 1 
 ubounds(1:4) = counts(1:4) 
 strides = 0 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInfo(array, localR84D, & 
 localR84D%R84Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

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
! !IROUTINE: ESMF_LocalArrayF90Deallocate - Deallocate an F90 pointer
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
! Deallocate data contents for an {\tt ESMF\_LocalArray} created from
! the C++ interface. The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt ESMF\_LocalArray} object.
! \item[rank]
! The {\tt ESMF\_LocalArray} rank.
! \item[type]
! The {\tt ESMF\_LocalArray} type (integer, real/float, etc).
! \item[kind]
! The {\tt ESMF\_LocalArray} kind (short/2, long/8, etc).
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:

    integer :: status ! local error status
    integer :: localkind, localtype

    !! local variables, expanded by macro
! <Created by macro - do not edit directly > 
 type(ESMF_ArrWrapI21D) :: localI21D 
 type(ESMF_ArrWrapI41D) :: localI41D 
 type(ESMF_ArrWrapI81D) :: localI81D 
 
 type(ESMF_ArrWrapI22D) :: localI22D 
 type(ESMF_ArrWrapI42D) :: localI42D 
 type(ESMF_ArrWrapI82D) :: localI82D 
 
 type(ESMF_ArrWrapI23D) :: localI23D 
 type(ESMF_ArrWrapI43D) :: localI43D 
 type(ESMF_ArrWrapI83D) :: localI83D 
 
 type(ESMF_ArrWrapI24D) :: localI24D 
 type(ESMF_ArrWrapI44D) :: localI44D 
 type(ESMF_ArrWrapI84D) :: localI84D 
 
 type(ESMF_ArrWrapI25D) :: localI25D 
 type(ESMF_ArrWrapI45D) :: localI45D 
 type(ESMF_ArrWrapI85D) :: localI85D 
 
 type(ESMF_ArrWrapR41D) :: localR41D 
 type(ESMF_ArrWrapR81D) :: localR81D 
 
 type(ESMF_ArrWrapR42D) :: localR42D 
 type(ESMF_ArrWrapR82D) :: localR82D 
 
 type(ESMF_ArrWrapR43D) :: localR43D 
 type(ESMF_ArrWrapR83D) :: localR83D 
 
 type(ESMF_ArrWrapR44D) :: localR44D 
 type(ESMF_ArrWrapR84D) :: localR84D 
 
 type(ESMF_ArrWrapR45D) :: localR45D 
 type(ESMF_ArrWrapR85D) :: localR85D 
 



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
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI21D, status) 
 deallocate(localI21D%I21Dptr, stat=status) 
 nullify(localI21D%I21Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI41D, status) 
 deallocate(localI41D%I41Dptr, stat=status) 
 nullify(localI41D%I41Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI81D, status) 
 deallocate(localI81D%I81Dptr, stat=status) 
 nullify(localI81D%I81Dptr) 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI22D, status) 
 deallocate(localI22D%I22Dptr, stat=status) 
 nullify(localI22D%I22Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI42D, status) 
 deallocate(localI42D%I42Dptr, stat=status) 
 nullify(localI42D%I42Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI82D, status) 
 deallocate(localI82D%I82Dptr, stat=status) 
 nullify(localI82D%I82Dptr) 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI23D, status) 
 deallocate(localI23D%I23Dptr, stat=status) 
 nullify(localI23D%I23Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI43D, status) 
 deallocate(localI43D%I43Dptr, stat=status) 
 nullify(localI43D%I43Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI83D, status) 
 deallocate(localI83D%I83Dptr, stat=status) 
 nullify(localI83D%I83Dptr) 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_KIND_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI24D, status) 
 deallocate(localI24D%I24Dptr, stat=status) 
 nullify(localI24D%I24Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI44D, status) 
 deallocate(localI44D%I44Dptr, stat=status) 
 nullify(localI44D%I44Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localI84D, status) 
 deallocate(localI84D%I84Dptr, stat=status) 
 nullify(localI84D%I84Dptr) 
! < End macro - do not edit directly > 

              case default
            end select

          case default
        end select

       case (ESMF_DATA_REAL%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localR41D, status) 
 deallocate(localR41D%R41Dptr, stat=status) 
 nullify(localR41D%R41Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localR81D, status) 
 deallocate(localR81D%R81Dptr, stat=status) 
 nullify(localR81D%R81Dptr) 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localR42D, status) 
 deallocate(localR42D%R42Dptr, stat=status) 
 nullify(localR42D%R42Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localR82D, status) 
 deallocate(localR82D%R82Dptr, stat=status) 
 nullify(localR82D%R82Dptr) 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localR43D, status) 
 deallocate(localR43D%R43Dptr, stat=status) 
 nullify(localR43D%R43Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localR83D, status) 
 deallocate(localR83D%R83Dptr, stat=status) 
 nullify(localR83D%R83Dptr) 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_KIND_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localR44D, status) 
 deallocate(localR44D%R44Dptr, stat=status) 
 nullify(localR44D%R44Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_KIND_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, localR84D, status) 
 deallocate(localR84D%R84Dptr, stat=status) 
 nullify(localR84D%R84Dptr) 
! < End macro - do not edit directly > 

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
      character (len = *), intent(in) :: name ! array name to restore
      type(ESMF_IOSpec), intent(in), optional :: iospec ! file specs
      integer, intent(out), optional :: rc ! return code
!
! !DESCRIPTION:
! Used to reinitialize all data associated with a {\tt ESMF\_LocalArray}
! from the last call to WriteRestart.
!
!EOP
! !REQUIREMENTS:

!
! TODO: code goes here
!
        type (ESMF_LocalArray) :: a

! this is just to shut the compiler up
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
! Used to write data to persistent storage in a variety of formats.
! (see WriteRestart/ReadRestart for quick data dumps.) Details of I/O
! options specified in the IOSpec derived type.
!
!
!EOP
! !REQUIREMENTS:

       character (len=16) :: defaultopts ! default write options
       character (len=16) :: defaultfile ! default filename
       integer :: status ! local error status
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
           call c_ESMC_LocalArrayWrite(array, defaultopts, trim(filename), status)
       else
           call c_ESMC_LocalArrayWrite(array, defaultopts, trim(defaultfile), status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "LocalArray write error"
         return
       endif

! set return values
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
        type (ESMF_LocalArray) :: a

! this is just to shut the compiler up
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
! Routine to print information about a {\tt ESMF\_LocalArray}.
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
           print *, "LocalArray not initialized or Destroyed"
           return
       endif

       if(present(options)) then
           !call c_ESMC_LocalArrayValidate(array, options, status)
       else
           !call c_ESMC_LocalArrayValidate(array, defaultopts, status)
       endif

       !if (status .ne. ESMF_SUCCESS) then
       ! print *, "LocalArray validate error"
       ! return
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
! Routine to print information about a {\tt ESMF\_LocalArray}.
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
           call c_ESMC_LocalArrayPrint(array, options, status)
       else
           call c_ESMC_LocalArrayPrint(array, defaultopts, status)
       endif

       if (status .ne. ESMF_SUCCESS) then
         print *, "LocalArray print error"
         return
       endif

! set return values
       if (rcpresent) rc = ESMF_SUCCESS

       end subroutine ESMF_LocalArrayPrint


        end module ESMF_LocalArrayMod
