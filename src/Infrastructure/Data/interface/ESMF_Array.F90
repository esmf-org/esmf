! $Id: ESMF_Array.F90,v 1.52 2003/04/24 16:45:37 nscollins Exp $
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
! < ignore blank lines below. they are created by the files which
! define various macros. >
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
      use ESMF_DELayoutMod
      implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
! ! ESMF_CopyFlag
!
! ! Indicates whether a data array should be copied or referenced.
! ! TODO: Should this be moved down to the base class? Is it useful
! ! anyplace else outside of the Array context?
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
! ! ESMF_ArrayOrigin
!
! ! Private flag which indicates the create was initiated on the F90 side.
! ! This matches an enum on the C++ side and the values must match.
! ! Update ../include/ESMC_Array.h if you change these values.
      type ESMF_ArrayOrigin
      sequence
      private
        integer :: origin
      end type
      type(ESMF_ArrayOrigin), parameter :: &
                            ESMF_FROM_FORTRAN = ESMF_ArrayOrigin(1), &
                            ESMF_FROM_CPLUSPLUS = ESMF_ArrayOrigin(2)
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
      public ESMF_ArrayRedist, ESMF_ArrayHalo, ESMF_ArrayAllGather
      public ESMF_ArrayGet, ESMF_ArrayGetName
      public ESMF_ArrayF90Allocate
      public ESMF_ArrayF90Deallocate
      public ESMF_ArrayConstructF90Ptr ! needed for C++ callback only
      public ESMF_ArrayCheckpoint
      public ESMF_ArrayRestore
      public ESMF_ArrayWrite
      public ESMF_ArrayRead
      public ESMF_ArrayValidate
      public ESMF_ArrayPrint
!EOP
      public operator(.eq.), operator(.ne.)
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_Array.F90,v 1.52 2003/04/24 16:45:37 nscollins Exp $'
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
        module procedure ESMF_ArrayCreateByList ! specify TKR
        module procedure ESMF_ArrayCreateBySpec ! specify ArraySpec
! ! < interfaces for each T/K/R >
! --Array--InterfaceMacro(ArrayCreateByMTArr)
!
! ! < interfaces for each T/K/R >
! --Array--InterfaceMacro(ArrayCreateByFullArr)
       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_ArrayCreateByMTPtrI21D 
 module procedure ESMF_ArrayCreateByMTPtrI41D 
 module procedure ESMF_ArrayCreateByMTPtrI81D 
 module procedure ESMF_ArrayCreateByMTPtrI22D 
 module procedure ESMF_ArrayCreateByMTPtrI42D 
 module procedure ESMF_ArrayCreateByMTPtrI82D 
 module procedure ESMF_ArrayCreateByMTPtrI23D 
 module procedure ESMF_ArrayCreateByMTPtrI43D 
 module procedure ESMF_ArrayCreateByMTPtrI83D 
 module procedure ESMF_ArrayCreateByMTPtrI24D 
 module procedure ESMF_ArrayCreateByMTPtrI44D 
 module procedure ESMF_ArrayCreateByMTPtrI84D 
 module procedure ESMF_ArrayCreateByMTPtrI25D 
 module procedure ESMF_ArrayCreateByMTPtrI45D 
 module procedure ESMF_ArrayCreateByMTPtrI85D 
 module procedure ESMF_ArrayCreateByMTPtrR41D 
 module procedure ESMF_ArrayCreateByMTPtrR81D 
 module procedure ESMF_ArrayCreateByMTPtrR42D 
 module procedure ESMF_ArrayCreateByMTPtrR82D 
 module procedure ESMF_ArrayCreateByMTPtrR43D 
 module procedure ESMF_ArrayCreateByMTPtrR83D 
 module procedure ESMF_ArrayCreateByMTPtrR44D 
 module procedure ESMF_ArrayCreateByMTPtrR84D 
 module procedure ESMF_ArrayCreateByMTPtrR45D 
 module procedure ESMF_ArrayCreateByMTPtrR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_ArrayCreateByFullPtrI21D 
 module procedure ESMF_ArrayCreateByFullPtrI41D 
 module procedure ESMF_ArrayCreateByFullPtrI81D 
 module procedure ESMF_ArrayCreateByFullPtrI22D 
 module procedure ESMF_ArrayCreateByFullPtrI42D 
 module procedure ESMF_ArrayCreateByFullPtrI82D 
 module procedure ESMF_ArrayCreateByFullPtrI23D 
 module procedure ESMF_ArrayCreateByFullPtrI43D 
 module procedure ESMF_ArrayCreateByFullPtrI83D 
 module procedure ESMF_ArrayCreateByFullPtrI24D 
 module procedure ESMF_ArrayCreateByFullPtrI44D 
 module procedure ESMF_ArrayCreateByFullPtrI84D 
 module procedure ESMF_ArrayCreateByFullPtrI25D 
 module procedure ESMF_ArrayCreateByFullPtrI45D 
 module procedure ESMF_ArrayCreateByFullPtrI85D 
 module procedure ESMF_ArrayCreateByFullPtrR41D 
 module procedure ESMF_ArrayCreateByFullPtrR81D 
 module procedure ESMF_ArrayCreateByFullPtrR42D 
 module procedure ESMF_ArrayCreateByFullPtrR82D 
 module procedure ESMF_ArrayCreateByFullPtrR43D 
 module procedure ESMF_ArrayCreateByFullPtrR83D 
 module procedure ESMF_ArrayCreateByFullPtrR44D 
 module procedure ESMF_ArrayCreateByFullPtrR84D 
 module procedure ESMF_ArrayCreateByFullPtrR45D 
 module procedure ESMF_ArrayCreateByFullPtrR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

! !DESCRIPTION:
! This interface provides a single (heavily overloaded) entry point for
! the various types of {\tt ESMF\_ArrayCreate} functions.
!
! There are 3 options for setting the contents of the {\tt ESMF\_Array}
! at creation time:
! \begin{description}
! \item[Allocate Space Only]
! Data space is allocated but not initialized. The caller can query
! for a pointer to the start of the space to address it directly.
! The caller must not deallocate the space; the
! {\tt ESMF\_Array} will release the space when it is destroyed.
! \item[Data Copy]
! An existing Fortran array is specified and the data contents are copied
! into new space allocated by the {\tt ESMF\_Array}.
! The caller must not deallocate the space; the
! {\tt ESMF\_Array} will release the space when it is destroyed.
! \item[Data Reference]
! An existing Fortran array is specified and the data contents reference
! it directly. The caller is responsible for deallocating the space;
! when the {\tt ESMF\_Array} is destroyed it will not release the space.
! \end{description}
!
! There are 4 options for
! specifying the type/kind/rank of the {\tt ESMF\_Array} data:
! \begin{description}
! \item[List]
! The characteristics of the {\tt ESMF\_Array} are given explicitly
! by individual arguments to the create function.
! \item[ArraySpec]
! A previously created {\tt ESMF\_ArraySpec} object is given which
! describes the characteristics.
! \item[Fortran array]
! An existing Fortran array is used to describe the characteristics.
! (Only available from the Fortran interface.)
! \item[Fortran 90 Pointer]
! An associated or unassociated Fortran 90 array pointer is used to
! describe the array.
! (Only available from the Fortran interface.)
! \end{description}
!
! The concept of an ``empty'' {\tt Array} does not exist. To make an
! ESMF object which stores the Type/Kind/Rank information create an
! {\tt ESMF\_ArraySpec} object which can then be used repeatedly in
! subsequent {\tt Array} Create calls.
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
      ! < declarations of interfaces for each T/K/R >
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

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_ArrayGetData} functions.
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
! Create a new Array and allocate data space, which remains uninitialized.
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
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!
! \end{description}
!
!EOP
! !REQUIREMENTS:
        ! Local vars
        type (ESMF_Array) :: array ! new C++ Array
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
! Create a new Array and allocate data space, which remains uninitialized.
! The return value is a new Array.
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
        type (ESMF_Array) :: array ! new C++ Array
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
! Take a partially created {\tt Array} and T/K/R information and call the
! proper subroutine to create an F90 pointer, allocate space, and set the
! corresponding values in the {\tt Array} object.
!
! The arguments are:
! \begin{description}
!
! \item[array]
! Partially created {\tt ESMF\_Array} object. This entry point is used
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
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI21D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI21D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI21D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI21D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI41D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI41D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI41D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI41D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI81D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI81D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI81D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI81D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI22D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI22D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI22D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI22D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI42D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI42D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI42D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI42D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI82D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI82D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI82D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI82D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI23D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI23D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI23D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI23D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI43D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI43D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI43D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI83D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI83D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI83D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI24D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI24D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI24D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI24D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI44D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI44D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI44D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI84D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI84D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI84D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI25D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI25D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI25D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI25D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI45D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI45D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI45D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrI85D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrI85D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI85D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrI85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR41D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR41D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR41D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR41D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR81D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR81D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR81D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR81D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR42D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR42D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR42D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR42D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR82D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR82D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR82D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR82D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR43D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR43D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR43D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR83D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR83D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR83D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR44D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR44D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR44D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR84D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR84D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR84D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR45D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR45D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR45D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTArrR85D - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArrR85D(f90arr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR85D(array, counts, newp,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArrR85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI21D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI21D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI21D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI21D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI41D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI41D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI41D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI41D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI81D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI81D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI81D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI81D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI22D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI22D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI22D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI22D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI42D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI42D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI42D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI42D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI82D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI82D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI82D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI82D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI23D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI23D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI23D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI23D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI43D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI43D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI43D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI83D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI83D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI83D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI24D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI24D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI24D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI24D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI44D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI44D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI44D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI84D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI84D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI84D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI25D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI25D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI25D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI25D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI45D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI45D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI45D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrI85D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrI85D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrI85D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrI85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR41D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR41D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR41D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR41D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR81D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR81D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR81D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR81D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR42D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR42D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR42D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR42D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR82D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR82D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR82D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR82D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR43D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR43D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR43D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR83D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR83D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR83D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR44D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR44D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR44D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR84D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR84D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR84D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR45D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR45D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR45D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullArrR85D - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArrR85D(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_IKIND_R8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 
! 90 array. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocated Fortran 90 array. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90PtrR85D(array, counts, newp,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArrR85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI21D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI21D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI21D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI21D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI41D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI41D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI41D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI41D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI81D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI81D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI81D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI81D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI22D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI22D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI22D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI22D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI42D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI42D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI42D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI42D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI82D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI82D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI82D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI82D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI23D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI23D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI23D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI23D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI43D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI43D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI43D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI83D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI83D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI83D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI24D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI24D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI24D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI24D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI44D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI44D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI44D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI84D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI84D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI84D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI25D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI25D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI25D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI25D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI45D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI45D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI45D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrI85D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrI85D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI85D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrI85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR41D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR41D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR41D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR41D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR81D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR81D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR81D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR81D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR42D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR42D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR42D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR42D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR82D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR82D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR82D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR82D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR43D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR43D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR43D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR83D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR83D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR83D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR44D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR44D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR44D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR84D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR84D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR84D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR45D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR45D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR45D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByMTPtrR85D - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtrR85D(f90ptr, counts, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. 
! This routine allocates memory to the array pointer and fills in 
! the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type with space allocated for data. 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR85D(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtrR85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI21D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI21D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI21D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI21D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI21D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI41D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI41D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI41D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI41D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI81D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI81D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI81D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI81D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI22D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI22D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI22D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI22D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI22D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI42D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI42D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI42D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI42D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI82D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI82D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI82D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI82D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI23D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI23D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI23D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI23D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI23D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI43D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI43D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI43D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI43D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI83D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI83D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI83D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI83D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI24D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI24D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI24D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI24D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI24D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI44D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI44D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI44D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI44D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI84D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI84D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI84D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI84D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI25D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI25D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI25D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI25D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI25D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI45D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI45D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI45D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI45D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrI85D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrI85D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrI85D 
! 
! !ARGUMENTS: 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_KIND_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrI85D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrI85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR41D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR41D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR41D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR41D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR41D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR81D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR81D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR81D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR81D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR81D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR42D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR42D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR42D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR42D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR42D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR82D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR82D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR82D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR82D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR82D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR43D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR43D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR43D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR43D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR43D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR83D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR83D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR83D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR83D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR83D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR44D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR44D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR44D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR44D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR44D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR84D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR84D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR84D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR84D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR84D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR45D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR45D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR45D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR45D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR45D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_ArrayCreateByFullPtrR85D - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtrR85D(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtrR85D 
! 
! !ARGUMENTS: 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an already allocated Fortran 90 array 
! pointer. This routine can make a copy or reference the existing data 
! and fills in the array object with all necessary information. 
! 
! The function return is an ESMF\_Array type. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90ptr] 
! An allocated Fortran 90 array pointer. 
! 
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
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
 type (ESMF_Array) :: array ! new array object 
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
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_KIND_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90PtrR85D(array, counts, f90ptr,& 
 copy, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtrR85D = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtrR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!! < start of macros which become actual function bodies after expansion >
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI21D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI21D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I21Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI41D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI41D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I41Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI81D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI81D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I81Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI22D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI22D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I22Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI42D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI42D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I42Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI82D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI82D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I82Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI23D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI23D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I23Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI43D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI43D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I43Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI83D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI83D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I83Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI24D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI24D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I24Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI44D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI44D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I44Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI84D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI84D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I84Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI25D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI25D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I2), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I25Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI45D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI45D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I4), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I45Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrI85D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrI85D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_IKIND_I8), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%I85Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR41D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR41D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R41Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR81D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR81D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R81Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR42D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR42D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R42Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR82D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR82D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R82Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR43D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR43D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R43Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR83D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR83D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R83Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR44D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR44D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R44Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR84D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR84D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R84Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR45D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR45D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R4), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R45Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90PtrR85D - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90PtrR85D(array, counts, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_IKIND_R8), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an F90 Pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
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
! The {\tt ESMF\_Array} to set the values into. 
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
 willalloc = .true. 
 willcopy = .false. 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 willalloc = .true. 
 willcopy = .true. 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 endif 
 endif 
 
 if (willalloc) then 
 allocate(newp ( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
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
 
 wrap%R85Dptr => newp 
 call c_ESMC_ArraySetInfo(array, wrap, newp ( 1,1,1,1,1 ), counts, & 
 lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90PtrR85D 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I21Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I21Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I41Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I41Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I81Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I81Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I22Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I22Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I42Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I42Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I82Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I82Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I23Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I23Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I43Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I43Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I83Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I83Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I24Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I24Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I44Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I44Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I84Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I84Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I25Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I25Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I45Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I45Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%I85Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%I85Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R41Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R41Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 1, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R81Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R81Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R42Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R42Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 2, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R82Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R82Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R43Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R43Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 3, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R83Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R83Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R44Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R44Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 4, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R84Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R84Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R45Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R45Dptr 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
 return 
 endif 
 
 ! Allocate a new buffer if requested and return a copy 
 if (copyreq) then 
 call c_ESMC_ArrayGetLengths(array, 5, counts, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( counts(1), counts(2), counts(3), counts(4), counts(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%R85Dptr 
 f90ptr => localp 
 else 
 f90ptr => wrap%R85Dptr 
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
 deallocate(wrap%I21Dptr) 
 
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
 deallocate(wrap%I41Dptr) 
 
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
 deallocate(wrap%I81Dptr) 
 
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
 deallocate(wrap%I22Dptr) 
 
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
 deallocate(wrap%I42Dptr) 
 
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
 deallocate(wrap%I82Dptr) 
 
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
 deallocate(wrap%I23Dptr) 
 
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
 deallocate(wrap%I43Dptr) 
 
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
 deallocate(wrap%I83Dptr) 
 
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
 deallocate(wrap%I24Dptr) 
 
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
 deallocate(wrap%I44Dptr) 
 
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
 deallocate(wrap%I84Dptr) 
 
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
 deallocate(wrap%I25Dptr) 
 
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
 deallocate(wrap%I45Dptr) 
 
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
 deallocate(wrap%I85Dptr) 
 
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
 deallocate(wrap%R41Dptr) 
 
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
 deallocate(wrap%R81Dptr) 
 
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
 deallocate(wrap%R42Dptr) 
 
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
 deallocate(wrap%R82Dptr) 
 
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
 deallocate(wrap%R43Dptr) 
 
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
 deallocate(wrap%R83Dptr) 
 
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
 deallocate(wrap%R44Dptr) 
 
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
 deallocate(wrap%R84Dptr) 
 
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
 deallocate(wrap%R45Dptr) 
 
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
 deallocate(wrap%R85Dptr) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocateR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!! < end of automatically generated function >
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
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
        ! before returning for good.
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
      subroutine ESMF_ArrayAllGather(array, layout, decompids, &
                                     AI_exc, AI_tot, array_out, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(in) :: array
      type(ESMF_DELayout), intent(in) :: layout
      integer, dimension(:), intent(in) :: decompids
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_exc
      type(ESMF_AxisIndex), dimension(:), intent(inout) :: AI_tot
      type(ESMF_Array), intent(out) :: array_out
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used to gather a distributed Array into a global Array on all DEs.
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
! This specification can be
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
      type(ESMF_Array) :: array
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
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: as
      integer, intent(out), optional :: rank
      type(ESMF_DataType), intent(out), optional :: type
      type(ESMF_DataKind), intent(out), optional :: kind
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
        end subroutine ESMF_ArraySpecGet
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is Allocate/Deallocate for Arrays
!
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!!! TODO: the interface now calls ESMF_ArrayConstructF90Ptr instead of
!!! this routine. It maybe can go away? and can we do something with
!!! ESMF_ArrayF90Deallocate to get rid of it as well, so the interfaces
!!! are more symmetric?
!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayF90Allocate - Allocate an F90 pointer and set Array info
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
! Allocate data contents for an array created from the C++ interface.
! The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt Array} object.
! \item[rank]
! The {\tt Array} rank.
! \item[type]
! The {\tt Array} type (integer, real/float, etc).
! \item[kind]
! The {\tt Array} kind (short/2, long/8, etc).
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
    !! macros which are expanded by the preprocessor
    select case (type%dtype)
      case (ESMF_DATA_INTEGER%dtype)
        select case (rank)
          case (1)
            select case (kind%dkind)
              case (ESMF_IKIND_I2)
! <Created by macro - do not edit directly > 
 allocate(localI21D%I21Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI21D, & 
 localI21D%I21Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 allocate(localI41D%I41Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI41D, & 
 localI41D%I41Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 allocate(localI81D%I81Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI81D, & 
 localI81D%I81Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select
          case (2)
            select case (kind%dkind)
              case (ESMF_IKIND_I2)
! <Created by macro - do not edit directly > 
 allocate(localI22D%I22Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI22D, & 
 localI22D%I22Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 allocate(localI42D%I42Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI42D, & 
 localI42D%I42Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 allocate(localI82D%I82Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI82D, & 
 localI82D%I82Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select
          case (3)
            select case (kind%dkind)
              case (ESMF_IKIND_I2)
! <Created by macro - do not edit directly > 
 allocate(localI23D%I23Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI23D, & 
 localI23D%I23Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 allocate(localI43D%I43Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI43D, & 
 localI43D%I43Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 allocate(localI83D%I83Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI83D, & 
 localI83D%I83Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select
          case (4)
            select case (kind%dkind)
              case (ESMF_IKIND_I2)
! <Created by macro - do not edit directly > 
 allocate(localI24D%I24Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI24D, & 
 localI24D%I24Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 allocate(localI44D%I44Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI44D, & 
 localI44D%I44Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 allocate(localI84D%I84Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localI84D, & 
 localI84D%I84Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
            select case (kind%dkind)
              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 allocate(localR41D%R41Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localR41D, & 
 localR41D%R41Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 allocate(localR81D%R81Dptr( counts(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localR81D, & 
 localR81D%R81Dptr( 1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select
          case (2)
            select case (kind%dkind)
              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 allocate(localR42D%R42Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localR42D, & 
 localR42D%R42Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 allocate(localR82D%R82Dptr( counts(1), counts(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localR82D, & 
 localR82D%R82Dptr( 1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select
          case (3)
            select case (kind%dkind)
              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 allocate(localR43D%R43Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localR43D, & 
 localR43D%R43Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 allocate(localR83D%R83Dptr( counts(1), counts(2), counts(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localR83D, & 
 localR83D%R83Dptr( 1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select
          case (4)
            select case (kind%dkind)
              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 allocate(localR44D%R44Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localR44D, & 
 localR44D%R44Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 allocate(localR84D%R84Dptr( counts(1), counts(2), counts(3), counts(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
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
 
 call c_ESMC_ArraySetInfo(array, localR84D, & 
 localR84D%R84Dptr( 1,1,1,1 ), & 
 counts, lbounds, ubounds, strides, offsets, & 
 ESMF_TF_TRUE, ESMF_TF_TRUE) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
     end subroutine ESMF_ArrayF90Allocate
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayF90Deallocate - Deallocate an F90 pointer
!
! !INTERFACE:
     subroutine ESMF_ArrayF90Deallocate(array, rank, type, kind, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      integer :: rank
      type(ESMF_DataType) :: type
      type(ESMF_DataKind) :: kind
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Deallocate data contents for an array created from the C++ interface.
! The arguments are:
! \begin{description}
! \item[array]
! A partially created {\tt Array} object.
! \item[rank]
! The {\tt Array} rank.
! \item[type]
! The {\tt Array} type (integer, real/float, etc).
! \item[kind]
! The {\tt Array} kind (short/2, long/8, etc).
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:
    integer :: status ! local error status
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
    !! macros which are expanded by the preprocessor
    select case (type%dtype)
      case (ESMF_DATA_INTEGER%dtype)
        select case (rank)
          case (1)
            select case (kind%dkind)
              case (ESMF_IKIND_I2)
! <Created by macro - do not edit directly > 
 deallocate(localI21D%I21Dptr, stat=status) 
 nullify(localI21D%I21Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 deallocate(localI41D%I41Dptr, stat=status) 
 nullify(localI41D%I41Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 deallocate(localI81D%I81Dptr, stat=status) 
 nullify(localI81D%I81Dptr) 
! < End macro - do not edit directly > 

              case default
            end select
          case (2)
            select case (kind%dkind)
              case (ESMF_IKIND_I2)
! <Created by macro - do not edit directly > 
 deallocate(localI22D%I22Dptr, stat=status) 
 nullify(localI22D%I22Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 deallocate(localI42D%I42Dptr, stat=status) 
 nullify(localI42D%I42Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 deallocate(localI82D%I82Dptr, stat=status) 
 nullify(localI82D%I82Dptr) 
! < End macro - do not edit directly > 

              case default
            end select
          case (3)
            select case (kind%dkind)
              case (ESMF_IKIND_I2)
! <Created by macro - do not edit directly > 
 deallocate(localI23D%I23Dptr, stat=status) 
 nullify(localI23D%I23Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 deallocate(localI43D%I43Dptr, stat=status) 
 nullify(localI43D%I43Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 deallocate(localI83D%I83Dptr, stat=status) 
 nullify(localI83D%I83Dptr) 
! < End macro - do not edit directly > 

              case default
            end select
          case (4)
            select case (kind%dkind)
              case (ESMF_IKIND_I2)
! <Created by macro - do not edit directly > 
 deallocate(localI24D%I24Dptr, stat=status) 
 nullify(localI24D%I24Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 deallocate(localI44D%I44Dptr, stat=status) 
 nullify(localI44D%I44Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
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
            select case (kind%dkind)
              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 deallocate(localR41D%R41Dptr, stat=status) 
 nullify(localR41D%R41Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 deallocate(localR81D%R81Dptr, stat=status) 
 nullify(localR81D%R81Dptr) 
! < End macro - do not edit directly > 

              case default
            end select
          case (2)
            select case (kind%dkind)
              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 deallocate(localR42D%R42Dptr, stat=status) 
 nullify(localR42D%R42Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 deallocate(localR82D%R82Dptr, stat=status) 
 nullify(localR82D%R82Dptr) 
! < End macro - do not edit directly > 

              case default
            end select
          case (3)
            select case (kind%dkind)
              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 deallocate(localR43D%R43Dptr, stat=status) 
 nullify(localR43D%R43Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
 deallocate(localR83D%R83Dptr, stat=status) 
 nullify(localR83D%R83Dptr) 
! < End macro - do not edit directly > 

              case default
            end select
          case (4)
            select case (kind%dkind)
              case (ESMF_IKIND_I4)
! <Created by macro - do not edit directly > 
 deallocate(localR44D%R44Dptr, stat=status) 
 nullify(localR44D%R44Dptr) 
! < End macro - do not edit directly > 

              case (ESMF_IKIND_I8)
! <Created by macro - do not edit directly > 
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
      subroutine ESMF_ArrayWrite(array, iospec, filename, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      type(ESMF_IOSpec), intent(in), optional :: iospec
      character(len=*), intent(in), optional :: filename
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
           call c_ESMC_ArrayWrite(array, defaultopts, trim(filename), status)
       else
           call c_ESMC_ArrayWrite(array, defaultopts, trim(defaultfile), status)
       endif
       if (status .ne. ESMF_SUCCESS) then
         print *, "Array write error"
         return
       endif
! set return values
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
