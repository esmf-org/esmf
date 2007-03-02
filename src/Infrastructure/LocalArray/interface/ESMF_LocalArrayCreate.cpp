! $Id: ESMF_LocalArrayCreate.cpp,v 1.11 2007/03/02 22:58:21 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
^define ESMF_FILENAME "ESMF_LocalArrayCreate.F90"
!==============================================================================
!
! ESMF LocalArrayCreate module
module ESMF_LocalArrayCreateMod
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
^include "ESMF.h"
#include "ESMF_TypeKindRankMacros.hcppF90"
#include "ESMF_LocalArrayMacros.h"
#include "ESMF_LocalAllocMacros.h"

!------------------------------------------------------------------------------
!BOPI
! !MODULE: ESMF_LocalArrayCreateMod - Manage data uniformly between F90 and C++
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
  use ESMF_UtilTypesMod     ! ESMF utility types
  use ESMF_InitMacrosMod    ! ESMF initializer macros
  use ESMF_BaseMod          ! ESMF base class
  use ESMF_LogErrMod        ! ESMF error handling
  use ESMF_IOSpecMod
  use ESMF_ArraySpecMod

  implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
  private
!------------------------------------------------------------------------------
  ! ESMF_CopyFlag

  ! Indicates whether a data array should be copied or referenced. 
  !  This matches an enum on the C++ side and the values must match.
  !  Update ../include/ESMC_LocalArray.h if you change these values.

  type ESMF_CopyFlag
  sequence
  private
    integer :: docopy
  end type

  type(ESMF_CopyFlag), parameter :: & 
    ESMF_DATA_COPY  = ESMF_CopyFlag(1), &
    ESMF_DATA_REF   = ESMF_CopyFlag(2), &
    ESMF_DATA_DEFER = ESMF_CopyFlag(3), &
    ESMF_DATA_SPACE = ESMF_CopyFlag(4), &
    ESMF_DATA_NONE  = ESMF_CopyFlag(5)    ! this value is _not_ public

!------------------------------------------------------------------------------
  ! ESMF_LocalArrayOrigin

  ! Private flag which indicates the create was initiated on the F90 side.
  !  This matches an enum on the C++ side and the values must match.
  !  Update ../include/ESMC_LocalArray.h if you change these values.

  type ESMF_LocalArrayOrigin
  sequence
  private
    integer :: origin
  end type

  type(ESMF_LocalArrayOrigin), parameter :: & 
    ESMF_FROM_FORTRAN   = ESMF_LocalArrayOrigin(1), &
    ESMF_FROM_CPLUSPLUS = ESMF_LocalArrayOrigin(2)

!------------------------------------------------------------------------------
  ! ESMF_LocalArray
 
  ! LocalArray data type.  All information is kept on the C++ side inside
  ! the class structure.

  type ESMF_LocalArray
  sequence
  !private
    type(ESMF_Pointer) :: this
    ESMF_INIT_DECLARE
  end type

!------------------------------------------------------------------------------
  ! Internal wrapper structures for passing f90 pointers to C++ and
  ! guaranteeing they are passed by reference on all compilers and all
  ! platforms.  These are never seen outside this module.

  ! < these expand into defined type declarations >
AllTypesMacro(LocalArrayType)


!------------------------------------------------------------------------------
! !PUBLIC TYPES:
  public ESMF_CopyFlag, ESMF_DATA_COPY, ESMF_DATA_REF, ESMF_DATA_SPACE
  public ESMF_LocalArray
!------------------------------------------------------------------------------

! !PUBLIC MEMBER FUNCTIONS:

  public ESMF_LocalArrayCreate
  public ESMF_LocalArrayDestroy

  public ESMF_LocalArraySetData
  public ESMF_LocalArraySetInfo, ESMF_LocalArrayGetInfo
  public ESMF_LocalArrayGet, ESMF_LocalArrayGetName
 
  public ESMF_LocalArrayF90Allocate
  public ESMF_LocalArrayF90Deallocate
  public ESMF_LocalArrConstrF90Ptr    ! needed for C++ callback only

  public ESMF_LocalArraySlice
  !public ESMF_LocalArrayReshape

  public ESMF_LocalArrayWriteRestart
  public ESMF_LocalArrayReadRestart
  public ESMF_LocalArrayWrite
  public ESMF_LocalArrayRead
  !public ESMF_LocalArraySerialize, ESMF_LocalArraySerializeNoData
  !public ESMF_LocalArrayDeserialize, ESMF_LocalArrayDeserializeNoData
 
  public ESMF_LocalArrayValidate
  public ESMF_LocalArrayPrint
  
  public ESMF_LocalArrayAdjust
!EOPI
  public ESMF_LocalArrayGetInit
  public ESMF_LocalArraySetInitCreated
  public ESMF_LocalArrayGetThis
  public ESMF_LocalArraySetThis
  public operator(.eq.), operator(.ne.)

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
  character(*), parameter, private :: version = &
    '$Id: ESMF_LocalArrayCreate.cpp,v 1.11 2007/03/02 22:58:21 theurich Exp $'

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
        module procedure ESMF_LocalArrayCreateByList  ! specify TKR
        module procedure ESMF_LocalArrayCreateByLst1D ! allow integer counts
        module procedure ESMF_LocalArrayCreateBySpec  ! specify ArraySpec
        module procedure ESMF_LocalArrayCreateCopy    ! create a copy

        ! Plus interfaces for each T/K/R expanded by macro.
!EOP
        
       ! < interfaces for each T/K/R >
TypeKindRankInterfaceMacro(LocalArrCreateByMTPtr)

       ! < interfaces for each T/K/R >
TypeKindRankInterfaceMacro(LocalArrCreateByFlPtr)


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
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayCreateByLst1D"
!BOPI
! !IROUTINE: ESMF_LocalArrayCreate -- Create an array with explicit arguments

! !INTERFACE:
      ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateByLst1D(rank, type, kind, counts, lbounds, &
    ubounds, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateByLst1D
!
! !ARGUMENTS:
    integer, intent(in) :: rank
    type(ESMF_DataType), intent(in) :: type
    type(ESMF_TypeKind), intent(in) :: kind
    integer, intent(in) :: counts   !! this is what differs from ...ByList
    integer, intent(in), optional :: lbounds
    integer, intent(in), optional :: ubounds
    integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[rank]
!   In this version of the call rank must be 1.   (This interface is simply
!   to allow counts to be a scalar instead of dimension(1), which fortran
!   finds to be a different argument signature.)
! \item[type]
! Array type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
! Array kind. Valid kinds include {\tt ESMF\_I4},
! {\tt ESMF\_I8}, {\tt ESMF\_R4}, {\tt ESMF\_R8}.
! %%% {\tt ESMF\_C8}, {\tt ESMF\_C16}. % add back when supported
! \item[counts]
! The number of items in the single dimension of the array.   This is a
! scalar.  Note that if you call {\tt ESMF\_LocalArrayCreate} with 
! rank $>$ 1 then you must specify a 1D array of counts and the compiler
! will match the ByList version of this interface instead of this one.
!  \item[{[lbounds]}]
!   An integer with the lower index for each dimension.
!  \item[{[ubounds]}]
!   An integer with the upper index for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI

    integer               :: localrc      ! local return code
    integer, dimension(1) :: countlist
    integer, dimension(1) :: lb, ub

    countlist(1) = counts
    lb(1) = 1
    if (present(lbounds)) lb(1) = lbounds
    ub(1) = counts
    if (present(ubounds)) ub(1) = ubounds
    
    ESMF_LocalArrayCreateByLst1D = ESMF_LocalArrayCreateByList(rank, &
      type, kind, countlist, lb, ub, localrc)
    if (ESMF_LogMsgFoundError(localrc, ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return
      
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateByLst1D)

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_LocalArrayCreateByLst1D


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayCreateByList"
!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Create an LocalArray specifying all options.

! !INTERFACE:
      ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateByList(rank, type, kind, counts, lbounds, &
    ubounds, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateByList
!
! !ARGUMENTS:
    integer, intent(in) :: rank
    type(ESMF_DataType), intent(in) :: type
    type(ESMF_TypeKind), intent(in) :: kind
    integer, dimension(:), intent(in) :: counts
    integer, dimension(:), intent(in), optional :: lbounds
    integer, dimension(:), intent(in), optional :: ubounds
    integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
!  uninitialized.  The return value is a new LocalArray.
!    
!  The arguments are:
!  \begin{description}
!  \item[rank]
!    Array rank (dimensionality, 1D, 2D, etc).  Maximum allowed is 5D.
!  \item[type]
!    Array type.  Valid types include {\tt ESMF\_DATA\_INTEGER},
!    {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL}, 
!    {\tt ESMF\_DATA\_CHARACTER}.
!  \item[kind]
!    Array kind.  Valid kinds include {\tt ESMF\_I4}, 
!    {\tt ESMF\_I8}, {\tt ESMF\_R4}, {\tt ESMF\_R8}. 
!    %%% {\tt ESMF\_C8}, {\tt ESMF\_C16}.  % add back when supported
!  \item[counts]
!    The number of items in each dimension of the array.  This is a 1D
!    integer array the same length as the rank.
!  \item[{[lbounds]}]
!   An integer array of length rank, with the lower index for each dimension.
!  \item[{[ubounds]}]
!   An integer array of length rank, with the upper index for each dimension.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOP

    ! Local vars
    integer                 :: localrc          ! local return code
    type (ESMF_LocalArray)  :: array            ! new C++ LocalArray
    integer, dimension(ESMF_MAXDIM) :: lb, ub   ! local bounds

    array%this = ESMF_NULL_POINTER

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_FAILURE

    ! Assume defaults first, then alter if lb or ub specified. 
    lb = 1
    ub(1:size(counts)) = counts
    if (present(lbounds)) then 
        lb(1:size(lbounds)) = lbounds
    endif 
    if (present(ubounds)) then 
        ub(1:size(ubounds)) = ubounds
    endif 

    ! TODO: should this take the counts, or not?  for now i am going to
    !  set the counts after i have created the f90 array and not here.
    call c_ESMC_LocalArrayCreateNoData(array, rank, type, kind, &
      ESMF_FROM_FORTRAN, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    call ESMF_LocalArrConstrF90Ptr(array, counts, rank, type, kind, lb, ub, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! Set return value
    ESMF_LocalArrayCreateByList = array
    
    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateByList)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_LocalArrayCreateByList


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayCreateBySpec"
!BOP
! !IROUTINE: ESMF_LocalArrayCreate -- Create a new LocalArray from an ArraySpec

! !INTERFACE:
      ! Private name; call using ESMF_LocalArrayCreate()
  function ESMF_LocalArrayCreateBySpec(arrayspec, counts, lbounds, ubounds, rc)
!
! !RETURN VALUE:
    type(ESMF_LocalArray) :: ESMF_LocalArrayCreateBySpec
!
! !ARGUMENTS:
    type(ESMF_ArraySpec), intent(inout) :: arrayspec
    integer, intent(in), dimension(:) :: counts
    integer, dimension(:), intent(in), optional :: lbounds
    integer, dimension(:), intent(in), optional :: ubounds 
    integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
!  uninitialized.  The return value is a new LocalArray.
!    
!  The arguments are:
!  \begin{description}
!  \item[arrayspec]
!    ArraySpec object.
!  \item[counts]
!   The number of items in each dimension of the array.  This is a 1D
!   integer array the same length as the rank.
!  \item[{[lbounds]}]
!   An integer array of length rank, with the lower index for each dimension.
!  \item[{[ubounds]}]
!   An integer array of length rank, with the upper index for each dimension.
!  \item[{[rc]}]
!   Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOP

    ! Local vars
    type (ESMF_LocalArray) :: array     ! new C++ LocalArray
    integer                :: localrc   ! local return code
    integer :: rank
    type(ESMF_DataType) :: type
    type(ESMF_TypeKind) :: kind

    array%this = ESMF_NULL_POINTER

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_FAILURE

    ! Check init status of arguments
    ESMF_INIT_CHECK_SHALLOW(ESMF_ArraySpecGetInit, ESMF_ArraySpecInit,arrayspec)

    call ESMF_ArraySpecGet(arrayspec, rank, type, kind, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    
    ! Call the list function to make the array
    ESMF_LocalArrayCreateBySpec = ESMF_LocalArrayCreateByList(rank, type, &
      kind, counts, lbounds, ubounds, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! Set init code
    ESMF_INIT_SET_CREATED(ESMF_LocalArrayCreateBySpec)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end function ESMF_LocalArrayCreateBySpec



!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayCreateCopy"
!BOPI
! !IROUTINE: ESMF_LocalArrayCreate -- Create a new LocalArray from existing one

! !INTERFACE:
      ! Private name; call using ESMF_LocalArrayCreate()
      function ESMF_LocalArrayCreateCopy(arrayIn, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayCreateCopy
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: arrayIn
      integer, intent(out), optional    :: rc 

!
! !DESCRIPTION:
!  Perform a deep copy of an existing {\tt ESMF\_LocalArray}.
!
!  The arguments are:
!  \begin{description}
!   \item[array]
!    Input local array
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!  \end{description}
!
!EOPI
        ! Local vars
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?
        integer :: localkind, localtype
        integer :: rank

        type(ESMF_LocalArray)           :: arrayOut
        type(ESMF_DataType)             :: type
        type(ESMF_TypeKind)             :: kind
        type(ESMF_Pointer)              :: base
        character(len=ESMF_MAXSTR)      :: name
        integer, dimension(ESMF_MAXDIM) :: counts
        integer, dimension(ESMF_MAXDIM) :: lbounds
        integer, dimension(ESMF_MAXDIM) :: ubounds

        status = ESMF_FAILURE
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, arrayIn, rc)

        arrayOut%this = ESMF_NULL_POINTER

        call ESMF_LocalArrayGet(arrayIn, rank, type, kind, counts, &
                                lbounds, ubounds, base, name, rc=status)

        localtype = type%dtype
        localkind = kind%dkind

        ! Call a T/K/R specific interface in order to create the proper
        !  type of F90 pointer, allocate the space, set the values in the
        !  Array object, and return.  (The routine this code is calling is
        !  generated by macro.)

        !! calling routines generated from macros by the preprocessor
        select case (localtype)
          case (ESMF_DATA_INTEGER%dtype)
            select case (rank)
              case (1)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayCopy1DI1(arrayIn, arrayOut, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayCopy1DI2(arrayIn, arrayOut, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayCopy1DI4(arrayIn, arrayOut, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayCopy1DI8(arrayIn, arrayOut, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (2)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayCopy2DI1(arrayIn, arrayOut, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayCopy2DI2(arrayIn, arrayOut, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayCopy2DI4(arrayIn, arrayOut, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayCopy2DI8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (3)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayCopy3DI1(arrayIn, arrayOut, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayCopy3DI2(arrayIn, arrayOut, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayCopy3DI4(arrayIn, arrayOut, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayCopy3DI8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (4)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayCopy4DI1(arrayIn, arrayOut, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayCopy4DI2(arrayIn, arrayOut, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayCopy4DI4(arrayIn, arrayOut, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayCopy4DI8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
^ifndef ESMF_NO_GREATER_THAN_4D
              case (5)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayCopy5DI1(arrayIn, arrayOut, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayCopy5DI2(arrayIn, arrayOut, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayCopy5DI4(arrayIn, arrayOut, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayCopy5DI8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (6)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayCopy6DI1(arrayIn, arrayOut, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayCopy6DI2(arrayIn, arrayOut, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayCopy6DI4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayCopy6DI8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (7)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayCopy7DI1(arrayIn, arrayOut, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayCopy7DI2(arrayIn, arrayOut, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayCopy7DI4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayCopy7DI8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
^endif
    
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
            end select
    
           case (ESMF_DATA_REAL%dtype)
            select case (rank)
              case (1)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayCopy1DR4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayCopy1DR8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (2)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayCopy2DR4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayCopy2DR8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (3)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayCopy3DR4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayCopy3DR8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (4)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayCopy4DR4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayCopy4DR8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
^ifndef ESMF_NO_GREATER_THAN_4D
              case (5)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayCopy5DR4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayCopy5DR8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (6)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayCopy6DR4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayCopy6DR8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (7)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayCopy7DR4(arrayIn, arrayOut, rc=status)

                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayCopy7DR8(arrayIn, arrayOut, rc=status)

                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
^endif
    
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
            end select
          case default
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported type", & 
                                 ESMF_CONTEXT, rc)) return
         end select

        ESMF_INIT_SET_CREATED(arrayOut)
        ESMF_LocalArrayCreateCopy = arrayOut
        ! Set init code

        ! Set return code if caller specified it
        if (rcpresent) rc = status

        end function ESMF_LocalArrayCreateCopy
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr"
!BOPI
! !IROUTINE: ESMF_LocalArrConstrF90Ptr - Create and add F90 ptr to array

! !INTERFACE:
     subroutine ESMF_LocalArrConstrF90Ptr(array, counts, rank, type, kind, &
                                          lbounds, ubounds, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(out), optional :: rc 
!
! !DESCRIPTION:
!  Take a partially created {\tt ESMF\_LocalArray} and T/K/R information and call
!  the proper subroutine to create an F90 pointer, allocate space, and set the
!  corresponding values in the {\tt ESMF\_LocalArray} object.
!    
!  The arguments are:
!  \begin{description}
!  \item[array]
!    Partially created {\tt ESMF\_LocalArray} object.  This entry point is used
!    during both the C++ and F90 create calls if we need to create an F90
!    pointer to be used later.
!  \item[counts]
!    The number of items in each dimension of the array.  This is a 1D
!    integer array the same length as the rank.
!  \item[rank]
!    Array rank.
!    This must match what is already in the array - it is here only as
!    a convienience.
!  \item[type]
!    Array type.
!    This must match what is already in the array - it is here only as
!    a convienience.
!  \item[kind]
!    Array kind. 
!    This must match what is already in the array - it is here only as
!    a convienience.
!   \item[lbounds]
!    The lower index values per rank.
!   \item[ubounds]
!    The upper index values per rank.
!   \item[{[rc]}]
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!   \end{description}
!
!EOPI

        ! Local vars
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?
        integer :: localkind, localtype

        status = ESMF_FAILURE
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Cannot check init status of array argument here because
        ! the array object is only partially created at this point
            
        localtype = type%dtype
        localkind = kind%dkind

        ! Call a T/K/R specific interface in order to create the proper
        !  type of F90 pointer, allocate the space, set the values in the
        !  Array object, and return.  (The routine this code is calling is
        !  generated by macro.)

        !! calling routines generated from macros by the preprocessor
        select case (localtype)
          case (ESMF_DATA_INTEGER%dtype)
            select case (rank)
              case (1)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return

                end select
    
              case (2)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (3)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (4)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
^ifndef ESMF_NO_GREATER_THAN_4D
              case (5)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (6)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrConstrF90Ptr6DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr6DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr6DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr6DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (7)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrConstrF90Ptr7DI1(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr7DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr7DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr7DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
^endif
    
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
            end select
    
           case (ESMF_DATA_REAL%dtype)
            select case (rank)
              case (1)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (2)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (3)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (4)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
^ifndef ESMF_NO_GREATER_THAN_4D
              case (5)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (6)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr6DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr6DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (7)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr7DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr7DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
^endif
    
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
            end select
          case default
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported type", & 
                                 ESMF_CONTEXT, rc)) return
         end select


        ! Set return code if caller specified it
        if (rcpresent) rc = status

        end subroutine ESMF_LocalArrConstrF90Ptr

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
TypeKindRankDeclarationMacro(LocalArrayCreateByMTPtr)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
TypeKindRankDeclarationMacro(LocalArrayCreateByFlPtr)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
TypeKindRankDeclarationMacro(LocalArrConstrF90Ptr)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
TypeKindRankDeclarationMacro(LocalArrayAdjust)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
TypeKindRankDeclarationMacro(LocalArrayCopy)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >
TypeKindRankDeclarationMacro(LocalArrayDeallocate)

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < end of automatically generated function >

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayDestroy"
!BOP
! !IROUTINE: ESMF_LocalArrayDestroy - Remove a LocalArray
!
! !INTERFACE:
  subroutine ESMF_LocalArrayDestroy(array, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray),  intent(inout)           :: array
    integer,                intent(out),  optional  :: rc
!
! !DESCRIPTION:
!     Releases all resources associated with this {\tt ESMF\_LocalArray}.
!
!     The arguments are:
!     \begin{description}
!     \item[array]
!       Destroy contents of this {\tt ESMF\_LocalArray}.
!     \item[[rc]]
!       Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

    ! Local vars
    integer :: localrc                  ! local return code
    logical :: needsdealloc             ! do we need to free space?
    integer :: rank
    type(ESMF_DataType) :: type
    type(ESMF_TypeKind) :: kind

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)

    needsdealloc = .FALSE.

    ! TODO: document the current rule - if we do the allocate in
    !   the case of ESMF_DATA_COPY at create time then we delete the
    !   space.  otherwise, the user needs to destroy the array 
    !   (we will ignore the data) and call deallocate themselves.

    ! Call Destruct first, then free this memory
    call c_ESMC_LocalArrayNeedsDealloc(array, needsdealloc, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return
    if (needsdealloc) then
      call c_ESMC_LocalArrayGetRank(array, rank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      call c_ESMC_LocalArrayGetType(array, type, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      call c_ESMC_LocalArrayGetTypeKind(array, kind, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      call ESMF_LocalArrayF90Deallocate(array, rank, type, kind, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
      call c_ESMC_LocalArraySetNoDealloc(array, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rc)) return
    endif

    ! Calling deallocate first means this will not return back to F90
    !  before returning for good.
    call c_ESMC_LocalArrayDestroy(array, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! Set init code
    ESMF_INIT_SET_DELETED(array)
 
    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocalArrayDestroy


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArraySetInfo"
!BOPI
! !IROUTINE: ESMF_LocalArraySetInfo
!
! !INTERFACE:
  subroutine ESMF_LocalArraySetInfo(array, counts, lbounds, ubounds, &
                                        offsets, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: array 
    integer, dimension(:), intent(in), optional :: counts
    integer, dimension(:), intent(in), optional :: lbounds
    integer, dimension(:), intent(in), optional :: ubounds
    integer, dimension(:), intent(in), optional :: offsets
    integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Must be used with care - if you set the values on an already created
!      array object to be inconsistent with the F90 pointer, then bad things
!      will happen.
!
!EOPI

    integer :: localrc

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)

    call c_ESMC_LocalArraySetInfo(array, counts, lbounds, ubounds, offsets, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocalArraySetInfo


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayGetInfo"
!BOPI
! !IROUTINE: ESMF_LocalArrayGetInfo
!
! !INTERFACE:
  subroutine ESMF_LocalArrayGetInfo(array, counts, lbounds, ubounds, offsets, &
    rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: array 
    integer, dimension(:), intent(out), optional :: counts
    integer, dimension(:), intent(out), optional :: lbounds
    integer, dimension(:), intent(out), optional :: ubounds
    integer, dimension(:), intent(out), optional :: offsets
    integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Get back information about counts and upper and lower bounds
!      from an already created array object.
!
!EOPI

    integer :: localrc

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)

    call c_ESMC_LocalArrayGetInfo(array, counts, lbounds, ubounds, offsets, &
      localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rcToReturn=rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocalArrayGetInfo


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArraySetData"
!BOPI
! !IROUTINE: ESMF_LocalArraySetData
!
! !INTERFACE:
  subroutine ESMF_LocalArraySetData(array, databuf, docopy, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(inout) :: array 
    real(ESMF_KIND_R8), dimension (:), pointer :: databuf    
    type(ESMF_CopyFlag), intent(in) :: docopy 
    integer, intent(out), optional :: rc     
!
! !DESCRIPTION:
!      Used only with the version of {\tt ESMF\_LocalArrayCreate} which
!      creates an empty {\tt ESMF\_LocalArray} and allows the Data to be
!      specified later.  Otherwise it is an error to replace the data contents
!      associated with a {\tt ESMF\_LocalArray}.  
!
!EOPI

!
!  added BOPI/EOPI until code is written
! TODO: code goes here
!
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)

    if (present(rc)) rc = ESMF_FAILURE

  end subroutine ESMF_LocalArraySetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! 
! Query for information from the array.
!
!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayGet"
!BOPI
! !IROUTINE: ESMF_LocalArrayGet - return information about a LocalArray
!
! !INTERFACE:
  subroutine ESMF_LocalArrayGet(array, rank, type, kind, counts, lbounds, &
    ubounds, base, name, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in) :: array
    integer, intent(out), optional :: rank
    type(ESMF_DataType), intent(out), optional :: type
    type(ESMF_TypeKind), intent(out), optional :: kind
    integer, dimension(:), intent(out), optional :: counts
    integer, dimension(:), intent(out), optional :: lbounds
    integer, dimension(:), intent(out), optional :: ubounds
    type(ESMF_Pointer), intent(out), optional :: base
    character(len=ESMF_MAXSTR), intent(out), optional :: name
    integer, intent(out), optional :: rc             

!
! !DESCRIPTION:
!      Returns information about the {\tt ESMF\_LocalArray}.  For queries
!      where the caller only wants a single value, specify the argument by name.
!      All the arguments after the array input are optional to facilitate this.
!
!EOPI

    integer :: localrc    ! local return code
    integer :: lrank      ! Local use to get rank

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_FAILURE

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)

    if (present(rank)) then
      call c_ESMC_LocalArrayGetRank(array, rank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(type)) then
      call c_ESMC_LocalArrayGetType(array, type, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(kind)) then
      call c_ESMC_LocalArrayGetTypeKind(array, kind, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(counts)) then
      call c_ESMC_LocalArrayGetRank(array, lrank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call c_ESMC_LocalArrayGetLengths(array, lrank, counts, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(lbounds)) then
      call c_ESMC_LocalArrayGetRank(array, lrank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call c_ESMC_LocalArrayGetLbounds(array, lrank, lbounds, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(ubounds)) then
      call c_ESMC_LocalArrayGetRank(array, lrank, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
      call c_ESMC_LocalArrayGetUbounds(array, lrank, ubounds, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(base)) then
      call c_ESMC_LocalArrayGetBaseAddr(array, base, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    if (present(name)) then
      call c_ESMC_GetName(array, name, localrc)
      if (ESMF_LogMsgFoundError(localrc, &
        ESMF_ERR_PASSTHRU, &
        ESMF_CONTEXT, rcToReturn=rc)) return
    endif

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocalArrayGet

!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayGetName"
!BOPI
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
!EOPI

    integer :: localrc    ! local return code

    ! Initialize return code; assume failure until success is certain
    if (present(rc)) rc = ESMF_FAILURE

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)

    call c_ESMC_GetName(array, name, localrc)
    if (ESMF_LogMsgFoundError(localrc, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! Return successfully
    if (present(rc)) rc = ESMF_SUCCESS

  end subroutine ESMF_LocalArrayGetName


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
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayF90Allocate"
!BOPI
! !IROUTINE:  ESMF_LocalArrayF90Allocate - Allocate an F90 pointer and set LocalArray info
!
! !INTERFACE: 
     subroutine ESMF_LocalArrayF90Allocate(array, rank, type, kind, counts, &
                                           lbounds, ubounds, rc)
! 
! !ARGUMENTS: 
      type(ESMF_LocalArray), intent(inout) :: array 
      integer, intent(in) :: rank   
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts 
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
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
!     \item[lbounds]
!          An integer array, size {\tt rank}, of each dimensions lower index.
!     \item[ubounds]
!          An integer array, size {\tt rank}, of each dimensions upper index.
!     \item[{[rc]}]  
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
!   \end{description} 
! 
!EOPI
 
    integer :: status                               ! local error status 
    integer, dimension(ESMF_MAXDIM) :: lb, ub
    integer, dimension(ESMF_MAXDIM) :: offsets
    integer :: localkind, localtype

    !! local variables, expanded by macro
AllTypesMacro(ArrayLocalVar)

 
    status = ESMF_FAILURE  
    if (present(rc)) rc = ESMF_FAILURE
 
    lb(1:size(lbounds)) = lbounds
    ub(1:size(ubounds)) = ubounds

    localtype = type%dtype
    localkind = kind%dkind

    !! calling routines generated from macros by the preprocessor
    select case (localtype)
      case (ESMF_DATA_INTEGER%dtype)
        select case (rank)
          case (1)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocAllocateMacro(integer, I1, 1, COL1, LEN1, RNG1, LOC1)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocAllocateMacro(integer, I2, 1, COL1, LEN1, RNG1, LOC1)
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocAllocateMacro(integer, I4, 1, COL1, LEN1, RNG1, LOC1)
              case (ESMF_TYPEKIND_I8%dkind)
AllocAllocateMacro(integer, I8, 1, COL1, LEN1, RNG1, LOC1)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (2)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocAllocateMacro(integer, I1, 2, COL2, LEN2, RNG2, LOC2)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocAllocateMacro(integer, I2, 2, COL2, LEN2, RNG2, LOC2)
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocAllocateMacro(integer, I4, 2, COL2, LEN2, RNG2, LOC2)
              case (ESMF_TYPEKIND_I8%dkind)
AllocAllocateMacro(integer, I8, 2, COL2, LEN2, RNG2, LOC2)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (3)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocAllocateMacro(integer, I1, 3, COL3, LEN3, RNG3, LOC3)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocAllocateMacro(integer, I2, 3, COL3, LEN3, RNG3, LOC3)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocAllocateMacro(integer, I4, 3, COL3, LEN3, RNG3, LOC3)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocAllocateMacro(integer, I8, 3, COL3, LEN3, RNG3, LOC3)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (4)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocAllocateMacro(integer, I1, 4, COL4, LEN4, RNG4, LOC4)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocAllocateMacro(integer, I2, 4, COL4, LEN4, RNG4, LOC4)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocAllocateMacro(integer, I4, 4, COL4, LEN4, RNG4, LOC4)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocAllocateMacro(integer, I8, 4, COL4, LEN4, RNG4, LOC4)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

^ifndef ESMF_NO_GREATER_THAN_4D
          case (5)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocAllocateMacro(integer, I1, 5, COL5, LEN5, RNG5, LOC5)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocAllocateMacro(integer, I2, 5, COL5, LEN5, RNG5, LOC5)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocAllocateMacro(integer, I4, 5, COL5, LEN5, RNG5, LOC5)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocAllocateMacro(integer, I8, 5, COL5, LEN5, RNG5, LOC5)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (6)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocAllocateMacro(integer, I1, 6, COL6, LEN6, RNG6, LOC6)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocAllocateMacro(integer, I2, 6, COL6, LEN6, RNG6, LOC6)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocAllocateMacro(integer, I4, 6, COL6, LEN6, RNG6, LOC6)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocAllocateMacro(integer, I8, 6, COL6, LEN6, RNG6, LOC6)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (7)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocAllocateMacro(integer, I1, 7, COL7, LEN7, RNG7, LOC7)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocAllocateMacro(integer, I2, 7, COL7, LEN7, RNG7, LOC7)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocAllocateMacro(integer, I4, 7, COL7, LEN7, RNG7, LOC7)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocAllocateMacro(integer, I8, 7, COL7, LEN7, RNG7, LOC7)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select
^endif

          case default
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
        end select

       case (ESMF_DATA_REAL%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocAllocateMacro(real, R4, 1, COL1, LEN1, RNG1, LOC1)
              case (ESMF_TYPEKIND_R8%dkind)
AllocAllocateMacro(real, R8, 1, COL1, LEN1, RNG1, LOC1)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (2)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocAllocateMacro(real, R4, 2, COL2, LEN2, RNG2, LOC2)
              case (ESMF_TYPEKIND_R8%dkind)
AllocAllocateMacro(real, R8, 2, COL2, LEN2, RNG2, LOC2)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (3)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocAllocateMacro(real, R4, 3, COL3, LEN3, RNG3, LOC3)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocAllocateMacro(real, R8, 3, COL3, LEN3, RNG3, LOC3)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (4)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocAllocateMacro(real, R4, 4, COL4, LEN4, RNG4, LOC4)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocAllocateMacro(real, R8, 4, COL4, LEN4, RNG4, LOC4)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

^ifndef ESMF_NO_GREATER_THAN_4D
          case (5)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocAllocateMacro(real, R4, 5, COL5, LEN5, RNG5, LOC5)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocAllocateMacro(real, R8, 5, COL5, LEN5, RNG5, LOC5)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (6)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocAllocateMacro(real, R4, 6, COL6, LEN6, RNG6, LOC6)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocAllocateMacro(real, R8, 6, COL6, LEN6, RNG6, LOC6)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (7)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocAllocateMacro(real, R4, 7, COL7, LEN7, RNG7, LOC7)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocAllocateMacro(real, R8, 7, COL7, LEN7, RNG7, LOC7)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select
^endif

          case default
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
        end select
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported type", & 
                                 ESMF_CONTEXT, rc)) return
     end select

     if (present(rc)) rc = status 
 
     end subroutine ESMF_LocalArrayF90Allocate
 

!------------------------------------------------------------------------------ 
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayF90Deallocate"
!BOPI
! !IROUTINE:  ESMF_LocalArrayF90Deallocate - Deallocate an F90 pointer 
!
! !INTERFACE: 
     subroutine ESMF_LocalArrayF90Deallocate(array, rank, type, kind, rc)
! 
! !ARGUMENTS: 
      type(ESMF_LocalArray), intent(inout) :: array 
      integer :: rank   
      type(ESMF_DataType) :: type
      type(ESMF_TypeKind) :: kind
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
!EOPI
 
    integer :: status                               ! local error status 
    integer :: localkind, localtype

    !! local variables, expanded by macro
AllTypesMacro(ArrayLocalVar)


    if (present(rc)) rc = ESMF_FAILURE
 
    localtype = type
    localkind = kind

    !! calling routines generated from macros by the preprocessor
    select case (localtype)
      case (ESMF_DATA_INTEGER%dtype)
        select case (rank)
          case (1)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocDeallocateMacro(integer, I1, 1, COL1, LEN1, RNG1, LOC1)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 1, COL1, LEN1, RNG1, LOC1)
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 1, COL1, LEN1, RNG1, LOC1)
              case (ESMF_TYPEKIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 1, COL1, LEN1, RNG1, LOC1)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (2)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocDeallocateMacro(integer, I1, 2, COL2, LEN2, RNG2, LOC2)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 2, COL2, LEN2, RNG2, LOC2)
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 2, COL2, LEN2, RNG2, LOC2)
              case (ESMF_TYPEKIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 2, COL2, LEN2, RNG2, LOC2)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (3)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocDeallocateMacro(integer, I1, 3, COL3, LEN3, RNG3, LOC3)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 3, COL3, LEN3, RNG3, LOC3)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 3, COL3, LEN3, RNG3, LOC3)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 3, COL3, LEN3, RNG3, LOC3)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (4)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocDeallocateMacro(integer, I1, 4, COL4, LEN4, RNG4, LOC4)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 4, COL4, LEN4, RNG4, LOC4)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 4, COL4, LEN4, RNG4, LOC4)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 4, COL4, LEN4, RNG4, LOC4)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

^ifndef ESMF_NO_GREATER_THAN_4D
          case (5)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocDeallocateMacro(integer, I1, 5, COL5, LEN5, RNG5, LOC5)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 5, COL5, LEN5, RNG5, LOC5)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 5, COL5, LEN5, RNG5, LOC5)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 5, COL5, LEN5, RNG5, LOC5)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (6)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocDeallocateMacro(integer, I1, 6, COL6, LEN6, RNG6, LOC6)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 6, COL6, LEN6, RNG6, LOC6)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 6, COL6, LEN6, RNG6, LOC6)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 6, COL6, LEN6, RNG6, LOC6)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (7)
            select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
              case (ESMF_TYPEKIND_I1%dkind)
AllocDeallocateMacro(integer, I1, 7, COL7, LEN7, RNG7, LOC7)       
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
              case (ESMF_TYPEKIND_I2%dkind)
AllocDeallocateMacro(integer, I2, 7, COL7, LEN7, RNG7, LOC7)       
^endif
              case (ESMF_TYPEKIND_I4%dkind)
AllocDeallocateMacro(integer, I4, 7, COL7, LEN7, RNG7, LOC7)       
              case (ESMF_TYPEKIND_I8%dkind)
AllocDeallocateMacro(integer, I8, 7, COL7, LEN7, RNG7, LOC7)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select
^endif

          case default
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
        end select

       case (ESMF_DATA_REAL%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocDeallocateMacro(real, R4, 1, COL1, LEN1, RNG1, LOC1)
              case (ESMF_TYPEKIND_R8%dkind)
AllocDeallocateMacro(real, R8, 1, COL1, LEN1, RNG1, LOC1)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (2)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocDeallocateMacro(real, R4, 2, COL2, LEN2, RNG2, LOC2)
              case (ESMF_TYPEKIND_R8%dkind)
AllocDeallocateMacro(real, R8, 2, COL2, LEN2, RNG2, LOC2)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (3)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocDeallocateMacro(real, R4, 3, COL3, LEN3, RNG3, LOC3)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocDeallocateMacro(real, R8, 3, COL3, LEN3, RNG3, LOC3)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (4)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocDeallocateMacro(real, R4, 4, COL4, LEN4, RNG4, LOC4)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocDeallocateMacro(real, R8, 4, COL4, LEN4, RNG4, LOC4)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

^ifndef ESMF_NO_GREATER_THAN_4D
          case (5)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocDeallocateMacro(real, R4, 5, COL5, LEN5, RNG5, LOC5)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocDeallocateMacro(real, R8, 5, COL5, LEN5, RNG5, LOC5)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (6)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocDeallocateMacro(real, R4, 6, COL6, LEN6, RNG6, LOC6)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocDeallocateMacro(real, R8, 6, COL6, LEN6, RNG6, LOC6)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select

          case (7)
            select case (localkind)
              case (ESMF_TYPEKIND_R4%dkind)
AllocDeallocateMacro(real, R4, 7, COL7, LEN7, RNG7, LOC7)       
              case (ESMF_TYPEKIND_R8%dkind)
AllocDeallocateMacro(real, R8, 7, COL7, LEN7, RNG7, LOC7)
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
            end select
^endif

          case default
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
        end select
      case default
        if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported type", & 
                                 ESMF_CONTEXT, rc)) return
     end select

     if (ESMF_LogMsgFoundAllocError(status, "LocalArray Deallocation", &
                                    ESMF_CONTEXT, rc)) return

     if (present(rc)) rc = ESMF_SUCCESS
 
     end subroutine ESMF_LocalArrayF90Deallocate
 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is for higher level LocalArray funcs
!
!------------------------------------------------------------------------------ 
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArraySlice"
!BOPI
! !IROUTINE: ESMF_LocalArraySlice - extract a subset from a LocalArray
!
! !INTERFACE:
      function ESMF_LocalArraySlice(array, slicedim, sliceloc, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArraySlice
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array 
      integer, intent(in) :: slicedim
      integer, intent(in) :: sliceloc
      integer, intent(out), optional :: rc            
!
! !DESCRIPTION:
!      Extract an (N-1)D array from an N-D array.  The dimension to be
!      dropped is the {\tt slicedim} argument, and the location along
!      the dropped dimension is the {\tt sliceloc} argument.  This routine
!      allocates new space and copies the data, leaving the original array
!      unchanged.
!
!EOPI
        ! BOPI/EOPI because the actual function is not working yet

        ! Local vars
        type (ESMF_LocalArray) :: newarray  ! new C++ LocalArray
        integer :: status                   ! local error status
        logical :: rcpresent                ! did user specify rc?
        integer :: rank
        type (ESMF_TypeKind) :: kind
        type (ESMF_DataType) :: type
        integer :: i, counts(ESMF_MAXDIM), lb(ESMF_MAXDIM), ub(ESMF_MAXDIM)

        status = ESMF_FAILURE
        rcpresent = .FALSE.
        newarray%this = ESMF_NULL_POINTER

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
    
        ! Get info from the existing array
        call c_ESMC_LocalArrayGetRank(array, rank, status)
        call c_ESMC_LocalArrayGetType(array, type, status)  
        call c_ESMC_LocalArrayGetTypeKind(array, kind, status)    
        call c_ESMC_LocalArrayGetLengths(array, rank, counts, status)  

        ! Basic sanity checks - slice dim is ok, sliced location exists, etc.
        if ((slicedim .lt. 1) .or. (slicedim .gt. rank)) then
          if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Bad value for slicedim", &
                                 ESMF_CONTEXT, rc)) return

            ! "ESMF_LocalArraySlice: slicedim value ", slicedim, &
            ! " must be between 1 and ", rank
        endif
        if ((sliceloc .lt. 1) .or. (sliceloc .gt. counts(rank))) then
           if (ESMF_LogMsgFoundError(ESMF_RC_ARG_VALUE, &
                                "Bad value for sliceloc", &
                                 ESMF_CONTEXT, rc)) return
            !"ESMF_LocalArraySlice: sliceloc value ", sliceloc, &
            ! " must be between 1 and ", counts(rank)
        endif

        ! This slice will be rank < 1.  Remove the counts corresponding
        ! to the sliced dim (save for later error checking).
        ! TODO: add error checks
        do i=sliceloc, rank-1
           counts(i) = counts(i+1)
        enddo
        rank = rank - 1

        call c_ESMC_LocalArrayCreateNoData(newarray, rank, type, kind, &
                                           ESMF_FROM_FORTRAN, status)
        if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

        call ESMF_LocalArrConstrF90Ptr(newarray, counts, rank, type, kind, &
                                          lb, ub, status)

        ! At this point the new array exists, and has space allocated, but it
        ! does not contain data from the old array.  Now we have a T/K/R prob.

        ! put old F90 ptr into wrap
        ! call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status)

        ! put new F90 ptr into wrap
        ! call c_ESMC_LocalArrayGetF90Ptr(newarray, wrap, status)

        ! there must be something like this we can do here...
        ! newarray = RESHAPE(array(sliceloc, :, :), counts)


        ! Set return values
        ESMF_LocalArraySlice = newarray
        if (rcpresent) rc = status

        end function ESMF_LocalArraySlice


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is I/O for LocalArrays
!
!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayWriteRestart"
!BOPI
! !IROUTINE: ESMF_LocalArrayWriteRestart - checkpoint a LocalArray
!
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
!EOPI

!
! TODO: code goes here
!
        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP_SHORT(ESMF_LocalArrayGetInit, array, rc)
        !todo: init check iospec

        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_LocalArrayWriteRestart


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayReadRestart"
!BOPI
! !IROUTINE: ESMF_LocalArrayReadRestart - restore a restart file
!
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
!EOPI

        type (ESMF_LocalArray) :: a 

!       this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER

!
! TODO: add code here
!

        ESMF_LocalArrayReadRestart = a 
 
        if (present(rc)) rc = ESMF_FAILURE

        end function ESMF_LocalArrayReadRestart


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayWrite"
!BOPI
! !IROUTINE: ESMF_LocalArrayWrite - save contents to file
!
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
!EOPI

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

        ! Check init status of arguments
        ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
        !todo: init check iospec

       defaultopts = "singlefile"
       defaultfile = "datafile"

       if(present(filename)) then
           call c_ESMC_LocalArrayWrite(array, defaultopts, trim(filename), status) 
       else
           call c_ESMC_LocalArrayWrite(array, defaultopts, trim(defaultfile), status) 
       endif

      if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! set return values
       if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LocalArrayWrite


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayRead"
!BOPI
! !IROUTINE: ESMF_LocalArrayRead - read data contents
!
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
!EOPI

        type (ESMF_LocalArray) :: a

!       this is just to shut the compiler up
        a%this = ESMF_NULL_POINTER

!
! TODO: add code here
!

        ESMF_LocalArrayRead = a 
 
        if (present(rc)) rc = ESMF_FAILURE

        end function ESMF_LocalArrayRead


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayValidate"
!BOPI
! !IROUTINE: ESMF_LocalArrayValidate - Check validity of LocalArray object
!
! !INTERFACE:
  subroutine ESMF_LocalArrayValidate(array, options, rc)
!
!
! !ARGUMENTS:
    type(ESMF_LocalArray),  intent(in)              :: array
    character(len = *),     intent(in),   optional  :: options
    integer,                intent(out),  optional  :: rc 
!
! !DESCRIPTION:
!      Validate a {\tt ESMF\_LocalArray} object.
!
!EOPI

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
     
    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
      
    defaultopts = "brief"

    if(present(options)) then
      !call c_ESMC_LocalArrayValidate(array, options, status) 
    else
      !call c_ESMC_LocalArrayValidate(array, defaultopts, status) 
    endif

    ! Return successfully
    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_LocalArrayValidate


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayPrint"
!BOPI
! !IROUTINE: ESMF_LocalArrayPrint - Print contents of an LocalArray object
!
! !INTERFACE:
  subroutine ESMF_LocalArrayPrint(array, options, rc)
!
!
! !ARGUMENTS:
    type(ESMF_LocalArray),  intent(in)              :: array
    character(len = *),     intent(in),   optional  :: options
    integer,                intent(out),  optional  :: rc 
!
! !DESCRIPTION:
!      Print information about a {\tt ESMF\_LocalArray}.
!
!EOPI

    character (len=6) :: defaultopts      ! default print options 
    integer :: status                     ! local error status
    logical :: rcpresent        
    !character(len=ESMF_MAXSTR) :: msgbuf

    ! Initialize return code; assume failure until success is certain
    status = ESMF_FAILURE
    rcpresent = .FALSE.
    if (present(rc)) then
      rcpresent = .TRUE.
      rc = ESMF_FAILURE
    endif

    ! Check init status of arguments
    ESMF_INIT_CHECK_DEEP(ESMF_LocalArrayGetInit, array, rc)
      
    if (array%this .eq. ESMF_NULL_POINTER) then
      !write(msgbuf,*)  "LocalArray Print:"
      !call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write(*,*)  "LocalArray Print:"
      !write(msgbuf,*)  " Empty or Uninitialized Array"
      !call ESMF_LogWrite(msgbuf, ESMF_LOG_INFO)
      write(*,*)  " Empty or Uninitialized Array"
      if (present(rc)) rc = ESMF_SUCCESS
      return
    endif

    defaultopts = "brief"

    if(present(options)) then
        call c_ESMC_LocalArrayPrint(array, options, status) 
    else
        call c_ESMC_LocalArrayPrint(array, defaultopts, status) 
    endif

    if (ESMF_LogMsgFoundError(status, &
      ESMF_ERR_PASSTHRU, &
      ESMF_CONTEXT, rc)) return

    ! Return successfully
    if (rcpresent) rc = ESMF_SUCCESS

  end subroutine ESMF_LocalArrayPrint


!------------------------------------------------------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayAdjust"
!BOPI
! !IROUTINE: ESMF_LocalArrayAdjust - Adjust bounds of F90 array member
!
! !INTERFACE:
      subroutine ESMF_LocalArrayAdjust(array, counts, rank, type, kind, &
        lbounds, ubounds, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_TypeKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(out), optional :: rc 

!
! !DESCRIPTION:
!      Adjust bounds of F90 array member in {\tt ESMF\_LocalArray} object.
!
!EOPI

        ! Local vars
        integer :: status                   ! local error status
        integer :: localkind, localtype

        status = ESMF_FAILURE

        ! Cannot check init status of array argument here because
        ! the array object is only partially created at this point
            
        localtype = type%dtype
        localkind = kind%dkind

        
        ! Call a T/K/R specific interface

        select case (localtype)
          case (ESMF_DATA_INTEGER%dtype)
            select case (rank)
              case (1)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayAdjust1DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayAdjust1DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayAdjust1DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayAdjust1DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                                 
                end select
    
              case (2)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayAdjust2DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayAdjust2DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayAdjust2DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayAdjust2DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (3)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayAdjust3DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayAdjust3DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayAdjust3DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayAdjust3DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (4)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayAdjust4DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayAdjust4DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayAdjust4DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayAdjust4DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
^ifndef ESMF_NO_GREATER_THAN_4D
              case (5)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayAdjust5DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayAdjust5DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayAdjust5DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayAdjust5DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (6)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayAdjust6DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayAdjust6DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayAdjust6DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayAdjust6DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (7)
                select case (localkind)
^ifndef ESMF_NO_INTEGER_1_BYTE
                  case (ESMF_TYPEKIND_I1%dkind)
                    call ESMF_LocalArrayAdjust7DI1(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
^ifndef ESMF_NO_INTEGER_2_BYTE
                  case (ESMF_TYPEKIND_I2%dkind)
                    call ESMF_LocalArrayAdjust7DI2(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
^endif
                  case (ESMF_TYPEKIND_I4%dkind)
                    call ESMF_LocalArrayAdjust7DI4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_I8%dkind)
                    call ESMF_LocalArrayAdjust7DI8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
^endif
    
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
            end select
    
           case (ESMF_DATA_REAL%dtype)
            select case (rank)
              case (1)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayAdjust1DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayAdjust1DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (2)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayAdjust2DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayAdjust2DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (3)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayAdjust3DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayAdjust3DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (4)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayAdjust4DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayAdjust4DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
^ifndef ESMF_NO_GREATER_THAN_4D
              case (5)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayAdjust5DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayAdjust5DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (6)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayAdjust6DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayAdjust6DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
    
              case (7)
                select case (localkind)
                  case (ESMF_TYPEKIND_R4%dkind)
                    call ESMF_LocalArrayAdjust7DR4(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case (ESMF_TYPEKIND_R8%dkind)
                    call ESMF_LocalArrayAdjust7DR8(array, counts, &
                                    lb=lbounds, ub=ubounds, rc=status)
                  case default
                    if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported kind", & 
                                 ESMF_CONTEXT, rc)) return
                end select
^endif
    
              case default
                if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported rank", & 
                                 ESMF_CONTEXT, rc)) return
            end select
          case default
            if (ESMF_LogMsgFoundError(ESMF_RC_ARG_BAD, &
                                "Unsupported type", & 
                                 ESMF_CONTEXT, rc)) return
         end select

       ! check status for errors
       if (ESMF_LogMsgFoundError(status, &
                                  ESMF_ERR_PASSTHRU, &
                                  ESMF_CONTEXT, rc)) return

       ! return successfully
       if (present(rc)) rc = ESMF_SUCCESS

       end subroutine ESMF_LocalArrayAdjust
       
       
! -------------------------- ESMF-private method ------------------------------
^undef  ESMF_METHOD
^define ESMF_METHOD "ESMF_LocalArrayGetInit"
!BOPI
! !IROUTINE: ESMF_LocalArrayGetInit - Internal access routine for init code
!
! !INTERFACE:
  function ESMF_LocalArrayGetInit(array) 
!
! !RETURN VALUE:
    ESMF_INIT_TYPE :: ESMF_LocalArrayGetInit   
!
! !ARGUMENTS:
    type(ESMF_LocalArray), intent(in), optional :: array
!
! !DESCRIPTION:
!      Access deep object init code.
!
!     The arguments are:
!     \begin{description}
!     \item [array]
!           LocalArray object.
!     \end{description}
!
!EOPI

    if (present(array)) then
      ESMF_LocalArrayGetInit = ESMF_INIT_GET(array)
    else
      ESMF_LocalArrayGetInit = ESMF_INIT_CREATED
    endif

  end function ESMF_LocalArrayGetInit
!------------------------------------------------------------------------------
       

! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArraySetInitCreated()"
!BOPI
! !IROUTINE: ESMF_LocalArraySetInitCreated - Set LocalArray init code to "CREATED"

! !INTERFACE:
  subroutine ESMF_LocalArraySetInitCreated(array, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray),  intent(inout)           :: array
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!      Set init code in LocalArray object to "CREATED".
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          Specified {\tt ESMF\_LocalArray} object.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Set init code
    ESMF_INIT_SET_CREATED(array)

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_LocalArraySetInitCreated
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArrayGetThis()"
!BOPI
! !IROUTINE: ESMF_LocalArrayGetThis - Internal access routine for C++ pointer

! !INTERFACE:
  subroutine ESMF_LocalArrayGetThis(array, this, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray),  intent(in), optional  :: array
    type(ESMF_Pointer),     intent(out)           :: this
    integer,                intent(out),optional  :: rc  
!         
!
! !DESCRIPTION:
!     Internal access routine for C++ pointer.
!
!     The arguments are:
!     \begin{description}
!     \item[array] 
!          Specified {\tt ESMF\_LocalArray} object.
!     \item[this] 
!          C++ pointer.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Copy C++ pointer
    this = array%this

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_LocalArrayGetThis
!------------------------------------------------------------------------------


! -------------------------- ESMF-public method -------------------------------
#undef  ESMF_METHOD
#define ESMF_METHOD "ESMF_LocalArraySetThis()"
!BOPI
! !IROUTINE: ESMF_LocalArraySetThis - Set C++ pointer in LocalArray

! !INTERFACE:
  subroutine ESMF_LocalArraySetThis(localarray, this, rc)
!
! !ARGUMENTS:
    type(ESMF_LocalArray),  intent(inout)           :: localarray
    type(ESMF_Pointer),     intent(in)              :: this
    integer,                intent(out),  optional  :: rc  
!         
!
! !DESCRIPTION:
!     Set C++ pointer in LocalArray.
!
!     The arguments are:
!     \begin{description}
!     \item[localarray] 
!          Specified {\tt ESMF\_LocalArray} object.
!     \item[this] 
!          C++ pointer.
!     \item[{[rc]}] 
!          Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
!     \end{description}
!
!EOPI
! !REQUIREMENTS:  SSSn.n, GGGn.n
!------------------------------------------------------------------------------
    integer :: localrc                        ! local return code

    ! Assume failure until success
    if (present(rc)) rc = ESMF_FAILURE
    
    ! Copy C++ pointer
    localarray%this = this

    ! Return success
    if (present(rc)) rc = ESMF_SUCCESS
    
  end subroutine ESMF_LocalArraySetThis
!------------------------------------------------------------------------------


!------------------------------------------------------------------------------
end module ESMF_LocalArrayCreateMod

