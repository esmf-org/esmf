! $Id: ESMF_LocalArray.F90,v 1.18 2004/03/11 18:06:50 nscollins Exp $
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
#include "ESMF.h"
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
                            ESMF_DATA_DEFER = ESMF_CopyFlag(3), &
                            ESMF_DATA_SPACE = ESMF_CopyFlag(4), &
                            ESMF_DATA_NONE = ESMF_CopyFlag(5) ! private

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
        ! disable this for now - it causes too many compiler problems



        type(ESMF_Pointer) :: this

      end type

!------------------------------------------------------------------------------
! ! Internal wrapper structures for passing f90 pointers to C++ and
! ! guaranteeing they are passed by reference on all compilers and all
! ! platforms. These are never seen outside this module.
!
      ! < these expand into defined type declarations >

 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DI2 
 integer (ESMF_KIND_I2),dimension(:),pointer :: ptr1DI2 
 end type ESMF_ArrWrap1DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DI4 
 integer (ESMF_KIND_I4),dimension(:),pointer :: ptr1DI4 
 end type ESMF_ArrWrap1DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DI8 
 integer (ESMF_KIND_I8),dimension(:),pointer :: ptr1DI8 
 end type ESMF_ArrWrap1DI8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DI2 
 integer (ESMF_KIND_I2),dimension(:,:),pointer :: ptr2DI2 
 end type ESMF_ArrWrap2DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DI4 
 integer (ESMF_KIND_I4),dimension(:,:),pointer :: ptr2DI4 
 end type ESMF_ArrWrap2DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DI8 
 integer (ESMF_KIND_I8),dimension(:,:),pointer :: ptr2DI8 
 end type ESMF_ArrWrap2DI8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DI2 
 integer (ESMF_KIND_I2),dimension(:,:,:),pointer :: ptr3DI2 
 end type ESMF_ArrWrap3DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DI4 
 integer (ESMF_KIND_I4),dimension(:,:,:),pointer :: ptr3DI4 
 end type ESMF_ArrWrap3DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DI8 
 integer (ESMF_KIND_I8),dimension(:,:,:),pointer :: ptr3DI8 
 end type ESMF_ArrWrap3DI8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DI2 
 integer (ESMF_KIND_I2),dimension(:,:,:,:),pointer :: ptr4DI2 
 end type ESMF_ArrWrap4DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DI4 
 integer (ESMF_KIND_I4),dimension(:,:,:,:),pointer :: ptr4DI4 
 end type ESMF_ArrWrap4DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DI8 
 integer (ESMF_KIND_I8),dimension(:,:,:,:),pointer :: ptr4DI8 
 end type ESMF_ArrWrap4DI8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DI2 
 integer (ESMF_KIND_I2),dimension(:,:,:,:,:),pointer :: ptr5DI2 
 end type ESMF_ArrWrap5DI2 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DI4 
 integer (ESMF_KIND_I4),dimension(:,:,:,:,:),pointer :: ptr5DI4 
 end type ESMF_ArrWrap5DI4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DI8 
 integer (ESMF_KIND_I8),dimension(:,:,:,:,:),pointer :: ptr5DI8 
 end type ESMF_ArrWrap5DI8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DR4 
 real (ESMF_KIND_R4),dimension(:),pointer :: ptr1DR4 
 end type ESMF_ArrWrap1DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap1DR8 
 real (ESMF_KIND_R8),dimension(:),pointer :: ptr1DR8 
 end type ESMF_ArrWrap1DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DR4 
 real (ESMF_KIND_R4),dimension(:,:),pointer :: ptr2DR4 
 end type ESMF_ArrWrap2DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap2DR8 
 real (ESMF_KIND_R8),dimension(:,:),pointer :: ptr2DR8 
 end type ESMF_ArrWrap2DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DR4 
 real (ESMF_KIND_R4),dimension(:,:,:),pointer :: ptr3DR4 
 end type ESMF_ArrWrap3DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap3DR8 
 real (ESMF_KIND_R8),dimension(:,:,:),pointer :: ptr3DR8 
 end type ESMF_ArrWrap3DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DR4 
 real (ESMF_KIND_R4),dimension(:,:,:,:),pointer :: ptr4DR4 
 end type ESMF_ArrWrap4DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap4DR8 
 real (ESMF_KIND_R8),dimension(:,:,:,:),pointer :: ptr4DR8 
 end type ESMF_ArrWrap4DR8 
 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DR4 
 real (ESMF_KIND_R4),dimension(:,:,:,:,:),pointer :: ptr5DR4 
 end type ESMF_ArrWrap5DR4 
 
 ! <Created by macro - do not edit directly > 
 type ESMF_ArrWrap5DR8 
 real (ESMF_KIND_R8),dimension(:,:,:,:,:),pointer :: ptr5DR8 
 end type ESMF_ArrWrap5DR8 
 
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

      public ESMF_ArraySpecInit
      public ESMF_ArraySpecGet

      public ESMF_LocalArraySetData, ESMF_LocalArrayGetData
      public ESMF_LocalArraySetInfo, ESMF_LocalArrayGetInfo
      public ESMF_LocalArrayGet, ESMF_LocalArrayGetName

      public ESMF_LocalArrayF90Allocate
      public ESMF_LocalArrayF90Deallocate
      public ESMF_LocalArrConstrF90Ptr ! needed for C++ callback only

      public ESMF_LocalArraySlice
      !public ESMF_LocalArrayReshape

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
      '$Id: ESMF_LocalArray.F90,v 1.18 2004/03/11 18:06:50 nscollins Exp $'

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
        module procedure ESMF_LocalArrayCreateByLst1D ! allow integer counts
        module procedure ESMF_LocalArrayCreateBySpec ! specify ArraySpec

        ! Plus interfaces for each T/K/R expanded by macro.
!EOP


! ! < interfaces for each T/K/R >
! --LocalArray--InterfaceMacro(LocalArrCreateByMTArr)
!
! ! < interfaces for each T/K/R >
! --LocalArray--InterfaceMacro(LocalArrCreateByFullArr)

       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_LocalArrCreateByMTPtr1DI2 
 module procedure ESMF_LocalArrCreateByMTPtr1DI4 
 module procedure ESMF_LocalArrCreateByMTPtr1DI8 
 module procedure ESMF_LocalArrCreateByMTPtr2DI2 
 module procedure ESMF_LocalArrCreateByMTPtr2DI4 
 module procedure ESMF_LocalArrCreateByMTPtr2DI8 
 module procedure ESMF_LocalArrCreateByMTPtr3DI2 
 module procedure ESMF_LocalArrCreateByMTPtr3DI4 
 module procedure ESMF_LocalArrCreateByMTPtr3DI8 
 module procedure ESMF_LocalArrCreateByMTPtr4DI2 
 module procedure ESMF_LocalArrCreateByMTPtr4DI4 
 module procedure ESMF_LocalArrCreateByMTPtr4DI8 
 module procedure ESMF_LocalArrCreateByMTPtr5DI2 
 module procedure ESMF_LocalArrCreateByMTPtr5DI4 
 module procedure ESMF_LocalArrCreateByMTPtr5DI8 
 module procedure ESMF_LocalArrCreateByMTPtr1DR4 
 module procedure ESMF_LocalArrCreateByMTPtr1DR8 
 module procedure ESMF_LocalArrCreateByMTPtr2DR4 
 module procedure ESMF_LocalArrCreateByMTPtr2DR8 
 module procedure ESMF_LocalArrCreateByMTPtr3DR4 
 module procedure ESMF_LocalArrCreateByMTPtr3DR8 
 module procedure ESMF_LocalArrCreateByMTPtr4DR4 
 module procedure ESMF_LocalArrCreateByMTPtr4DR8 
 module procedure ESMF_LocalArrCreateByMTPtr5DR4 
 module procedure ESMF_LocalArrCreateByMTPtr5DR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_LocalArrCreateByFlPtr1DI2 
 module procedure ESMF_LocalArrCreateByFlPtr1DI4 
 module procedure ESMF_LocalArrCreateByFlPtr1DI8 
 module procedure ESMF_LocalArrCreateByFlPtr2DI2 
 module procedure ESMF_LocalArrCreateByFlPtr2DI4 
 module procedure ESMF_LocalArrCreateByFlPtr2DI8 
 module procedure ESMF_LocalArrCreateByFlPtr3DI2 
 module procedure ESMF_LocalArrCreateByFlPtr3DI4 
 module procedure ESMF_LocalArrCreateByFlPtr3DI8 
 module procedure ESMF_LocalArrCreateByFlPtr4DI2 
 module procedure ESMF_LocalArrCreateByFlPtr4DI4 
 module procedure ESMF_LocalArrCreateByFlPtr4DI8 
 module procedure ESMF_LocalArrCreateByFlPtr5DI2 
 module procedure ESMF_LocalArrCreateByFlPtr5DI4 
 module procedure ESMF_LocalArrCreateByFlPtr5DI8 
 module procedure ESMF_LocalArrCreateByFlPtr1DR4 
 module procedure ESMF_LocalArrCreateByFlPtr1DR8 
 module procedure ESMF_LocalArrCreateByFlPtr2DR4 
 module procedure ESMF_LocalArrCreateByFlPtr2DR8 
 module procedure ESMF_LocalArrCreateByFlPtr3DR4 
 module procedure ESMF_LocalArrCreateByFlPtr3DR8 
 module procedure ESMF_LocalArrCreateByFlPtr4DR4 
 module procedure ESMF_LocalArrCreateByFlPtr4DR8 
 module procedure ESMF_LocalArrCreateByFlPtr5DR4 
 module procedure ESMF_LocalArrCreateByFlPtr5DR8 
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
 module procedure ESMF_LocalArrayGetData1DI2 
 module procedure ESMF_LocalArrayGetData1DI4 
 module procedure ESMF_LocalArrayGetData1DI8 
 module procedure ESMF_LocalArrayGetData2DI2 
 module procedure ESMF_LocalArrayGetData2DI4 
 module procedure ESMF_LocalArrayGetData2DI8 
 module procedure ESMF_LocalArrayGetData3DI2 
 module procedure ESMF_LocalArrayGetData3DI4 
 module procedure ESMF_LocalArrayGetData3DI8 
 module procedure ESMF_LocalArrayGetData4DI2 
 module procedure ESMF_LocalArrayGetData4DI4 
 module procedure ESMF_LocalArrayGetData4DI8 
 module procedure ESMF_LocalArrayGetData5DI2 
 module procedure ESMF_LocalArrayGetData5DI4 
 module procedure ESMF_LocalArrayGetData5DI8 
 module procedure ESMF_LocalArrayGetData1DR4 
 module procedure ESMF_LocalArrayGetData1DR8 
 module procedure ESMF_LocalArrayGetData2DR4 
 module procedure ESMF_LocalArrayGetData2DR8 
 module procedure ESMF_LocalArrayGetData3DR4 
 module procedure ESMF_LocalArrayGetData3DR8 
 module procedure ESMF_LocalArrayGetData4DR4 
 module procedure ESMF_LocalArrayGetData4DR8 
 module procedure ESMF_LocalArrayGetData5DR4 
 module procedure ESMF_LocalArrayGetData5DR8 
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
! !IROUTINE: ESMF_LocalArrayCreateByLst1D -- Convenience cover for 1D counts

! !INTERFACE:
      function ESMF_LocalArrayCreateByLst1D(rank, type, kind, counts, &
                                            lbounds, ubounds, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayCreateByLst1D
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, intent(in) :: counts !! this is what differs from ...ByList
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
! In this version of the call rank must be 1. (This interface is simply
! to allow counts to be a scalar instead of dimension(1), which fortran
! finds to be a different argument signature.)
! \item[type]
! Array type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
! Array kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
! \item[counts]
! The number of items in the single dimension of the array. This is a
! scalar. Note that if you call {\tt ESMF\_LocalArrayCreate} with
! rank $>$ 1 then you must specify a 1D array of counts and the compiler
! will match the ByList version of this interface instead of this one.
! \item[{[lbounds]}]
! An integer with the lower index for each dimension.
! \item[{[ubounds]}]
! An integer with the upper index for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:

        integer, dimension(1) :: countlist
        integer, dimension(1) :: lb, ub

        countlist(1) = counts
        lb(1) = 1
        if (present(lbounds)) lb(1) = lbounds
        ub(1) = counts
        if (present(ubounds)) ub(1) = ubounds

        ESMF_LocalArrayCreateByLst1D = ESMF_LocalArrayCreateByList(rank, &
                                             type, kind, countlist, lb, ub, rc)

        end function ESMF_LocalArrayCreateByLst1D


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayCreateByList -- Create an LocalArray specifying all options.

! !INTERFACE:
      function ESMF_LocalArrayCreateByList(rank, type, kind, counts, &
                                           lbounds, ubounds, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayCreateByList
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, dimension(:), intent(in), optional :: lbounds
      integer, dimension(:), intent(in), optional :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[rank]
! Array rank (dimensionality, 1D, 2D, etc). Maximum allowed is 5D.
! \item[type]
! Array type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
! Array kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
! \item[{[lbounds]}]
! An integer array of length rank, with the lower index for each dimension.
! \item[{[ubounds]}]
! An integer array of length rank, with the upper index for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        type (ESMF_LocalArray) :: array ! new C++ LocalArray
        integer, dimension(ESMF_MAXDIM) :: lb, ub ! local bounds
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

        ! Assume defaults first, then alter if lb or ub specified.
        lb = 1
        ub(1:size(counts)) = counts
        if (present(lbounds)) then
            lb(1:size(lbounds)) = lbounds
        endif
        if (present(ubounds)) then
            ub(1:size(ubounds)) = ubounds
        endif

        ! TODO: should this take the counts, or not? for now i am going to
        ! set the counts after i have created the f90 array and not here.
        call c_ESMC_LocalArrayCreateNoData(array, rank, type, kind, &
                                           ESMF_FROM_FORTRAN, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "LocalArray construction error"
          return
        endif

        call ESMF_LocalArrConstrF90Ptr(array, counts, rank, type, kind, &
                                       lb, ub, status)

        ! Set return values
        ESMF_LocalArrayCreateByList = array
        if (rcpresent) rc = status

        end function ESMF_LocalArrayCreateByList


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayCreateBySpec -- Create a new LocalArray from an ArraySpec

! !INTERFACE:
      function ESMF_LocalArrayCreateBySpec(arrayspec, counts, lbounds, ubounds, rc)
!
! !RETURN VALUE:
      type(ESMF_LocalArray) :: ESMF_LocalArrayCreateBySpec
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: arrayspec
      integer, intent(in), dimension(:) :: counts
      integer, dimension(:), intent(in), optional :: lbounds
      integer, dimension(:), intent(in), optional :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new {\tt ESMF\_LocalArray} and allocate data space, which remains
! uninitialized. The return value is a new LocalArray.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
! ArraySpec object.
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
! \item[{[lbounds]}]
! An integer array of length rank, with the lower index for each dimension.
! \item[{[ubounds]}]
! An integer array of length rank, with the upper index for each dimension.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

        call ESMF_ArraySpecGet(arrayspec, rank, type, kind, status)
        if (status .ne. ESMF_SUCCESS) return

        ! Call the list function to make the array
        ESMF_LocalArrayCreateBySpec = ESMF_LocalArrayCreateByList(rank, type, &
                                         kind, counts, lbounds, ubounds, status)
        if (rcpresent) rc = status

        end function ESMF_LocalArrayCreateBySpec


!------------------------------------------------------------------------------
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
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Take a partially created {\tt ESMF\_LocalArray} and T/K/R information and call
! the proper subroutine to create an F90 pointer, allocate space, and set the
! corresponding values in the {\tt ESMF\_LocalArray} object.
!
! The arguments are:
! \begin{description}
! \item[array]
! Partially created {\tt ESMF\_LocalArray} object. This entry point is used
! during both the C++ and F90 create calls if we need to create an F90
! pointer to be used later.
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
! \item[rank]
! Array rank.
! This must match what is already in the array - it is here only as
! a convienience.
! \item[type]
! Array type.
! This must match what is already in the array - it is here only as
! a convienience.
! \item[kind]
! Array kind.
! This must match what is already in the array - it is here only as
! a convienience.
! \item[lbounds]
! The lower index values per rank.
! \item[ubounds]
! The upper index values per rank.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: localkind, localtype

        status = ESMF_FAILURE
        rcpresent = .FALSE.

        ! Initialize return code; assume failure until success is certain
        if (present(rc)) then
          rcpresent = .TRUE.
          rc = ESMF_FAILURE
        endif

        localtype = type%dtype
        localkind = kind%dkind

        ! Call a T/K/R specific interface in order to create the proper
        ! type of F90 pointer, allocate the space, set the values in the
        ! Array object, and return. (The routine this code is calling is
        ! generated by macro.)

        !! macros which are expanded by the preprocessor
        select case (localtype)
          case (ESMF_DATA_INTEGER%dtype)
            select case (rank)
              case (1)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case (2)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case (3)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case (4)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case (5)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DI2(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I4%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DI4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_I8%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DI8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case default
            end select

           case (ESMF_DATA_REAL%dtype)
            select case (rank)
              case (1)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr1DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case (2)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr2DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case (3)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr3DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case (4)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr4DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case (5)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DR4(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case (ESMF_R8%dkind)
                    call ESMF_LocalArrConstrF90Ptr5DR8(array, counts, &
                                    lbounds=lbounds, ubounds=ubounds, rc=status)
                  case default
                end select

              case default
            end select
          case default
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
! !IROUTINE: ESMF_LocalArrCreateByMTArr1DI2 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr1DI2(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr1DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I2), dimension(:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DI2(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr1DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr1DI4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr1DI4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr1DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I4), dimension(:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DI4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr1DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr1DI8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr1DI8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr1DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I8), dimension(:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DI8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr1DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr2DI2 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr2DI2(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr2DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DI2(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr2DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr2DI4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr2DI4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr2DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DI4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr2DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr2DI8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr2DI8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr2DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DI8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr2DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr3DI2 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr3DI2(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr3DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DI2(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr3DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr3DI4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr3DI4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr3DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DI4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr3DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr3DI8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr3DI8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr3DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DI8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr3DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr4DI2 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr4DI2(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr4DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DI2(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr4DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr4DI4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr4DI4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr4DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DI4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr4DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr4DI8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr4DI8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr4DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DI8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr4DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr5DI2 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr5DI2(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr5DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DI2(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr5DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr5DI4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr5DI4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr5DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DI4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr5DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr5DI8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr5DI8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr5DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DI8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr5DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr1DR4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr1DR4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr1DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R4), dimension(:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DR4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr1DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr1DR8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr1DR8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr1DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R8), dimension(:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DR8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr1DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr2DR4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr2DR4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr2DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DR4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr2DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr2DR8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr2DR8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr2DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DR8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr2DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr3DR4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr3DR4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr3DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DR4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr3DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr3DR8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr3DR8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr3DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DR8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr3DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr4DR4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr4DR4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr4DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DR4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr4DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr4DR8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr4DR8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr4DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DR8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr4DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr5DR4 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr5DR4(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr5DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DR4(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr5DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTArr5DR8 - make an ESMF array from an unallocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTArr5DR8(f90arr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr5DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DR8(array, counts, newp,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTArr5DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTArr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr1DI2 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr1DI2(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr1DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! lower index bounds 
 integer, dimension(1) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I2), dimension(:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DI2(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr1DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr1DI4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr1DI4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr1DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! lower index bounds 
 integer, dimension(1) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I4), dimension(:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DI4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr1DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr1DI8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr1DI8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr1DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! lower index bounds 
 integer, dimension(1) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I8), dimension(:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DI8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr1DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr2DI2 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr2DI2(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr2DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! lower index bounds 
 integer, dimension(2) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DI2(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr2DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr2DI4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr2DI4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr2DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! lower index bounds 
 integer, dimension(2) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DI4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr2DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr2DI8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr2DI8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr2DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! lower index bounds 
 integer, dimension(2) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DI8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr2DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr3DI2 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr3DI2(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr3DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! lower index bounds 
 integer, dimension(3) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DI2(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr3DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr3DI4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr3DI4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr3DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! lower index bounds 
 integer, dimension(3) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DI4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr3DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr3DI8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr3DI8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr3DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! lower index bounds 
 integer, dimension(3) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DI8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr3DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr4DI2 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr4DI2(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr4DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! lower index bounds 
 integer, dimension(4) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DI2(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr4DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr4DI4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr4DI4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr4DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! lower index bounds 
 integer, dimension(4) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DI4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr4DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr4DI8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr4DI8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr4DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! lower index bounds 
 integer, dimension(4) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DI8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr4DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr5DI2 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr5DI2(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr5DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! lower index bounds 
 integer, dimension(5) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DI2(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr5DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr5DI4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr5DI4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr5DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! lower index bounds 
 integer, dimension(5) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DI4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr5DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr5DI8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr5DI8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr5DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! lower index bounds 
 integer, dimension(5) :: ubounds ! upper index bounds 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DI8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr5DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr1DR4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr1DR4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr1DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! lower index bounds 
 integer, dimension(1) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R4), dimension(:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DR4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr1DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr1DR8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr1DR8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr1DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! lower index bounds 
 integer, dimension(1) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R8), dimension(:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr1DR8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr1DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr2DR4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr2DR4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr2DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! lower index bounds 
 integer, dimension(2) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DR4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr2DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr2DR8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr2DR8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr2DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! lower index bounds 
 integer, dimension(2) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr2DR8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr2DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr3DR4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr3DR4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr3DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! lower index bounds 
 integer, dimension(3) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DR4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr3DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr3DR8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr3DR8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr3DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! lower index bounds 
 integer, dimension(3) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr3DR8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr3DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr4DR4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr4DR4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr4DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! lower index bounds 
 integer, dimension(4) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DR4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr4DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr4DR8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr4DR8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr4DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! lower index bounds 
 integer, dimension(4) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr4DR8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr4DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr5DR4 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr5DR4(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr5DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! lower index bounds 
 integer, dimension(5) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DR4(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr5DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlArr5DR8 - make an ESMF array from an Allocated F90 array 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlArr5DR8(f90arr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr5DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! lower index bounds 
 integer, dimension(5) :: ubounds ! upper index bounds 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 
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
 lbounds = lbound(f90arr) 
 ubounds = ubound(f90arr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_LocalArrConstrF90Ptr5DR8(array, counts, newp,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlArr5DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlArr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr1DI2 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr1DI2(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr1DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DI2(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr1DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr1DI4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr1DI4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr1DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DI4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr1DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr1DI8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr1DI8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr1DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DI8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr1DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr2DI2 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr2DI2(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr2DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DI2(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr2DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr2DI4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr2DI4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr2DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DI4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr2DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr2DI8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr2DI8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr2DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DI8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr2DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr3DI2 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr3DI2(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr3DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DI2(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr3DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr3DI4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr3DI4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr3DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DI4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr3DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr3DI8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr3DI8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr3DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DI8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr3DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr4DI2 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr4DI2(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr4DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DI2(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr4DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr4DI4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr4DI4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr4DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DI4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr4DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr4DI8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr4DI8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr4DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DI8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr4DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr5DI2 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr5DI2(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr5DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DI2(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr5DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr5DI4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr5DI4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr5DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DI4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr5DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr5DI8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr5DI8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr5DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DI8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr5DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr1DR4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr1DR4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr1DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DR4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr1DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr1DR8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr1DR8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr1DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DR8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr1DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr2DR4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr2DR4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr2DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DR4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr2DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr2DR8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr2DR8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr2DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DR8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr2DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr3DR4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr3DR4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr3DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DR4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr3DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr3DR8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr3DR8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr3DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DR8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr3DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr4DR4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr4DR4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr4DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DR4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr4DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr4DR8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr4DR8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr4DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DR8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr4DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr5DR4 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr5DR4(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr5DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DR4(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr5DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByMTPtr5DR8 - make an ESMF array from an unallocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByMTPtr5DR8(f90ptr, counts, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr5DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
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
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DR8(array, counts, f90ptr,& 
 ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByMTPtr5DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByMTPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 




!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr1DI2 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr1DI2(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr1DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DI2(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr1DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr1DI4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr1DI4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr1DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DI4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr1DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr1DI8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr1DI8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr1DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DI8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr1DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr2DI2 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr2DI2(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr2DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DI2(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr2DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr2DI4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr2DI4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr2DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DI4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr2DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr2DI8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr2DI8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr2DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DI8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr2DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr3DI2 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr3DI2(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr3DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DI2(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr3DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr3DI4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr3DI4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr3DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DI4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr3DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr3DI8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr3DI8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr3DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DI8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr3DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr4DI2 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr4DI2(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr4DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DI2(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr4DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr4DI4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr4DI4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr4DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DI4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr4DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr4DI8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr4DI8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr4DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DI8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr4DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr5DI2 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr5DI2(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr5DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DI2(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr5DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr5DI4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr5DI4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr5DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DI4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr5DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr5DI8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr5DI8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr5DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DI8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr5DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr1DR4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr1DR4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr1DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DR4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr1DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr1DR8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr1DR8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr1DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(1) :: lbounds ! per dim 
 integer, dimension(1) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr1DR8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr1DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr2DR4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr2DR4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr2DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DR4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr2DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr2DR8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr2DR8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr2DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(2) :: lbounds ! per dim 
 integer, dimension(2) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr2DR8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr2DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr3DR4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr3DR4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr3DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DR4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr3DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr3DR8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr3DR8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr3DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(3) :: lbounds ! per dim 
 integer, dimension(3) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr3DR8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr3DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr4DR4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr4DR4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr4DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DR4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr4DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr4DR8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr4DR8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr4DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(4) :: lbounds ! per dim 
 integer, dimension(4) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr4DR8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr4DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr5DR4 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr5DR4(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr5DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DR4(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr5DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_LocalArrCreateByFlPtr5DR8 - make an ESMF array from an Allocated F90 pointer 
 
! !INTERFACE: 
 function ESMF_LocalArrCreateByFlPtr5DR8(f90ptr, docopy, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr5DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
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
 integer, dimension(5) :: lbounds ! per dim 
 integer, dimension(5) :: ubounds ! per dim 
 
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
 lbounds = lbound(f90ptr) 
 ubounds = ubound(f90ptr) 
 
 ! Call create routine 
 call c_ESMC_LocalArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_LocalArrConstrF90Ptr5DR8(array, counts, f90ptr,& 
 copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_LocalArrCreateByFlPtr5DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_LocalArrCreateByFlPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr1DI2 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr1DI2(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DI2 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr1DI4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr1DI4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DI4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr1DI8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr1DI8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DI8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr2DI2 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr2DI2(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DI2 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr2DI4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr2DI4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DI4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr2DI8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr2DI8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DI8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr3DI2 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr3DI2(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DI2 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr3DI4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr3DI4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DI4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr3DI8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr3DI8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DI8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr4DI2 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr4DI2(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DI2 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr4DI4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr4DI4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DI4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr4DI8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr4DI8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DI8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr5DI2 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr5DI2(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DI2) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DI2 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr5DI4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr5DI4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DI4) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DI4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr5DI8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr5DI8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DI8) :: wrap ! to pass f90 ptr to C++ 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DI8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr1DR4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr1DR4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DR4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr1DR8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr1DR8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap1DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr1DR8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr2DR4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr2DR4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DR4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr2DR8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr2DR8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap2DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr2DR8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr3DR4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr3DR4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DR4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr3DR8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr3DR8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap3DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr3DR8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr4DR4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr4DR4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DR4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr4DR8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr4DR8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap4DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr4DR8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr5DR4 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr5DR4(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DR4) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DR4 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_LocalArrConstrF90Ptr5DR8 - Create an F90 Ptr of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_LocalArrConstrF90Ptr5DR8(array, counts, f90ptr, docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[f90ptr]}] 
! An optional existing F90 pointer. Will be used instead of an 
! internally generated F90 pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an F90 pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be same length as the rank. 
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
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInternal call 
 
 type (ESMF_ArrWrap5DR8) :: wrap ! to pass f90 ptr to C++ 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 integer, dimension(ESMF_MAXDIM) :: lb, ub 
 integer, dimension(ESMF_MAXDIM) :: offsets 
 
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
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else if (docopy .eq. ESMF_DATA_COPY) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .true. 
 do_dealloc = ESMF_TRUE 
 else ! ESMF_DATA_REF 
 newp => f90ptr ! ptr alias, important this be => 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lb always needs a value even if not allocating 
 lb = 1 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 ub(1:size(counts)) = counts 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
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
 
 ! Until we need offsets, use 0. 
 offsets = 0 
 
 wrap%ptr5DR8 => newp 
 call c_ESMC_LocalArraySetInternal(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lbounds, ubounds, offsets, & 
 ESMF_TRUE, do_dealloc, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_LocalArrConstrF90Ptr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData1DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap1DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 1, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 1, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr1DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr1DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData1DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap1DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 1, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 1, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr1DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr1DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData1DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap1DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(1), ub(1) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 1, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 1, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr1DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr1DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData2DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap2DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 2, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 2, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr2DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr2DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData2DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap2DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 2, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 2, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr2DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr2DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData2DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap2DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(2), ub(2) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 2, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 2, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr2DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr2DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData3DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap3DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 3, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 3, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr3DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr3DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData3DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap3DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 3, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 3, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr3DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr3DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData3DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap3DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(3), ub(3) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 3, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 3, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr3DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr3DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData4DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap4DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 4, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 4, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr4DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr4DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData4DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap4DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 4, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 4, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr4DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr4DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData4DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap4DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(4), ub(4) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 4, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 4, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr4DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr4DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData5DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap5DI2) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 5, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 5, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr5DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr5DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData5DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap5DI4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 5, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 5, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr5DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr5DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData5DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap5DI8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(5), ub(5) ! size info for the array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 5, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 5, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr5DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr5DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData1DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap1DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 1, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 1, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr1DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr1DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData1DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap1DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(1), ub(1) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 1, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 1, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr1DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr1DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData2DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap2DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 2, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 2, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr2DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr2DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData2DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap2DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(2), ub(2) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 2, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 2, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr2DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr2DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData3DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap3DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 3, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 3, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr3DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr3DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData3DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap3DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(3), ub(3) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 3, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 3, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr3DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr3DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData4DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap4DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 4, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 4, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr4DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr4DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData4DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap4DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(4), ub(4) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 4, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 4, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr4DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr4DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData5DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap5DR4) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 5, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 5, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr5DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr5DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayGetData5DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
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
 
 type (ESMF_ArrWrap5DR8) :: wrap ! for passing f90 ptr to C++ 
 integer :: rank, lb(5), ub(5) ! size info for the array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: localp ! local copy 
 
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
 call c_ESMC_ArrayGetLbounds(array, 5, lb, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 call c_ESMC_ArrayGetUbounds(array, 5, ub, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - cannot retrieve array dim sizes" 
 return 
 endif 
 allocate(localp( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "LocalArray do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap%ptr5DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap%ptr5DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_LocalArrayGetData5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate1DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DI2) :: wrap 
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
 deallocate(wrap%ptr1DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate1DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DI4) :: wrap 
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
 deallocate(wrap%ptr1DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate1DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DI8) :: wrap 
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
 deallocate(wrap%ptr1DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate2DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DI2) :: wrap 
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
 deallocate(wrap%ptr2DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate2DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DI4) :: wrap 
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
 deallocate(wrap%ptr2DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate2DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DI8) :: wrap 
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
 deallocate(wrap%ptr2DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate3DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DI2) :: wrap 
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
 deallocate(wrap%ptr3DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate3DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DI4) :: wrap 
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
 deallocate(wrap%ptr3DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate3DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DI8) :: wrap 
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
 deallocate(wrap%ptr3DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate4DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DI2) :: wrap 
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
 deallocate(wrap%ptr4DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate4DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DI4) :: wrap 
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
 deallocate(wrap%ptr4DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate4DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DI8) :: wrap 
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
 deallocate(wrap%ptr4DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate5DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DI2) :: wrap 
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
 deallocate(wrap%ptr5DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate5DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DI4) :: wrap 
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
 deallocate(wrap%ptr5DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate5DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DI8) :: wrap 
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
 deallocate(wrap%ptr5DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate1DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DR4) :: wrap 
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
 deallocate(wrap%ptr1DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate1DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap1DR8) :: wrap 
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
 deallocate(wrap%ptr1DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate2DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DR4) :: wrap 
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
 deallocate(wrap%ptr2DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate2DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap2DR8) :: wrap 
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
 deallocate(wrap%ptr2DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate3DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DR4) :: wrap 
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
 deallocate(wrap%ptr3DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate3DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap3DR8) :: wrap 
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
 deallocate(wrap%ptr3DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate4DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DR4) :: wrap 
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
 deallocate(wrap%ptr4DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate4DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap4DR8) :: wrap 
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
 deallocate(wrap%ptr4DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate5DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DR4) :: wrap 
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
 deallocate(wrap%ptr5DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !INTERFACE: 
 subroutine ESMF_LocalArrayDeallocate5DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_LocalArray) :: array 
 type (ESMF_ArrWrap5DR8) :: wrap 
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
 deallocate(wrap%ptr5DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_LocalArrayDeallocate5DR8 
 
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
! \item[array]
! Destroy contents of this {\tt ESMF\_LocalArray}.
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

        ! Simple validity check
        if (array%this .eq. ESMF_NULL_POINTER) then
            print *, "LocalArray not initialized or Destroyed"
            return
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

        ! mark this as destroyed
        array%this = ESMF_NULL_POINTER

        ! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_LocalArrayDestroy


!------------------------------------------------------------------------------
!BOP
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
! Must be used with care - if you set the values on an already created
! array object to be inconsistent with the F90 pointer, then bad things
! will happen.
!
!EOP

        integer :: status

        call c_ESMC_LocalArraySetInfo(array, counts, lbounds, ubounds, &
                                      offsets, status)

        if (present(rc)) rc = status

        end subroutine ESMF_LocalArraySetInfo


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArrayGetInfo
!
! !INTERFACE:
      subroutine ESMF_LocalArrayGetInfo(array, counts, lbounds, ubounds, &
                                        offsets, rc)
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
! Get back information about counts and upper and lower bounds
! from an already created array object.
!
!EOP

        integer :: status

        call c_ESMC_LocalArrayGetInfo(array, counts, lbounds, ubounds, &
                                      offsets, status)

        if (present(rc)) rc = status

        end subroutine ESMF_LocalArrayGetInfo


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_LocalArraySetData
!
! !INTERFACE:
      subroutine ESMF_LocalArraySetData(array, databuf, docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
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
        if (present(rc)) rc = ESMF_FAILURE

        end subroutine ESMF_LocalArraySetData

!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
     subroutine ESMF_ArraySpecInit(arrayspec, rank, type, kind, rc)
!
!
! !ARGUMENTS:
     type(ESMF_ArraySpec), intent(inout) :: arrayspec
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
! \item[arrayspec]
! Uninitialized array spec.
! \item[rank]
! Array rank (dimensionality, 1D, 2D, etc). Maximum allowed is 7D.
! \item[type]
! {\tt ESMF\_Array} type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
! {\tt ESMF\_Array} kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

        ! Set arrayspec contents with some checking to keep Silverio at bay
        if (rank.ge.1 .and. rank.le.ESMF_MAXDIM) then
          arrayspec%rank = rank
        else
          print *, "ERROR in ESMF_ArraySpecInit: bad rank"
          ! something to trigger on next time that this is bad
          arrayspec%rank = 0
          return
        endif

        ! Since type and kind are derived types, you cannot set them to
        ! illegal values, so no additional tests are needed.
        arrayspec%type = type
        arrayspec%kind = kind

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
      subroutine ESMF_LocalArrayGet(array, rank, type, kind, counts, &
                                    lbounds, ubounds, base, name, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(in) :: array
      integer, intent(out), optional :: rank
      type(ESMF_DataType), intent(out), optional :: type
      type(ESMF_DataKind), intent(out), optional :: kind
      integer, dimension(:), intent(out), optional :: counts
      integer, dimension(:), intent(out), optional :: lbounds
      integer, dimension(:), intent(out), optional :: ubounds
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
         if (status .ne. ESMF_SUCCESS) return
      endif

      if (present(type)) then
         call c_ESMC_LocalArrayGetType(array, type, status)
         if (status .ne. ESMF_SUCCESS) return
      endif

      if (present(kind)) then
         call c_ESMC_LocalArrayGetKind(array, kind, status)
         if (status .ne. ESMF_SUCCESS) return
      endif

      if (present(counts)) then
         call c_ESMC_LocalArrayGetRank(array, lrank, status)
         if (status .ne. ESMF_SUCCESS) return
         call c_ESMC_LocalArrayGetLengths(array, lrank, counts, status)
         if (status .ne. ESMF_SUCCESS) return
      endif

      if (present(lbounds)) then
         call c_ESMC_LocalArrayGetRank(array, lrank, status)
         if (status .ne. ESMF_SUCCESS) return
         call c_ESMC_LocalArrayGetLbounds(array, lrank, lbounds, status)
         if (status .ne. ESMF_SUCCESS) return
      endif

      if (present(ubounds)) then
         call c_ESMC_LocalArrayGetRank(array, lrank, status)
         if (status .ne. ESMF_SUCCESS) return
         call c_ESMC_LocalArrayGetUbounds(array, lrank, ubounds, status)
         if (status .ne. ESMF_SUCCESS) return
      endif

      if (present(base)) then
         call c_ESMC_LocalArrayGetBaseAddr(array, base, status)
         if (status .ne. ESMF_SUCCESS) return
      endif

      if (present(name)) then
         call c_ESMC_LocalArrayGetName(array, name, status)
         if (status .ne. ESMF_SUCCESS) return
      endif

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

      call c_ESMC_LocalArrayGetName(array, name, status)
      if(status .eq. ESMF_FAILURE) then
        print *, "ERROR in ESMF_LocalArrayGetName"
        return
      endif

      if (rcpresent) rc = ESMF_SUCCESS

      end subroutine ESMF_LocalArrayGetName


!------------------------------------------------------------------------------
!BOP
! !INTERFACE:
      subroutine ESMF_ArraySpecGet(arrayspec, rank, type, kind, rc)
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: arrayspec
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
! \item[arrayspec]
! An {\tt ESMF\_ArraySpec} object.
! \item[rank]
! {\tt ESMF\_Array} rank (dimensionality, 1D, 2D, etc). Maximum
! allowed is 7D.
! \item[type]
! {\tt ESMF\_Array} type. Valid types include {\tt ESMF\_DATA\_INTEGER},
! {\tt ESMF\_DATA\_REAL}, {\tt ESMF\_DATA\_LOGICAL},
! {\tt ESMF\_DATA\_CHARACTER}.
! \item[kind]
! {\tt ESMF\_Array} kind. Valid kinds include {\tt ESMF\_KIND\_I4},
! {\tt ESMF\_KIND\_I8}, {\tt ESMF\_KIND\_R4}, {\tt ESMF\_KIND\_R8},
! {\tt ESMF\_KIND\_C8}, {\tt ESMF\_KIND\_C16}.
! \item[[rc]]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
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

        if(present(rank)) rank = arrayspec%rank
        if(present(type)) type = arrayspec%type
        if(present(kind)) kind = arrayspec%kind

        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArraySpecGet


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
     subroutine ESMF_LocalArrayF90Allocate(array, rank, type, kind, counts, &
                                           lbounds, ubounds, rc)
!
! !ARGUMENTS:
      type(ESMF_LocalArray), intent(inout) :: array
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
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
! \item[lbounds]
! An integer array, size {\tt rank}, of each dimensions lower index.
! \item[ubounds]
! An integer array, size {\tt rank}, of each dimensions upper index.
! \item[{[rc]}]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOPI
! !REQUIREMENTS:

    integer :: status ! local error status
    integer, dimension(ESMF_MAXDIM) :: lb, ub
    integer, dimension(ESMF_MAXDIM) :: offsets
    integer :: localkind, localtype

    !! local variables, expanded by macro
! <Created by macro - do not edit directly > 
 type(ESMF_ArrWrap1DI2) :: local1DI2 
 type(ESMF_ArrWrap1DI4) :: local1DI4 
 type(ESMF_ArrWrap1DI8) :: local1DI8 
 
 type(ESMF_ArrWrap2DI2) :: local2DI2 
 type(ESMF_ArrWrap2DI4) :: local2DI4 
 type(ESMF_ArrWrap2DI8) :: local2DI8 
 
 type(ESMF_ArrWrap3DI2) :: local3DI2 
 type(ESMF_ArrWrap3DI4) :: local3DI4 
 type(ESMF_ArrWrap3DI8) :: local3DI8 
 
 type(ESMF_ArrWrap4DI2) :: local4DI2 
 type(ESMF_ArrWrap4DI4) :: local4DI4 
 type(ESMF_ArrWrap4DI8) :: local4DI8 
 
 type(ESMF_ArrWrap5DI2) :: local5DI2 
 type(ESMF_ArrWrap5DI4) :: local5DI4 
 type(ESMF_ArrWrap5DI8) :: local5DI8 
 
 type(ESMF_ArrWrap1DR4) :: local1DR4 
 type(ESMF_ArrWrap1DR8) :: local1DR8 
 
 type(ESMF_ArrWrap2DR4) :: local2DR4 
 type(ESMF_ArrWrap2DR8) :: local2DR8 
 
 type(ESMF_ArrWrap3DR4) :: local3DR4 
 type(ESMF_ArrWrap3DR8) :: local3DR8 
 
 type(ESMF_ArrWrap4DR4) :: local4DR4 
 type(ESMF_ArrWrap4DR8) :: local4DR8 
 
 type(ESMF_ArrWrap5DR4) :: local5DR4 
 type(ESMF_ArrWrap5DR8) :: local5DR8 
 



    status = ESMF_FAILURE
    if (present(rc)) rc = ESMF_FAILURE

    lb(1:size(lbounds)) = lbounds
    ub(1:size(ubounds)) = ubounds

    localtype = type%dtype
    localkind = kind%dkind

    !! macros which are expanded by the preprocessor
    select case (localtype)
      case (ESMF_DATA_INTEGER%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DI2 % ptr1DI2( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local1DI2, & 
 ESMF_DATA_ADDRESS(local1DI2 % ptr1DI2 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DI4 % ptr1DI4( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local1DI4, & 
 ESMF_DATA_ADDRESS(local1DI4 % ptr1DI4 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DI8 % ptr1DI8( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local1DI8, & 
 ESMF_DATA_ADDRESS(local1DI8 % ptr1DI8 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 allocate(local2DI2 % ptr2DI2( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local2DI2, & 
 ESMF_DATA_ADDRESS(local2DI2 % ptr2DI2 ( lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local2DI4 % ptr2DI4( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local2DI4, & 
 ESMF_DATA_ADDRESS(local2DI4 % ptr2DI4 ( lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local2DI8 % ptr2DI8( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local2DI8, & 
 ESMF_DATA_ADDRESS(local2DI8 % ptr2DI8 ( lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 allocate(local3DI2 % ptr3DI2( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local3DI2, & 
 ESMF_DATA_ADDRESS(local3DI2 % ptr3DI2 ( lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local3DI4 % ptr3DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local3DI4, & 
 ESMF_DATA_ADDRESS(local3DI4 % ptr3DI4 ( lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local3DI8 % ptr3DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local3DI8, & 
 ESMF_DATA_ADDRESS(local3DI8 % ptr3DI8 ( lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 allocate(local4DI2 % ptr4DI2( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local4DI2, & 
 ESMF_DATA_ADDRESS(local4DI2 % ptr4DI2 ( lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local4DI4 % ptr4DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local4DI4, & 
 ESMF_DATA_ADDRESS(local4DI4 % ptr4DI4 ( lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local4DI8 % ptr4DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local4DI8, & 
 ESMF_DATA_ADDRESS(local4DI8 % ptr4DI8 ( lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (5)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 allocate(local5DI2 % ptr5DI2( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local5DI2, & 
 ESMF_DATA_ADDRESS(local5DI2 % ptr5DI2 ( lb(1),lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local5DI4 % ptr5DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local5DI4, & 
 ESMF_DATA_ADDRESS(local5DI4 % ptr5DI4 ( lb(1),lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local5DI8 % ptr5DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local5DI8, & 
 ESMF_DATA_ADDRESS(local5DI8 % ptr5DI8 ( lb(1),lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
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
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DR4 % ptr1DR4( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local1DR4, & 
 ESMF_DATA_ADDRESS(local1DR4 % ptr1DR4 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DR8 % ptr1DR8( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local1DR8, & 
 ESMF_DATA_ADDRESS(local1DR8 % ptr1DR8 ( lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local2DR4 % ptr2DR4( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local2DR4, & 
 ESMF_DATA_ADDRESS(local2DR4 % ptr2DR4 ( lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local2DR8 % ptr2DR8( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local2DR8, & 
 ESMF_DATA_ADDRESS(local2DR8 % ptr2DR8 ( lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local3DR4 % ptr3DR4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local3DR4, & 
 ESMF_DATA_ADDRESS(local3DR4 % ptr3DR4 ( lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local3DR8 % ptr3DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local3DR8, & 
 ESMF_DATA_ADDRESS(local3DR8 % ptr3DR8 ( lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local4DR4 % ptr4DR4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local4DR4, & 
 ESMF_DATA_ADDRESS(local4DR4 % ptr4DR4 ( lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local4DR8 % ptr4DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local4DR8, & 
 ESMF_DATA_ADDRESS(local4DR8 % ptr4DR8 ( lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case default
            end select

          case (5)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local5DR4 % ptr5DR4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local5DR4, & 
 ESMF_DATA_ADDRESS(local5DR4 % ptr5DR4 ( lb(1),lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "LocalArray internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local5DR8 % ptr5DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_LocalArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Set offsets for now to 0, since this is apparently unused. 
 offsets = 0 
 
 call c_ESMC_LocalArraySetInternal(array, local5DR8, & 
 ESMF_DATA_ADDRESS(local5DR8 % ptr5DR8 ( lb(1),lb(1),lb(1),lb(1),lb(1) )), & 
 counts, lbounds, ubounds, offsets, & 
 ESMF_TRUE, ESMF_TRUE, status) 
 
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
 type(ESMF_ArrWrap1DI2) :: local1DI2 
 type(ESMF_ArrWrap1DI4) :: local1DI4 
 type(ESMF_ArrWrap1DI8) :: local1DI8 
 
 type(ESMF_ArrWrap2DI2) :: local2DI2 
 type(ESMF_ArrWrap2DI4) :: local2DI4 
 type(ESMF_ArrWrap2DI8) :: local2DI8 
 
 type(ESMF_ArrWrap3DI2) :: local3DI2 
 type(ESMF_ArrWrap3DI4) :: local3DI4 
 type(ESMF_ArrWrap3DI8) :: local3DI8 
 
 type(ESMF_ArrWrap4DI2) :: local4DI2 
 type(ESMF_ArrWrap4DI4) :: local4DI4 
 type(ESMF_ArrWrap4DI8) :: local4DI8 
 
 type(ESMF_ArrWrap5DI2) :: local5DI2 
 type(ESMF_ArrWrap5DI4) :: local5DI4 
 type(ESMF_ArrWrap5DI8) :: local5DI8 
 
 type(ESMF_ArrWrap1DR4) :: local1DR4 
 type(ESMF_ArrWrap1DR8) :: local1DR8 
 
 type(ESMF_ArrWrap2DR4) :: local2DR4 
 type(ESMF_ArrWrap2DR8) :: local2DR8 
 
 type(ESMF_ArrWrap3DR4) :: local3DR4 
 type(ESMF_ArrWrap3DR8) :: local3DR8 
 
 type(ESMF_ArrWrap4DR4) :: local4DR4 
 type(ESMF_ArrWrap4DR8) :: local4DR8 
 
 type(ESMF_ArrWrap5DR4) :: local5DR4 
 type(ESMF_ArrWrap5DR8) :: local5DR8 
 



    if (present(rc)) rc = ESMF_FAILURE

    localtype = type
    localkind = kind

    !! macros which are expanded by the preprocessor
    select case (localtype)
      case (ESMF_DATA_INTEGER%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local1DI2, status) 
 deallocate(local1DI2 % ptr1DI2, stat=status) 
 nullify(local1DI2 % ptr1DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local1DI4, status) 
 deallocate(local1DI4 % ptr1DI4, stat=status) 
 nullify(local1DI4 % ptr1DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local1DI8, status) 
 deallocate(local1DI8 % ptr1DI8, stat=status) 
 nullify(local1DI8 % ptr1DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local2DI2, status) 
 deallocate(local2DI2 % ptr2DI2, stat=status) 
 nullify(local2DI2 % ptr2DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local2DI4, status) 
 deallocate(local2DI4 % ptr2DI4, stat=status) 
 nullify(local2DI4 % ptr2DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local2DI8, status) 
 deallocate(local2DI8 % ptr2DI8, stat=status) 
 nullify(local2DI8 % ptr2DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local3DI2, status) 
 deallocate(local3DI2 % ptr3DI2, stat=status) 
 nullify(local3DI2 % ptr3DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local3DI4, status) 
 deallocate(local3DI4 % ptr3DI4, stat=status) 
 nullify(local3DI4 % ptr3DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local3DI8, status) 
 deallocate(local3DI8 % ptr3DI8, stat=status) 
 nullify(local3DI8 % ptr3DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local4DI2, status) 
 deallocate(local4DI2 % ptr4DI2, stat=status) 
 nullify(local4DI2 % ptr4DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local4DI4, status) 
 deallocate(local4DI4 % ptr4DI4, stat=status) 
 nullify(local4DI4 % ptr4DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local4DI8, status) 
 deallocate(local4DI8 % ptr4DI8, stat=status) 
 nullify(local4DI8 % ptr4DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (5)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local5DI2, status) 
 deallocate(local5DI2 % ptr5DI2, stat=status) 
 nullify(local5DI2 % ptr5DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local5DI4, status) 
 deallocate(local5DI4 % ptr5DI4, stat=status) 
 nullify(local5DI4 % ptr5DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local5DI8, status) 
 deallocate(local5DI8 % ptr5DI8, stat=status) 
 nullify(local5DI8 % ptr5DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case default
        end select

       case (ESMF_DATA_REAL%dtype)
        select case (rank)
          case (1)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local1DR4, status) 
 deallocate(local1DR4 % ptr1DR4, stat=status) 
 nullify(local1DR4 % ptr1DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local1DR8, status) 
 deallocate(local1DR8 % ptr1DR8, stat=status) 
 nullify(local1DR8 % ptr1DR8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local2DR4, status) 
 deallocate(local2DR4 % ptr2DR4, stat=status) 
 nullify(local2DR4 % ptr2DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local2DR8, status) 
 deallocate(local2DR8 % ptr2DR8, stat=status) 
 nullify(local2DR8 % ptr2DR8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local3DR4, status) 
 deallocate(local3DR4 % ptr3DR4, stat=status) 
 nullify(local3DR4 % ptr3DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local3DR8, status) 
 deallocate(local3DR8 % ptr3DR8, stat=status) 
 nullify(local3DR8 % ptr3DR8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local4DR4, status) 
 deallocate(local4DR4 % ptr4DR4, stat=status) 
 nullify(local4DR4 % ptr4DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local4DR8, status) 
 deallocate(local4DR8 % ptr4DR8, stat=status) 
 nullify(local4DR8 % ptr4DR8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (5)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local5DR4, status) 
 deallocate(local5DR4 % ptr5DR4, stat=status) 
 nullify(local5DR4 % ptr5DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_LocalArrayGetF90Ptr(array, local5DR8, status) 
 deallocate(local5DR8 % ptr5DR8, stat=status) 
 nullify(local5DR8 % ptr5DR8) 
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
! This section is for higher level LocalArray funcs
!
!------------------------------------------------------------------------------
!BOP
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
! Extract an (N-1)D array from an N-D array. The dimension to be
! dropped is the {\tt slicedim} argument, and the location along
! the dropped dimension is the {\tt sliceloc} argument. This routine
! allocates new space and copies the data, leaving the original array
! unchanged.
!
!EOP
        ! Local vars
        type (ESMF_LocalArray) :: newarray ! new C++ LocalArray
        integer :: status ! local error status
        logical :: rcpresent ! did user specify rc?
        integer :: rank
        type (ESMF_DataKind) :: kind
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

        ! Get info from the existing array
        call c_ESMC_LocalArrayGetRank(array, rank, status)
        call c_ESMC_LocalArrayGetType(array, type, status)
        call c_ESMC_LocalArrayGetKind(array, kind, status)
        call c_ESMC_LocalArrayGetLengths(array, rank, counts, status)

        ! Basic sanity checks - slice dim is ok, sliced location exists, etc.
        if ((slicedim .lt. 1) .or. (slicedim .gt. rank)) then
            print *, "ESMF_LocalArraySlice: slicedim value ", slicedim, &
                                               " must be between 1 and ", rank
            return
        endif
        if ((sliceloc .lt. 1) .or. (sliceloc .gt. counts(rank))) then
            print *, "ESMF_LocalArraySlice: sliceloc value ", sliceloc, &
                                        " must be between 1 and ", counts(rank)
            return
        endif

        ! This slice will be rank < 1. Remove the counts corresponding
        ! to the sliced dim (save for later error checking).
        ! TODO: add error checks
        do i=sliceloc, rank-1
           counts(i) = counts(i+1)
        enddo
        rank = rank - 1

        call c_ESMC_LocalArrayCreateNoData(newarray, rank, type, kind, &
                                           ESMF_FROM_FORTRAN, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "LocalArray construction error"
          return
        endif

        call ESMF_LocalArrConstrF90Ptr(newarray, counts, rank, type, kind, &
                                          lb, ub, status)

        ! At this point the new array exists, and has space allocated, but it
        ! does not contain data from the old array. Now we have a T/K/R prob.

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
        if (present(rc)) rc = ESMF_FAILURE

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

        if (present(rc)) rc = ESMF_FAILURE

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

        if (present(rc)) rc = ESMF_FAILURE

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

       if (array%this .eq. ESMF_NULL_POINTER) then
         print *, "LocalArray Print:"
         print *, " Empty or Uninitialized Array"
         if (present(rc)) rc = ESMF_SUCCESS
         return
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
