! $Id: ESMF_ArrayExpand.F90,v 1.19 2004/03/11 20:15:59 nscollins Exp $
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
      module ESMF_ArrayExpandMod
!
!==============================================================================
!
! This file contains the Array class methods which are automatically
! generated from macros to handle the type/kind/rank overloading.
! See ESMF_ArrayBase.F90 for non-macroized entry points.
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
#include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayBaseMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

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
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_ArrayCreate, ESMF_ArrayDestroy

      public ESMF_ArraySetData
      public ESMF_ArrayGetData

      public ESMF_ArrayF90Allocate
      public ESMF_ArrayF90Deallocate
      public ESMF_ArrayConstructF90Ptr ! needed for C++ callback only

!EOP
      public operator(.eq.), operator(.ne.)

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_ArrayExpand.F90,v 1.19 2004/03/11 20:15:59 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================

!BOP
! !IROUTINE: ESMF_ArrayCreate -- Generic interface to create an Array
!
! !INTERFACE:
     interface ESMF_ArrayCreate
!
! !PRIVATE MEMBER FUNCTIONS:
!
        module procedure ESMF_ArrayCreateByList ! specify TKR
        module procedure ESMF_ArrayCreateBySpec ! specify ArraySpec
!
        ! Plus interfaces for each T/K/R

!EOP


! ! < interfaces for each T/K/R >
! --Array--InterfaceMacro(ArrayCreateByMTArr)
!
! ! < interfaces for each T/K/R >
! --Array--InterfaceMacro(ArrayCreateByFullArr)

       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_ArrayCreateByMTPtr1DI2 
 module procedure ESMF_ArrayCreateByMTPtr1DI4 
 module procedure ESMF_ArrayCreateByMTPtr1DI8 
 module procedure ESMF_ArrayCreateByMTPtr2DI2 
 module procedure ESMF_ArrayCreateByMTPtr2DI4 
 module procedure ESMF_ArrayCreateByMTPtr2DI8 
 module procedure ESMF_ArrayCreateByMTPtr3DI2 
 module procedure ESMF_ArrayCreateByMTPtr3DI4 
 module procedure ESMF_ArrayCreateByMTPtr3DI8 
 module procedure ESMF_ArrayCreateByMTPtr4DI2 
 module procedure ESMF_ArrayCreateByMTPtr4DI4 
 module procedure ESMF_ArrayCreateByMTPtr4DI8 
 module procedure ESMF_ArrayCreateByMTPtr5DI2 
 module procedure ESMF_ArrayCreateByMTPtr5DI4 
 module procedure ESMF_ArrayCreateByMTPtr5DI8 
 module procedure ESMF_ArrayCreateByMTPtr1DR4 
 module procedure ESMF_ArrayCreateByMTPtr1DR8 
 module procedure ESMF_ArrayCreateByMTPtr2DR4 
 module procedure ESMF_ArrayCreateByMTPtr2DR8 
 module procedure ESMF_ArrayCreateByMTPtr3DR4 
 module procedure ESMF_ArrayCreateByMTPtr3DR8 
 module procedure ESMF_ArrayCreateByMTPtr4DR4 
 module procedure ESMF_ArrayCreateByMTPtr4DR8 
 module procedure ESMF_ArrayCreateByMTPtr5DR4 
 module procedure ESMF_ArrayCreateByMTPtr5DR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


       ! < interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_ArrayCreateByFullPtr1DI2 
 module procedure ESMF_ArrayCreateByFullPtr1DI4 
 module procedure ESMF_ArrayCreateByFullPtr1DI8 
 module procedure ESMF_ArrayCreateByFullPtr2DI2 
 module procedure ESMF_ArrayCreateByFullPtr2DI4 
 module procedure ESMF_ArrayCreateByFullPtr2DI8 
 module procedure ESMF_ArrayCreateByFullPtr3DI2 
 module procedure ESMF_ArrayCreateByFullPtr3DI4 
 module procedure ESMF_ArrayCreateByFullPtr3DI8 
 module procedure ESMF_ArrayCreateByFullPtr4DI2 
 module procedure ESMF_ArrayCreateByFullPtr4DI4 
 module procedure ESMF_ArrayCreateByFullPtr4DI8 
 module procedure ESMF_ArrayCreateByFullPtr5DI2 
 module procedure ESMF_ArrayCreateByFullPtr5DI4 
 module procedure ESMF_ArrayCreateByFullPtr5DI8 
 module procedure ESMF_ArrayCreateByFullPtr1DR4 
 module procedure ESMF_ArrayCreateByFullPtr1DR8 
 module procedure ESMF_ArrayCreateByFullPtr2DR4 
 module procedure ESMF_ArrayCreateByFullPtr2DR8 
 module procedure ESMF_ArrayCreateByFullPtr3DR4 
 module procedure ESMF_ArrayCreateByFullPtr3DR8 
 module procedure ESMF_ArrayCreateByFullPtr4DR4 
 module procedure ESMF_ArrayCreateByFullPtr4DR8 
 module procedure ESMF_ArrayCreateByFullPtr5DR4 
 module procedure ESMF_ArrayCreateByFullPtr5DR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!BOP
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
! There are 3 options for
! specifying the type/kind/rank of the {\tt ESMF\_Array} data:
! \begin{description}
! \item[List]
! The characteristics of the {\tt ESMF\_Array} are given explicitly
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
! The concept of an ``empty'' {\tt Array} does not exist. To make an
! ESMF object which stores the Type/Kind/Rank information create an
! {\tt ESMF\_ArraySpec} object which can then be used repeatedly in
! subsequent {\tt Array} Create calls.
!
end interface
!EOP

!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_ArrayGetData -- Get a Fortran pointer to the data contents
!
! !INTERFACE:
     interface ESMF_ArrayGetData
!
! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_ArrayGetData1DI2 
 module procedure ESMF_ArrayGetData1DI4 
 module procedure ESMF_ArrayGetData1DI8 
 module procedure ESMF_ArrayGetData2DI2 
 module procedure ESMF_ArrayGetData2DI4 
 module procedure ESMF_ArrayGetData2DI8 
 module procedure ESMF_ArrayGetData3DI2 
 module procedure ESMF_ArrayGetData3DI4 
 module procedure ESMF_ArrayGetData3DI8 
 module procedure ESMF_ArrayGetData4DI2 
 module procedure ESMF_ArrayGetData4DI4 
 module procedure ESMF_ArrayGetData4DI8 
 module procedure ESMF_ArrayGetData5DI2 
 module procedure ESMF_ArrayGetData5DI4 
 module procedure ESMF_ArrayGetData5DI8 
 module procedure ESMF_ArrayGetData1DR4 
 module procedure ESMF_ArrayGetData1DR8 
 module procedure ESMF_ArrayGetData2DR4 
 module procedure ESMF_ArrayGetData2DR8 
 module procedure ESMF_ArrayGetData3DR4 
 module procedure ESMF_ArrayGetData3DR8 
 module procedure ESMF_ArrayGetData4DR4 
 module procedure ESMF_ArrayGetData4DR8 
 module procedure ESMF_ArrayGetData5DR4 
 module procedure ESMF_ArrayGetData5DR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_ArrayGetData} functions.
!
!EOP
end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section includes the Array Create methods.
!
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateBySpec -- Create a new Array from an ArraySpec
!
! !INTERFACE:
      function ESMF_ArrayCreateBySpec(arrayspec, counts, halo_width, &
                                      lbounds, ubounds, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateBySpec
!
! !ARGUMENTS:
      type(ESMF_ArraySpec), intent(in) :: arrayspec
      integer, intent(in), dimension(:) :: counts
      integer, intent(in), optional :: halo_width
      integer, dimension(:), intent(in), optional :: lbounds
      integer, dimension(:), intent(in), optional :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new Array and allocate data space, which remains uninitialized.
! The return value is a new Array.
!
! The arguments are:
! \begin{description}
! \item[arrayspec]
! ArraySpec object.
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
! \item[{[halo_width]}]
! Set the maximum width of the halo region on all edges. Defaults to 0.
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

        call ESMF_ArraySpecGet(arrayspec, rank, type, kind, status)
        if (status .ne. ESMF_SUCCESS) return

        ! Call the list function to make the array
        ESMF_ArrayCreateBySpec = ESMF_ArrayCreateByList(rank, type, kind, &
                                                       counts, halo_width, &
                                                       lbounds, ubounds, status)
        if (rcpresent) rc = status

        end function ESMF_ArrayCreateBySpec


!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_ArrayCreateByList -- Create an Array specifying all options.
!
! !INTERFACE:
      function ESMF_ArrayCreateByList(rank, type, kind, counts, &
                                      halo_width, lbounds, ubounds, rc)
!
! !RETURN VALUE:
      type(ESMF_Array) :: ESMF_ArrayCreateByList
!
! !ARGUMENTS:
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, intent(in), optional :: halo_width
      integer, dimension(:), intent(in), optional :: lbounds
      integer, dimension(:), intent(in), optional :: ubounds
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Create a new Array and allocate data space, which remains uninitialized.
! The return value is a new Array.
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
! \item[{[halo_width]}]
! Set the maximum width of the halo region on all edges. Defaults to 0.
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
        type (ESMF_Array) :: array ! new C++ Array
        integer :: hwidth ! local copy of halo width
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

        ! Always supply a halo value, setting it to 0 if not specified.
        if (present(halo_width)) then
          hwidth = halo_width
        else
          hwidth = 0
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
        call c_ESMC_ArrayCreateNoData(array, rank, type, kind, &
                                            ESMF_FROM_FORTRAN, status)
        if (status .ne. ESMF_SUCCESS) then
          print *, "Array construction error"
          return
        endif

        call ESMF_ArrayConstructF90Ptr(array, counts, hwidth, rank, type, &
                                       kind, lb, ub, status)

        ! Set return values
        ESMF_ArrayCreateByList = array
        if (rcpresent) rc = status

        end function ESMF_ArrayCreateByList


!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayConstructF90Ptr - Create and add a Fortran ptr to array
!
! !INTERFACE:
     subroutine ESMF_ArrayConstructF90Ptr(array, counts, hwidth, &
                                         rank, type, kind, lbounds, ubounds, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      integer, dimension(:), intent(in) :: counts
      integer, intent(in) :: hwidth
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(out) :: rc
!
! !DESCRIPTION:
! Take a partially created {\tt Array} and T/K/R information and call the
! proper subroutine to create an F90 pointer, allocate space, and set the
! corresponding values in the {\tt Array} object.
!
! The arguments are:
! \begin{description}
! \item[array]
! Partially created {\tt ESMF\_Array} object. This entry point is used
! during both the C++ and F90 create calls if we need to create an F90
! pointer to be used later.
! \item[counts]
! The number of items in each dimension of the array. This is a 1D
! integer array the same length as the rank.
! \item[hwidth]
! The halo width on all edges. Used to set the computational area
! in the array.
! \item[rank]
! Array rank.
! This must match what is already in the array - it is here only as
! a convenience.
! \item[type]
! Array type.
! This must match what is already in the array - it is here only as
! a convenience.
! \item[kind]
! Array kind.
! This must match what is already in the array - it is here only as
! a convenience.
! \item[lbounds]
! The lower index values per rank.
! \item[ubounds]
! The upper index values per rank.
! \item[rc]
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors.
! \end{description}
!
!EOP
! !REQUIREMENTS:


        ! Local vars
        integer :: localkind, localtype

        ! Initialize return code; assume failure until success is certain
        ! Note from this point down in the calling stack rc is not optional.
        ! This is all internal code, heavily macroized - no reason to add
        ! unnecessary code to check for non-present error return variables.
        rc = ESMF_FAILURE

        localtype = type%dtype
        localkind = kind%dkind

        ! Call a T/K/R specific interface in order to create the proper
        ! type of F90 pointer, allocate the space, set the values in the
        ! Array object, and return. (The routine this code is calling is
        ! generated by macro.)

        ! Call proper create F90 ptr routine
        select case (localtype)
          case (ESMF_DATA_INTEGER%dtype)
            select case (rank)
              case (1)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I4%dkind)
                    call ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I8%dkind)
                    call ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case (2)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I4%dkind)
                    call ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I8%dkind)
                    call ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case (3)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I4%dkind)
                    call ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I8%dkind)
                    call ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case (4)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I4%dkind)
                    call ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I8%dkind)
                    call ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case (5)
                select case (localkind)
                  case (ESMF_I2%dkind)
                    call ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I4%dkind)
                    call ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_I8%dkind)
                    call ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case default
            end select

           case (ESMF_DATA_REAL%dtype)
            select case (rank)
              case (1)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_R8%dkind)
                    call ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case (2)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_R8%dkind)
                    call ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case (3)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_R8%dkind)
                    call ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case (4)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_R8%dkind)
                    call ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case (5)
                select case (localkind)
                  case (ESMF_R4%dkind)
                    call ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case (ESMF_R8%dkind)
                    call ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth, &
                         lbounds=lbounds, ubounds=ubounds, rc=rc)
                  case default
                    print *, "unsupported kind"
                end select

              case default
                print *, "unsupported rank"
            end select
          case default
            print *, "unsupported type"
         end select

        ! Note: rc is already set, nothing to do here.

        end subroutine ESMF_ArrayConstructF90Ptr

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArraySetData
!
! !INTERFACE:
      subroutine ESMF_ArraySetData(array, databuf, docopy, rc)
!
! !ARGUMENTS:
      type(ESMF_Array) :: array
      real, dimension (:), pointer :: databuf
      type(ESMF_CopyFlag), intent(in) :: docopy
      integer, intent(out), optional :: rc
!
! !DESCRIPTION:
! Used only with the version of ArrayCreate which creates an empty
! Array and allows the Data to be specified later. Otherwise it is an
! error to replace the data contents associated with a Array.
!
! TODO: this needs to be macroized for T/K/R, just like create
!
!EOPI
! !REQUIREMENTS:

!
! Changed BOP/EOP to BOPI/EOPI until code is added.
! TODO: code goes here
!
        rc = ESMF_FAILURE

        end subroutine ESMF_ArraySetData

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr1DI2 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr1DI2(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr1DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr1DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr1DI4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr1DI4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr1DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr1DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr1DI8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr1DI8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr1DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr1DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr2DI2 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr2DI2(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr2DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr2DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr2DI4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr2DI4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr2DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr2DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr2DI8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr2DI8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr2DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr2DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr3DI2 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr3DI2(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr3DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr3DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr3DI4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr3DI4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr3DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr3DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr3DI8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr3DI8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr3DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr3DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr4DI2 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr4DI2(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr4DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr4DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr4DI4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr4DI4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr4DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr4DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr4DI8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr4DI8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr4DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr4DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr5DI2 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr5DI2(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr5DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr5DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr5DI4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr5DI4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr5DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr5DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr5DI8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr5DI8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr5DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr5DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr1DR4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr1DR4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr1DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr1DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr1DR8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr1DR8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr1DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr1DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr2DR4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr2DR4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr2DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr2DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr2DR8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr2DR8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr2DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr2DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr3DR4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr3DR4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr3DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr3DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr3DR8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr3DR8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr3DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr3DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr4DR4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr4DR4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr4DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr4DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr4DR8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr4DR8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr4DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr4DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr5DR4 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr5DR4(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr5DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr5DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTArr5DR8 - make an ESMF array from an unallocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTArr5DR8(f90arr, counts, haloWidth, lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTArr5DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran 
! 90 array. This routine allocates memory to the array and fills in 
! the array object with all necessary information. 
! 
! The function return is an {\tt ESMF\_Array} type with space 
! allocated for data. 
! 
! The arguments are: 
! \begin{description} 
! \item[f90arr] 
! An allocatable (but currently unallocated) Fortran 90 array. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! An integer count of the width of the halo region on all sides of 
! the array. The default is 0, no halo region. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 ! print *, "Array cannot already be allocated" 
 ! return 
 !endif 
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth, & 
 newp, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTArr5DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTArr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr1DI2 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr1DI2(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr1DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr1DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr1DI4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr1DI4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr1DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr1DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr1DI8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr1DI8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr1DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr1DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr2DI2 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr2DI2(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr2DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr2DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr2DI4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr2DI4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr2DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr2DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr2DI8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr2DI8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr2DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr2DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr3DI2 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr3DI2(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr3DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr3DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr3DI4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr3DI4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr3DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr3DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr3DI8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr3DI8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr3DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr3DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr4DI2 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr4DI2(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr4DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr4DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr4DI4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr4DI4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr4DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr4DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr4DI8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr4DI8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr4DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr4DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr5DI2 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr5DI2(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr5DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I2), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr5DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr5DI4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr5DI4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr5DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr5DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr5DI8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr5DI8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr5DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), target :: f90arr 
 !integer (ESMF_KIND_I8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr5DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr1DR4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr1DR4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr1DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr1DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr1DR8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr1DR8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr1DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr1DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr2DR4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr2DR4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr2DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr2DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr2DR8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr2DR8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr2DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr2DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr3DR4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr3DR4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr3DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr3DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr3DR8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr3DR8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr3DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr3DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr4DR4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr4DR4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr4DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr4DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr4DR8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr4DR8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr4DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr4DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr5DR4 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr5DR4(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr5DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R4), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr5DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullArr5DR8 - make an ESMF array from an Allocated Fortran array 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullArr5DR8(f90arr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullArr5DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), target :: f90arr 
 !real (ESMF_KIND_R8), dimension(:,:,:,:,:), allocatable, target :: f90arr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 newp => f90arr ! must be ptr assignment, => 
 call ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth,& 
 newp, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullArr5DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullArr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr1DI2 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr1DI2(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr1DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr1DI4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr1DI4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr1DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr1DI8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr1DI8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr1DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr2DI2 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr2DI2(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr2DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr2DI4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr2DI4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr2DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr2DI8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr2DI8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr2DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr3DI2 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr3DI2(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr3DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr3DI4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr3DI4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr3DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr3DI8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr3DI8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr3DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr4DI2 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr4DI2(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr4DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr4DI4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr4DI4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr4DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr4DI8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr4DI8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr4DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr5DI2 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr5DI2(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr5DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr5DI4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr5DI4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr5DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr5DI8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr5DI8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr5DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr1DR4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr1DR4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr1DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr1DR8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr1DR8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr1DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr1DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr2DR4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr2DR4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr2DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr2DR8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr2DR8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr2DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr2DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr3DR4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr3DR4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr3DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr3DR8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr3DR8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr3DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr3DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr4DR4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr4DR4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr4DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr4DR8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr4DR8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr4DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr4DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr5DR4 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr5DR4(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr5DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByMTPtr5DR8 - make an ESMF array from an unallocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByMTPtr5DR8(f90ptr, counts, haloWidth, & 
 lbounds, ubounds, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr5DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in), optional :: haloWidth 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
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
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth,& 
 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByMTPtr5DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByMTPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 




!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr1DI2 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr1DI2(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr1DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr1DI4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr1DI4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr1DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr1DI8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr1DI8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr1DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr2DI2 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr2DI2(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr2DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr2DI4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr2DI4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr2DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr2DI8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr2DI8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr2DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr3DI2 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr3DI2(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr3DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr3DI4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr3DI4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr3DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr3DI8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr3DI8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr3DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr4DI2 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr4DI2(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr4DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr4DI4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr4DI4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr4DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr4DI8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr4DI8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr4DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr5DI2 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr5DI2(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr5DI2 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I2, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DI2 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr5DI4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr5DI4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr5DI4 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DI4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr5DI8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr5DI8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr5DI8 
! 
! !ARGUMENTS: 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_integer, ESMF_I8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DI8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr1DR4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr1DR4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr1DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr1DR8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr1DR8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr1DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 1, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr1DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr2DR4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr2DR4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr2DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr2DR8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr2DR8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr2DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 2, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr2DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr3DR4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr3DR4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr3DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr3DR8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr3DR8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr3DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 3, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr3DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr4DR4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr4DR4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr4DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr4DR8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr4DR8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr4DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 4, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr4DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr5DR4 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr5DR4(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr5DR4 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R4, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DR4 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayCreateByFullPtr5DR8 - make an ESMF array from an Allocated Fortran pointer 
 
! !INTERFACE: 
 function ESMF_ArrayCreateByFullPtr5DR8(f90ptr, docopy, haloWidth, rc) 
! 
! !RETURN VALUE: 
 type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr5DR8 
! 
! !ARGUMENTS: 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(in), optional :: haloWidth 
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
! \item[{[docopy]}] 
! Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference 
! the existing data array. If set to {\tt ESMF\_DATA\_COPY} this routine 
! allocates new space and copies the data from the pointer into the 
! new array. 
! \item[{[haloWidth]}] 
! Set the maximum width of the halo region on all edges. Defaults to 0. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 type (ESMF_Array) :: array ! new array object 
 integer :: status ! local error status 
 integer :: hwidth ! local copy of halo width 
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
 
 ! Always supply a halo value, setting it to 0 if not specified. 
 if (present(haloWidth)) then 
 hwidth = haloWidth 
 else 
 hwidth = 0 
 endif 
 
 ! Call create routine 
 call c_ESMC_ArrayCreateNoData(array, 5, ESMF_DATA_real, ESMF_R8, & 
 ESMF_FROM_FORTRAN, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array initial construction error" 
 return 
 endif 
 
 call ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth,& 
 f90ptr, copy, lbounds, ubounds, status) 
 
 
! ! return value set by c_ESMC func above 
 ESMF_ArrayCreateByFullPtr5DR8 = array 
 if (rcpresent) rc = status 
 
 end function ESMF_ArrayCreateByFullPtr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr1DI2 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr1DI2(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DI2 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr1DI4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr1DI4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DI4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr1DI8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr1DI8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DI8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr2DI2 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr2DI2(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DI2 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr2DI4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr2DI4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DI4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr2DI8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr2DI8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DI8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr3DI2 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr3DI2(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DI2 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr3DI4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr3DI4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DI4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr3DI8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr3DI8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DI8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr4DI2 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr4DI2(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DI2 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr4DI4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr4DI4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DI4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr4DI8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr4DI8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DI8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr5DI2 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr5DI2(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DI2 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr5DI4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr5DI4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DI4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr5DI8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr5DI8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DI8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr1DR4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr1DR4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DR4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr1DR8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr1DR8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr1DR8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr2DR4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr2DR4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DR4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr2DR8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr2DR8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr2DR8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr3DR4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr3DR4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DR4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr3DR8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr3DR8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr3DR8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr4DR4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr4DR4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DR4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr4DR8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr4DR8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr4DR8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr5DR4 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr5DR4(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DR4 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !IROUTINE: ESMF_ArrayConstructF90Ptr5DR8 - Create a Fortran Pointer of the proper T/K/R 
 
! !INTERFACE: 
 subroutine ESMF_ArrayConstructF90Ptr5DR8(array, counts, hwidth, f90ptr, & 
 docopy, lbounds, ubounds, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array), intent(inout) :: array 
 integer, dimension(:), intent(in) :: counts 
 integer, intent(in) :: hwidth 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer, optional :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, dimension(:), intent(in), optional :: lbounds 
 integer, dimension(:), intent(in), optional :: ubounds 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Creates a Fortran pointer of the requested T/K/R. After creating the 
! pointer and doing the allocation based on counts, also goes ahead and 
! calls into the C++ interfaces to set values on the {\tt ESMF\_Array} 
! object. (This is to save on the total number of nested crossings of the 
! Fortran/C++ boundary.) 
! 
! Optional args are an existing Fortran pointer which if given is used 
! instead of a new one, and a docopy flag which if set to copy will 
! do a contents copy or reference. 
! The arguments are: 
! \begin{description} 
! \item[array] 
! The {\tt ESMF\_Array} to set the values into. 
! \item[counts] 
! An integer array of counts. Must be the same length as the rank. 
! \item[hwidth] 
! An integer halo width. Width on each edge. 
! \item[{[f90ptr]}] 
! An optional existing Fortran pointer. Will be used instead of an 
! internally generated Fortran pointer if given. Must be given if the 
! {\tt docopy} is specified. 
! \item[{[docopy]}] 
! An optional copy flag which can be specified if an Fortran pointer is also 
! given. Can either make a new copy of the data or ref existing data. 
! \item[{[lbounds]}] 
! An integer array of lower index values. Must be the same length as the rank. 
! \item[{[ubounds]}] 
! An integer array of upper index values. Must be the same length as the rank. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOPI 
 
 ! Local variables 
 integer :: i ! temp var 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 logical :: willalloc ! do we need to alloc/dealloc? 
 logical :: willcopy ! do we need to copy data? 
 type(ESMF_Logical) :: do_dealloc ! dealloc flag for SetInfo call 
 
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
 
 ! Assume defaults first, then alter if lb or ub specified, 
 ! or if an existing pointer is given and can be queried. 
 lb(:) = 1 
 ub(1:size(counts)) = counts 
 
 ! Decide if we need to do: make a new allocation, copy existing data 
 if (.not. present(f90ptr)) then 
 nullify(newp) 
 willalloc = .true. 
 willcopy = .false. 
 do_dealloc = ESMF_TRUE 
 else 
 if (docopy .eq. ESMF_DATA_SPACE) then 
 newp => f90ptr ! ptr alias, important this be => 
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
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
 lb(1:size(counts)) = lbound(f90ptr) 
 ub(1:size(counts)) = ubound(f90ptr) 
 willalloc = .false. 
 willcopy = .false. 
 do_dealloc = ESMF_FALSE 
 endif 
 endif 
 
 ! lbounds, if given, should be used 
 if (present(lbounds)) then 
 lb(1:size(lbounds)) = lbounds 
 endif 
 
 ! ub is only used during allocation 
 if (willalloc) then 
 if (present(ubounds)) then 
 ub(1:size(ubounds)) = ubounds 
 endif 
 allocate(newp ( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then ! f90 status, not ESMF 
 print *, "Array space allocate error" 
 return 
 endif 
 endif 
 
 if (willcopy) then 
 newp = f90ptr ! contents copy, important that this be = 
 endif 
 
 ! Now set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Until we need to use byte offsets, leave them 0. 
 offsets = 0 
 
 wrap % ptr5DR8 => newp 
 call c_ESMC_ArraySetInfo(array, wrap, & 
 ESMF_DATA_ADDRESS(newp(lb(1),lb(1),lb(1),lb(1),lb(1))), counts, & 
 lb, ub, offsets, & 
 ESMF_TRUE, do_dealloc, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_ArrayConstructF90Ptr5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData1DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr1DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr1DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData1DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr1DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr1DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData1DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr1DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr1DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData2DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr2DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr2DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData2DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr2DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr2DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData2DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr2DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr2DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData3DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr3DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr3DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData3DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr3DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr3DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData3DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr3DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr3DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData4DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr4DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr4DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData4DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr4DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr4DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData4DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr4DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr4DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData5DI2(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr5DI2 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr5DI2 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData5DI4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr5DI4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr5DI4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData5DI8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr5DI8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr5DI8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData1DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr1DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr1DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData1DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr1DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr1DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData2DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr2DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr2DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData2DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr2DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr2DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData3DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr3DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr3DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData3DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr3DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr3DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData4DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr4DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr4DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData4DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr4DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr4DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData5DR4(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr5DR4 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr5DR4 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayGetData5DR8(array, f90ptr, docopy, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: docopy 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Return a Fortran pointer to the existing data buffer, 
! or return a Fortran pointer to a new copy of the data. 
! 
!EOPI 
 
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
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array - get pointer error" 
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
 print *, "Array do_copy allocate error" 
 return 
 endif 
 ! this must do a contents assignment 
 localp = wrap % ptr5DR8 
 f90ptr => localp 
 else 
 f90ptr => wrap % ptr5DR8 
 endif 
 
 if (rcpresent) rc = ESMF_SUCCESS 
 
 end subroutine ESMF_ArrayGetData5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

!! < start of macros which become actual function bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate1DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap1DI2) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate1DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap1DI4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate1DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap1DI8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate2DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap2DI2) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate2DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap2DI4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate2DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap2DI8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate3DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap3DI2) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate3DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap3DI4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate3DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap3DI8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate4DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap4DI2) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate4DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap4DI4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate4DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap4DI8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate5DI2(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap5DI2) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DI2) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate5DI4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap5DI4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DI4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate5DI8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap5DI8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DI8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate1DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap1DR4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate1DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap1DR8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr1DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate2DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap2DR4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate2DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap2DR8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr2DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate3DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap3DR4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate3DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap3DR8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr3DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate4DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap4DR4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate4DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap4DR8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr4DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate5DR4(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap5DR4) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DR4) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOPI 
! !INTERFACE: 
 subroutine ESMF_ArrayDeallocate5DR8(array, wrap, rc) 
! 
! !RETURN VALUE: 
! 
! !ARGUMENTS: 
 type(ESMF_Array) :: array 
 type (ESMF_ArrWrap5DR8) :: wrap 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Deallocate data contents if Array object is responsible for cleaning up. 
! 
!EOPI 
 
 integer :: status ! local error status 
 
 status = ESMF_FAILURE 
 
 call c_ESMC_ArrayGetF90Ptr(array, wrap, status) 
 deallocate(wrap % ptr5DR8) 
 
 if (present(rc)) rc = status 
 
 end subroutine ESMF_ArrayDeallocate5DR8 
 
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
! \item[array]
! Destroy contents of this {\tt Array}.
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
            print *, "Array not initialized or Destroyed"
            return
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

        ! mark this as destroyed
        array%this = ESMF_NULL_POINTER

        ! set return code if user specified it
        if (rcpresent) rc = ESMF_SUCCESS

        end subroutine ESMF_ArrayDestroy


!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
!
! This section is Allocate/Deallocate for Arrays
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!BOPI
! !IROUTINE: ESMF_ArrayF90Allocate - Allocate an F90 pointer and set Array info
!
! !INTERFACE:
     subroutine ESMF_ArrayF90Allocate(array, rank, type, kind, &
                                      counts, lbounds, ubounds, hwidth, rc)
!
! !ARGUMENTS:
      type(ESMF_Array), intent(inout) :: array
      integer, intent(in) :: rank
      type(ESMF_DataType), intent(in) :: type
      type(ESMF_DataKind), intent(in) :: kind
      integer, dimension(:), intent(in) :: counts
      integer, dimension(:), intent(in) :: lbounds
      integer, dimension(:), intent(in) :: ubounds
      integer, intent(in) :: hwidth
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
! \item[lbounds]
! An integer array, size {\tt rank}, of each dimensions lower index.
! \item[ubounds]
! An integer array, size {\tt rank}, of each dimensions upper index.
! \item[hwidth]
! An integer width, single value, applied to each dimension.
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local1DI2, & 
 ESMF_DATA_ADDRESS(local1DI2 % ptr1DI2 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DI4 % ptr1DI4( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local1DI4, & 
 ESMF_DATA_ADDRESS(local1DI4 % ptr1DI4 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DI8 % ptr1DI8( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local1DI8, & 
 ESMF_DATA_ADDRESS(local1DI8 % ptr1DI8 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local2DI2, & 
 ESMF_DATA_ADDRESS(local2DI2 % ptr2DI2 (lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local2DI4 % ptr2DI4( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local2DI4, & 
 ESMF_DATA_ADDRESS(local2DI4 % ptr2DI4 (lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local2DI8 % ptr2DI8( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local2DI8, & 
 ESMF_DATA_ADDRESS(local2DI8 % ptr2DI8 (lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local3DI2, & 
 ESMF_DATA_ADDRESS(local3DI2 % ptr3DI2 (lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local3DI4 % ptr3DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local3DI4, & 
 ESMF_DATA_ADDRESS(local3DI4 % ptr3DI4 (lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local3DI8 % ptr3DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local3DI8, & 
 ESMF_DATA_ADDRESS(local3DI8 % ptr3DI8 (lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local4DI2, & 
 ESMF_DATA_ADDRESS(local4DI2 % ptr4DI2 (lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local4DI4 % ptr4DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local4DI4, & 
 ESMF_DATA_ADDRESS(local4DI4 % ptr4DI4 (lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local4DI8 % ptr4DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local4DI8, & 
 ESMF_DATA_ADDRESS(local4DI8 % ptr4DI8 (lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local5DI2, & 
 ESMF_DATA_ADDRESS(local5DI2 % ptr5DI2 (lb(1),lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local5DI4 % ptr5DI4( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local5DI4, & 
 ESMF_DATA_ADDRESS(local5DI4 % ptr5DI4 (lb(1),lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local5DI8 % ptr5DI8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local5DI8, & 
 ESMF_DATA_ADDRESS(local5DI8 % ptr5DI8 (lb(1),lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
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
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DR4 % ptr1DR4( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local1DR4, & 
 ESMF_DATA_ADDRESS(local1DR4 % ptr1DR4 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local1DR8 % ptr1DR8( lb(1):ub(1) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local1DR8, & 
 ESMF_DATA_ADDRESS(local1DR8 % ptr1DR8 (lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local2DR4, & 
 ESMF_DATA_ADDRESS(local2DR4 % ptr2DR4 (lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local2DR8 % ptr2DR8( lb(1):ub(1),lb(2):ub(2) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local2DR8, & 
 ESMF_DATA_ADDRESS(local2DR8 % ptr2DR8 (lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local3DR4, & 
 ESMF_DATA_ADDRESS(local3DR4 % ptr3DR4 (lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local3DR8 % ptr3DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local3DR8, & 
 ESMF_DATA_ADDRESS(local3DR8 % ptr3DR8 (lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local4DR4, & 
 ESMF_DATA_ADDRESS(local4DR4 % ptr4DR4 (lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local4DR8 % ptr4DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local4DR8, & 
 ESMF_DATA_ADDRESS(local4DR8 % ptr4DR8 (lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
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
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local5DR4, & 
 ESMF_DATA_ADDRESS(local5DR4 % ptr5DR4 (lb(1),lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Array internal set info error" 
 return 
 endif 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 allocate(local5DR8 % ptr5DR8( lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5) ), stat=status) 
 if (status .ne. 0) then 
 print *, "ESMC_ArrayCreate: Allocation error" 
 return 
 endif 
 
 ! Set all the new accumulated information about the array - the 
 ! F90 pointer, the base addr, the counts, etc. 
 
 ! Since I am not sure what these are used for, leave them 0 for now. 
 offsets = 0 
 
 call c_ESMC_ArraySetInfo(array, local5DR8, & 
 ESMF_DATA_ADDRESS(local5DR8 % ptr5DR8 (lb(1),lb(1),lb(1),lb(1),lb(1)) ), & 
 counts, lb, ub, offsets, & 
 ESMF_TRUE, ESMF_TRUE, hwidth, status) 
 
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
!BOPI
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
!EOPI
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
 call c_ESMC_ArrayGetF90Ptr(array, local1DI2, status) 
 deallocate(local1DI2 % ptr1DI2, stat=status) 
 nullify(local1DI2 % ptr1DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local1DI4, status) 
 deallocate(local1DI4 % ptr1DI4, stat=status) 
 nullify(local1DI4 % ptr1DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local1DI8, status) 
 deallocate(local1DI8 % ptr1DI8, stat=status) 
 nullify(local1DI8 % ptr1DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local2DI2, status) 
 deallocate(local2DI2 % ptr2DI2, stat=status) 
 nullify(local2DI2 % ptr2DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local2DI4, status) 
 deallocate(local2DI4 % ptr2DI4, stat=status) 
 nullify(local2DI4 % ptr2DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local2DI8, status) 
 deallocate(local2DI8 % ptr2DI8, stat=status) 
 nullify(local2DI8 % ptr2DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local3DI2, status) 
 deallocate(local3DI2 % ptr3DI2, stat=status) 
 nullify(local3DI2 % ptr3DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local3DI4, status) 
 deallocate(local3DI4 % ptr3DI4, stat=status) 
 nullify(local3DI4 % ptr3DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local3DI8, status) 
 deallocate(local3DI8 % ptr3DI8, stat=status) 
 nullify(local3DI8 % ptr3DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local4DI2, status) 
 deallocate(local4DI2 % ptr4DI2, stat=status) 
 nullify(local4DI2 % ptr4DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local4DI4, status) 
 deallocate(local4DI4 % ptr4DI4, stat=status) 
 nullify(local4DI4 % ptr4DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local4DI8, status) 
 deallocate(local4DI8 % ptr4DI8, stat=status) 
 nullify(local4DI8 % ptr4DI8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (5)
            select case (localkind)
              case (ESMF_I2%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local5DI2, status) 
 deallocate(local5DI2 % ptr5DI2, stat=status) 
 nullify(local5DI2 % ptr5DI2) 
! < End macro - do not edit directly > 

              case (ESMF_I4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local5DI4, status) 
 deallocate(local5DI4 % ptr5DI4, stat=status) 
 nullify(local5DI4 % ptr5DI4) 
! < End macro - do not edit directly > 

              case (ESMF_I8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local5DI8, status) 
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
 call c_ESMC_ArrayGetF90Ptr(array, local1DR4, status) 
 deallocate(local1DR4 % ptr1DR4, stat=status) 
 nullify(local1DR4 % ptr1DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local1DR8, status) 
 deallocate(local1DR8 % ptr1DR8, stat=status) 
 nullify(local1DR8 % ptr1DR8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (2)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local2DR4, status) 
 deallocate(local2DR4 % ptr2DR4, stat=status) 
 nullify(local2DR4 % ptr2DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local2DR8, status) 
 deallocate(local2DR8 % ptr2DR8, stat=status) 
 nullify(local2DR8 % ptr2DR8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (3)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local3DR4, status) 
 deallocate(local3DR4 % ptr3DR4, stat=status) 
 nullify(local3DR4 % ptr3DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local3DR8, status) 
 deallocate(local3DR8 % ptr3DR8, stat=status) 
 nullify(local3DR8 % ptr3DR8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (4)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local4DR4, status) 
 deallocate(local4DR4 % ptr4DR4, stat=status) 
 nullify(local4DR4 % ptr4DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local4DR8, status) 
 deallocate(local4DR8 % ptr4DR8, stat=status) 
 nullify(local4DR8 % ptr4DR8) 
! < End macro - do not edit directly > 

              case default
            end select

          case (5)
            select case (localkind)
              case (ESMF_R4%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local5DR4, status) 
 deallocate(local5DR4 % ptr5DR4, stat=status) 
 nullify(local5DR4 % ptr5DR4) 
! < End macro - do not edit directly > 

              case (ESMF_R8%dkind)
! <Created by macro - do not edit directly > 
 call c_ESMC_ArrayGetF90Ptr(array, local5DR8, status) 
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

     end subroutine ESMF_ArrayF90Deallocate

!------------------------------------------------------------------------------


        end module ESMF_ArrayExpandMod
