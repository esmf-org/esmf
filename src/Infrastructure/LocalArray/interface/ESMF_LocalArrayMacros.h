#if 0
! $Id: ESMF_LocalArrayMacros.h,v 1.10 2004/03/11 18:06:50 nscollins Exp $
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
#endif
#if 0
!------------------------------------------------------------------------------
! Macros for the Array class.

! these are defined because they contain a variable number of commas.
! they are used as parms in macro calls, and are rescanned by the
! preprocessor after expansion.
!------------------------------------------------------------------------------
#endif

#define COL1 :
#define COL2 :,:
#define COL3 :,:,:
#define COL4 :,:,:,:
#define COL5 :,:,:,:,:

#define LEN1 counts(1)
#define LEN2 counts(1), counts(2)
#define LEN3 counts(1), counts(2), counts(3)
#define LEN4 counts(1), counts(2), counts(3), counts(4)
#define LEN5 counts(1), counts(2), counts(3), counts(4), counts(5)

#define RNG1 lb(1):ub(1)
#define RNG2 lb(1):ub(1),lb(2):ub(2)
#define RNG3 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)
#define RNG4 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)
#define RNG5 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)
#define RNG6 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6)
#define RNG7 lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5),lb(6):ub(6),lb(7):ub(7)


#define LOC1 lb(1)
#define LOC2 lb(1),lb(1)
#define LOC3 lb(1),lb(1),lb(1)
#define LOC4 lb(1),lb(1),lb(1),lb(1)
#define LOC5 lb(1),lb(1),lb(1),lb(1),lb(1)

#if 0
!------------------------------------------------------------------------------
! Declare a wrapper for an F90 array pointer, a local variable of that type,
!  type which wraps every pointer type, and a declaration of local variables,
!  one of each wrapper type.
!------------------------------------------------------------------------------
#endif

#define ArrayTypeMacro(mname, mtypekind, mrank, mdim) \
! <Created by macro - do not edit directly > @\
      type ESMF_ArrWrap##mrank##D##mtypekind @\
        mname (ESMF_KIND_##mtypekind),dimension(mdim),pointer :: ptr##mrank##D##mtypekind @\
      end type ESMF_ArrWrap##mrank##D##mtypekind @\

#define ArrayAllTypeMacro() \
 @\
      ArrayTypeMacro(integer, I2, 1, COL1) @\
      ArrayTypeMacro(integer, I4, 1, COL1) @\
      ArrayTypeMacro(integer, I8, 1, COL1) @\
 @\
      ArrayTypeMacro(integer, I2, 2, COL2) @\
      ArrayTypeMacro(integer, I4, 2, COL2) @\
      ArrayTypeMacro(integer, I8, 2, COL2) @\
 @\
      ArrayTypeMacro(integer, I2, 3, COL3) @\
      ArrayTypeMacro(integer, I4, 3, COL3) @\
      ArrayTypeMacro(integer, I8, 3, COL3) @\
 @\
      ArrayTypeMacro(integer, I2, 4, COL4) @\
      ArrayTypeMacro(integer, I4, 4, COL4) @\
      ArrayTypeMacro(integer, I8, 4, COL4) @\
 @\
      ArrayTypeMacro(integer, I2, 5, COL5) @\
      ArrayTypeMacro(integer, I4, 5, COL5) @\
      ArrayTypeMacro(integer, I8, 5, COL5) @\
 @\
      ArrayTypeMacro(real, R4, 1, COL1) @\
      ArrayTypeMacro(real, R8, 1, COL1) @\
 @\
      ArrayTypeMacro(real, R4, 2, COL2) @\
      ArrayTypeMacro(real, R8, 2, COL2) @\
 @\
      ArrayTypeMacro(real, R4, 3, COL3) @\
      ArrayTypeMacro(real, R8, 3, COL3) @\
 @\
      ArrayTypeMacro(real, R4, 4, COL4) @\
      ArrayTypeMacro(real, R8, 4, COL4) @\
 @\
      ArrayTypeMacro(real, R4, 5, COL5) @\
      ArrayTypeMacro(real, R8, 5, COL5) @\
! < end macro - do not edit directly >  @\
 @\

#define ArrayPointerMacro(mname, mtypekind, mrank, mdim) \
        mname (ESMF_KIND_##mtypekind),dimension(mdim),pointer :: ptr##mrank##D##mtypekind

#define ArrayAllPointerMacro() \
! <Created by macro - do not edit directly > @\
      type ESMF_ArrAllWrap @\
        ArrayPointerMacro(integer, I2, 1, COL1) @\
        ArrayPointerMacro(integer, I4, 1, COL1) @\
        ArrayPointerMacro(integer, I8, 1, COL1) @\
 @\
        ArrayPointerMacro(integer, I2, 2, COL2) @\
        ArrayPointerMacro(integer, I4, 2, COL2) @\
        ArrayPointerMacro(integer, I8, 2, COL2) @\
 @\
        ArrayPointerMacro(integer, I2, 3, COL3) @\
        ArrayPointerMacro(integer, I4, 3, COL3) @\
        ArrayPointerMacro(integer, I8, 3, COL3) @\
 @\
        ArrayPointerMacro(integer, I2, 4, COL4) @\
        ArrayPointerMacro(integer, I4, 4, COL4) @\
        ArrayPointerMacro(integer, I8, 4, COL4) @\
 @\
        ArrayPointerMacro(integer, I2, 5, COL5) @\
        ArrayPointerMacro(integer, I4, 5, COL5) @\
        ArrayPointerMacro(integer, I8, 5, COL5) @\
 @\
        ArrayPointerMacro(real, R4, 1, COL1) @\
        ArrayPointerMacro(real, R8, 1, COL1) @\
 @\
        ArrayPointerMacro(real, R4, 2, COL2) @\
        ArrayPointerMacro(real, R8, 2, COL2) @\
 @\
        ArrayPointerMacro(real, R4, 3, COL3) @\
        ArrayPointerMacro(real, R8, 3, COL3) @\
 @\
        ArrayPointerMacro(real, R4, 4, COL4) @\
        ArrayPointerMacro(real, R8, 4, COL4) @\
 @\
        ArrayPointerMacro(real, R4, 5, COL5) @\
        ArrayPointerMacro(real, R8, 5, COL5) @\
      end type ESMF_ArrAllWrap @\
 @\
! < end macro - do not edit directly >  @\
 @\

#define ArrayLocalVarMacro(mname, mtypekind, mrank, mdim) \
        type(ESMF_ArrWrap##mrank##D##mtypekind) :: local##mrank##D##mtypekind

#define ArrayAllLocalVarMacro() \
! <Created by macro - do not edit directly > @\
        ArrayLocalVarMacro(integer, I2, 1, COL1) @\
        ArrayLocalVarMacro(integer, I4, 1, COL1) @\
        ArrayLocalVarMacro(integer, I8, 1, COL1) @\
 @\
        ArrayLocalVarMacro(integer, I2, 2, COL2) @\
        ArrayLocalVarMacro(integer, I4, 2, COL2) @\
        ArrayLocalVarMacro(integer, I8, 2, COL2) @\
 @\
        ArrayLocalVarMacro(integer, I2, 3, COL3) @\
        ArrayLocalVarMacro(integer, I4, 3, COL3) @\
        ArrayLocalVarMacro(integer, I8, 3, COL3) @\
 @\
        ArrayLocalVarMacro(integer, I2, 4, COL4) @\
        ArrayLocalVarMacro(integer, I4, 4, COL4) @\
        ArrayLocalVarMacro(integer, I8, 4, COL4) @\
 @\
        ArrayLocalVarMacro(integer, I2, 5, COL5) @\
        ArrayLocalVarMacro(integer, I4, 5, COL5) @\
        ArrayLocalVarMacro(integer, I8, 5, COL5) @\
 @\
        ArrayLocalVarMacro(real, R4, 1, COL1) @\
        ArrayLocalVarMacro(real, R8, 1, COL1) @\
 @\
        ArrayLocalVarMacro(real, R4, 2, COL2) @\
        ArrayLocalVarMacro(real, R8, 2, COL2) @\
 @\
        ArrayLocalVarMacro(real, R4, 3, COL3) @\
        ArrayLocalVarMacro(real, R8, 3, COL3) @\
 @\
        ArrayLocalVarMacro(real, R4, 4, COL4) @\
        ArrayLocalVarMacro(real, R8, 4, COL4) @\
 @\
        ArrayLocalVarMacro(real, R4, 5, COL5) @\
        ArrayLocalVarMacro(real, R8, 5, COL5) @\
 @\

#if 0
!------------------------------------------------------------------------------
! Expand a string into each of the T/K/R procedure interface blocks
!------------------------------------------------------------------------------
#endif

#define LocalArrayInterfaceMacro(funcname) \
!------------------------------------------------------------------------------ @\
! <This section created by macro - do not edit directly> @\
    module procedure ESMF_##funcname##1DI2 @\
    module procedure ESMF_##funcname##1DI4 @\
    module procedure ESMF_##funcname##1DI8 @\
    module procedure ESMF_##funcname##2DI2 @\
    module procedure ESMF_##funcname##2DI4 @\
    module procedure ESMF_##funcname##2DI8 @\
    module procedure ESMF_##funcname##3DI2 @\
    module procedure ESMF_##funcname##3DI4 @\
    module procedure ESMF_##funcname##3DI8 @\
    module procedure ESMF_##funcname##4DI2 @\
    module procedure ESMF_##funcname##4DI4 @\
    module procedure ESMF_##funcname##4DI8 @\
    module procedure ESMF_##funcname##5DI2 @\
    module procedure ESMF_##funcname##5DI4 @\
    module procedure ESMF_##funcname##5DI8 @\
    module procedure ESMF_##funcname##1DR4 @\
    module procedure ESMF_##funcname##1DR8 @\
    module procedure ESMF_##funcname##2DR4 @\
    module procedure ESMF_##funcname##2DR8 @\
    module procedure ESMF_##funcname##3DR4 @\
    module procedure ESMF_##funcname##3DR8 @\
    module procedure ESMF_##funcname##4DR4 @\
    module procedure ESMF_##funcname##4DR8 @\
    module procedure ESMF_##funcname##5DR4 @\
    module procedure ESMF_##funcname##5DR8 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated F90 array and a list of counts.
!------------------------------------------------------------------------------
#endif

#define LocalArrayCreateByMTArrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_LocalArrCreateByMTArr##mrank##D##mtypekind - make an ESMF array from an unallocated F90 array @\
 @\
! !INTERFACE: @\
      function ESMF_LocalArrCreateByMTArr##mrank##D##mtypekind(f90arr, counts, lbounds, ubounds, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTArr##mrank##D##mtypekind @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), target :: f90arr @\
      !mname (ESMF_KIND_##mtypekind), dimension(mdim), allocatable, target :: f90arr @\
      integer, dimension(:), intent(in) :: counts @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt LocalArray} based on an unallocated (but allocatable) Fortran @\
!   90 array.  This routine allocates memory to the array and fills in @\
!   the array object with all necessary information. @\
! @\
! The function return is an ESMF\_LocalArray type with space allocated for data. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90arr] @\
!   An allocatable (but currently unallocated) Fortran 90 array.  @\
!  \item[counts] @\
!   An integer array of counts.  Must be the same length as the rank. @\
!  \item[{[lbounds]}] @\
!  An integer array of lower index values.  Must be the same length as the rank. @\
!  \item[{[ubounds]}] @\
! An integer array of upper index values.  Must be the same length as the rank. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
 @\
! @\
!EOP @\
! !REQUIREMENTS: @\
 @\
        ! Local variables @\
        type (ESMF_LocalArray) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
 @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: newp  @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        ! Test to see if array already allocated, and fail if so. @\
        !if (allocated(f90arr)) then @\
        !  print *, "LocalArray cannot already be allocated" @\
        !  return @\
        !endif @\
 @\
        ! Call create routine @\
        call c_ESMC_LocalArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_##mtypekind, & @\
                                             ESMF_FROM_FORTRAN, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        newp => f90arr    ! must be ptr assignment, => @\
        call ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind(array, counts, newp,& @\
                                  ESMF_DATA_SPACE, lbounds, ubounds, status) @\
        @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_LocalArrCreateByMTArr##mrank##D##mtypekind = array @\
        if (rcpresent) rc = status @\
 @\
        end function ESMF_LocalArrCreateByMTArr##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an allocated F90 array and a copy flag.
!------------------------------------------------------------------------------
#endif

#define LocalArrayCreateByFlArrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_LocalArrCreateByFlArr##mrank##D##mtypekind - make an ESMF array from an Allocated F90 array @\
 @\
! !INTERFACE: @\
      function ESMF_LocalArrCreateByFlArr##mrank##D##mtypekind(f90arr, docopy, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlArr##mrank##D##mtypekind @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), target :: f90arr @\
      !mname (ESMF_KIND_##mtypekind), dimension(mdim), allocatable, target :: f90arr @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt LocalArray} based on an already allocated Fortran @\
!   90 array.  This routine can make a copy or reference the existing data @\
!   and fills in the array object with all necessary information. @\
! @\
! The function return is an ESMF\_LocalArray type. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90arr] @\
!   An allocated Fortran 90 array.  @\
!  \item[{[docopy]}] @\
!   Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference @\
!   the existing data array.  If set to {\tt ESMF\_DATA\_COPY} this routine @\
!   allocates new space and copies the data from the pointer into the @\
!   new array. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
 @\
! @\
!EOP @\
! !REQUIREMENTS: @\
 @\
        ! Local variables @\
        type (ESMF_LocalArray) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
        type (ESMF_CopyFlag) :: copy        ! do we copy or ref? @\
        integer, dimension(mrank) :: counts ! per dim @\
        integer, dimension(mrank) :: lbounds ! lower index bounds @\
        integer, dimension(mrank) :: ubounds ! upper index bounds @\
 @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: newp @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        ! Set default for copyflag @\
        if (present(docopy)) then @\
            copy = docopy @\
        else @\
            copy = ESMF_DATA_REF @\
        endif @\
 @\
        ! TODO: will this work with a statically declared array if I take @\
        !  this test out?  or will the function signature not match? @\
        ! Test to see if array is not already allocated, and fail if so. @\
        !if (.not.allocated(f90arr)) then @\
        !  print *, "Array must already be allocated" @\
        !  return @\
        !endif @\
 @\
        ! Get sizes from current array, although the construct routine @\
        !   does not need it for an already allocated array.  @\
        counts = shape(f90arr) @\
        lbounds = lbound(f90arr) @\
        ubounds = ubound(f90arr) @\
 @\
        ! Call create routine @\
        call c_ESMC_LocalArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_##mtypekind, & @\
                                           ESMF_FROM_FORTRAN, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        newp => f90arr    ! must be ptr assignment, => @\
        call ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind(array, counts, newp,& @\
                                  copy, lbounds, ubounds, status) @\
        @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_LocalArrCreateByFlArr##mrank##D##mtypekind = array @\
        if (rcpresent) rc = status @\
 @\
        end function ESMF_LocalArrCreateByFlArr##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated F90 pointer and a list of counts.
!------------------------------------------------------------------------------
#endif

#define LocalArrayCreateByMTPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind - make an ESMF array from an unallocated F90 pointer @\
 @\
! !INTERFACE: @\
      function ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind(f90ptr, counts, lbounds, ubounds, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
      integer, dimension(:), intent(in) :: counts @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt LocalArray} based on an unassociated Fortran 90 pointer. @\
!   This routine allocates memory to the array pointer and fills in @\
!   the array object with all necessary information. @\
! @\
! The function return is an ESMF\_LocalArray type with space allocated for data. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90ptr] @\
!   An unassociated Fortran 90 array pointer.  @\
!  \item[counts] @\
!   An integer array of counts.  Must be the same length as the rank. @\
!  \item[{[lbounds]}] @\
!  An integer array of lower index values.  Must be the same length as the rank. @\
!  \item[{[ubounds]}] @\
! An integer array of upper index values.  Must be the same length as the rank. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
 @\
! @\
!EOP @\
! !REQUIREMENTS: @\
 @\
        ! Local variables @\
        type (ESMF_LocalArray) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        ! Test to see if array already allocated, and fail if so. @\
        if (associated(f90ptr)) then @\
          print *, "Pointer cannot already be allocated" @\
          return @\
        endif @\
 @\
        ! Call create routine @\
        call c_ESMC_LocalArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_##mtypekind, & @\
                                             ESMF_FROM_FORTRAN, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        call ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind(array, counts, f90ptr,& @\
                                  ESMF_DATA_SPACE, lbounds, ubounds, status) @\
        @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind = array @\
        if (rcpresent) rc = status @\
 @\
        end function ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an allocated F90 pointer and a copy flag.
!------------------------------------------------------------------------------
#endif

#define LocalArrayCreateByFlPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind - make an ESMF array from an Allocated F90 pointer @\
 @\
! !INTERFACE: @\
      function ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind(f90ptr, docopy, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt LocalArray} based on an already allocated Fortran 90 array @\
!   pointer.  This routine can make a copy or reference the existing data @\
!   and fills in the array object with all necessary information. @\
! @\
! The function return is an ESMF\_LocalArray type. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90ptr] @\
!   An allocated Fortran 90 array pointer.  @\
!  \item[{[docopy]}] @\
!   Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference @\
!   the existing data array.  If set to {\tt ESMF\_DATA\_COPY} this routine @\
!   allocates new space and copies the data from the pointer into the @\
!   new array. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
 @\
! @\
!EOP @\
! !REQUIREMENTS: @\
 @\
        ! Local variables @\
        type (ESMF_LocalArray) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
        type (ESMF_CopyFlag) :: copy        ! do we copy or ref? @\
        integer, dimension(mrank) :: counts ! per dim @\
        integer, dimension(mrank) :: lbounds ! per dim @\
        integer, dimension(mrank) :: ubounds ! per dim @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        ! Set default for copyflag @\
        if (present(docopy)) then @\
            copy = docopy @\
        else @\
            copy = ESMF_DATA_REF @\
        endif @\
 @\
        ! TODO: will this work with a statically declared array if I take @\
        !  this test out?  or will the function signature not match? @\
        ! Test to see if array is not already allocated, and fail if so. @\
        if (.not.associated(f90ptr)) then @\
          print *, "Pointer must already be associated" @\
          return @\
        endif @\
 @\
        ! Get sizes from current array, although the construct routine @\
        !   does not need it for an already allocated array.  @\
        counts = shape(f90ptr) @\
        lbounds = lbound(f90ptr) @\
        ubounds = ubound(f90ptr) @\
 @\
        ! Call create routine @\
        call c_ESMC_LocalArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_##mtypekind, & @\
                                             ESMF_FROM_FORTRAN, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        call ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind(array, counts, f90ptr,& @\
                                  copy, lbounds, ubounds, status) @\
        @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind = array @\
        if (rcpresent) rc = status @\
 @\
        end function ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create an F90 pointer of the proper type and set the corresponding values
!  in the array object.
!------------------------------------------------------------------------------
#endif

#define LocalArrConstrF90PtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind - Create an F90 Ptr of the proper T/K/R @\
 @\
! !INTERFACE: @\
      subroutine ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind(array, counts, f90ptr, docopy, lbounds, ubounds, rc) @\
! @\
! !ARGUMENTS: @\
      type(ESMF_LocalArray), intent(inout) :: array @\
      integer, dimension(:), intent(in) :: counts @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer, optional :: f90ptr  @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
!  Creates an F90 Pointer of the requested T/K/R.  After creating the @\
!  pointer and doing the allocation based on counts, also goes ahead and @\
!  calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} @\
!  object. (This is to save on the total number of nested crossings of the @\
!  F90/C++ boundary.) @\
! @\
!  Optional args are an existing F90 pointer which if given is used @\
!  instead of a new one, and a docopy flag which if set to copy will @\
!  do a contents copy or reference. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[array] @\
!   The {\tt ESMF\_LocalArray} to set the values into. @\
!  \item[counts] @\
!   An integer array of counts.  Must be the same length as the rank. @\
!  \item[{[f90ptr]}] @\
!   An optional existing F90 pointer.  Will be used instead of an @\
!   internally generated F90 pointer if given.  Must be given if the @\
!   {\tt docopy} is specified. @\
!  \item[{[docopy]}] @\
!   An optional copy flag which can be specified if an F90 pointer is also @\
!   given.  Can either make a new copy of the data or ref existing data. @\
!  \item[{[lbounds]}] @\
!  An integer array of lower index values.  Must be same length as the rank. @\
!  \item[{[ubounds]}] @\
! An integer array of upper index values.  Must be same length as the rank. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
 @\
! @\
!EOPI @\
! !REQUIREMENTS: @\
 @\
        ! Local variables @\
        integer :: i                        ! temp var @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
        logical :: willalloc                ! do we need to alloc/dealloc? @\
        logical :: willcopy                 ! do we need to copy data? @\
        type(ESMF_Logical) :: do_dealloc    ! dealloc flag for SetInternal call @\
 @\
        type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap ! to pass f90 ptr to C++ @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: newp  @\
        integer, dimension(ESMF_MAXDIM) :: lb, ub @\
        integer, dimension(ESMF_MAXDIM) :: offsets @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        ! Decide if we need to do: make a new allocation, copy existing data @\
        if (.not. present(f90ptr)) then @\
           nullify(newp) @\
           willalloc = .true. @\
           willcopy = .false. @\
           do_dealloc = ESMF_TRUE @\
        else @\
           if (docopy .eq. ESMF_DATA_SPACE) then @\
               newp => f90ptr    ! ptr alias, important this be =>  @\
               willalloc = .true. @\
               willcopy = .false. @\
               do_dealloc = ESMF_TRUE @\
           else if (docopy .eq. ESMF_DATA_COPY) then @\
               nullify(newp) @\
               willalloc = .true. @\
               willcopy = .true. @\
               do_dealloc = ESMF_TRUE @\
           else       ! ESMF_DATA_REF @\
               newp => f90ptr    ! ptr alias, important this be =>  @\
               willalloc = .false. @\
               willcopy = .false. @\
               do_dealloc = ESMF_FALSE @\
           endif @\
        endif @\
 @\
        ! lb always needs a value even if not allocating @\
        lb = 1 @\
        if (present(lbounds)) then @\
            lb(1:size(lbounds)) = lbounds @\
        endif @\
 @\
        ! ub is only used during allocation @\
        if (willalloc) then @\
            ub(1:size(counts)) = counts @\
            if (present(ubounds)) then @\
                ub(1:size(ubounds)) = ubounds @\
            endif @\
 @\
            allocate(newp ( mrng ), stat=status) @\
            if (status .ne. 0) then     ! f90 status, not ESMF @\
              print *, "LocalArray space allocate error" @\
              return @\
            endif @\
        endif @\
 @\
        if (willcopy) then @\
            newp = f90ptr      ! contents copy, important that this be = @\
        endif @\
 @\
        ! Now set all the new accumulated information about the array - the @\
        ! F90 pointer, the base addr, the counts, etc. @\
 @\
        ! Until we need offsets, use 0. @\
        offsets = 0 @\
 @\
        wrap%ptr##mrank##D##mtypekind => newp @\
        call c_ESMC_LocalArraySetInternal(array, wrap, & @\
                                 ESMF_DATA_ADDRESS(newp(mloc)), counts, & @\
                                 lbounds, ubounds, offsets, & @\
                                 ESMF_TRUE, do_dealloc, status) @\
 @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "LocalArray internal set info error" @\
          return @\
        endif @\
 @\
        if (rcpresent) rc = status @\
 @\
        end subroutine ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Get an F90 pointer to the data contained in this array
!------------------------------------------------------------------------------
#endif

#define LocalArrayGetDataMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOP @\
! !INTERFACE: @\
      subroutine ESMF_LocalArrayGetData##mrank##D##mtypekind(array, f90ptr, docopy, rc) @\
! @\
! !ARGUMENTS: @\
      type(ESMF_LocalArray) :: array @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!      Return an F90 pointer to the data buffer, or return an F90 pointer @\
!      to a new copy of the data. @\
! @\
!EOP @\
! !REQUIREMENTS: @\
 @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
        logical :: copyreq                  ! did user specify copy? @\
 @\
        type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap     ! for passing f90 ptr to C++ @\
        integer :: rank, lb(mrank), ub(mrank)  ! size info for the array @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: localp ! local copy @\
 @\
        ! initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        copyreq = .FALSE. @\
 @\
        ! check copyflag to see if we are making a reference @\
        ! or making a new array and a copy @\
        if (present(docopy)) then @\
          if (docopy .eq. ESMF_DATA_COPY) copyreq = .TRUE. @\
        endif @\
 @\
        call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "LocalArray - get pointer error" @\
          return @\
        endif @\
 @\
        ! Allocate a new buffer if requested and return a copy @\
        if (copyreq) then @\
          call c_ESMC_ArrayGetLbounds(array, mrank, lb, status) @\
          if (status .ne. ESMF_SUCCESS) then @\
            print *, "Array - cannot retrieve array dim sizes" @\
            return @\
          endif @\
          call c_ESMC_ArrayGetUbounds(array, mrank, ub, status) @\
          if (status .ne. ESMF_SUCCESS) then @\
            print *, "Array - cannot retrieve array dim sizes" @\
            return @\
          endif @\
          allocate(localp( mrng ), stat=status) @\
          if (status .ne. 0) then     ! f90 status, not ESMF @\
            print *, "LocalArray do_copy allocate error" @\
            return @\
          endif @\
          ! this must do a contents assignment @\
          localp = wrap%ptr##mrank##D##mtypekind @\
          f90ptr => localp  @\
        else @\
          f90ptr => wrap%ptr##mrank##D##mtypekind @\
        endif @\
 @\
        if (rcpresent) rc = ESMF_SUCCESS @\
 @\
        end subroutine ESMF_LocalArrayGetData##mrank##D##mtypekind @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Deallocate the contents of the array.
!------------------------------------------------------------------------------
#endif

#define LocalArrayDeallocateMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOP @\
! !INTERFACE: @\
      subroutine ESMF_LocalArrayDeallocate##mrank##D##mtypekind(array, wrap, rc) @\
! @\
! !RETURN VALUE: @\
! @\
! !ARGUMENTS: @\
      type(ESMF_LocalArray) :: array @\
      type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap @\
      integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!      Deallocate data contents if LocalArray object is responsible for cleaning up. @\
! @\
!EOP @\
! !REQUIREMENTS: @\
 @\
        integer :: status                               ! local error status @\
 @\
        status = ESMF_FAILURE  @\
 @\
        call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) @\
        deallocate(wrap%ptr##mrank##D##mtypekind) @\
 @\
        if (present(rc)) rc = status @\
 @\
        end subroutine ESMF_LocalArrayDeallocate##mrank##D##mtypekind @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

