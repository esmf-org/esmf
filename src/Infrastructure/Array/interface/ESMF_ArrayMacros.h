#if 0
! $Id: ESMF_ArrayMacros.h,v 1.13 2004/03/05 20:39:08 nscollins Exp $
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
#define COL6 :,:,:,:,:,:
#define COL7 :,:,:,:,:,:,:

#define LEN1 counts(1)
#define LEN2 counts(1),counts(2)
#define LEN3 counts(1),counts(2),counts(3)
#define LEN4 counts(1),counts(2),counts(3),counts(4)
#define LEN5 counts(1),counts(2),counts(3),counts(4),counts(5)
#define LEN6 counts(1),counts(2),counts(3),counts(4),counts(5),counts(6)
#define LEN7 counts(1),counts(2),counts(3),counts(4),counts(5),counts(6),counts(7)

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
#define LOC6 lb(1),lb(1),lb(1),lb(1),lb(1),lb(1)
#define LOC7 lb(1),lb(1),lb(1),lb(1),lb(1),lb(1),lb(1)

#if 0
!------------------------------------------------------------------------------
! Declare a wrapper for an F90 array pointer, a local variable of that type,
!  type which wraps every pointer type, and a declaration of local variables,
!  one of each wrapper type.
!------------------------------------------------------------------------------
#endif

#define ArrayTypeMacro(mname, mtypekind, mrank, mdim) \
! <Created by macro - do not edit directly > @\
      type ESMF_ArrWrap##mtypekind##mrank##D @\
        mname (ESMF_KIND_##mtypekind),dimension(mdim),pointer :: mtypekind##mrank##Dptr @\
      end type ESMF_ArrWrap##mtypekind##mrank##D @\

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
        mname (ESMF_KIND_##mtypekind),dimension(mdim),pointer :: mtypekind##mrank##Dptr

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
        type(ESMF_ArrWrap##mtypekind##mrank##D) :: local##mtypekind##mrank##D

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

#define ArrayInterfaceMacro(funcname) \
!------------------------------------------------------------------------------ @\
! <This section created by macro - do not edit directly> @\
    module procedure ESMF_##funcname##I21D @\
    module procedure ESMF_##funcname##I41D @\
    module procedure ESMF_##funcname##I81D @\
    module procedure ESMF_##funcname##I22D @\
    module procedure ESMF_##funcname##I42D @\
    module procedure ESMF_##funcname##I82D @\
    module procedure ESMF_##funcname##I23D @\
    module procedure ESMF_##funcname##I43D @\
    module procedure ESMF_##funcname##I83D @\
    module procedure ESMF_##funcname##I24D @\
    module procedure ESMF_##funcname##I44D @\
    module procedure ESMF_##funcname##I84D @\
    module procedure ESMF_##funcname##I25D @\
    module procedure ESMF_##funcname##I45D @\
    module procedure ESMF_##funcname##I85D @\
    module procedure ESMF_##funcname##R41D @\
    module procedure ESMF_##funcname##R81D @\
    module procedure ESMF_##funcname##R42D @\
    module procedure ESMF_##funcname##R82D @\
    module procedure ESMF_##funcname##R43D @\
    module procedure ESMF_##funcname##R83D @\
    module procedure ESMF_##funcname##R44D @\
    module procedure ESMF_##funcname##R84D @\
    module procedure ESMF_##funcname##R45D @\
    module procedure ESMF_##funcname##R85D @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated F90 array and a list of counts.
!------------------------------------------------------------------------------
#endif

#define ArrayCreateByMTArrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_ArrayCreateByMTArr##mtypekind##mrank##D - make an ESMF array from an unallocated F90 array @\
 @\
! !INTERFACE: @\
      function ESMF_ArrayCreateByMTArr##mtypekind##mrank##D(f90arr, counts, halo_width, lbounds, ubounds, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_Array) :: ESMF_ArrayCreateByMTArr##mtypekind##mrank##D @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), target :: f90arr @\
      !mname (ESMF_KIND_##mtypekind), dimension(mdim), allocatable, target :: f90arr @\
      integer, dimension(:), intent(in) :: counts @\
      integer, intent(in), optional :: halo_width @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt Array} based on an unallocated (but allocatable) Fortran @\
!   90 array.  This routine allocates memory to the array and fills in @\
!   the array object with all necessary information. @\
! @\
! The function return is an {\tt ESMF\_Array} type with space @\
! allocated for data. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90arr] @\
!   An allocatable (but currently unallocated) Fortran 90 array.  @\
! @\
!  \item[counts] @\
!   An integer array of counts.  Must be the same length as the rank. @\
! @\
!  \item[{[halo_width]}] @\
!   An integer count of the width of the halo region on all sides of @\
!   the array. The default is 0, no halo region. @\
! @\
!  \item[{[lbounds]}] @\
!  An integer array of lower index values.  Must be the same length as the rank. @\
! @\
!  \item[{[ubounds]}] @\
! An integer array of upper index values.  Must be the same length as the rank. @\
! @\
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
        type (ESMF_Array) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        integer :: hwidth                   ! local copy of halo width @\
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
        !  print *, "Array cannot already be allocated" @\
        !  return @\
        !endif @\
 @\
        ! Always supply a halo value, setting it to 0 if not specified. @\
        if (present(halo_width)) then @\
          hwidth = halo_width @\
        else @\
          hwidth = 0 @\
        endif @\
 @\
        ! Call create routine @\
        call c_ESMC_ArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_##mtypekind, & @\
                                       ESMF_FROM_FORTRAN, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        newp => f90arr    ! must be ptr assignment, => @\
        call ESMF_ArrayConstructF90Ptr##mtypekind##mrank##D(array, counts, hwidth, & @\
                                  newp, ESMF_DATA_SPACE, lbounds, ubounds, status) @\
 @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_ArrayCreateByMTArr##mtypekind##mrank##D = array @\
        if (rcpresent) rc = status @\
 @\
        end function ESMF_ArrayCreateByMTArr##mtypekind##mrank##D   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an allocated F90 array and a copy flag.
!------------------------------------------------------------------------------
#endif

#define ArrayCreateByFullArrMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_ArrayCreateByFullArr##mtypekind##mrank##D - make an ESMF array from an Allocated F90 array @\
 @\
! !INTERFACE: @\
      function ESMF_ArrayCreateByFullArr##mtypekind##mrank##D(f90arr, docopy, halo_width, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_Array) :: ESMF_ArrayCreateByFullArr##mtypekind##mrank##D @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), target :: f90arr @\
      !mname (ESMF_KIND_##mtypekind), dimension(mdim), allocatable, target :: f90arr @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, intent(in), optional :: halo_width @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt Array} based on an already allocated Fortran @\
!   90 array.  This routine can make a copy or reference the existing data @\
!   and fills in the array object with all necessary information. @\
! @\
! The function return is an ESMF\_Array type. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90arr] @\
!   An allocated Fortran 90 array.  @\
! @\
!  \item[{[docopy]}] @\
!   Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference @\
!   the existing data array.  If set to {\tt ESMF\_DATA\_COPY} this routine @\
!   allocates new space and copies the data from the pointer into the @\
!   new array. @\
! @\
!  \item[{[halo_width]}] @\
!   Set the maximum width of the halo region on all edges. Defaults to 0. @\
! @\
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
        type (ESMF_Array) :: array           ! new array object @\
        integer :: status                    ! local error status @\
        logical :: rcpresent                 ! did user specify rc? @\
        integer :: hwidth                    ! local copy of halo width @\
        type (ESMF_CopyFlag) :: copy         ! do we copy or ref? @\
        integer, dimension(mrank) :: counts  ! per dim @\
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
        ! Always supply a halo value, setting it to 0 if not specified. @\
        if (present(halo_width)) then @\
          hwidth = halo_width @\
        else @\
          hwidth = 0 @\
        endif @\
 @\
        ! Call create routine @\
        call c_ESMC_ArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_##mtypekind, & @\
                                     ESMF_FROM_FORTRAN, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        newp => f90arr    ! must be ptr assignment, => @\
        call ESMF_ArrayConstructF90Ptr##mtypekind##mrank##D(array, counts, hwidth,& @\
                                  newp, copy, lbounds, ubounds, status) @\
 @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_ArrayCreateByFullArr##mtypekind##mrank##D = array @\
        if (rcpresent) rc = status @\
 @\
        end function ESMF_ArrayCreateByFullArr##mtypekind##mrank##D   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated F90 pointer and a list of counts.
!------------------------------------------------------------------------------
#endif

#define ArrayCreateByMTPtrMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_ArrayCreateByMTPtr##mtypekind##mrank##D - make an ESMF array from an unallocated F90 pointer @\
 @\
! !INTERFACE: @\
      function ESMF_ArrayCreateByMTPtr##mtypekind##mrank##D(f90ptr, counts, halo_width, & @\
                                                     lbounds, ubounds, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_Array) :: ESMF_ArrayCreateByMTPtr##mtypekind##mrank##D @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
      integer, dimension(:), intent(in) :: counts @\
      integer, intent(in), optional :: halo_width @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt Array} based on an unassociated Fortran 90 pointer. @\
!   This routine allocates memory to the array pointer and fills in @\
!   the array object with all necessary information. @\
! @\
! The function return is an ESMF\_Array type with space allocated for data. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90ptr] @\
!   An unassociated Fortran 90 array pointer.  @\
! @\
!  \item[counts] @\
!   An integer array of counts.  Must be the same length as the rank. @\
! @\
!  \item[{[halo_width]}] @\
!   Set the maximum width of the halo region on all edges. Defaults to 0. @\
! @\
!  \item[{[lbounds]}] @\
!  An integer array of lower index values.  Must be the same length as the rank. @\
! @\
!  \item[{[ubounds]}] @\
! An integer array of upper index values.  Must be the same length as the rank. @\
! @\
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
        type (ESMF_Array) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        integer :: hwidth                   ! local copy of halo width @\
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
        ! Always supply a halo value, setting it to 0 if not specified. @\
        if (present(halo_width)) then @\
          hwidth = halo_width @\
        else @\
          hwidth = 0 @\
        endif @\
 @\
        ! Call create routine @\
        call c_ESMC_ArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_##mtypekind, & @\
                                          ESMF_FROM_FORTRAN, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        call ESMF_ArrayConstructF90Ptr##mtypekind##mrank##D(array, counts, hwidth,& @\
                                 f90ptr, ESMF_DATA_SPACE, lbounds, ubounds, status) @\
 @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_ArrayCreateByMTPtr##mtypekind##mrank##D = array @\
        if (rcpresent) rc = status @\
 @\
        end function ESMF_ArrayCreateByMTPtr##mtypekind##mrank##D   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an allocated F90 pointer and a copy flag.
!------------------------------------------------------------------------------
#endif

#define ArrayCreateByFullPtrMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_ArrayCreateByFullPtr##mtypekind##mrank##D - make an ESMF array from an Allocated F90 pointer @\
 @\
! !INTERFACE: @\
      function ESMF_ArrayCreateByFullPtr##mtypekind##mrank##D(f90ptr, docopy, halo_width, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_Array) :: ESMF_ArrayCreateByFullPtr##mtypekind##mrank##D @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, intent(in), optional :: halo_width @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt Array} based on an already allocated Fortran 90 array @\
!   pointer.  This routine can make a copy or reference the existing data @\
!   and fills in the array object with all necessary information. @\
! @\
! The function return is an ESMF\_Array type. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90ptr] @\
!   An allocated Fortran 90 array pointer.  @\
! @\
!  \item[{[docopy]}] @\
!   Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference @\
!   the existing data array.  If set to {\tt ESMF\_DATA\_COPY} this routine @\
!   allocates new space and copies the data from the pointer into the @\
!   new array. @\
! @\
!  \item[{[halo_width]}] @\
!   Set the maximum width of the halo region on all edges. Defaults to 0. @\
! @\
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
        type (ESMF_Array) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        integer :: hwidth                   ! local copy of halo width @\
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
        ! Always supply a halo value, setting it to 0 if not specified. @\
        if (present(halo_width)) then @\
          hwidth = halo_width @\
        else @\
          hwidth = 0 @\
        endif @\
 @\
        ! Call create routine @\
        call c_ESMC_ArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_##mtypekind, & @\
                                      ESMF_FROM_FORTRAN, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        call ESMF_ArrayConstructF90Ptr##mtypekind##mrank##D(array, counts, hwidth,& @\
                                  f90ptr, copy, lbounds, ubounds, status) @\
 @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_ArrayCreateByFullPtr##mtypekind##mrank##D = array @\
        if (rcpresent) rc = status @\
 @\
        end function ESMF_ArrayCreateByFullPtr##mtypekind##mrank##D   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create an F90 pointer of the proper type and set the corresponding values
!  in the array object.
!------------------------------------------------------------------------------
#endif

#define ArrayConstructF90PtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_ArrayConstructF90Ptr##mtypekind##mrank##D - Create an F90 Ptr of the proper T/K/R @\
 @\
! !INTERFACE: @\
      subroutine ESMF_ArrayConstructF90Ptr##mtypekind##mrank##D(array, counts, hwidth, f90ptr, & @\
                                                   docopy, lbounds, ubounds, rc) @\
! @\
! !ARGUMENTS: @\
      type(ESMF_Array), intent(inout) :: array @\
      integer, dimension(:), intent(in) :: counts @\
      integer, intent(in) :: hwidth @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer, optional :: f90ptr  @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
!  Creates an F90 Pointer of the requested T/K/R.  After creating the @\
!  pointer and doing the allocation based on counts, also goes ahead and @\
!  calls into the C++ interfaces to set values on the {\tt ESMF\_Array} @\
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
!   The {\tt ESMF\_Array} to set the values into. @\
! @\
!  \item[counts] @\
!   An integer array of counts.  Must be the same length as the rank. @\
! @\
!  \item[hwidth] @\
!   An integer halo width. Width on each edge. @\
! @\
!  \item[{[f90ptr]}] @\
!   An optional existing F90 pointer.  Will be used instead of an @\
!   internally generated F90 pointer if given.  Must be given if the @\
!   {\tt docopy} is specified. @\
! @\
!  \item[{[docopy]}] @\
!   An optional copy flag which can be specified if an F90 pointer is also @\
!   given.  Can either make a new copy of the data or ref existing data. @\
! @\
!  \item[{[lbounds]}] @\
!  An integer array of lower index values.  Must be the same length as the rank. @\
! @\
!  \item[{[ubounds]}] @\
! An integer array of upper index values.  Must be the same length as the rank. @\
! @\
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
        type(ESMF_Logical) :: do_dealloc    ! dealloc flag for SetInfo call @\
 @\
        type (ESMF_ArrWrap##mtypekind##mrank##D) :: wrap ! to pass f90 ptr to C++ @\
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
        ! Assume defaults first, then alter if lb or ub specified, @\
        ! or if an existing pointer is given and can be queried. @\
        lb(:) = 1 @\
        ub(1:size(counts)) = counts @\
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
               lb(1:size(counts)) = lbound(f90ptr) @\
               ub(1:size(counts)) = ubound(f90ptr) @\
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
               lb(1:size(counts)) = lbound(f90ptr) @\
               ub(1:size(counts)) = ubound(f90ptr) @\
               willalloc = .false. @\
               willcopy = .false. @\
               do_dealloc = ESMF_FALSE @\
           endif @\
        endif @\
 @\
        if (willalloc) then @\
            if (present(lbounds)) then @\
                lb(1:size(lbounds)) = lbounds @\
            endif @\
            if (present(ubounds)) then @\
                ub(1:size(ubounds)) = ubounds @\
            endif @\
            allocate(newp ( mrng ), stat=status) @\
            if (status .ne. 0) then     ! f90 status, not ESMF @\
              print *, "Array space allocate error" @\
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
	! Until we need to use byte offsets, leave them 0. @\
        offsets = 0 @\
 @\
        wrap % ##mtypekind##mrank##Dptr => newp @\
        call c_ESMC_ArraySetInfo(array, wrap, & @\
                                 ESMF_DATA_ADDRESS(newp(mloc)), counts, & @\
                                 lb, ub, offsets, & @\
                                 ESMF_TRUE, do_dealloc, hwidth, status) @\
 @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array internal set info error" @\
          return @\
        endif @\
 @\
        if (rcpresent) rc = status @\
 @\
        end subroutine ESMF_ArrayConstructF90Ptr##mtypekind##mrank##D  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Get an F90 pointer to the data contained in this array
!------------------------------------------------------------------------------
#endif

#define ArrayGetDataMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOP @\
! !INTERFACE: @\
      subroutine ESMF_ArrayGetData##mtypekind##mrank##D(array, f90ptr, docopy, rc) @\
! @\
! !ARGUMENTS: @\
      type(ESMF_Array) :: array @\
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
        type (ESMF_ArrWrap##mtypekind##mrank##D) :: wrap     ! for passing f90 ptr to C++ @\
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
        call c_ESMC_ArrayGetF90Ptr(array, wrap, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array - get pointer error" @\
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
            print *, "Array do_copy allocate error" @\
            return @\
          endif @\
          ! this must do a contents assignment @\
          localp = wrap % ##mtypekind##mrank##Dptr @\
          f90ptr => localp  @\
        else @\
          f90ptr => wrap % ##mtypekind##mrank##Dptr @\
        endif @\
 @\
        if (rcpresent) rc = ESMF_SUCCESS @\
 @\
        end subroutine ESMF_ArrayGetData##mtypekind##mrank##D @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Deallocate the contents of the array.
!------------------------------------------------------------------------------
#endif

#define ArrayDeallocateMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOP @\
! !INTERFACE: @\
      subroutine ESMF_ArrayDeallocate##mtypekind##mrank##D(array, wrap, rc) @\
! @\
! !RETURN VALUE: @\
! @\
! !ARGUMENTS: @\
      type(ESMF_Array) :: array @\
      type (ESMF_ArrWrap##mtypekind##mrank##D) :: wrap @\
      integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!      Deallocate data contents if Array object is responsible for cleaning up. @\
! @\
!EOP @\
! !REQUIREMENTS: @\
 @\
        integer :: status                               ! local error status @\
 @\
        status = ESMF_FAILURE  @\
 @\
        call c_ESMC_ArrayGetF90Ptr(array, wrap, status) @\
        deallocate(wrap % ##mtypekind##mrank##Dptr) @\
 @\
        if (present(rc)) rc = status @\
 @\
        end subroutine ESMF_ArrayDeallocate##mtypekind##mrank##D @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

