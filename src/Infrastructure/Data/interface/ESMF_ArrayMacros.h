#if 0
! $Id: ESMF_ArrayMacros.h,v 1.7 2003/04/02 22:14:51 nscollins Exp $
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

! Macros for the Array class.

#define COL1 :
#define COL2 :,:
#define COL3 :,:,:
#define COL4 :,:,:,:
#define COL5 :,:,:,:,:

#define ArrayWrapperMacro(mname, mtypekind, mrank, mdim) \
! <Created by macro - do not edit directly > @  \
      type ESMF_ArrWrap##mtypekind##mrank##D @  \
        mname (ESMF_IKIND_##mtypekind),dimension(mdim),pointer :: mtypekind##mrank##Dptr @  \
      end type ESMF_ArrWrap##mtypekind##mrank##D @  \

#define LEN1 lengths(1)
#define LEN2 lengths(1), lengths(2)
#define LEN3 lengths(1), lengths(2), lengths(3)
#define LEN4 lengths(1), lengths(2), lengths(3), lengths(4)
#define LEN5 lengths(1), lengths(2), lengths(3), lengths(4), lengths(5)

#define LOC1 1
#define LOC2 1,1
#define LOC3 1,1,1
#define LOC4 1,1,1,1
#define LOC5 1,1,1,1,1



#define ArrayCreateMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_ArrayCreateByPtr##mtypekind##mrank##D - make an ESMF array from an F90 ptr @\
 @\
! !INTERFACE: @\
      function ESMF_ArrayCreateByPtr##mtypekind##mrank##D(f90ptr, docopy, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_Array) :: ESMF_ArrayCreateByPtr##mtypekind##mrank##D @\
! @\
! !ARGUMENTS: @\
      mname (ESMF_IKIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy  @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt Array} based on an existing Fortran 90 pointer which @\
!   is already associated with memory. @\
! @\
! The function return is an ESMF\_Array type. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[f90ptr] @\
!   A Fortran 90 array pointer which can be queried for info about @\
!    type/kind/rank and sizes. @\
! @\
!  \item[{[docopy]}] @\
!   Default to {\tt ESMF\_NO\_COPY}, makes the {\tt ESMF\_Array} reference @\
!   the existing data array.  If set to {\tt ESMF\_DO\_COPY} this routine @\
!   allocates new space and copies the data from the pointer into the @\
!   new array. @\
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
        ! local variables @\
        type (ESMF_Array) :: array          ! what C++ is going to return @\
        integer :: i                        ! local variable @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
        logical :: copyreq                  ! did user specify copy? @\
 @\
        type (ESMF_ArrWrap##mtypekind##mrank##D) :: wrap     ! for passing f90 ptr to C++ @\
        integer :: rank, lengths(mrank)         ! size info for the array @\
        mname (ESMF_IKIND_##mtypekind), dimension(mdim), pointer :: localp ! local copy @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        copyreq = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
!       ! call create routine @\
        do i=1, mrank @\
            lengths(i) = size(f90ptr, i) @\
        enddo @\
 @\
        call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_##mname, ESMF_KIND_##mtypekind, & @\
                                             mrank, lengths, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        if (present(docopy)) then @\
          if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. @\
        endif @\
        if (copyreq) then @\
          allocate(localp( mlen ), stat=status) @\
          if (status .ne. 0) then     ! f90 status, not ESMF @\
            print *, "Array do_copy allocate error" @\
            return @\
          endif @\
          call c_ESMC_ArraySetDealloc(array, status) @\
          localp = f90ptr   ! this needs to be a real contents copy @\
        else @\
          call c_ESMC_ArraySetNoDealloc(array, status) @\
          localp => f90ptr  ! simply a reference to existing space @\
        endif @\
        @\
 @\
!       ! set base address @\
        call c_ESMC_ArraySetBaseAddr(array, localp( mloc ), status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array base address construction error" @\
          return @\
        endif @\
 @\
!       ! save an (uninterpreted) copy of the f90 array information @\
        wrap%##mtypekind##mrank##Dptr => localp @\
        call c_ESMC_ArraySetF90Ptr(array, wrap, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array internal info save error" @\
          return @\
        endif @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_ArrayCreateByPtr##mtypekind##mrank##D = array @\
        if (rcpresent) rc = ESMF_SUCCESS @\
 @\
        end function ESMF_ArrayCreateByPtr##mtypekind##mrank##D   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\



#define ArrayCreateSpecMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_ArrayCreateBySpec##mtypekind##mrank##D - make an ESMF array from a Spec @\
 @\
! !INTERFACE: @\
      function ESMF_ArrayCreateBySpec##mtypekind##mrank##D(lengths, rc) @\
! @\
! !RETURN VALUE: @\
      type(ESMF_Array) :: ESMF_ArrayCreateBySpec##mtypekind##mrank##D @\
! @\
! !ARGUMENTS: @\
      integer, dimension(:), intent(in) :: lengths  @\
      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt Array} based on a spec and counts. @\
! @\
! The function return is an ESMF\_Array type. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[lengths] @\
!   An integer array of counts.  Must be the same length as the rank. @\
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
        ! local variables @\
        type (ESMF_Array) :: array          ! what C++ is going to return @\
        integer :: i                        ! local variable @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
        logical :: copyreq                  ! did user specify copy? @\
 @\
        type (ESMF_ArrWrap##mtypekind##mrank##D) :: wrap     ! for passing f90 ptr to C++ @\
        mname (ESMF_IKIND_##mtypekind), dimension(mdim), pointer :: localp ! local copy @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
        rcpresent = .FALSE. @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_FAILURE @\
        endif @\
 @\
        copyreq = .FALSE. @\
        array%this = ESMF_NULL_POINTER @\
 @\
!       ! call create routine @\
        call c_ESMC_ArrayCreateByPtr(array, ESMF_DATA_##mname, ESMF_KIND_##mtypekind, & @\
                                             mrank, lengths, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array initial construction error" @\
          return @\
        endif @\
 @\
        allocate(localp( mlen ), stat=status) @\
        if (status .ne. 0) then     ! f90 status, not ESMF @\
          print *, "Array do_copy allocate error" @\
          return @\
        endif @\
        call c_ESMC_ArraySetDealloc(array, status) @\
        @\
 @\
!       ! set base address @\
        call c_ESMC_ArraySetBaseAddr(array, localp( mloc ), status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array base address construction error" @\
          return @\
        endif @\
 @\
!       ! save an (uninterpreted) copy of the f90 array information @\
        wrap%##mtypekind##mrank##Dptr => localp @\
        call c_ESMC_ArraySetF90Ptr(array, wrap, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array internal info save error" @\
          return @\
        endif @\
 @\
!       ! return value set by c_ESMC func above @\
        ESMF_ArrayCreateBySpec##mtypekind##mrank##D = array @\
        if (rcpresent) rc = ESMF_SUCCESS @\
 @\
        end function ESMF_ArrayCreateBySpec##mtypekind##mrank##D   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\



#define ArrayGetDataMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOP @\
! !INTERFACE: @\
      subroutine ESMF_ArrayGetData##mtypekind##mrank##D(array, f90ptr, docopy, rc) @\
! @\
! !ARGUMENTS: @\
      type(ESMF_Array) :: array @\
      mname (ESMF_IKIND_##mtypekind), dimension(mdim), pointer :: f90ptr @\
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
        integer :: rank, lengths(mrank)         ! size info for the array @\
        mname (ESMF_IKIND_##mtypekind), dimension(mdim), pointer :: localp ! local copy @\
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
          if (docopy%docopy .eq. ESMF_DO_COPY%docopy) copyreq = .TRUE. @\
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
          call c_ESMC_ArrayGetLengths(array, mrank, lengths, status) @\
          if (status .ne. ESMF_SUCCESS) then @\
            print *, "Array - cannot retrieve array dim sizes" @\
            return @\
          endif @\
          allocate(localp( mlen ), stat=status) @\
          if (status .ne. 0) then     ! f90 status, not ESMF @\
            print *, "Array do_copy allocate error" @\
            return @\
          endif @\
          ! this must do a contents assignment @\
          localp = wrap%##mtypekind##mrank##Dptr @\
          f90ptr => localp  @\
        else @\
          f90ptr => wrap%##mtypekind##mrank##Dptr @\
        endif @\
 @\
        if (rcpresent) rc = ESMF_SUCCESS @\
 @\
        end subroutine ESMF_ArrayGetData##mtypekind##mrank##D @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\


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
        deallocate(wrap%##mtypekind##mrank##Dptr) @\
 @\
        if (present(rc)) rc = status @\
 @\
        end subroutine ESMF_ArrayDeallocate##mtypekind##mrank##D @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

