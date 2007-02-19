#if 0
! $Id: ESMF_LocalArrayMacros.h,v 1.26 2007/02/19 23:44:44 rosalind Exp $
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
!
#endif

#if 0
!------------------------------------------------------------------------------
! Declare a wrapper for a Fortran array pointer which is expanded later to
! one of every type, and a local variable of every type.
!------------------------------------------------------------------------------
#endif

#define LocalArrayTypeMacro(mname, mtypekind, mrank, mdim) \
! <Created by macro - do not edit directly > @\
      type ESMF_ArrWrap##mrank##D##mtypekind @\
        mname (ESMF_KIND_##mtypekind),dimension(mdim),pointer :: ptr##mrank##D##mtypekind @\
      end type ESMF_ArrWrap##mrank##D##mtypekind @\

#define ArrayLocalVarMacro(mname, mtypekind, mrank, mdim) \
        type(ESMF_ArrWrap##mrank##D##mtypekind) :: l##mrank##D##mtypekind


#if 0
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Create a new array based on an unallocated Fortran pointer and a list of counts.
! Documentation for LocalArrayCreate from unallocated Fortran pointer, followed
! by Macro containing code to be expanded for each T/K/R.
!------------------------------------------------------------------------------
#endif

#define LocalArrayCreateByMTPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_LocalArrayCreate - Create an ESMF array from an unallocated Fortran pointer @\
! @\
! !INTERFACE: @\
!      ! Private name; call using ESMF_LocalArrayCreate() @\
!      function ESMF_LocalArrCreateByMTPtr<rank><type><kind>(fptr, counts, lbounds, ubounds, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr<rank><type><kind> @\
! @\
! !ARGUMENTS: @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!      integer, dimension(:), intent(in) :: counts @\
!      integer, dimension(:), intent(in), optional :: lbounds @\
!      integer, dimension(:), intent(in), optional :: ubounds @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt ESMF\_LocalArray} based on an unassociated Fortran pointer. @\
! This routine allocates memory to the array pointer and fills in @\
! the array object with all necessary information. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The function return is an ESMF\_LocalArray type with space allocated for data. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[fptr] @\
!   An unassociated Fortran array pointer.  @\
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
!EOPI @\
 @\


#define LocalArrayCreateByMTPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_LocalArrCreateByMTPtr" @\
  function ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind(fptr, counts, lbounds, ubounds, rc) @\
 @\
    type(ESMF_LocalArray) :: ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind @\
 @\
    mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
    integer, dimension(:), intent(in) :: counts @\
    integer, dimension(:), intent(in), optional :: lbounds @\
    integer, dimension(:), intent(in), optional :: ubounds @\
    integer, intent(out), optional :: rc   @\
 @\
    ! Local variables @\
    type (ESMF_LocalArray) :: array     ! new array object @\
    integer                :: localrc   ! local return code @\
 @\
    ! Initialize return code; assume failure until success is certain @\
    if (present(rc)) rc = ESMF_FAILURE @\
 @\
    array%this = ESMF_NULL_POINTER @\
 @\
    ! Test to see if array already allocated, and fail if so. @\
    if (associated(fptr)) then @\
      if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, & @\
        "Pointer cannot already be allocated", & @\
        ESMF_CONTEXT, rc)) return @\
    endif @\
 @\
    ! Call create routine @\
    call c_ESMC_LocalArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_TYPEKIND_##mtypekind, & @\
      ESMF_FROM_FORTRAN, localrc) @\
    if (ESMF_LogMsgFoundError(localrc, & @\
      ESMF_ERR_PASSTHRU, & @\
      ESMF_CONTEXT, rc)) return @\
 @\
    call ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind(array, counts, fptr,& @\
      ESMF_DATA_SPACE, lbounds, ubounds, localrc) @\
    if (ESMF_LogMsgFoundError(localrc, & @\
      ESMF_ERR_PASSTHRU, & @\
      ESMF_CONTEXT, rc)) return @\
 @\
    ! return value set by c_ESMC func above @\
    ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind = array @\
 @\
    ! Set init code  @\
    ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind) @\
 @\
    ! return successfully @\
    if (present(rc)) rc = ESMF_SUCCESS @\
 @\
  end function ESMF_LocalArrCreateByMTPtr##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Create a new array based on an allocated Fortran pointer and a copy flag.
! Documentation for creating a LocalArray based on an allocated Fortran Pointer
! followed by Macro which contains code to be expanded for each T/K/R.
!------------------------------------------------------------------------------
#endif

#define LocalArrayCreateByFlPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_LocalArrayCreate - make an ESMF array from an allocated Fortran pointer @\
! @\
! !INTERFACE: @\
!      ! Private name; call using ESMF_LocalArrayCreate() @\
!      function ESMF_LocalArrCreateByFlPtr<rank><type><kind>(fptr, docopy, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr<rank><type><kind> @\
! @\
! !ARGUMENTS: @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt ESMF\_LocalArray} based on an already allocated @\
! Fortran array pointer.  This routine can make a copy or reference the @\
! existing data and fills in the array object with all necessary information. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The function return is an ESMF\_LocalArray type. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[fptr] @\
!   An allocated Fortran array pointer.  @\
!  \item[{[docopy]}] @\
!   Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_LocalArray} reference @\
!   the existing data array.  If set to {\tt ESMF\_DATA\_COPY} this routine @\
!   allocates new space and copies the data from the pointer into the @\
!   new array. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\
 @\

#define LocalArrayCreateByFlPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_LocalArrCreateByFlPtr" @\
 @\
  function ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind(fptr, docopy, rc) @\
 @\
    type(ESMF_LocalArray) :: ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind @\
 @\
    mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
    type(ESMF_CopyFlag), intent(in), optional :: docopy @\
    integer, intent(out), optional :: rc   @\
 @\
    ! Local variables @\
    type (ESMF_LocalArray)    :: array   ! new array object @\
    integer                   :: localrc ! local return code @\
    type (ESMF_CopyFlag)      :: copy    ! do we copy or ref? @\
    integer, dimension(mrank) :: counts  ! per dim @\
    integer, dimension(mrank) :: lbounds ! per dim @\
    integer, dimension(mrank) :: ubounds ! per dim @\
 @\
    ! Initialize return code; assume failure until success is certain @\
    if (present(rc)) rc = ESMF_FAILURE @\
 @\
    ! Set default for copyflag @\
    if (present(docopy)) then @\
      copy = docopy @\
    else @\
      copy = ESMF_DATA_REF @\
    endif @\
 @\
    ! Test to see if F90 pointer is associated - fail otherwise. @\
    if (.not.associated(fptr)) then @\
      if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, & @\
        "Pointer must already be allocated", & @\
        ESMF_CONTEXT, rc)) return @\
    endif @\
 @\
    ! Get sizes from current F90 array, although the construct routine @\
    !   does not need it for an already allocated array.  @\
    counts = shape(fptr) @\
    lbounds = lbound(fptr) @\
    ubounds = ubound(fptr) @\
 @\
    ! Call create routine @\
    call c_ESMC_LocalArrayCreateNoData(array, mrank, ESMF_DATA_##mname, ESMF_TYPEKIND_##mtypekind, & @\
      ESMF_FROM_FORTRAN, localrc) @\
    if (ESMF_LogMsgFoundError(localrc, & @\
      ESMF_ERR_PASSTHRU, & @\
      ESMF_CONTEXT, rc)) return @\
 @\
    call ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind(array, counts, fptr,& @\
      copy, lbounds, ubounds, localrc) @\
    if (ESMF_LogMsgFoundError(localrc, & @\
      ESMF_ERR_PASSTHRU, & @\
      ESMF_CONTEXT, rc)) return @\
 @\
    ! return value set by c_ESMC func above @\
    ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind = array @\
 @\
    ! Set init code  @\
    ESMF_INIT_SET_CREATED(ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind) @\
 @\
    ! return successfully @\
    if (present(rc)) rc = ESMF_SUCCESS @\
 @\
  end function ESMF_LocalArrCreateByFlPtr##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Create a Fortran pointer of the proper type and set the corresponding values
!  in the array object.  First the documentation macro, 
! followed by Macro which contains code to be expanded for each T/K/R.
!------------------------------------------------------------------------------
#endif

#define LocalArrConstrF90PtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_LocalArrConstrF90Ptr<rank><type><kind> - Create a Fortran Ptr of the proper T/K/R @\
! @\
! !INTERFACE: @\
!      subroutine ESMF_LocalArrConstrF90Ptr<rank><type><kind>(array, counts, fptr, docopy, lbounds, ubounds, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_LocalArray), intent(inout) :: array @\
!      integer, dimension(:), intent(in) :: counts @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer, optional :: fptr  @\
!      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
!      integer, dimension(:), intent(in), optional :: lbounds @\
!      integer, dimension(:), intent(in), optional :: ubounds @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
!  Creates a Fortran Pointer of the requested T/K/R.  After creating the @\
!  pointer and doing the allocation based on counts, also goes ahead and @\
!  calls into the C++ interfaces to set values on the {\tt ESMF\_LocalArray} @\
!  object. (This is to save on the total number of nested crossings of the @\
!  F90/C++ boundary.) @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
!  Optional args are an existing Fortran pointer which if given is used @\
!  instead of a new one, and a docopy flag which if set to copy will @\
!  do a contents copy or reference. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[array] @\
!   The {\tt ESMF\_LocalArray} to set the values into. @\
!  \item[counts] @\
!   An integer array of counts.  Must be the same length as the rank. @\
!  \item[{[fptr]}] @\
!   An optional existing Fortran pointer.  Will be used instead of an @\
!   internally generated Fortran pointer if given.  Must be given if the @\
!   {\tt docopy} is specified. @\
!  \item[{[docopy]}] @\
!   An optional copy flag which can be specified if a Fortran pointer is also @\
!   given.  Can either make a new copy of the data or ref existing data. @\
!  \item[{[lbounds]}] @\
!  An integer array of lower index values.  Must be same length as the rank. @\
!  \item[{[ubounds]}] @\
! An integer array of upper index values.  Must be same length as the rank. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\
 @\

#define LocalArrConstrF90PtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_LocalArrConstrF90Ptr" @\
 @\
      subroutine ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind(array, counts, fptr, docopy, lbounds, ubounds, rc) @\
 @\
      type(ESMF_LocalArray), intent(inout) :: array @\
      integer, dimension(:), intent(in) :: counts @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer, optional :: fptr  @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
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
        ! Assume defaults first, then alter if lb or ub specified, @\
        ! or if an existing pointer is given and can be queried. @\
        lb(:) = 1 @\
        ub(1:size(counts)) = counts @\
 @\
        ! Decide if we need to do: make a new allocation, copy existing data @\
        if (.not. present(fptr)) then @\
           nullify(newp) @\
           willalloc = .true. @\
           willcopy = .false. @\
           do_dealloc = ESMF_TRUE @\
        else @\
           if (docopy .eq. ESMF_DATA_SPACE) then @\
               newp => fptr    ! ptr alias, important this be =>  @\
               lb(1:size(counts)) = lbound(fptr) @\
               ub(1:size(counts)) = ubound(fptr) @\
               willalloc = .true. @\
               willcopy = .false. @\
               do_dealloc = ESMF_TRUE @\
           else if (docopy .eq. ESMF_DATA_COPY) then @\
               nullify(newp) @\
               willalloc = .true. @\
               willcopy = .true. @\
               do_dealloc = ESMF_TRUE @\
           else       ! ESMF_DATA_REF @\
               newp => fptr    ! ptr alias, important this be =>  @\
               lb(1:size(counts)) = lbound(fptr) @\
               ub(1:size(counts)) = ubound(fptr) @\
               willalloc = .false. @\
               willcopy = .false. @\
               do_dealloc = ESMF_FALSE @\
           endif @\
        endif @\
 @\
        ! lbounds, if given, should be used @\
        if (present(lbounds)) then @\
            lb(1:size(lbounds)) = lbounds @\
        endif @\
 @\
        ! ub is only used during allocation @\
        if (willalloc) then @\
            if (present(ubounds)) then @\
                ub(1:size(ubounds)) = ubounds @\
            endif @\
            allocate(newp(mrng), stat=status) @\
            if (ESMF_LogMsgFoundAllocError(status, & @\
                                 "LocalArray data space", & @\
                                 ESMF_CONTEXT, rc)) return @\
        endif @\
 @\
        if (willcopy) then @\
            newp = fptr      ! contents copy, important that this be = @\
        endif @\
 @\
        ! Now set all the new accumulated information about the array - the @\
        ! Fortran pointer, the base addr, the counts, etc. @\
 @\
        ! Until we need offsets, use 0. @\
        offsets = 0 @\
 @\
        wrap%ptr##mrank##D##mtypekind => newp @\
        call c_ESMC_LocalArraySetInternal(array, wrap, & @\
                                 ESMF_DATA_ADDRESS(newp(mloc)), counts, & @\
                                 lb, ub, offsets, & @\
                                 ESMF_TRUE, do_dealloc, status) @\
 @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        if (rcpresent) rc = status @\
 @\
        end subroutine ESMF_LocalArrConstrF90Ptr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Adjust the bounds of the Fortran pointer member in a LocalArray object
! First the documentation macro, followed by Macro which contains code to be
! expanded for each T/K/R.
!------------------------------------------------------------------------------
#endif

#define LocalArrayAdjustDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_LocalArrayAdjust<rank><type><kind> - Adjust the bounds of the Fortran pointer member according to the proper T/K/R @\
! @\
! !INTERFACE: @\
!      recursive subroutine ESMF_LocalArrayAdjust<rank><type><kind>(array,&@\
!  counts, lb, ub, fshape, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_LocalArray), intent(inout) :: array @\
!      integer, dimension(:), intent(in) :: counts @\
!      integer, dimension(:), intent(in), optional :: lb @\
!      integer, dimension(:), intent(in), optional :: ub @\
!      mname (ESMF_KIND_##mtypekind), dimension(mdim), target, optional ::&@\
! fshape(mrng) @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Each LocalArray object internally keeps a reference to an F90 array pointer. @\
! This call modifies the meta-data associated with this F90 array pointer @\
! by passing the F90 array pointer into a F90 subroutine with an explicit shape @\
! dummy argument. On this interface the bounds meta data for the dummy argument @\
! is not those of the actual argument but is reset to the bounds specified @\
! on the subroutine interface. Using macros the bounds on the callee side are @\
! set to match those of the LocalArray object meta data. Finally the internal @\
! F90 array pointer is reset to reflect the desired bounds in the F90 dope @\
! vector. The risk of data copy on this interface should be minimal because @\
! the shape is not changed and the dummy argument has the target attribute. @\
!EOPI @\
 @\

#define LocalArrayAdjustMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_LocalArrayAdjust##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_LocalArrayAdjust" @\
 @\
      recursive subroutine ESMF_LocalArrayAdjust##mrank##D##mtypekind(array,&@\
        counts, lb, ub, fshape, rc) @\
 @\
      type(ESMF_LocalArray), intent(inout) :: array @\
      integer, dimension(:), intent(in) :: counts @\
      integer, dimension(:), intent(in) :: lb @\
      integer, dimension(:), intent(in) :: ub @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), target, optional :: &@\
  fshape(mrng) @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        integer :: status                   ! local error status @\
 @\
        type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap ! to pass f90 ptr to C++ @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr  @\
 @\
        ! Initialize return code; assume failure until success is certain @\
        status = ESMF_FAILURE @\
 @\
        ! Recursive branch @\
        if (present(fshape)) then @\
          ! second recursion -> set the member in LocalArray @\
!print *, "Second recursion: ", lbound(fshape), ubound(fshape) @\
!call c_esmc_vmpointerprint(fshape) @\
          wrap%ptr##mrank##D##mtypekind => fshape @\
          call c_ESMC_LocalArraySetF90Ptr(array, wrap, status) @\
        else @\
          ! first recursion -> get F90ptr member and call subr. recursively @\
          call c_ESMC_LocalArrayGetF90Ptr(array, wrap, status) @\
          if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
          fptr => wrap%ptr##mrank##D##mtypekind @\
!print *, "First recursion: ", lbound(fptr), ubound(fptr) @\
!call c_esmc_vmpointerprint(fptr) @\
          call ESMF_LocalArrayAdjust##mrank##D##mtypekind(array, counts, lb, ub, fptr, rc=status) @\
        endif @\
 @\
        if (present(rc)) rc = ESMF_SUCCESS @\
 @\
        end subroutine ESMF_LocalArrayAdjust##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
! Get a Fortran pointer to the data contained in this array
! First the doc macro, then the actual source
!------------------------------------------------------------------------------
#endif

#define LocalArrayGetDataDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOPI @\
! !INTERFACE: @\
!     subroutine ESMF_LocalArrayGetData<rank><type><kind>(array, fptr, docopy, rc) @\
! @\
! !ARGUMENTS: @\
!     type(ESMF_LocalArray) :: array @\
!     <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!     type(ESMF_CopyFlag), intent(in), optional :: docopy @\
!     integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
! Return a Fortran pointer to the data buffer, or return a Fortran pointer @\
! to a new copy of the data. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[array] @\
!   The {\tt ESMF\_LocalArray} to get the value from. @\
!  \item[fptr] @\
!   An unassociated Fortran pointer. @\
!  \item[{[docopy]}] @\
!   An optional copy flag which can be specified. @\
!   Can either make a new copy of the data or reference existing data. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\
 @\

#define LocalArrayGetDataMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_LocalArrayGetData##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_LocalArrayGetData" @\
      subroutine ESMF_LocalArrayGetData##mrank##D##mtypekind(array, fptr, docopy, rc) @\
 @\
      type(ESMF_LocalArray) :: array @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, intent(out), optional :: rc @\
 @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
        logical :: copyreq                  ! did user specify copy? @\
 @\
        type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap     ! for passing f90 ptr to C++ @\
        integer :: lb(mrank), ub(mrank)  ! size info for the array @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: lp ! local copy @\
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
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        ! Allocate a new buffer if requested and return a copy @\
        if (copyreq) then @\
          call c_ESMC_IArrayGetLbounds(array, mrank, lb, status) @\
          if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
          call c_ESMC_IArrayGetUbounds(array, mrank, ub, status) @\
          if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
          allocate(lp(mrng), stat=status) @\
          if (ESMF_LogMsgFoundAllocError(status, & @\
                                     "local data space", & @\
                                      ESMF_CONTEXT, rc)) return @\
          ! this must do a contents assignment @\
          lp = wrap%ptr##mrank##D##mtypekind @\
          fptr => lp  @\
        else @\
          fptr => wrap%ptr##mrank##D##mtypekind @\
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
!------------------------------------------------------------------------------
! Deallocate the contents of the array.
!------------------------------------------------------------------------------
#endif

#define LocalArrayDeallocateDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOPI @\
! !INTERFACE: @\
!      subroutine ESMF_LocalArrayDeallocate<rank><type><kind>(array, wrap, rc) @\
! @\
! !RETURN VALUE: @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_LocalArray) :: array @\
!      type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap @\
!      integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!      Deallocate data contents if LocalArray object is responsible for cleaning up. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[array] @\
!   The {\tt ESMF\_LocalArray} to get the value from. @\
!  \item[wrap] @\
!   An internal derived type containing the Fortran pointer. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\
 @\

#define LocalArrayDeallocateMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_LocalArrayDeallocate##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_LocalArrayDeallocate" @\
      subroutine ESMF_LocalArrayDeallocate##mrank##D##mtypekind(array, wrap, rc) @\
 @\
      type(ESMF_LocalArray) :: array @\
      type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap @\
      integer, intent(out), optional :: rc @\
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

