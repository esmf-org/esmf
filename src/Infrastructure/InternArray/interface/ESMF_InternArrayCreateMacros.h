#if 0
! $Id: ESMF_InternArrayCreateMacros.h,v 1.10.2.2 2009/01/21 21:25:21 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
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
! Declare a wrapper for a Fortran array pointer, a local variable of that type,
!  type which wraps every pointer type, and a declaration of local variables,
!  one of each wrapper type.
!------------------------------------------------------------------------------
#endif

#define ArrayTypeMacro(mname, mtypekind, mrank, mdim) \
! <Created by macro - do not edit directly > @\
      type ESMF_ArrWrap##mrank##D##mtypekind @\
        mname (ESMF_KIND_##mtypekind),dimension(mdim),pointer :: ptr##mrank##D##mtypekind @\
      end type ESMF_ArrWrap##mrank##D##mtypekind @\


#define ArrayLocalVarMacro(mname, mtypekind, mrank, mdim) \
        type(ESMF_ArrWrap##mrank##D##mtypekind) :: l##mrank##D##mtypekind


#if 0
!------------------------------------------------------------------------------
! Create a new array from an unallocated Fortran array ptr and a list of counts.
! Documentation for this class of creates, then the actual macro.
!------------------------------------------------------------------------------
#endif

#define ArrayCreateByMTPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_ArrayCreate - Make an ESMF array from an unallocated Fortran array pointer @\
! @\
! !INTERFACE: @\
!      ! Private name; call using ESMF_ArrayCreate() @\
!      function ESMF_ArrayCreateByMTPtr<rank><type><kind>(farr, counts, haloWidth, lbounds, ubounds, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr<rank><type><kind> @\
! @\
! !ARGUMENTS: @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farr @\
!      integer, dimension(:), intent(in) :: counts @\
!      integer, intent(in), optional :: haloWidth @\
!      integer, dimension(:), intent(in), optional :: lbounds @\
!      integer, dimension(:), intent(in), optional :: ubounds @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Creates an {\tt ESMF\_Array} based on an unallocated (but allocatable) @\
! Fortran array pointer.  This routine allocates memory to the array and @\
! saves all necessary information about bounds, data type, kind, etc. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The function return is an {\tt ESMF\_Array} type with space @\
! allocated for data. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[farr] @\
!    An allocatable (but currently unallocated) Fortran array pointer.  @\
!  \item[counts] @\
!    An integer array of counts.  Must be the same length as the rank. @\
!  \item[{[haloWidth]}]  @\
!    An integer count of the width of the halo region on all sides of @\
!   the array. The default is 0, no halo region. @\
!  \item[{[lbounds]}]  @\
!    An integer array of lower index values.  Must be the same length @\
!    as the rank. @\
!  \item[{[ubounds]}]  @\
!    An integer array of upper index values.  Must be the same length @\
!    as the rank. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\
 @\

#define ArrayCreateByMTPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_ArrayCreateByMTPtr##rank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_ArrayCreateByMTPtr" @\
      function ESMF_ArrayCreateByMTPtr##mrank##D##mtypekind(fptr, counts, & @\
                                           haloWidth, lbounds, ubounds, rc) @\
 @\
      type(ESMF_InternArray) :: ESMF_ArrayCreateByMTPtr##mrank##D##mtypekind @\
 @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
      integer, dimension(:), intent(in) :: counts @\
      integer, intent(in), optional :: haloWidth @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        type(ESMF_InternArray) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        integer :: hwidth                   ! local copy of halo width @\
 @\
        ! Initialize return code; assume routine not implemented @\
        status = ESMF_RC_NOT_IMPL @\
        if (present(rc)) rc = ESMF_RC_NOT_IMPL @\
        array%this = ESMF_NULL_POINTER @\
 @\
        ! Test to see if array already allocated, and fail if so. @\
        if (associated(fptr)) then @\
           call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & @\
                                "Pointer cannot already be allocated", & @\
                                 ESMF_CONTEXT, rc) @\
           return @\
        endif @\
 @\
        ! Always supply a halo value, setting it to 0 if not specified. @\
        if (present(haloWidth)) then @\
          hwidth = haloWidth @\
        else @\
          hwidth = 0 @\
        endif @\
 @\
        ! Call create routine @\
        call c_ESMC_IArrayCreateNoData(array, mrank, ESMF_TYPEKIND_##mtypekind, & @\
                                          ESMF_FROM_FORTRAN, status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        call ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind(array, counts, hwidth,& @\
                                 fptr, ESMF_DATA_SPACE, lbounds, ubounds, status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        ! return value set by c_ESMC func above @\
        ESMF_ArrayCreateByMTPtr##mrank##D##mtypekind = array @\
        if (present(rc)) rc = status @\
        ! Set init code  @\
        ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByMTPtr##mrank##D##mtypekind) @\
 @\
        end function ESMF_ArrayCreateByMTPtr##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\


#if 0
!------------------------------------------------------------------------------
! Create a new array based on an allocated Fortran array and a copy flag.
! Documentation for this class of creates, then the actual macro.
!------------------------------------------------------------------------------
#endif

#define ArrayCreateByFullPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_ArrayCreate - Make an ESMF array from an allocated Fortran array @\
! @\
! !INTERFACE: @\
!     ! Private name; call using ESMF_ArrayCreate() @\
!      function ESMF_ArrayCreateByFullPtr<rank><type><kind>(farr, docopy, haloWidth, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr<rank><type><kind> @\
! @\
! !ARGUMENTS: @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: farr @\
!      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
!      integer, intent(in), optional :: haloWidth @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Create an {\tt ESMF\_Array} based on an already allocated Fortran array @\
! pointer.  This routine can make a copy or reference the existing data @\
! and saves all necessary information about bounds, data type, kind, etc. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The function return is an {\tt ESMF\_Array} type. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[farr] @\
!    An allocated Fortran array pointer. @\
!  \item[{[docopy]}] @\
!   Default to {\tt ESMF\_DATA\_REF}, makes the {\tt ESMF\_Array} reference @\
!   the existing data array.  If set to {\tt ESMF\_DATA\_COPY} this routine @\
!   allocates new space and copies the data from the pointer @\
!   into the new array. @\
!  \item[{[haloWidth]}] @\
!    Set the maximum width of the halo region on all edges. Defaults to 0. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\
 @\


#define ArrayCreateByFullPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_ArrayCreateByFullPtr" @\
 @\
      function ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind(fptr, docopy, & @\
                                                haloWidth, rc) @\
 @\
      type(ESMF_InternArray) :: ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind @\
 @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, intent(in), optional :: haloWidth @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        type(ESMF_InternArray) :: array          ! new array object @\
        integer :: status                   ! local error status @\
        integer :: hwidth                   ! local copy of halo width @\
        type (ESMF_CopyFlag) :: copy        ! do we copy or ref? @\
        integer, dimension(mrank) :: counts ! per dim @\
        integer, dimension(mrank) :: lbounds ! per dim @\
        integer, dimension(mrank) :: ubounds ! per dim @\
 @\
        ! Initialize return code; assume routine not implemented @\
        status = ESMF_RC_NOT_IMPL @\
        if (present(rc)) rc = ESMF_RC_NOT_IMPL @\
        array%this = ESMF_NULL_POINTER @\
 @\
        ! Set default for copyflag @\
        if (present(docopy)) then @\
            copy = docopy @\
        else @\
            copy = ESMF_DATA_REF @\
        endif @\
 @\
        ! Test to see if array is not already allocated, and fail if so. @\
        if (.not.associated(fptr)) then @\
          call ESMF_LogMsgSetError(ESMF_RC_OBJ_BAD, & @\
                                "Pointer must already be allocated", & @\
                                 ESMF_CONTEXT, rc) @\
          return @\
        endif @\
 @\
        ! Get sizes from current array, although the construct routine @\
        !   does not need it for an already allocated array.  @\
        counts = shape(fptr) @\
        lbounds = lbound(fptr) @\
        ubounds = ubound(fptr) @\
 @\
        ! Always supply a halo value, setting it to 0 if not specified. @\
        if (present(haloWidth)) then @\
          hwidth = haloWidth @\
        else @\
          hwidth = 0 @\
        endif @\
 @\
        ! Call create routine @\
        call c_ESMC_IArrayCreateNoData(array, mrank, ESMF_TYPEKIND_##mtypekind, & @\
                                      ESMF_FROM_FORTRAN, status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        call ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind(array, counts, hwidth,& @\
                                  fptr, copy, lbounds, ubounds, status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        ! return value set by c_ESMC func above @\
        ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind = array @\
        if (present(rc)) rc = status @\
        ! Set init code  @\
        ESMF_INIT_SET_CREATED(ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind) @\
 @\
        end function ESMF_ArrayCreateByFullPtr##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Create a Fortran pointer of the proper type and set the corresponding values
!  in the array object.  The doc and then actual macro.
!------------------------------------------------------------------------------
#endif

#define ArrayConstructF90PtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_ArrayConstructF90Ptr - Create a Fortran Pointer of the proper T/K/R @\
! @\
! !INTERFACE: @\
!      subroutine ESMF_ArrayConstructF90Ptr<rank><type><kind>(array, counts, hwidth, fptr, & @\
!                                                   docopy, lbounds, ubounds, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_InternArray), intent(inout) :: array @\
!      integer, dimension(:), intent(in) :: counts @\
!      integer, intent(in) :: hwidth @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer, optional :: fptr  @\
!      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
!      integer, dimension(:), intent(in), optional :: lbounds @\
!      integer, dimension(:), intent(in), optional :: ubounds @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
!  Create a Fortran pointer of the requested type/kind/rank. @\
!  After creating the pointer and doing the allocation @\
!  based on counts, also goes ahead and @\
!  calls into the C++ interfaces to set values on the {\tt ESMF\_Array} @\
!  object. (This is to save on the total number of nested crossings of the @\
!  Fortran/C++ boundary.) @\
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
!  \item[array]  The {\tt ESMF\_Array} to set the values into. @\
!  \item[counts]  An integer array of counts.  Must be the same length as the rank. @\
!  \item[hwidth]  An integer halo width. Currently same width on each edge. @\
!  \item[{[fptr]}] An optional existing Fortran pointer.  Will be used instead of an @\
!   internally generated Fortran pointer if given.  Must be given if the {\tt docopy} is specified. @\
!  \item[{[docopy]}]  An optional copy flag which can be specified if an Fortran pointer is also @\
!   given.  Can either make a new copy of the data or ref existing data. @\
!  \item[{[lbounds]}]  An integer array of lower index values.  Must be the same length as the rank. @\
!  \item[{[ubounds]}]  An integer array of upper index values.  Must be the same length as the rank. @\
!  \item[{[rc]}]  Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\
 @\

#define ArrayConstructF90PtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_ArrayConstructF90Ptr" @\
      subroutine ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind(array, counts, hwidth, fptr, & @\
                                                   docopy, lbounds, ubounds, rc) @\
 @\
      type(ESMF_InternArray), intent(inout) :: array @\
      integer, dimension(:), intent(in) :: counts @\
      integer, intent(in) :: hwidth @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer, optional :: fptr  @\
      type(ESMF_CopyFlag), intent(in), optional :: docopy @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        integer :: status                   ! local error status @\
        integer :: i                        ! loop counter @\
        logical :: willalloc                ! do we need to alloc/dealloc? @\
        logical :: willcopy                 ! do we need to copy data? @\
        logical :: zerosize                 ! one or more counts = 0 @\
        type(ESMF_Logical) :: do_dealloc    ! dealloc flag for SetInfo call @\
 @\
        type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap ! to pass f90 ptr to C++ @\
        mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: newp  @\
        integer, dimension(ESMF_MAXDIM) :: lb, ub @\
        integer, dimension(ESMF_MAXDIM) :: offsets @\
 @\
        ! Initialize return code; assume routine not implemented @\
        status = ESMF_RC_NOT_IMPL @\
        if (present(rc)) rc = ESMF_RC_NOT_IMPL @\
        zerosize = .FALSE. @\
 @\
        ! Assume defaults first, then alter if lb or ub specified, @\
        ! or if an existing pointer is given and can be queried. @\
        lb(:) = 1 @\
        ub(:) = 1 @\
        ub(1:size(counts)) = counts @\
 @\
        ! Decide if we need to do: make a new allocation, copy existing data @\
        if (.not. present(fptr)) then @\
           nullify(newp) @\
           willalloc = .true. @\
           willcopy = .false. @\
           do_dealloc = ESMF_TRUE @\
        else @\
           if (.not. associated(fptr)) then @\
               nullify(newp) @\
               willalloc = .true. @\
               willcopy = .false. @\
               do_dealloc = ESMF_TRUE @\
           else if (docopy .eq. ESMF_DATA_SPACE) then @\
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
            ! more error checking; for allocation lb must be @\
            ! less than or equal ub.  if any count is 0, all counts @\
            ! will be 0 for now. @\
            zerosize = .FALSE. @\
            do i=1, mrank @\
                if (counts(i) .le. 0) then @\
                    zerosize = .TRUE. @\
                    lb(i) = 0 @\
                    ub(i) = 0 @\
                else if (lb(i) .gt. ub(i)) then @\
                    call ESMF_LogMsgSetError(ESMF_RC_ARG_BAD, & @\
                                 "Lower bounds must be .le. upper bounds", & @\
                                 ESMF_CONTEXT, rc) @\
                    return @\
                endif @\
            enddo @\
            if (zerosize) then @\
                allocate(newp ( mlen ), stat=status) @\
            else @\
                allocate(newp ( mrng ), stat=status) @\
            endif @\
            if (ESMF_LogMsgFoundAllocError(status, "Array data space", & @\
                                       ESMF_CONTEXT, rc)) return @\
        endif @\
 @\
        if (willcopy) then @\
            newp = fptr      ! contents copy, important that this be = @\
        endif @\
 @\
        ! Now set all the new accumulated information about the array - the @\
        ! F90 pointer, the base addr, the counts, etc. @\
 @\
	! Until we need to use byte offsets, leave them 0. @\
        offsets = 0 @\
 @\
        wrap % ptr##mrank##D##mtypekind => newp @\
        if (zerosize) then @\
            call c_ESMC_IArraySetInfo(array, wrap, & @\
                                     ESMF_NULL_POINTER, counts, & @\
                                     lb, ub, offsets, & @\
                                     ESMF_TRUE, do_dealloc, hwidth, status) @\
        else @\
            call c_ESMC_IArraySetInfo(array, wrap, & @\
                                     ESMF_DATA_ADDRESS(newp(mloc)), counts, & @\
                                     lb, ub, offsets, & @\
                                     ESMF_TRUE, do_dealloc, hwidth, status) @\
        endif @\
 @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        if (present(rc)) rc = status @\
 @\
        end subroutine ESMF_ArrayConstructF90Ptr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

#if 0
!------------------------------------------------------------------------------
! Deallocate the contents of the array.
!------------------------------------------------------------------------------
#endif

#define ArrayDeallocateDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
!BOPI @\
! !INTERFACE: @\
!      subroutine ESMF_ArrayDeallocate<rank><type><kind>(array, wrap, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_InternArray) :: array @\
!      type (ESMF_ArrWrap<rank><type><kind>) :: wrap @\
!      integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!   Deallocate data contents if {\tt ESMF\_Array} is responsible @\
!   for deleting data space.  This routine is for internal use only. @\
! @\
!  \begin{description} @\
!  \item[array] @\
!    An {\tt ESMF\_Array} object. @\
!  \item[wrap] @\
!    A Fortran pointer of the proper type/kind/rank, wrapped in a @\
!    derived type to allow the pointer itself to be passed by reference. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\
 @\

#define ArrayDeallocateMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly >  @\
^undef  ESMF_METHOD @\
!define ESMF_METHOD "ESMF_ArrayDeallocate##mrank##D##mtypekind" @\
^define ESMF_METHOD "ESMF_ArrayDeallocate" @\
      subroutine ESMF_ArrayDeallocate##mrank##D##mtypekind(array, wrap, rc) @\
 @\
      type(ESMF_InternArray) :: array @\
      type (ESMF_ArrWrap##mrank##D##mtypekind) :: wrap @\
      integer, intent(out), optional :: rc @\
 @\
        integer :: status                               ! local error status @\
 @\
        status = ESMF_RC_NOT_IMPL  @\
 @\
        call c_ESMC_IArrayGetF90Ptr(array, wrap, status) @\
        deallocate(wrap % ptr##mrank##D##mtypekind) @\
 @\
        if (present(rc)) rc = status @\
 @\
        end subroutine ESMF_ArrayDeallocate##mrank##D##mtypekind @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

