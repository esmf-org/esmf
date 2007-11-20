#if 0
! $Id: ESMF_FieldCreateMacros.h,v 1.24 2007/11/20 20:57:34 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2007, University Corporation for Atmospheric Research,
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
! Macros for the Field Create functions.
!------------------------------------------------------------------------------
#endif

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldCreateDPtr<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldCreateDPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_FieldCreate - Create a Field using an existing Fortran data pointer @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_FieldCreate() @\
!   function ESMF_FieldCreateDPtr<rank><type><kind>(grid, fptr, copyflag, & @\
!             staggerloc, haloWidth, datamap, name, iospec, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_Field) :: ESMF_FieldCreateDPtr<rank><type><kind> @\
! @\
! !ARGUMENTS: @\
!     type(ESMF_Grid), intent(in) :: grid @\
!     <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!     type(ESMF_CopyFlag), intent(in) :: copyflag @\
!     type(ESMF_StaggerLoc), intent(in), optional :: staggerloc @\
!     integer, intent(in), optional :: haloWidth @\
!     type(ESMF_FieldDataMap), intent(inout), optional :: datamap @\
!     character (len=*), intent(in), optional :: name  @\
!     type(ESMF_IOSpec), intent(in), optional :: iospec @\
!     integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!   Create an {\tt ESMF\_Field} and associate the data in the Fortran @\
!   array with the {\tt ESMF\_Field}. Return a new {\tt ESMF\_Field}. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
!   The arguments are: @\
!   \begin{description} @\
!   \item [grid] @\
!     Pointer to an {\tt ESMF\_Grid} object. @\
!   \item [fptr] @\
!     A Fortran array pointer which must be already allocated and the @\
!     proper size for this portion of the grid. @\
!   \item [copyflag] @\
!     Whether to copy the existing data space or reference directly. Valid @\
!     values are {\tt ESMF\_DATA\_COPY} or {\tt ESMF\_DATA\_REF}. @\
!   \item [{[staggerloc]}] @\
!     Relative location of data per grid cell/vertex in the grid. @\
!   \item [{[haloWidth]}] @\
!     Maximum halo depth along all edges.  Default is 0. @\
!   \item [{[datamap]}] @\
!     Describes the mapping of data to the {\tt ESMF\_Grid}. @\
!   \item [{[name]}] @\
!     {\tt Field} name. @\
!   \item [{[iospec]}] @\
!     I/O specification. @\
!   \item [{[rc]}] @\
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!   \end{description} @\
! @\
!EOPI @\

#if 0
!------------------------------------------------------------------------------
! Create a new Field based on an already allocated Fortran array pointer.
!------------------------------------------------------------------------------
#endif

#define FieldCreateDPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldCreateDPtr" @\
      function ESMF_FieldCreateDPtr##mrank##D##mtypekind(grid, fptr, copyflag, & @\
                staggerloc, haloWidth, datamap, name, iospec, rc) @\
 @\
      type(ESMF_Field) :: ESMF_FieldCreateDPtr##mrank##D##mtypekind @\
 @\
      type(ESMF_Grid), intent(in) :: grid @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
      type(ESMF_CopyFlag), intent(in) :: copyflag @\
      type(ESMF_StaggerLoc), intent(in), optional :: staggerloc @\
      integer, intent(in), optional :: haloWidth @\
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap @\
      character (len=*), intent(in), optional :: name  @\
      type(ESMF_IOSpec), intent(in), optional :: iospec @\
      integer, intent(out), optional :: rc @\
 @\
        ! Local variables @\
        type(ESMF_FieldType), pointer :: ftype  ! Pointer to new field @\
        type(ESMF_DistGrid) :: distgrid         ! distgrid object @\
        type(ESMF_Array) :: array               ! array object @\
        integer :: localrc                      ! local error status @\
        logical :: rcpresent                    ! did user specify rc? @\
 @\
        ! Initialize return code; assume routine not implemented @\
        localrc = ESMF_RC_NOT_IMPL @\
        rcpresent = .FALSE. @\
        ! TODO:FIELDINTEGRATION Initialize array pointer correctly in field create @\
        ! array%this = ESMF_NULL_POINTER @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_RC_NOT_IMPL @\
        endif @\
 @\
        ! check variables @\
        ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap) @\
 @\
        ! Test to see if pointer not associated, and fail if so. @\
        if (.not.associated(fptr)) then @\
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, & @\
                                "Data Pointer must already be associated", & @\
                                ESMF_CONTEXT, rc)) return @\
        endif @\
 @\
        call ESMF_GridGet(grid, distgrid=distgrid, rc=localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        array = ESMF_ArrayCreate(fptr, distgrid=distgrid, rc=localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        allocate(ftype, stat=localrc) @\
        if (ESMF_LogMsgFoundAllocError(localrc, & @\
                                       "Allocating Field information", & @\
                                       ESMF_CONTEXT, rc)) return @\
 @\
        ! Construction method allocates and initializes field internals. @\
        call ESMF_FieldConstructIA(ftype, grid, array, & @\
                                    staggerloc, & @\
                                    datamap, name, iospec, localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        ESMF_FieldCreateDPtr##mrank##D##mtypekind%ftypep => ftype  @\
 @\
        ESMF_INIT_SET_CREATED(ESMF_FieldCreateDPtr##mrank##D##mtypekind) @\
 @\
        if (rcpresent) rc = localrc @\
 @\
        end function ESMF_FieldCreateDPtr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldCreateEPtr<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldCreateEPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_FieldCreate - Create a Field using an unallocated Fortran data pointer @\
 @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_FieldCreate() @\
!   function ESMF_FieldCreateEPtr<rank><type><kind>(grid, fptr, allocflag, & @\
!             staggerloc, haloWidth, lbounds, ubounds, & @\
!             indexflag, datamap, name, iospec, rc) @\
! @\
! !RETURN VALUE: @\
!      type(ESMF_Field) :: ESMF_FieldCreateEPtr<rank><type><kind> @\
! @\
! !ARGUMENTS: @\
!     type(ESMF_Grid), intent(in) :: grid @\
!     <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!     type(ESMF_AllocFlag), intent(in), optional :: allocflag @\
!     type(ESMF_StaggerLoc), intent(in), optional :: staggerloc @\
!     integer, intent(in), optional :: haloWidth @\
!     integer, dimension(:), intent(in), optional :: lbounds @\
!     integer, dimension(:), intent(in), optional :: ubounds @\
!     type(ESMF_IndexFlag), intent(in), optional :: indexflag @\
!     type(ESMF_FieldDataMap), intent(in), optional :: datamap @\
!     character (len=*), intent(in), optional :: name  @\
!     type(ESMF_IOSpec), intent(in), optional :: iospec @\
!     integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!   Create an {\tt ESMF\_Field}, allocate necessary data space, and return @\
!   with the Fortran array pointer initialized to point to the data space. @\
!   Function return value is the new {\tt ESMF\_Field}. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
!  The arguments are: @\
!  \begin{description} @\
!  \item [grid] @\
!    Pointer to an {\tt ESMF\_Grid} object. @\
!  \item [fptr] @\
!    A Fortran array pointer which must be unallocated but of the @\
!    proper rank, type, and kind for the data to be associated with @\
!    this {\tt EWSF\_Field}. @\
!  \item [{[allocflag]}] @\
!    Whether to allocate space for the array.  @\
!    See Section~\ref{opt:allocflag} for possible values.  Default @\
!    is {\tt ESMF\_ALLOC}. @\
!  \item [{[staggerloc]}] @\
!    Relative location of data per grid cell/vertex in the grid. @\
!  \item [{[haloWidth]}] @\
!    Maximum halo depth along all edges.  Default is 0. @\
!  \item[{[lbounds]}]  @\
!    An integer array of lower index values.  Must be the same length @\
!    as the rank. @\
!  \item[{[ubounds]}]  @\
!    An integer array of upper index values.  Must be the same length @\
!    as the rank. @\
!  \item [{[indexflag]}] @\
!    Local or global indices.  See section \ref{opt:indexflag} for a @\
!    list of valid indexflag options.  The default is {ESMF\_INDEX\_DELOCAL}. @\
!  \item [{[datamap]}] @\
!    Describes the mapping of data to the {\tt ESMF\_Grid}. @\
!  \item [{[name]}] @\
!    {\tt Field} name. @\
!  \item [{[iospec]}] @\
!    I/O specification. @\
!  \item [{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\


#if 0
!------------------------------------------------------------------------------
! Create a new Field based on an unallocated Fortran array pointer.
!------------------------------------------------------------------------------
#endif

#define FieldCreateEPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
      function ESMF_FieldCreateEPtr##mrank##D##mtypekind(grid, fptr, allocflag, & @\
                staggerloc, haloWidth, lbounds, ubounds, & @\
                indexflag, datamap, name, iospec, rc) @\
 @\
      type(ESMF_Field) :: ESMF_FieldCreateEPtr##mrank##D##mtypekind @\
 @\
      type(ESMF_Grid), intent(in) :: grid @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
      type(ESMF_AllocFlag), intent(in), optional :: allocflag @\
      type(ESMF_StaggerLoc), intent(in), optional :: staggerloc @\
! TODO: this should not be lbounds, ubounds - but some global addr flag. @\
      integer, intent(in), optional :: haloWidth @\
      integer, dimension(:), intent(in), optional :: lbounds @\
      integer, dimension(:), intent(in), optional :: ubounds @\
      type(ESMF_IndexFlag), intent(in), optional :: indexflag @\
      type(ESMF_FieldDataMap), intent(inout), optional :: datamap @\
      character (len=*), intent(in), optional :: name  @\
      type(ESMF_IOSpec), intent(in), optional :: iospec @\
      integer, intent(out), optional :: rc @\
@\
        ! Local variables @\
        type (ESMF_FieldType), pointer :: ftype  ! Pointer to new field @\
        type (ESMF_ArraySpec) :: arrayspec  ! arrayspec object @\
        integer :: localrc                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
 @\
        ! Initialize return code; assume routine not implemented @\
        localrc = ESMF_RC_NOT_IMPL @\
        rcpresent = .FALSE. @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_RC_NOT_IMPL @\
        endif @\
 @\
        ! check variables @\
        ESMF_INIT_CHECK_SHALLOW(ESMF_FieldDataMapGetInit,ESMF_FieldDataMapInit,datamap) @\
 @\
        ! Test to see if pointer not already associated, and fail if so. @\
        if (associated(fptr)) then @\
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, & @\
                             "Data Pointer cannot already be associated", & @\
                              ESMF_CONTEXT, rc)) return @\
        endif @\
 @\
        ! Get rank, type, kind from ptr and initialize arrayspec @\
        call ESMF_ArraySpecSet(arrayspec, mrank, ESMF_TYPEKIND_##mtypekind, localrc) @\
 @\
        allocate(ftype, stat=localrc) @\
        if (ESMF_LogMsgFoundAllocError(localrc, & @\
                                       "Allocating Field information", & @\
                                       ESMF_CONTEXT, rc)) return @\
 @\
        ! Construction method allocates and initializes field internals. @\
        call ESMF_FieldConstructIA(ftype, grid, arrayspec, allocflag, & @\
                                    staggerloc, indexflag, datamap, & @\
                                    name, iospec, localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        ! Set return value, and then get pointer back. @\
        ESMF_FieldCreateEPtr##mrank##D##mtypekind%ftypep => ftype  @\
        call ESMF_FieldGetDataPointer(ESMF_FieldCreateEPtr##mrank##D##mtypekind, & @\
                                      fptr, ESMF_DATA_REF, rc=localrc) @\
 @\
        ESMF_INIT_SET_CREATED(ESMF_FieldCreateEPtr##mrank##D##mtypekind) @\
        if (rcpresent) rc = localrc @\
 @\
        end function ESMF_FieldCreateEPtr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\


