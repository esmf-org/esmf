#if 0
! $Id: ESMF_FieldSetMacros.h,v 1.26 2008/02/15 23:36:01 theurich Exp $
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
! Macros for the Field class.
!------------------------------------------------------------------------------
#endif

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldSetDataPtr<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldSetDataPtrDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOPI @\
! !IROUTINE: ESMF_FieldSetDataPtr - Add data to a field directly by Fortran pointer @\
! @\
! !INTERFACE: @\
! ! Private name; call using ESMF_FieldSetDataPtr() @\
!      subroutine ESMF_FieldSetDataPtr<rank><type><kind>(field, & @\
!                                 dataptr, copyflag, indexflag, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Field), intent(inout) :: field @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: dataptr @\
!      integer, intent(in), optional :: staggerloc  @\
!      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
!      type(ESMF_IndexFlag), intent(in), optional :: indexflag  @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Set data in an {\tt ESMF\_Field} directly from a Fortran pointer. @\
! Valid type/kind/rank combinations supported by the @\
! framework are: ranks 1 to 7, type real of kind *4 or *8, @\
! and type integer of kind *1, *2, *4, or *8. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[field] @\
!   The {\tt ESMF\_Field} to query. @\
!  \item[dataptr] @\
!   An associated Fortran pointer of the proper Type, Kind, and Rank as @\
!   the data in the Field.  When this call returns successfully, the pointer @\
!   will now point to the data in the Field.  This is either a reference or @\
!   a copy, depending on the setting of the following argument.  @\
!  \item[{[copyflag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, @\
!   a separate copy of the data will be allocated and the pointer will point @\
!   at the copy.  If a new copy of the data is made, the caller is @\
!   responsible for deallocating the space. @\
!  \item[{[staggerloc]}] @\
!   Defaults to 0.  If specified, the staggerloc specifies the relative position of @\
!   data array and grid. @\
!  \item[{[indexflag]}] @\
!   See Section~\ref{opt:indexflag} for possible values.  Defaults @\
!   to {\tt ESMF\_INDEX\_DELOCAL}.  If set to @\
!   {\tt ESMF\_INDEX\_GLOBAL} and the {\tt ESMF\_Grid} associated with the @\
!   {\tt ESMF\_Field} is regular, then the lower bounds and upper bounds will @\
!   be allocated with global index numbers corresponding to the grid. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOPI @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated Fortran array and a list of counts.
!------------------------------------------------------------------------------
#endif

#define FieldSetDataPtrMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldSetDataPtr" @\
      subroutine ESMF_FieldSetDataPtr##mrank##D##mtypekind(field, & @\
                        dataptr, copyflag, staggerloc, indexflag, rc) @\
 @\
      type(ESMF_Field), intent(inout) :: field @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: dataptr @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: old_dataptr @\
      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
      integer, intent(in), optional :: staggerloc  @\
      type(ESMF_IndexFlag), intent(in), optional :: indexflag @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        type(ESMF_Array) :: array, newarray  ! array object @\
        integer :: localrc                   ! local error status @\
        logical :: rcpresent                 ! did user specify rc? @\
 @\
        type(ESMF_DistGrid) :: distgrid    ! distgrid in field%grid @\
        integer, dimension(mrank) :: comp_edge_u_width @\
 @\
        ! Initialize return code assume routine not implemented @\
        localrc = ESMF_RC_NOT_IMPL @\
        rcpresent = .FALSE. @\
 @\
        if (present(rc)) then @\
          rcpresent = .TRUE. @\
          rc = ESMF_RC_NOT_IMPL @\
        endif @\
 @\
        ! check variables @\
        ESMF_INIT_CHECK_DEEP(ESMF_FieldGetInit,field,rc) @\
 @\
        ! Test to see if pointer already associated, and fail if not so. @\
        if (.not.associated(dataptr)) then @\
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, & @\
                              "Data Ptr must already be associated", & @\
                              ESMF_CONTEXT, rc)) return @\
        endif @\
 @\ 
        ! do sanity check on existing internal data array @\
        ! destroy existing internal data array if it was copied @\
        if ( (field%ftypep%datastatus .eq. ESMF_STATUS_READY) ) then  @\
            if (field%ftypep%array_internal) then @\
                call ESMF_ArrayDestroy(field%ftypep%array, rc=localrc) @\
                if (ESMF_LogMsgFoundError(localrc, & @\
                  ESMF_ERR_PASSTHRU, & @\
                  ESMF_CONTEXT, rc)) return @\
            endif @\
        end if @\
 @\
        ! fetch the distgrid from field%grid @\
        call ESMF_GridGet(field%ftypep%grid, distgrid=distgrid, rc=localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
        ! create a new array to be used to replace existing field%array @\
        comp_edge_u_width = -1 @\
        array = ESMF_ArrayCreate(dataptr, distgrid=distgrid, staggerloc=staggerloc, & @\
                             computationalEdgeUWidth=comp_edge_u_width, rc=localrc) @\
        if (ESMF_LogMsgFoundError(localrc, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        ! set array as data in field. @\
        ! default copyflag value is ESMF_DATA_REF @\
        ! in the case setdataptr creates an array first to be copied @\
        ! that array is destroyed after being copied. @\
        field%ftypep%array_internal = .true. @\
        if(.not. present(copyflag)) then @\
            field%ftypep%array = array @\
        else @\
            if(copyflag == ESMF_DATA_REF) then @\
                field%ftypep%array = array @\
            else @\
                newarray = ESMF_ArrayCreate(array, rc=localrc) @\
                if (ESMF_LogMsgFoundError(localrc, & @\
                                        ESMF_ERR_PASSTHRU, & @\
                                        ESMF_CONTEXT, rc)) return @\
                field%ftypep%array = newarray @\
                call ESMF_ArrayDestroy(array, rc=localrc) @\
                if (ESMF_LogMsgFoundError(localrc, & @\
                                        ESMF_ERR_PASSTHRU, & @\
                                        ESMF_CONTEXT, rc)) return @\
            endif @\
        endif @\
 @\
        call ESMF_FieldValidate(field, rc=localrc) @\
 @\
        if (ESMF_LogMsgFoundError(localrc, & @\
          ESMF_ERR_PASSTHRU, & @\
          ESMF_CONTEXT, rc)) return @\
 @\
        field%ftypep%datastatus = ESMF_STATUS_READY @\
        if (rcpresent) rc = localrc @\
 @\
        end subroutine ESMF_FieldSetDataPtr##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

