#if 0
! $Id: ESMF_FieldSetMacros.h,v 1.2 2004/06/07 05:21:07 nscollins Exp $
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
! Macros for the Field class.
!------------------------------------------------------------------------------
#endif

#include "ESMF_StdCppMacros.h"

#if 0
!------------------------------------------------------------------------------
! Documentation for the general FieldSetDataPointer<> macros.
!------------------------------------------------------------------------------
#endif

#define FieldSetDataPointerDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_FieldSetDataPointer - Add data to a field directly by Fortran pointer @\
! @\
! !INTERFACE: @\
!      ! Private name; call using ESMF_FieldSetDataPointer() @\
!      subroutine ESMF_FieldSetDataPointer<rank><type><kind>(field, fptr, copyFlag, indexFlag, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Field), intent(inout) :: field @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!      integer, intent(in), optional :: haloWidth  @\
!      type(ESMF_CopyFlag), intent(in), optional :: copyFlag @\
!      type(ESMF_IndexFlag), intent(in), optional :: indexFlag  @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Returns a direct Fortran pointer to the data in an {\tt ESMF\_Field}. @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[field] @\
!   The {\tt ESMF\_Field} to query. @\
!  \item[fptr] @\
!   An associated Fortran pointer of the proper Type, Kind, and Rank as @\
!   the data in the Field.  When this call returns successfully, the pointer @\
!   will now point to the data in the Field.  This is either a reference or @\
!   a copy, depending on the setting of the following argument.  @\
!  \item[{[copyFlag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, @\
!   a separate copy of the data will be allocated and the pointer will point @\
!   at the copy.  If a new copy of the data is made, the caller is @\
!   responsible for deallocating the space. @\
!  \item[{[haloWidth]}] @\
!   Defaults to 0.  If specified, the halo width to add to all sides of the @\
!   data array. @\
!  \item[{[indexFlag]}] @\
!   Defaults to {\tt ESMF\_LOCAL\_INDEX}.  If set to @\
!   {\tt ESMF\_GLOBAL\_INDEX} and the {\tt ESMF\_Grid} associated with the @\
!   {\tt ESMF\_Field} is regular, then the lower bounds and upper bounds will @\
!   be allocated with global index numbers corresponding to the grid. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\

#if 0
!------------------------------------------------------------------------------
! Create a new array based on an unallocated Fortran array and a list of counts.
!------------------------------------------------------------------------------
#endif

#define FieldSetDataPointerMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
^undef  ESMF_METHOD @\
^define ESMF_METHOD "ESMF_FieldSetDataPointer" @\
      subroutine ESMF_FieldSetDataPointer##mrank##D##mtypekind(field, fptr, copyFlag, haloWidth, indexFlag, rc) @\
 @\
      type(ESMF_Field), intent(inout) :: field @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
      type(ESMF_CopyFlag), intent(in), optional :: copyFlag @\
      integer, intent(in), optional :: haloWidth  @\
      type(ESMF_IndexFlag), intent(in), optional :: indexFlag @\
      integer, intent(out), optional :: rc   @\
@\
        ! Local variables @\
        type (ESMF_Array) :: array          ! array object @\
        integer :: status                   ! local error status @\
        logical :: rcpresent                ! did user specify rc? @\
        integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds @\
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
        ! Test to see if pointer already associated, and fail if so. @\
        if (associated(fptr)) then @\
          if (ESMF_LogMsgFoundError(ESMF_RC_OBJ_BAD, & @\
                              "Data Pointer cannot already be associated", & @\
                              ESMF_CONTEXT, rc)) return @\
        endif @\
 @\
        array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
 @\
        ! TODO: set array as data in field. @\
        field%ftypep%localfield%localdata = array @\
        field%ftypep%datastatus = ESMF_STATE_READY @\
 @\
        if (rcpresent) rc = status @\
 @\
        end subroutine ESMF_FieldSetDataPointer##mrank##D##mtypekind  @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

