#if 0
! $Id: ESMF_StateMacros.h,v 1.5 2004/05/21 09:25:29 nscollins Exp $
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
! Macros for the State class.
!------------------------------------------------------------------------------
#endif

#include "ESMF_StdCppMacros.h"

#if 0
!------------------------------------------------------------------------------
! Documentation for the general StateGetDataPointer<> macros.
!------------------------------------------------------------------------------
#endif

#define StateGetDataPointerDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_StateGetDataPointer - Retrieve Fortran pointer directly from a State @\
! @\
! !INTERFACE: @\
!      subroutine ESMF_StateGetDataPointer<rank><type><kind>(state, dataname, fptr, copyflag, statename, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_State), intent(in) :: state @\
!      character(len=*), intent(in) :: dataname @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
!      character(len=*), intent(in), optional :: statename @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Retrieves data from a state, returning a direct Fortran pointer to @\
!  the data array.  @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[state] @\
!   The {\tt ESMF\_State} to query. @\
!  \item[dataname] @\
!   The name of the Bundle, Field, or Array to return data from. @\
!  \item[fptr] @\
!   An unassociated Fortran pointer of the proper Type, Kind, and Rank as the data @\
!   in the State.  When this call returns successfully, the pointer will now reference @\
!   the data in the State.  This is either a reference or a copy, depending on the @\
!   setting of the following argument.  The default is to return a reference. @\
!  \item[{[copyflag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, a separate @\
!   copy of the data will be made and the pointer will point at the copy. @\
!  \item[{[statename]}] @\
!   Optional.  If multiple states are present, a specific state name must be given. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\
 @\

#if 0
!------------------------------------------------------------------------------
! Get an F90 pointer back directly to the data.
!------------------------------------------------------------------------------
#endif

#define StateGetDataPointerMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
      subroutine ESMF_StateGetDataPointer##mrank##D##mtypekind(state, dataname, fptr, copyflag, statename, rc) @\
 @\
      type(ESMF_State), intent(in) :: state @\
      character(len=*), intent(in) :: dataname @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
      character(len=*), intent(in), optional :: statename @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        !!type (ESMF_Bundle) :: bundle        ! bundle object @\
        type (ESMF_Field) :: field          ! field object @\
        type (ESMF_Array) :: array          ! array object @\
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
        ! Test to see if array already associated, and fail if so. @\
        if (associated(fptr)) then @\
          print *, "Error: Data Pointer cannot already be associated" @\
          return @\
        endif @\
 @\
        ! TODO: make this check the data type, and switch based on that. @\
        ! For now, assume field only. @\
        call ESMF_StateGetField(state, dataname, field, statename, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Error: StateGetField failed" @\
          return @\
        endif @\
 @\
        call ESMF_FieldGetArray(field, array, rc=status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Error: FieldGetArray failed" @\
          return @\
        endif @\
 @\
        call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Error: ArrayGetData failed" @\
          return @\
        endif @\
 @\
        if (rcpresent) rc = status @\
 @\
        end subroutine ESMF_StateGetDataPointer##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\


