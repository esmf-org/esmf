#if 0
! $Id: ESMF_BundleMacros.h,v 1.7 2004/04/14 20:43:31 nscollins Exp $
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
! Macros for the Bundle class.
!------------------------------------------------------------------------------
#endif

#include "ESMF_StdCppMacros.h"

#if 0
!------------------------------------------------------------------------------
! Documentation for the general BundleGetDataPointer macro
!------------------------------------------------------------------------------
#endif

#define BundleGetDataPointerDoc() \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_BundleGetDataPointer - Retrieve Fortran pointer directly from a Bundle @\
! @\
! !INTERFACE: @\
!     ! Private name; call using ESMF_BundleGetDataPointer() @\
!      subroutine ESMF_BundleGetDataPointer<rank><type><kind>(bundle, fieldname, fptr, copyflag, rc) @\
! @\
! !ARGUMENTS: @\
!      type(ESMF_Bundle), intent(in) :: bundle @\
!      character(len=*), intent(in) :: fieldname @\
!      <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr @\
!      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
!      integer, intent(out), optional :: rc   @\
! @\
! !DESCRIPTION: @\
! Retrieves data from a bundle, returning a direct Fortran pointer to @\
!   the data.  @\
! @\
! The arguments are: @\
!  \begin{description} @\
!  \item[bundle] @\
!   The {\tt ESMF\_Bundle} to query. @\
!  \item[fieldname] @\
!   The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} @\
!   to return.  The {\tt ESMF\_Bundle} cannot have packed data. @\
!  \item[fptr] @\
!   An unassociated Fortran pointer of the proper Type, Kind, and Rank as the data @\
!   in the Bundle.  When this call returns successfully, the pointer will now point to @\
!   the data in the Bundle.  This is either a reference or a copy, depending on the @\
!   setting of the following argument.  The default is to return a reference. @\
!  \item[{[copyflag]}] @\
!   Defaults to {\tt ESMF\_DATA\_REF}.  If set to {\tt ESMF\_DATA\_COPY}, a separate @\
!   copy of the data will be made and the pointer will point at the copy. @\
!  \item[{[rc]}] @\
!    Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!  \end{description} @\
! @\
!EOP @\
 @\

#if 0
!------------------------------------------------------------------------------
! Get a Fortran pointer back to Bundle Data macro
!------------------------------------------------------------------------------
#endif

#define BundleGetDataPointerMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
      subroutine ESMF_BundleGetDataPointer##mrank##D##mtypekind(bundle, fieldname, fptr, copyflag, rc) @\
 @\
      type(ESMF_Bundle), intent(in) :: bundle @\
      character(len=*), intent(in) :: fieldname @\
      mname (ESMF_KIND_##mtypekind), dimension(mdim), pointer :: fptr @\
      type(ESMF_CopyFlag), intent(in), optional :: copyflag @\
      integer, intent(out), optional :: rc   @\
 @\
        ! Local variables @\
        type (ESMF_Field) :: field          ! field object @\
        type (ESMF_Array) :: array          ! array object @\
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
        if (associated(fptr)) then @\
          print *, "Error: Data Pointer cannot already be associated" @\
          return @\
        endif @\
 @\
        call ESMF_BundleGetField(bundle, fieldname, field, status) @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Error: BundleGetField failed" @\
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
        end subroutine ESMF_BundleGetDataPointer##mrank##D##mtypekind   @\
 @\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @\

