#pragma GCC set_debug_pwd "/Users/nancy/esmf/esmf_cvs/mod/modg/Darwin.absoft.32.default"
! $Id: ESMF_FieldSet.F90,v 1.1 2004/04/19 22:01:52 nscollins Exp $
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
! ESMF FieldSet module
      module ESMF_FieldSetMod
!
!==============================================================================
!
! This file contains the Field class methods which are automatically
! generated from macros to handle the type/kind/rank overloading.
! See ESMF_Field.F90 for non-macroized functions and subroutines.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
#include "ESMF.h"
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayMod
      use ESMF_ArrayCreateMod
      use ESMF_FieldMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_FieldSetDataPointer

!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldSet.F90,v 1.1 2004/04/19 22:01:52 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_FieldSetDataPointer -- Set a Fortran pointer to the data contents

! !INTERFACE:
     interface ESMF_FieldSetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_FieldSetDataPointer1DI1 
 module procedure ESMF_FieldSetDataPointer1DI2 
 module procedure ESMF_FieldSetDataPointer1DI4 
 module procedure ESMF_FieldSetDataPointer1DI8 
 module procedure ESMF_FieldSetDataPointer2DI1 
 module procedure ESMF_FieldSetDataPointer2DI2 
 module procedure ESMF_FieldSetDataPointer2DI4 
 module procedure ESMF_FieldSetDataPointer2DI8 
 module procedure ESMF_FieldSetDataPointer3DI1 
 module procedure ESMF_FieldSetDataPointer3DI2 
 module procedure ESMF_FieldSetDataPointer3DI4 
 module procedure ESMF_FieldSetDataPointer3DI8 
 module procedure ESMF_FieldSetDataPointer4DI1 
 module procedure ESMF_FieldSetDataPointer4DI2 
 module procedure ESMF_FieldSetDataPointer4DI4 
 module procedure ESMF_FieldSetDataPointer4DI8 
 module procedure ESMF_FieldSetDataPointer5DI1 
 module procedure ESMF_FieldSetDataPointer5DI2 
 module procedure ESMF_FieldSetDataPointer5DI4 
 module procedure ESMF_FieldSetDataPointer5DI8 
 module procedure ESMF_FieldSetDataPointer6DI1 
 module procedure ESMF_FieldSetDataPointer6DI2 
 module procedure ESMF_FieldSetDataPointer6DI4 
 module procedure ESMF_FieldSetDataPointer6DI8 
 module procedure ESMF_FieldSetDataPointer7DI1 
 module procedure ESMF_FieldSetDataPointer7DI2 
 module procedure ESMF_FieldSetDataPointer7DI4 
 module procedure ESMF_FieldSetDataPointer7DI8 
 module procedure ESMF_FieldSetDataPointer1DR4 
 module procedure ESMF_FieldSetDataPointer1DR8 
 module procedure ESMF_FieldSetDataPointer2DR4 
 module procedure ESMF_FieldSetDataPointer2DR8 
 module procedure ESMF_FieldSetDataPointer3DR4 
 module procedure ESMF_FieldSetDataPointer3DR8 
 module procedure ESMF_FieldSetDataPointer4DR4 
 module procedure ESMF_FieldSetDataPointer4DR8 
 module procedure ESMF_FieldSetDataPointer5DR4 
 module procedure ESMF_FieldSetDataPointer5DR8 
 module procedure ESMF_FieldSetDataPointer6DR4 
 module procedure ESMF_FieldSetDataPointer6DR8 
 module procedure ESMF_FieldSetDataPointer7DR4 
 module procedure ESMF_FieldSetDataPointer7DR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_FieldSetDataPointer} subroutines.
!
!EOP
end interface

!==============================================================================

      contains

!==============================================================================

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------

      ! < declarations of subroutines for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 
!! < start of macros which become actual subroutine bodies after expansion > 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldSetDataPointer - Add data to a field directly by Fortran pointer 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldSetDataPointer() 
! subroutine ESMF_FieldSetDataPointer<rank><type><kind>(field, fptr, copyFlag, indexFlag, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_Field), intent(inout) :: field 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr 
! integer, intent(in), optional :: haloWidth 
! type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
! type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
! integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Returns a direct Fortran pointer to the data in an {\tt ESMF\_Field}. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! \item[fptr] 
! An associated Fortran pointer of the proper Type, Kind, and Rank as 
! the data in the Field. When this call returns successfully, the pointer 
! will now point to the data in the Field. This is either a reference or 
! a copy, depending on the setting of the following argument. 
! \item[{[copyFlag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, 
! a separate copy of the data will be allocated and the pointer will point 
! at the copy. If a new copy of the data is made, the caller is 
! responsible for deallocating the space. 
! \item[{[haloWidth]}] 
! Defaults to 0. If specified, the halo width to add to all sides of the 
! data array. 
! \item[{[indexFlag]}] 
! Defaults to {\tt ESMF\_LOCAL\_INDEX}. If set to 
! {\tt ESMF\_GLOBAL\_INDEX} and the {\tt ESMF\_Grid} associated with the 
! {\tt ESMF\_Field} is regular, then the lower bounds and upper bounds will 
! be allocated with global index numbers corresponding to the grid. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer1DI1(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer1DI2(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer1DI4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer1DI8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer2DI1(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer2DI2(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer2DI4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer2DI8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer3DI1(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer3DI2(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer3DI4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer3DI8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer4DI1(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer4DI2(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer4DI4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer4DI8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer5DI1(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer5DI2(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer5DI4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer5DI8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer6DI1(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer6DI2(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer6DI4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer6DI8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer7DI1(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer7DI2(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer7DI4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer7DI8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer1DR4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer1DR8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer2DR4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer2DR8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer3DR4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer3DR8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer4DR4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer4DR8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer5DR4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer5DR8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer6DR4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer6DR8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer7DR4(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldSetDataPointer7DR8(field, fptr, copyFlag, haloWidth, indexFlag, rc) 
 
 type(ESMF_Field), intent(inout) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyFlag 
 integer, intent(in), optional :: haloWidth 
 type(ESMF_IndexFlag), intent(in), optional :: indexFlag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 integer, dimension(ESMF_MAXDIM) :: counts, lbounds, ubounds 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if pointer already associated, and fail if so. 
 if (associated(fptr)) then 
 print *, "Error: Data Pointer cannot already be associated" 
 return 
 endif 
 
 array = ESMF_ArrayCreate(fptr, counts, haloWidth, lbounds, ubounds, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayCreate failed" 
 return 
 endif 
 
 ! TODO: set array as data in field. 
 field%ftypep%localfield%localdata = array 
 field%ftypep%datastatus = ESMF_STATE_READY 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldSetDataPointer7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



        end module ESMF_FieldSetMod
