#pragma GCC set_debug_pwd "/Users/nancy/esmf/esmf_cvs/mod/modg/Darwin.absoft.32.default"
! $Id: ESMF_FieldGet.F90,v 1.6 2004/04/19 22:01:50 nscollins Exp $
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
! ESMF FieldGet module
      module ESMF_FieldGetMod
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
      use ESMF_ArrayGetMod
      use ESMF_FieldMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_FieldGetDataPointer

!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_FieldGet.F90,v 1.6 2004/04/19 22:01:50 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_FieldGetDataPointer -- Get a Fortran pointer to the data contents

! !INTERFACE:
     interface ESMF_FieldGetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_FieldGetDataPointer1DI1 
 module procedure ESMF_FieldGetDataPointer1DI2 
 module procedure ESMF_FieldGetDataPointer1DI4 
 module procedure ESMF_FieldGetDataPointer1DI8 
 module procedure ESMF_FieldGetDataPointer2DI1 
 module procedure ESMF_FieldGetDataPointer2DI2 
 module procedure ESMF_FieldGetDataPointer2DI4 
 module procedure ESMF_FieldGetDataPointer2DI8 
 module procedure ESMF_FieldGetDataPointer3DI1 
 module procedure ESMF_FieldGetDataPointer3DI2 
 module procedure ESMF_FieldGetDataPointer3DI4 
 module procedure ESMF_FieldGetDataPointer3DI8 
 module procedure ESMF_FieldGetDataPointer4DI1 
 module procedure ESMF_FieldGetDataPointer4DI2 
 module procedure ESMF_FieldGetDataPointer4DI4 
 module procedure ESMF_FieldGetDataPointer4DI8 
 module procedure ESMF_FieldGetDataPointer5DI1 
 module procedure ESMF_FieldGetDataPointer5DI2 
 module procedure ESMF_FieldGetDataPointer5DI4 
 module procedure ESMF_FieldGetDataPointer5DI8 
 module procedure ESMF_FieldGetDataPointer6DI1 
 module procedure ESMF_FieldGetDataPointer6DI2 
 module procedure ESMF_FieldGetDataPointer6DI4 
 module procedure ESMF_FieldGetDataPointer6DI8 
 module procedure ESMF_FieldGetDataPointer7DI1 
 module procedure ESMF_FieldGetDataPointer7DI2 
 module procedure ESMF_FieldGetDataPointer7DI4 
 module procedure ESMF_FieldGetDataPointer7DI8 
 module procedure ESMF_FieldGetDataPointer1DR4 
 module procedure ESMF_FieldGetDataPointer1DR8 
 module procedure ESMF_FieldGetDataPointer2DR4 
 module procedure ESMF_FieldGetDataPointer2DR8 
 module procedure ESMF_FieldGetDataPointer3DR4 
 module procedure ESMF_FieldGetDataPointer3DR8 
 module procedure ESMF_FieldGetDataPointer4DR4 
 module procedure ESMF_FieldGetDataPointer4DR8 
 module procedure ESMF_FieldGetDataPointer5DR4 
 module procedure ESMF_FieldGetDataPointer5DR8 
 module procedure ESMF_FieldGetDataPointer6DR4 
 module procedure ESMF_FieldGetDataPointer6DR8 
 module procedure ESMF_FieldGetDataPointer7DR4 
 module procedure ESMF_FieldGetDataPointer7DR8 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_FieldGetDataPointer} subroutines.
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
! !IROUTINE: ESMF_FieldGetDataPointer - Retrieve Fortran pointer directly from a Field 
! 
! !INTERFACE: 
! ! Private name; call using ESMF_FieldGetDataPointer() 
! subroutine ESMF_FieldGetDataPointer<rank><type><kind>(field, fptr, copyflag, rc) 
! 
! !ARGUMENTS: 
! type(ESMF_Field), intent(in) :: field 
! <type> (ESMF_KIND_<kind>), dimension(<rank>), pointer :: fptr 
! type(ESMF_CopyFlag), intent(in), optional :: copyflag 
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
! An unassociated Fortran pointer of the proper Type, Kind, and Rank as 
! the data in the Field. When this call returns successfully, the pointer 
! will now point to the data in the Field. This is either a reference or 
! a copy, depending on the setting of the following argument. 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, 
! a separate copy of the data will be allocated and the pointer will point 
! at the copy. If a new copy of the data is made, the caller is 
! responsible for deallocating the space. 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
!EOP 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer1DI1(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I1), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer1DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer1DI2(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer1DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer1DI4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer1DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer1DI8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer1DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer2DI1(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I1), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer2DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer2DI2(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer2DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer2DI4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer2DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer2DI8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer2DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer3DI1(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer3DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer3DI2(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer3DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer3DI4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer3DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer3DI8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer3DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer4DI1(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer4DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer4DI2(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer4DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer4DI4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer4DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer4DI8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer4DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer5DI1(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer5DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer5DI2(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer5DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer5DI4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer5DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer5DI8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer5DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer6DI1(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer6DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer6DI2(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer6DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer6DI4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer6DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer6DI8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer6DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer7DI1(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I1), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer7DI1 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer7DI2(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer7DI2 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer7DI4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer7DI4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer7DI8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer7DI8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer1DR4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer1DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer1DR8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer1DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer2DR4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer2DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer2DR8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer2DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer3DR4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer3DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer3DR8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer3DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer4DR4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer4DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer4DR8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer4DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer5DR4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer5DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer5DR8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer5DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer6DR4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer6DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer6DR8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer6DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer7DR4(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer7DR4 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
 subroutine ESMF_FieldGetDataPointer7DR8(field, fptr, copyflag, rc) 
 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:,:,:), pointer :: fptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 

 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
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
 
 call ESMF_FieldGetArray(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, fptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointer7DR8 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 
 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



        end module ESMF_FieldGetMod
