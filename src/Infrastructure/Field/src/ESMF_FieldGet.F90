#pragma GCC set_debug_pwd "/Users/nancy/esmf/esmf_cvs/src/Infrastructure/Field/src"
! $Id: ESMF_FieldGet.F90,v 1.1 2004/02/05 21:50:12 nscollins Exp $
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
! See ESMF_Field.F90 for non-macroized entry points.
! class methods.
!
!------------------------------------------------------------------------------
! INCLUDES
! < ignore blank lines below. they are created by the files which
! define various macros. >
!------------------------------------------------------------------------------
! !USES:
      use ESMF_BaseMod
      use ESMF_LocalArrayMod
      use ESMF_ArrayBaseMod
      use ESMF_ArrayExpandMod
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
      '$Id: ESMF_FieldGet.F90,v 1.1 2004/02/05 21:50:12 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_FieldGetDataPointer -- Get an F90 pointer to the data contents

! !INTERFACE:
     interface ESMF_FieldGetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_FieldGetDataPointerI21D 
 module procedure ESMF_FieldGetDataPointerI41D 
 module procedure ESMF_FieldGetDataPointerI81D 
 module procedure ESMF_FieldGetDataPointerI22D 
 module procedure ESMF_FieldGetDataPointerI42D 
 module procedure ESMF_FieldGetDataPointerI82D 
 module procedure ESMF_FieldGetDataPointerI23D 
 module procedure ESMF_FieldGetDataPointerI43D 
 module procedure ESMF_FieldGetDataPointerI83D 
 module procedure ESMF_FieldGetDataPointerI24D 
 module procedure ESMF_FieldGetDataPointerI44D 
 module procedure ESMF_FieldGetDataPointerI84D 
 module procedure ESMF_FieldGetDataPointerI25D 
 module procedure ESMF_FieldGetDataPointerI45D 
 module procedure ESMF_FieldGetDataPointerI85D 
 module procedure ESMF_FieldGetDataPointerR41D 
 module procedure ESMF_FieldGetDataPointerR81D 
 module procedure ESMF_FieldGetDataPointerR42D 
 module procedure ESMF_FieldGetDataPointerR82D 
 module procedure ESMF_FieldGetDataPointerR43D 
 module procedure ESMF_FieldGetDataPointerR83D 
 module procedure ESMF_FieldGetDataPointerR44D 
 module procedure ESMF_FieldGetDataPointerR84D 
 module procedure ESMF_FieldGetDataPointerR45D 
 module procedure ESMF_FieldGetDataPointerR85D 
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

!! < start of macros which become actual subroutine bodies after expansion >

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI21D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI21D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I2), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI41D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI41D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I4), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI81D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI81D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I8), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI22D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI22D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI42D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI42D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI82D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI82D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI23D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI23D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI43D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI43D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI83D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI83D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI24D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI24D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI44D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI44D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI84D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI84D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI25D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI25D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI45D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI45D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerI85D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerI85D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR41D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR41D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R4), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR81D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR81D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R8), dimension(:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR42D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR42D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR82D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR82D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR43D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR43D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR83D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR83D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR44D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR44D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR84D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR84D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR45D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR45D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_FieldGetDataPointerR85D - Retrieve F90 pointer directly from a Field 
 
! !INTERFACE: 
 subroutine ESMF_FieldGetDataPointerR85D(field, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Field), intent(in) :: field 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a field, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[field] 
! The {\tt ESMF\_Field} to query. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Field. When this call returns successfully, the pointer will now reference 
! the data in the Field. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Array) :: array ! array object 
 integer :: status ! local error status 
 logical :: rcpresent ! did user specify rc? 
 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: newp 
 
 ! Initialize return code; assume failure until success is certain 
 status = ESMF_FAILURE 
 rcpresent = .FALSE. 
 array%this = ESMF_NULL_POINTER 
 
 if (present(rc)) then 
 rcpresent = .TRUE. 
 rc = ESMF_FAILURE 
 endif 
 
 ! Test to see if array already allocated, and fail if so. 
 !if (allocated(f90arr)) then 
 ! print *, "Error: Data Pointer cannot already be allocated" 
 ! return 
 !endif 
 
 call ESMF_FieldGetData(field, array, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: FieldGetData failed" 
 return 
 endif 
 
 call ESMF_ArrayGetData(array, f90ptr, copyflag, rc=status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: ArrayGetData failed" 
 return 
 endif 
 
 if (rcpresent) rc = status 
 
 end subroutine ESMF_FieldGetDataPointerR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



        end module ESMF_FieldGetMod
