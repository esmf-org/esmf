! $Id: ESMF_BundleGet.F90,v 1.3 2004/03/03 18:09:50 jwolfe Exp $
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
! ESMF BundleGet module
      module ESMF_BundleGetMod
!
!==============================================================================
!
! This file contains the Bundle class methods which are automatically
! generated from macros to handle the type/kind/rank overloading.
! See ESMF_Bundle.F90 for non-macroized entry points.
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
      use ESMF_BundleMod
      implicit none
!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private
!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:
      public ESMF_BundleGetDataPointer
!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_BundleGet.F90,v 1.3 2004/03/03 18:09:50 jwolfe Exp $'
!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================
!------------------------------------------------------------------------------
!BOP
! !IROUTINE: ESMF_BundleGetDataPointer -- Get an F90 pointer to the data contents
! !INTERFACE:
     interface ESMF_BundleGetDataPointer
! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_BundleGetDataPointerI21D 
 module procedure ESMF_BundleGetDataPointerI41D 
 module procedure ESMF_BundleGetDataPointerI81D 
 module procedure ESMF_BundleGetDataPointerI22D 
 module procedure ESMF_BundleGetDataPointerI42D 
 module procedure ESMF_BundleGetDataPointerI82D 
 module procedure ESMF_BundleGetDataPointerI23D 
 module procedure ESMF_BundleGetDataPointerI43D 
 module procedure ESMF_BundleGetDataPointerI83D 
 module procedure ESMF_BundleGetDataPointerI24D 
 module procedure ESMF_BundleGetDataPointerI44D 
 module procedure ESMF_BundleGetDataPointerI84D 
 module procedure ESMF_BundleGetDataPointerI25D 
 module procedure ESMF_BundleGetDataPointerI45D 
 module procedure ESMF_BundleGetDataPointerI85D 
 module procedure ESMF_BundleGetDataPointerR41D 
 module procedure ESMF_BundleGetDataPointerR81D 
 module procedure ESMF_BundleGetDataPointerR42D 
 module procedure ESMF_BundleGetDataPointerR82D 
 module procedure ESMF_BundleGetDataPointerR43D 
 module procedure ESMF_BundleGetDataPointerR83D 
 module procedure ESMF_BundleGetDataPointerR44D 
 module procedure ESMF_BundleGetDataPointerR84D 
 module procedure ESMF_BundleGetDataPointerR45D 
 module procedure ESMF_BundleGetDataPointerR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_BundleGetDataPointer} subroutines.
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
! !IROUTINE: ESMF_BundleGetDataPointerI21D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI21D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI41D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI41D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI81D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI81D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI22D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI22D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI42D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI42D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI82D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI82D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI23D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI23D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI43D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI43D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI83D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI83D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI24D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI24D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI44D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI44D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI84D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI84D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI25D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI25D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI45D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI45D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerI85D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerI85D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR41D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR41D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR81D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR81D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR42D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR42D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR82D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR82D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR43D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR43D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR83D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR83D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR44D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR44D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR84D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR84D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR45D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR45D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_BundleGetDataPointerR85D - Retrieve F90 pointer directly from a Bundle 
 
! !INTERFACE: 
 subroutine ESMF_BundleGetDataPointerR85D(bundle, fieldname, f90ptr, copyflag, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_Bundle), intent(in) :: bundle 
 character(len=*), intent(in) :: fieldname 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a bundle, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[bundle] 
! The {\tt ESMF\_Bundle} to query. 
! 
! \item[fieldname] 
! The name of the {\tt ESMF\_Field} inside the {\tt ESMF\_Bundle} 
! to return. The {\tt ESMF\_Bundle} cannot have packed data. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the Bundle. When this call returns successfully, the pointer will now reference 
! the data in the Bundle. This is either a reference or a copy, depending on the 
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
 type (ESMF_Field) :: field ! field object 
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
 
 call ESMF_BundleGetField(bundle, fieldname, field, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: BundleGetField failed" 
 return 
 endif 
 
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
 
 end subroutine ESMF_BundleGetDataPointerR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 

        end module ESMF_BundleGetMod
