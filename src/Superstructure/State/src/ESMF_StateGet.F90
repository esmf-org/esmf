! $Id: ESMF_StateGet.F90,v 1.2 2004/02/05 22:29:31 nscollins Exp $
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
! ESMF StateGet module
      module ESMF_StateGetMod
!
!==============================================================================
!
! This file contains the State class methods which are automatically
! generated from macros to handle the type/kind/rank overloading.
! See ESMF_State.F90 for non-macroized entry points.
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
      use ESMF_StateMod
      implicit none

!------------------------------------------------------------------------------
! !PRIVATE TYPES:
      private

!------------------------------------------------------------------------------
! !PUBLIC MEMBER FUNCTIONS:

      public ESMF_StateGetDataPointer

!EOP
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter, private :: version = &
      '$Id: ESMF_StateGet.F90,v 1.2 2004/02/05 22:29:31 nscollins Exp $'

!==============================================================================
!
! INTERFACE BLOCKS
!
!==============================================================================


!------------------------------------------------------------------------------

!BOP
! !IROUTINE: ESMF_StateGetDataPointer -- Get an F90 pointer to the data contents

! !INTERFACE:
     interface ESMF_StateGetDataPointer

! !PRIVATE MEMBER FUNCTIONS:
!
      ! < declarations of interfaces for each T/K/R >
!------------------------------------------------------------------------------ 
! <This section created by macro - do not edit directly> 
 module procedure ESMF_StateGetDataPointerI21D 
 module procedure ESMF_StateGetDataPointerI41D 
 module procedure ESMF_StateGetDataPointerI81D 
 module procedure ESMF_StateGetDataPointerI22D 
 module procedure ESMF_StateGetDataPointerI42D 
 module procedure ESMF_StateGetDataPointerI82D 
 module procedure ESMF_StateGetDataPointerI23D 
 module procedure ESMF_StateGetDataPointerI43D 
 module procedure ESMF_StateGetDataPointerI83D 
 module procedure ESMF_StateGetDataPointerI24D 
 module procedure ESMF_StateGetDataPointerI44D 
 module procedure ESMF_StateGetDataPointerI84D 
 module procedure ESMF_StateGetDataPointerI25D 
 module procedure ESMF_StateGetDataPointerI45D 
 module procedure ESMF_StateGetDataPointerI85D 
 module procedure ESMF_StateGetDataPointerR41D 
 module procedure ESMF_StateGetDataPointerR81D 
 module procedure ESMF_StateGetDataPointerR42D 
 module procedure ESMF_StateGetDataPointerR82D 
 module procedure ESMF_StateGetDataPointerR43D 
 module procedure ESMF_StateGetDataPointerR83D 
 module procedure ESMF_StateGetDataPointerR44D 
 module procedure ESMF_StateGetDataPointerR84D 
 module procedure ESMF_StateGetDataPointerR45D 
 module procedure ESMF_StateGetDataPointerR85D 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


! !DESCRIPTION:
! This interface provides a single entry point for the various
! types of {\tt ESMF\_StateGetDataPointer} subroutines.
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
! !IROUTINE: ESMF_StateGetDataPointerI21D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI21D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I2), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI21D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI41D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI41D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI81D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI81D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI22D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI22D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I2), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI22D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI42D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI42D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI82D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI82D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI23D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI23D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I2), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI23D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI43D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI43D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI83D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI83D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI24D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI24D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I2), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI24D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI44D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI44D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI84D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI84D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI25D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI25D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I2), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI25D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI45D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI45D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerI85D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerI85D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 integer (ESMF_KIND_I8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerI85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR41D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR41D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R4), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR41D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR81D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR81D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R8), dimension(:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR81D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR42D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR42D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R4), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR42D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR82D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR82D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR82D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR43D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR43D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R4), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR43D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR83D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR83D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R8), dimension(:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR83D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR44D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR44D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R4), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR44D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR84D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR84D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R8), dimension(:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR84D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR45D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR45D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R4), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR45D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 


!------------------------------------------------------------------------------ 
! <Created by macro - do not edit directly > 
!BOP 
! !IROUTINE: ESMF_StateGetDataPointerR85D - Retrieve F90 pointer directly from a State 
 
! !INTERFACE: 
 subroutine ESMF_StateGetDataPointerR85D(state, dataname, f90ptr, copyflag, statename, rc) 
! 
! !ARGUMENTS: 
 type(ESMF_State), intent(in) :: state 
 character(len=*), intent(in) :: dataname 
 real (ESMF_KIND_R8), dimension(:,:,:,:,:), pointer :: f90ptr 
 type(ESMF_CopyFlag), intent(in), optional :: copyflag 
 character(len=*), intent(in), optional :: statename 
 integer, intent(out), optional :: rc 
! 
! !DESCRIPTION: 
! Retrieves data from a state, returning a direct F90 pointer to the start 
! of the actual data array. 
! 
! The arguments are: 
! \begin{description} 
! \item[state] 
! The {\tt ESMF\_State} to query. 
! 
! \item[dataname] 
! The name of the Bundle, Field, or Array to return. 
! 
! \item[f90ptr] 
! An unassociated Fortrn 90 pointer of the proper Type, Kind, and Rank as the data 
! in the State. When this call returns successfully, the pointer will now reference 
! the data in the State. This is either a reference or a copy, depending on the 
! setting of the following argument. The default is to return a reference. 
! 
! \item[{[copyflag]}] 
! Defaults to {\tt ESMF\_DATA\_REF}. If set to {\tt ESMF\_DATA\_COPY}, a separate 
! copy of the data will be made and the pointer will point at the copy. 
! 
! \item[{[statename]}] 
! Optional. If multiple states are present, a specific state name must be given. 
! 
! \item[{[rc]}] 
! Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. 
! \end{description} 
! 
 
! 
!EOP 
! !REQUIREMENTS: 
 
 ! Local variables 
 type (ESMF_Bundle) :: bundle ! bundle object 
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
 
 ! TODO: make this check the data type, and switch based on that. 
 ! For now, assume field only. 
 call ESMF_StateGetField(state, dataname, field, statename, status) 
 if (status .ne. ESMF_SUCCESS) then 
 print *, "Error: StateGetField failed" 
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
 
 end subroutine ESMF_StateGetDataPointerR85D 
 
! < end macro - do not edit directly > 
!------------------------------------------------------------------------------ 



        end module ESMF_StateGetMod
