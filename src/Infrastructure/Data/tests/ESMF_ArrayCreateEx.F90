! $Id: ESMF_ArrayCreateEx.F90,v 1.1 2002/12/13 16:18:14 nscollins Exp $
!
! Example/test code which creates a new field.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! See the following code fragments for examples of how to create new Arrays.  
!  Also see the Programming Model section of this document.
!
!
!\begin{verbatim}

!   ! Example program showing various ways to create a Array object
    program ESMF_ArrayCreateEx
    
!   ! Other ESMF modules which are needed by Arrays
    use ESMF_IOMod
    use ESMF_ArrayMod
    implicit none

!   ! Dummy type to allow arrays of f90 pointers, to check the actual 
!   ! pointer size on various architectures by looking at the address
!   ! of fred(1) vs fred(2).  (you can't have direct arrays of pointers
!   ! in F90, but you can hide them inside a derived type and then
!   ! have arrays of derived types.)
    type fred
    sequence
       real (selected_real_kind(6,45)), dimension(:,:), pointer :: barney
    end type
 
!   ! Local variables
    integer :: nx, ny, rc       
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2, array3
    character (len = 20) :: fieldname
    real (selected_real_kind(6,45)), dimension(:,:), pointer :: realptr

    type(fred), pointer :: ptr1, ptr2
    type(fred), target :: fredlist(10)
  
    ptr1 => fredlist(1)
    ptr2 => fredlist(2)
    
    
!   ! Example 1:
!   !
 
    nx = 10
    ny = 20    
    array1 = ESMF_ArrayCreate(realptr, nx, ny, rc)
    print *, "array 1 create returned"

    call ESMF_ArrayPrint(array1, "foo", rc)
    print *, "array 1 print returned"


    call ESMF_ArrayDestroy(array1)
    print *, "array 1 destroy returned"


!   ! Example 2:
!   !

!   !   arrayspec = ESMF_ArraySpecCreate()

     end program ESMF_ArrayCreateEx
    
!\end{verbatim}
    
