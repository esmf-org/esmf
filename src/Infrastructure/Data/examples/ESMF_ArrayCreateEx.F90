! $Id: ESMF_ArrayCreateEx.F90,v 1.4 2002/12/16 22:47:26 nscollins Exp $
!
! Example/test code which creates a new field.

!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------

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

!   ! Local variables
    integer :: nx, ny, arank, rc       
    type(ESMF_ArraySpec) :: arrayspec
    type(ESMF_Array) :: array1, array2, array3
    real (selected_real_kind(6,45)), dimension(:,:), pointer :: realptr

    
!-------------------------------------------------------------------------------
!   ! Example 1:
!   !  Create based on an existing F90 pointer.  The pointer is unassociated,
!   !   so the sizes must be specified.  This routine allocates space for the
!   !   array, but leaves the data uninitialized.
 
    nx = 10
    ny = 20    
    array1 = ESMF_ArrayCreate(realptr, nx, ny, rc)
    print *, "array 1 create returned"

    call ESMF_ArrayPrint(array1, "foo", rc)
    print *, "array 1 print returned"

    call ESMF_ArrayDestroy(array1)
    print *, "array 1 destroy returned"


!-------------------------------------------------------------------------------
!   ! Example 2:
!   !  Create based on an array specification.

    arank = 2
!   !   arrayspec = ESMF_ArraySpecCreate(arank, ESMF_DATA_REAL, ESMF_KIND_4, &
    !                                         nx, ny, rc)

!   !   array2 = ESMF_ArrayCreate(arrayspec, ESMF_NO_DATA, rc)

     end program ESMF_ArrayCreateEx
    
!\end{verbatim}
    
