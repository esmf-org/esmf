
    program test
    
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_LocalArrayMod
    use ESMF_ArrayBaseMod
    use ESMF_ArrayExpandMod
    implicit none

    integer :: i, j, nx, ny, items, rc
    type(ESMF_Array) :: array1
    type(ESMF_CopyFlag) :: docopy = ESMF_DATA_REF
    integer (ESMF_IKIND_I4),dimension(:,:),pointer :: intptr, intptr2

    nx = 3
    ny = 5 
    items = nx * ny 
    allocate(intptr(nx,ny), stat=rc)

    do i=1,nx
     do j=1,ny
      intptr(i,j) = i + (j-1)*nx
     enddo
    enddo

    !intptr = reshape( (/ (i, i = 1, items) /), (/ nx, ny /) )
 
    print *, "initial data is", intptr
    array1 = ESMF_ArrayCreate(intptr, rc=rc)
    !array1 = ESMF_ArrayCreate(intptr, docopy, rc)
    print *, "array 1 create returned"
 
    call ESMF_ArrayPrint(array1, "opts", rc)
    print *, "array print returns"

    call ESMF_ArrayGetData(array1, intptr2, docopy, rc)
    print *, "array 1 ptr returned"
    print *, "get data returns", intptr2

    call ESMF_ArrayDestroy(array1)
    print *, "array 1 destroy returned"


    end program test
    
    
