
    program test
    
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_ArrayMod
    implicit none

    type PtrWrap  
    sequence
      integer (KIND=2),dimension(:,:),pointer :: dummy
    end type PtrWrap 
    
    type AllocWrap  
    sequence
      integer (KIND=4),dimension(:,:),pointer :: dummy
    end type AllocWrap 
    
    integer :: i, j, nx, ny, items, arank, rc, status
    type(ESMF_Array) :: array1, array2, array3
    type(ESMF_CopyFlag) :: docopy = ESMF_NO_COPY
    real (ESMF_IKIND_R8),dimension(:,:),pointer :: realptr, realptr2
    integer (ESMF_IKIND_I4),dimension(:,:),pointer :: intptr, intptr2

    type(PtrWrap) :: sizetest(2)
    
    call c_ESMF_SizePrint(sizetest(1), sizetest(2))

    nx = 3
    ny = 5 
    items = nx * ny 
    allocate(intptr(nx,ny), stat=status)

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
    
    
