
    program test
    
    use ESMF_Mod
    implicit none

    integer :: rc, npets
    type(ESMF_VM):: vm

    ! Pointers to arrays of data type Integer * 4 
    type PtrIWrap1  
    sequence
      integer (ESMF_KIND_I4),dimension(:),pointer :: dummy
    end type 
    
    type PtrIWrap2  
    sequence
      integer (ESMF_KIND_I4),dimension(:,:),pointer :: dummy
    end type 
    
    type PtrIWrap3  
    sequence
      integer (ESMF_KIND_I4),dimension(:,:,:),pointer :: dummy
    end type 
    
    type PtrIWrap4  
    sequence
      integer (ESMF_KIND_I4),dimension(:,:,:,:),pointer :: dummy
    end type 
    
    type PtrIWrap5  
    sequence
      integer (ESMF_KIND_I4),dimension(:,:,:,:,:),pointer :: dummy
    end type 
    

#ifdef ESMF_EXHAUSTIVE
    ! Pointers to arrays of data type Real * 4 
    type PtrRWrap1  
    sequence
      real (ESMF_KIND_R4),dimension(:),pointer :: dummy
    end type 
    
    type PtrRWrap2  
    sequence
      real (ESMF_KIND_R4),dimension(:,:),pointer :: dummy
    end type 
    
    type PtrRWrap3  
    sequence
      real (ESMF_KIND_R4),dimension(:,:,:),pointer :: dummy
    end type 
    
    type PtrRWrap4  
    sequence
      real (ESMF_KIND_R4),dimension(:,:,:,:),pointer :: dummy
    end type 
    
    type PtrRWrap5  
    sequence
      real (ESMF_KIND_R4),dimension(:,:,:,:,:),pointer :: dummy
    end type 
    

    ! Pointers to arrays of data type Real * 8 
    type PtrR8Wrap1  
    sequence
      real (ESMF_KIND_R8),dimension(:),pointer :: dummy
    end type 
    
    type PtrR8Wrap2  
    sequence
      real (ESMF_KIND_R8),dimension(:,:),pointer :: dummy
    end type 
    
    type PtrR8Wrap3  
    sequence
      real (ESMF_KIND_R8),dimension(:,:,:),pointer :: dummy
    end type 
    
    type PtrR8Wrap4  
    sequence
      real (ESMF_KIND_R8),dimension(:,:,:,:),pointer :: dummy
    end type 
    
    type PtrR8Wrap5  
    sequence
      real (ESMF_KIND_R8),dimension(:,:,:,:,:),pointer :: dummy
    end type 
    

    ! Pointers to arrays of a derived data type
    type PtrSWrap1  
    sequence
      type(PtrIWrap1),dimension(:),pointer :: dummy
    end type 
    
    type PtrSWrap2  
    sequence
      type(PtrIWrap1),dimension(:,:),pointer :: dummy
    end type 
    
    type PtrSWrap3  
    sequence
      type(PtrIWrap1),dimension(:,:,:),pointer :: dummy
    end type 
    
    type PtrSWrap4  
    sequence
      type(PtrIWrap1),dimension(:,:,:,:),pointer :: dummy
    end type 
    
    type PtrSWrap5  
    sequence
      type(PtrIWrap1),dimension(:,:,:,:,:),pointer :: dummy
    end type 
#endif
    

    
    type(PtrIWrap1) :: sizetest1I(2)
    type(PtrIWrap2) :: sizetest2I(2)
    type(PtrIWrap3) :: sizetest3I(2)
    type(PtrIWrap4) :: sizetest4I(2)
    type(PtrIWrap5) :: sizetest5I(2)
    
#ifdef ESMF_EXHAUSTIVE
    type(PtrRWrap1) :: sizetest1R(2)
    type(PtrRWrap2) :: sizetest2R(2)
    type(PtrRWrap3) :: sizetest3R(2)
    type(PtrRWrap4) :: sizetest4R(2)
    type(PtrRWrap5) :: sizetest5R(2)
    
    type(PtrR8Wrap1) :: sizetest1R8(2)
    type(PtrR8Wrap2) :: sizetest2R8(2)
    type(PtrR8Wrap3) :: sizetest3R8(2)
    type(PtrR8Wrap4) :: sizetest4R8(2)
    type(PtrR8Wrap5) :: sizetest5R8(2)
    
    type(PtrSWrap1) :: sizetest1S(2)
    type(PtrSWrap2) :: sizetest2S(2)
    type(PtrSWrap3) :: sizetest3S(2)
    type(PtrSWrap4) :: sizetest4S(2)
    type(PtrSWrap5) :: sizetest5S(2)
#endif
    

    call ESMF_Initialize(vm=vm, rc=rc)
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    print '(/, a, i3)' , "NUMBER_OF_PROCESSORS", npets
    
    !NEX_UTest
    call c_ESMF_SizePrint(sizetest1I(1), sizetest1I(2), 1, rc)
    !NEX_UTest
    call c_ESMF_SizePrint(sizetest2I(1), sizetest2I(2), 2, rc)
    !NEX_UTest
    call c_ESMF_SizePrint(sizetest3I(1), sizetest3I(2), 3, rc)
    !NEX_UTest
    call c_ESMF_SizePrint(sizetest4I(1), sizetest4I(2), 4, rc)
    !NEX_UTest
    call c_ESMF_SizePrint(sizetest5I(1), sizetest5I(2), 5, rc)

#ifdef ESMF_EXHAUSTIVE
    !EX_UTest
    call c_ESMF_SizePrint(sizetest1R(1), sizetest1R(2), 1, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest2R(1), sizetest2R(2), 2, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest3R(1), sizetest3R(2), 3, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest4R(1), sizetest4R(2), 4, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest5R(1), sizetest5R(2), 5, rc)

    !EX_UTest
    call c_ESMF_SizePrint(sizetest1R8(1), sizetest1R8(2), 1, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest2R8(1), sizetest2R8(2), 2, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest3R8(1), sizetest3R8(2), 3, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest4R8(1), sizetest4R8(2), 4, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest5R8(1), sizetest5R8(2), 5, rc)

    !EX_UTest
    call c_ESMF_SizePrint(sizetest1S(1), sizetest1S(2), 1, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest2S(1), sizetest2S(2), 2, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest3S(1), sizetest3S(2), 3, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest4S(1), sizetest4S(2), 4, rc)
    !EX_UTest
    call c_ESMF_SizePrint(sizetest5S(1), sizetest5S(2), 5, rc)
#endif

    call ESMF_Finalize(rc=rc)

    end program test
    
    
