
    program test
    
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_ArrayMod
    implicit none

    type PtrWrap1  
    sequence
      integer (KIND=4),dimension(:),pointer :: dummy
    end type 
    
    type PtrWrap2  
    sequence
      integer (KIND=4),dimension(:,:),pointer :: dummy
    end type 
    
    type PtrWrap3  
    sequence
      integer (KIND=4),dimension(:,:,:),pointer :: dummy
    end type 
    
    type PtrWrap4  
    sequence
      integer (KIND=4),dimension(:,:,:,:),pointer :: dummy
    end type 
    
    type PtrWrap5  
    sequence
      integer (KIND=4),dimension(:,:,:,:,:),pointer :: dummy
    end type 
    
    type PtrRWrap1  
    sequence
      real (KIND=4),dimension(:),pointer :: dummy
    end type 
    
    type PtrRWrap2  
    sequence
      real (KIND=4),dimension(:,:),pointer :: dummy
    end type 
    
    type PtrRWrap3  
    sequence
      real (KIND=4),dimension(:,:,:),pointer :: dummy
    end type 
    
    type PtrRWrap4  
    sequence
      real (KIND=4),dimension(:,:,:,:),pointer :: dummy
    end type 
    
    type PtrRWrap5  
    sequence
      real (KIND=4),dimension(:,:,:,:,:),pointer :: dummy
    end type 
    

    
    type(PtrWrap1) :: sizetest1(2)
    type(PtrWrap2) :: sizetest2(2)
    type(PtrWrap3) :: sizetest3(2)
    type(PtrWrap4) :: sizetest4(2)
    type(PtrWrap5) :: sizetest5(2)
    
    type(PtrRWrap1) :: sizetest1R(2)
    type(PtrRWrap2) :: sizetest2R(2)
    type(PtrRWrap3) :: sizetest3R(2)
    type(PtrRWrap4) :: sizetest4R(2)
    type(PtrRWrap5) :: sizetest5R(2)
    


    call c_ESMF_SizePrint(sizetest1(1), sizetest1(2), 1)
    call c_ESMF_SizePrint(sizetest2(1), sizetest2(2), 2)
    call c_ESMF_SizePrint(sizetest3(1), sizetest3(2), 3)
    call c_ESMF_SizePrint(sizetest4(1), sizetest4(2), 4)
    call c_ESMF_SizePrint(sizetest5(1), sizetest5(2), 5)

    call c_ESMF_SizePrint(sizetest1(1), sizetest1(2), 1)
    call c_ESMF_SizePrint(sizetest2(1), sizetest2(2), 2)
    call c_ESMF_SizePrint(sizetest3(1), sizetest3(2), 3)
    call c_ESMF_SizePrint(sizetest4(1), sizetest4(2), 4)
    call c_ESMF_SizePrint(sizetest5(1), sizetest5(2), 5)


    end program test
    
    
