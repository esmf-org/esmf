
    program test
    
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_LocalArrayMod
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
    

    type PtrSWrap1  
    sequence
      type(PtrWrap1),dimension(:),pointer :: dummy
    end type 
    
    type PtrSWrap2  
    sequence
      type(PtrWrap1),dimension(:,:),pointer :: dummy
    end type 
    
    type PtrSWrap3  
    sequence
      type(PtrWrap1),dimension(:,:,:),pointer :: dummy
    end type 
    
    type PtrSWrap4  
    sequence
      type(PtrWrap1),dimension(:,:,:,:),pointer :: dummy
    end type 
    
    type PtrSWrap5  
    sequence
      type(PtrWrap1),dimension(:,:,:,:,:),pointer :: dummy
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
    
    type(PtrSWrap1) :: sizetest1S(2)
    type(PtrSWrap2) :: sizetest2S(2)
    type(PtrSWrap3) :: sizetest3S(2)
    type(PtrSWrap4) :: sizetest4S(2)
    type(PtrSWrap5) :: sizetest5S(2)
    

    call ESMF_FrameworkInitialize()

    call c_ESMF_SizePrint(sizetest1(1), sizetest1(2), 1)
    call c_ESMF_SizePrint(sizetest2(1), sizetest2(2), 2)
    call c_ESMF_SizePrint(sizetest3(1), sizetest3(2), 3)
    call c_ESMF_SizePrint(sizetest4(1), sizetest4(2), 4)
    call c_ESMF_SizePrint(sizetest5(1), sizetest5(2), 5)

    call c_ESMF_SizePrint(sizetest1R(1), sizetest1R(2), 1)
    call c_ESMF_SizePrint(sizetest2R(1), sizetest2R(2), 2)
    call c_ESMF_SizePrint(sizetest3R(1), sizetest3R(2), 3)
    call c_ESMF_SizePrint(sizetest4R(1), sizetest4R(2), 4)
    call c_ESMF_SizePrint(sizetest5R(1), sizetest5R(2), 5)

    call c_ESMF_SizePrint(sizetest1S(1), sizetest1S(2), 1)
    call c_ESMF_SizePrint(sizetest2S(1), sizetest2S(2), 2)
    call c_ESMF_SizePrint(sizetest3S(1), sizetest3S(2), 3)
    call c_ESMF_SizePrint(sizetest4S(1), sizetest4S(2), 4)
    call c_ESMF_SizePrint(sizetest5S(1), sizetest5S(2), 5)

    call ESMF_FrameworkFinalize()

    end program test
    
    
