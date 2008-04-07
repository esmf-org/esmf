
    program test
    
#include "ESMF.h"

#define _FROM_FORTRAN 1
#include "ESMC_Conf.h"

    use ESMF_Mod
    use ESMF_TestMod
    implicit none

    ! This test verifies that the actual size of the Fortran 90 "dope vector"
    ! (private pointer information which stores the rank, indicies, data type,
    ! etc for any Fortran array) matches the #define values in ESMC_Conf.h
    !
    ! These tests compute (in C) on the fly the difference between the 
    ! addresses of a(1) and a(2) where a is an array of Fortran pointers.
    ! This gives the real number of bytes that a "dope vector" takes.
    ! On all the compilers we have seen there is a base size for rank 1
    ! arrays, and then a fixed number of additional bytes for each additional
    ! rank.  (presumably the extra space is where it stores the lower and 
    ! upper index bounds for that rank).
    !
    ! This test compares the run-time computed value with the compiled-in
    ! fixed numbers.   The variables used below in the tests are:
    !
    ! 1. compiled_size - the #define hardcoded value for this dimension array.
    ! 2. computed_size - computed on-the-fly and the correct, definitive value.
    !
    ! if they do not match, the compiled-in size must change in 
    ! $ESMF_DIR/build_config/platform_and_compiler_dep_name/ESMC_Conf.h.   
    !
    ! (this size is not computed at run-time because in some places it has
    ! to be known at compile time to declare fixed size buffers.)

    ! TODO: this test should be part of a preamble or prologue dir which runs
    ! first because nothing else is going to work reliably if the pointer 
    ! sizes are off.

    integer :: compiled_size, computed_size
    integer :: rc, result
    character(len=ESMF_MAXSTR) :: failMsg, name

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

    type PtrIWrap6  
    sequence
      integer (ESMF_KIND_I4),dimension(:,:,:,:,:,:),pointer :: dummy
    end type 

    type PtrIWrap7  
    sequence
      integer (ESMF_KIND_I4),dimension(:,:,:,:,:,:,:),pointer :: dummy
    end type 
    


#ifdef ESMF_TESTEXHAUSTIVE
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
    
    type PtrRWrap6  
    sequence
      real (ESMF_KIND_R4),dimension(:,:,:,:,:,:),pointer :: dummy
    end type 
    
    type PtrRWrap7  
    sequence
      real (ESMF_KIND_R4),dimension(:,:,:,:,:,:,:),pointer :: dummy
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
    
    type PtrR8Wrap6  
    sequence
      real (ESMF_KIND_R8),dimension(:,:,:,:,:,:),pointer :: dummy
    end type 
    
    type PtrR8Wrap7  
    sequence
      real (ESMF_KIND_R8),dimension(:,:,:,:,:,:,:),pointer :: dummy
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

    type PtrSWrap6  
    sequence
      type(PtrIWrap1),dimension(:,:,:,:,:,:),pointer :: dummy
    end type 
    
    type PtrSWrap7  
    sequence
      type(PtrIWrap1),dimension(:,:,:,:,:,:,:),pointer :: dummy
    end type 

#endif
    

    
    type(PtrIWrap1) :: sizetest1I(2)
    type(PtrIWrap2) :: sizetest2I(2)
    type(PtrIWrap3) :: sizetest3I(2)
    type(PtrIWrap4) :: sizetest4I(2)
    type(PtrIWrap5) :: sizetest5I(2)
    type(PtrIWrap6) :: sizetest6I(2)
    type(PtrIWrap7) :: sizetest7I(2)
    
#ifdef ESMF_TESTEXHAUSTIVE
    type(PtrRWrap1) :: sizetest1R(2)
    type(PtrRWrap2) :: sizetest2R(2)
    type(PtrRWrap3) :: sizetest3R(2)
    type(PtrRWrap4) :: sizetest4R(2)
    type(PtrRWrap5) :: sizetest5R(2)
    type(PtrRWrap6) :: sizetest6R(2)
    type(PtrRWrap7) :: sizetest7R(2)
    
    type(PtrR8Wrap1) :: sizetest1R8(2)
    type(PtrR8Wrap2) :: sizetest2R8(2)
    type(PtrR8Wrap3) :: sizetest3R8(2)
    type(PtrR8Wrap4) :: sizetest4R8(2)
    type(PtrR8Wrap5) :: sizetest5R8(2)
    type(PtrR8Wrap6) :: sizetest6R8(2)
    type(PtrR8Wrap7) :: sizetest7R8(2)
    
    type(PtrSWrap1) :: sizetest1S(2)
    type(PtrSWrap2) :: sizetest2S(2)
    type(PtrSWrap3) :: sizetest3S(2)
    type(PtrSWrap4) :: sizetest4S(2)
    type(PtrSWrap5) :: sizetest5S(2)
    type(PtrSWrap6) :: sizetest6S(2)
    type(PtrSWrap7) :: sizetest7S(2)
#endif
    
    result = 0

!------------------------------------------------------------------------
!   ! test that the compiled-in size is the same as the run-time computed
!   ! size of an F90 pointer.   this runs the same tests for various ranks
!   ! and pointer types.  so far the results have always been the same
!   ! for any data type - the size differs per rank only.  but test it all
!   ! anyway just to be sure.


    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !NEX_UTest
    compiled_size = ESMF_F90_PTR_BASE_SIZE
    call c_ESMF_F90PtrSizeGet(sizetest1I(1), sizetest1I(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "1D Integer array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest1I(1), sizetest1I(2), 1, rc)

    !------------------------------------------------------------------------
    !NEX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest2I(1), sizetest2I(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "2D Integer array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest2I(1), sizetest2I(2), 2, rc)

    !------------------------------------------------------------------------
    !NEX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest3I(1), sizetest3I(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "3D Integer array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest3I(1), sizetest3I(2), 3, rc)

    !------------------------------------------------------------------------
    !NEX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest4I(1), sizetest4I(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "4D Integer array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest4I(1), sizetest4I(2), 4, rc)

    !------------------------------------------------------------------------
    !NEX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest5I(1), sizetest5I(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "5D Integer array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest5I(1), sizetest5I(2), 5, rc)

    !------------------------------------------------------------------------
    !NEX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest6I(1), sizetest6I(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "6D Integer array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest6I(1), sizetest6I(2), 6, rc)

    !------------------------------------------------------------------------
    !NEX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest7I(1), sizetest7I(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "7D Integer array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest7I(1), sizetest7I(2), 7, rc)
    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = ESMF_F90_PTR_BASE_SIZE
    call c_ESMF_F90PtrSizeGet(sizetest1R(1), sizetest1R(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "1D Real*4 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest1R(1), sizetest1R(2), 1, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest2R(1), sizetest2R(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "2D Real*4 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest2R(1), sizetest2R(2), 2, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest3R(1), sizetest3R(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "3D Real*4 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest3R(1), sizetest3R(2), 3, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest4R(1), sizetest4R(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "4D Real*4 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest4R(1), sizetest4R(2), 4, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest5R(1), sizetest5R(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "5D Real*4 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest5R(1), sizetest5R(2), 5, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest6R(1), sizetest6R(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "6D Real*4 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest6R(1), sizetest6R(2), 6, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest7R(1), sizetest7R(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "7D Real*4 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest7R(1), sizetest7R(2), 7, rc)
    !------------------------------------------------------------------------
    !------------------------------------------------------------------------


    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = ESMF_F90_PTR_BASE_SIZE
    call c_ESMF_F90PtrSizeGet(sizetest1R8(1), sizetest1R8(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "1D Real*8 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest1R8(1), sizetest1R8(2), 1, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest2R8(1), sizetest2R8(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "2D Real*8 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest2R8(1), sizetest2R8(2), 2, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest3R8(1), sizetest3R8(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "3D Real*8 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest3R8(1), sizetest3R8(2), 3, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest4R8(1), sizetest4R8(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "4D Real*8 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest4R8(1), sizetest4R8(2), 4, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest5R8(1), sizetest5R8(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "5D Real*8 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest5R8(1), sizetest5R8(2), 5, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest6R8(1), sizetest6R8(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "6D Real*8 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest6R8(1), sizetest6R8(2), 6, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest7R8(1), sizetest7R8(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "7D Real*8 array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest7R8(1), sizetest7R8(2), 7, rc)
    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = ESMF_F90_PTR_BASE_SIZE
    call c_ESMF_F90PtrSizeGet(sizetest1S(1), sizetest1S(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "1D Derived type array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest1S(1), sizetest1S(2), 1, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest2S(1), sizetest2S(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "2D Derived type array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest2S(1), sizetest2S(2), 2, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest3S(1), sizetest3S(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "3D Derived type array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest3S(1), sizetest3S(2), 3, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest4S(1), sizetest4S(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "4D Derived type array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest4S(1), sizetest4S(2), 4, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest5S(1), sizetest5S(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "5D Derived type array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest5S(1), sizetest5S(2), 5, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest6S(1), sizetest6S(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "6D Derived type array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest6S(1), sizetest6S(2), 6, rc)

    !------------------------------------------------------------------------
    !EX_UTest
    compiled_size = compiled_size + ESMF_F90_PTR_PLUS_RANK
    call c_ESMF_F90PtrSizeGet(sizetest7S(1), sizetest7S(2), computed_size, rc)
    write(failMsg,*) "Compile-time size of ", compiled_size, &
                     "not equal Run-time size of ", computed_size
    write(name, *) "7D Derived type array pointer size test"
    call ESMF_Test((computed_size .eq. compiled_size), name, failMsg, result, ESMF_SRCLINE) 
    ! this writes a more detailed message on standard output.
    call c_ESMF_F90PtrSizePrint(sizetest7S(1), sizetest7S(2), 7, rc)
    !------------------------------------------------------------------------
    !------------------------------------------------------------------------


#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program test
    
    
