! $Id: ESMF_WordsizeUTest.F90,v 1.3 2005/10/12 19:06:22 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2005, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================

    program WordsizeTest
    
#include "ESMF.h"
#include "ESMC_Conf.h"

    use ESMF_Mod
    use ESMF_TestMod
    implicit none

    integer :: rc, result
    character(len=ESMF_MAXSTR) :: failMsg, name
    integer(ESMF_KIND_I8) :: diff, i1, i2
    integer :: i2size, i4size, i8size, r4size, r8size

    type testi 
    sequence
        integer :: fredi
    end type

    type testr 
    sequence
        real :: fredr
    end type

    type testp 
    sequence
        integer, pointer :: fredp
    end type

    type testi4 
    sequence
        integer(ESMF_KIND_I4) :: fredi4
    end type

    type testi8 
    sequence
        integer(ESMF_KIND_I8) :: fredi8
    end type

    type testr4 
    sequence
        integer(ESMF_KIND_R4) :: fredr4
    end type

    type testr8 
    sequence
        integer(ESMF_KIND_R8) :: fredr8
    end type

    type(testi)  :: vi(2)
    type(testr)  :: vr(2)
    type(testp)  :: vip(2)
    type(testi4) :: vi4(2)
    type(testi8) :: vi8(2)
    type(testr4) :: vr4(2)
    type(testr8) :: vr8(2)


!------------------------------------------------------------------------
! test of default variable wordsizes, selected_int_kind options, pointer
! sizes - both for single languages and interlanguage.

    result = 0

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    ! not a test - informational messages only.
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi(2), i2)
    diff = i2 - i1
    print *, "F90: Default Integer size = ", diff

    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr(2), i2)
    diff = i2 - i1
    print *, "F90: Default Real size = ", diff

    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vip(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vip(2), i2)
    diff = i2 - i1
    print *, "F90: Pointer size = ", diff

    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi4(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi4(2), i2)
    diff = i2 - i1
    print *, "F90: Explicit Integer I4 size = ", diff

    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi8(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi8(2), i2)
    diff = i2 - i1
    print *, "F90: Explicit Integer I8 size = ", diff

    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr4(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr4(2), i2)
    diff = i2 - i1
    print *, "F90: Explicit Integer R4 size = ", diff

    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr8(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr8(2), i2)
    diff = i2 - i1
    print *, "F90: Explicit Integer R8 size = ", diff

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for I4 variable"
    write(name, *) "Verifying I4 is 4 bytes"
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi4(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi4(2), i2)
    diff = i2 - i1
    call ESMF_Test((diff .eq. 4), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for R4 variable"
    write(name, *) "Verifying R4 is 4 bytes"
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr4(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr4(2), i2)
    diff = i2 - i1
    call ESMF_Test((diff .eq. 4), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for I8 variable"
    write(name, *) "Verifying I8 is 8 bytes"
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi8(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vi8(2), i2)
    diff = i2 - i1
    call ESMF_Test((diff .eq. 8), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for R8 variable"
    write(name, *) "Verifying R8 is 8 bytes"
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr8(1), i1)
    call ESMF_PointerToInt(ESMC_POINTER_SIZE, vr8(2), i2)
    diff = i2 - i1
    call ESMF_Test((diff .eq. 8), name, failMsg, result, ESMF_SRCLINE) 


    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !NEX_UTest
    ! get numbers from C for next set of tests
    call c_ints(i2size, i4size, i8size, r4size, r8size, rc)
    write(failMsg,*) "Failed getting int/float sizes"
    write(name, *) "Getting C int/float word sizes"
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for short variable"
    write(name, *) "Verifying short is 2 bytes"
    call ESMF_Test((i2size .eq. 2), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for int variable"
    write(name, *) "Verifying int is 4 bytes"
    call ESMF_Test((i4size .eq. 4), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for long long variable"
    write(name, *) "Verifying long long is 8 bytes"
    call ESMF_Test((i8size .eq. 8), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for float variable"
    write(name, *) "Verifying float is 4 bytes"
    call ESMF_Test((r4size .eq. 4), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Bad size for double variable"
    write(name, *) "Verifying double is 8 bytes"
    call ESMF_Test((r8size .eq. 8), name, failMsg, result, ESMF_SRCLINE) 



    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

#ifdef ESMF_EXHAUSTIVE

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

    ! no exhaustive tests (yet)

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------


#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program WordsizeTest
    
