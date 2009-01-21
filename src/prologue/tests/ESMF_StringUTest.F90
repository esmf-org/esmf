! $Id: ESMF_StringUTest.F90,v 1.5.2.3 2009/01/21 21:25:25 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================

    program StringTest
    
#include "ESMF.h"

    use ESMF_Mod
    use ESMF_TestMod
    implicit none

    integer :: rc, result
    character(len=ESMF_MAXSTR) :: failMsg, name

    character(len=120) :: fstr
    character(len=60) :: fstr2
    integer :: i1, i2, i3, i4
    external f90ints, f90string2, f90string3


!------------------------------------------------------------------------
! test of passing ints and strings between F90 and C++

    result = 0

    call ESMF_TestStart(ESMF_SRCLINE, rc=rc)
    

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

    ! warning: if you change these values, you also have to 
    ! change them in the subroutines below.
    i1 = 102
    i2 = 204
    i3 = 409
    i4 = 819

    fstr = "abcdefghijklmnopqrstuvwxyz0123456789"
    fstr2 = "0123456789abcdefghijklmnopqrstuvwxyz"


    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Failure calling f90ints() directly from F90"
    write(name, *) "Calling f90ints() directly from F90"
    call f90ints(i1, i2, i3, i4, rc)
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Failure calling f90string2() directly from F90"
    write(name, *) "Calling f90string2() directly from F90"
    call f90string2(i1, i2, fstr2, i3, i4, rc)
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Failure calling f90string3() directly from F90"
    write(name, *) "Calling f90string3() directly from F90"
    call f90string3(i1, fstr, i2, fstr2, i3, i4, rc)
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !NEX_UTest
    write(failMsg,*) "Failure calling c_strings with fstr"
    write(name, *) "Calling c_strings with fstr"
    call c_strings(f90ints, f90string2, f90string3, i1, i2, fstr, i3, i4, rc)
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

#ifdef ESMF_TESTEXHAUSTIVE

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------

    !------------------------------------------------------------------------
    !EX_UTest
    write(failMsg,*) "Failure calling c_strings with fstr2"
    write(name, *) "Calling c_strings with fstr2"
    call c_strings(f90ints, f90string2, f90string3, i1, i2, fstr2, i3, i4, rc)
    call ESMF_Test((rc .eq. ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE) 

    !------------------------------------------------------------------------
    !------------------------------------------------------------------------


#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    end program StringTest
    

subroutine f90ints(i1, i2, i3, i4, rc)
    use ESMF_Mod
    implicit none

    integer :: i1, i2, i3, i4
    integer :: rc

    integer :: check_i1, check_i2, check_i3, check_i4
 
    ! these must match the values in the main program
    check_i1 = 102
    check_i2 = 204
    check_i3 = 409
    check_i4 = 819


    print *, "-- entering f90ints subroutine"
    print *, " ints=", i1, i2, i3, i4

    ! assume ok, then set failure if any values do not match
    rc = ESMF_SUCCESS

    if (check_i1 .ne. i1) rc = ESMF_FAILURE
    if (check_i2 .ne. i2) rc = ESMF_FAILURE
    if (check_i3 .ne. i3) rc = ESMF_FAILURE
    if (check_i4 .ne. i4) rc = ESMF_FAILURE

    print *, " rc=", rc
    print *, "-- exiting f90ints subroutine"

end subroutine f90ints

subroutine f90string2(i1, i2, fstr, i3, i4, rc)
    use ESMF_Mod
    implicit none

    character(len=*) :: fstr
    integer :: i1, i2, i3, i4
    integer :: rc

    integer :: check_i1, check_i2, check_i3, check_i4
    character(len=120) :: check_fstr
    character(len=60) :: check_fstr2
 
    ! these must match the values in the main program
    check_i1 = 102
    check_i2 = 204
    check_i3 = 409
    check_i4 = 819

    check_fstr = "abcdefghijklmnopqrstuvwxyz0123456789"
    check_fstr2 = "0123456789abcdefghijklmnopqrstuvwxyz"

    print *, "-- entering f90string2 subroutine"
    print *, " ints=", i1, i2, i3, i4
    print *, " strlen =", len(fstr)
    print *, " fstr=", trim(fstr)
    print *, "-- leaving f90string2 subroutine"

end subroutine f90string2

subroutine f90string3(i1, fstr, i2, fstr2, i3, i4, rc)
    use ESMF_Mod
    implicit none

    character(len=*) :: fstr, fstr2
    integer :: i1, i2, i3, i4
    integer :: rc

    integer :: check_i1, check_i2, check_i3, check_i4
    character(len=120) :: check_fstr
    character(len=60) :: check_fstr2
 
    ! these must match the values in the main program
    check_i1 = 102
    check_i2 = 204
    check_i3 = 409
    check_i4 = 819

    check_fstr = "abcdefghijklmnopqrstuvwxyz0123456789"
    check_fstr2 = "0123456789abcdefghijklmnopqrstuvwxyz"

    print *, "-- entering f90string3 subroutine"
    print *, " ints=", i1, i2, i3, i4
    print *, " strlen =", len(fstr)
    print *, " fstr=", trim(fstr)
    print *, " strlen2 =", len(fstr2)
    print *, " fstr2=", trim(fstr2)
    print *, "-- leaving f90string3 subroutine"

end subroutine f90string3

