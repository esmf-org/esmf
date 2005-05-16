! $Id: ESMF_StringUTest.F90,v 1.1 2005/05/16 22:16:23 nscollins Exp $
!

! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================

! test of passing strings between F90 and C++

program StringTest

    character(len=120) :: fstr
    character(len=60) :: fstr2
    integer :: i1, i2, i3, i4
    external f90string, f90string2, f90string3

    i1 = 102
    i2 = 204
    i3 = 409
    i4 = 819

    fstr = "abcdefghijklmnopqrstuvwxyz0123456789"
    fstr2 = "0123456789abcdefghijklmnopqrstuvwxyz"

    print *, "-- in main routine"

    print *, "-- ready to call f90string directly"
    call f90string(i1, i2, i3, i4)
    print *, "-- return from call of f90string directly"

    print *, "-- back in main routine"

    print *, "-- ready to call f90string2 directly"
    call f90string2(i1, i2, fstr2, i3, i4)
    print *, "-- return from call of f90string2 directly"

    print *, "-- ready to call f90string3 directly"
    call f90string3(i1, fstr, i2, fstr2, i3, i4)
    print *, "-- return from call of f90string3 directly"

    print *, "-- back in main routine 2"

    print *, "-- ready to call c_strings with fstr"
    call c_strings(f90string, f90string2, f90string3, i1, i2, fstr, i3, i4)
    print *, "-- return from call of c_strings with fstr"

    print *, "-- back in main routine 3"

    print *, "-- ready to call c_strings with fstr2"
    call c_strings(f90string, f90string2, f90string3, i1, i2, fstr2, i3, i4)
    print *, "-- return from call of c_strings with fstr2"

    print *, "-- end of main"

end program StringTest

subroutine f90string(i1, i2, i3, i4)
    integer :: i1, i2, i3, i4

    print *, "-- entering f90string subroutine"
    print *, " ints=", i1, i2, i3, i4
    print *, "-- leaving f90string subroutine"

end subroutine f90string

subroutine f90string2(i1, i2, fstr, i3, i4)
    character(len=*) :: fstr
    integer :: i1, i2, i3, i4

    print *, "-- entering f90string2 subroutine"
    print *, " ints=", i1, i2, i3, i4
    print *, " strlen =", len(fstr)
    print *, " fstr=", trim(fstr)
    print *, "-- leaving f90string2 subroutine"

end subroutine f90string2

subroutine f90string3(i1, fstr, i2, fstr2, i3, i4)
    character(len=*) :: fstr, fstr2
    integer :: i1, i2, i3, i4

    print *, "-- entering f90string3 subroutine"
    print *, " ints=", i1, i2, i3, i4
    print *, " strlen =", len(fstr)
    print *, " fstr=", trim(fstr)
    print *, " strlen2 =", len(fstr2)
    print *, " fstr2=", trim(fstr2)
    print *, "-- leaving f90string3 subroutine"

end subroutine f90string3

