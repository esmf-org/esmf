! $Id: ESMF_SysTest70384.F90,v 1.2 2003/02/13 23:22:36 nscollins Exp $
!
! System test code #70384

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test number 70384.
!
!
!\begin{verbatim}

    program ESMF_SysTest70384

#include "ESMF.h"

    ! Modules needed
    ! TODO: (these will be collapsed into a single ESMD_Mod soon)
    use ESMF_BaseMod
    use ESMF_IOMod
    use ESMF_LayoutMod
    use ESMF_ClockMod
    use ESMF_ArrayMod
    use ESMF_StateMod
    
    implicit none
    
    ! Local variables
    integer :: nx, ny, i, j, ni, nj, rc
    integer, dimension(6) :: delist
    integer :: result, len, base, de_id
    integer :: i_max, j_max
    integer :: status
    integer :: nDE_i, nDE_j
    logical :: match
    integer(ESMF_IKIND_I4), dimension(:,:), pointer :: srcdata, dstdata, resdata
    integer(ESMF_IKIND_I4), dimension(:,:), pointer :: srcptr, dstptr, resptr
    character(len=ESMF_MAXSTR) :: cname, sname, gname, fname
    type(ESMF_Clock) :: clock1
    integer :: timestep
    type(ESMF_Layout) :: layout1 
    type(ESMF_Array) :: array1, array1a, array2, array2a, array3
    type(ESMF_AxisIndex) :: indexlist1(2), indexlist2(2), indexlist3(2)
    type(ESMF_State) :: state1
        
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

    print *, "System Test #70384:"


!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    ! Create a Layout
    delist = (/ 0, 1, 2, 3, 4, 5 /)
    layout1 = ESMF_LayoutCreate(2, 3, delist, ESMF_XFAST, rc)
    print *, "Layout Create finished, rc =", rc

    ! Create the State
    cname = "Atmosphere"
    sname = "Atmosphere Export State"
    state1 = ESMF_StateCreate(cname, ESMF_STATEEXPORT, sname, rc=rc)
    print *, "State Create finished, name = ", trim(sname), " rc =", rc

    ! Create a Clock
    !! TODO: this method doesn't exist yet?
    !clock1 = ESMF_ClockCreate(rc=rc)
    !print *, "Clock Create finished, rc =", rc
    print *, "==> Clock create needs to be called here"

    print *, "Create section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Init section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    !  Create the arrays and set the data.

    !  Create Array based on an existing, allocated F90 pointer.
    !  Data is type Integer, 2D.

    ! Allocate and set initial data values.  These are different on each DE.
    ni = 40
    nj = 20
    allocate(srcdata(ni, nj))
    allocate(dstdata(ni, nj))
    allocate(resdata(ni, nj))

    ! Get our local DE id
    call ESMF_LayoutGetDEId(layout1, de_id, rc)

    ! Create arrays, set and get axis info here before initializing
    ! the data.

    array1 = ESMF_ArrayCreate(srcdata, ESMF_NO_COPY, rc)
    array2 = ESMF_ArrayCreate(dstdata, ESMF_NO_COPY, rc)
    array3 = ESMF_ArrayCreate(resdata, ESMF_NO_COPY, rc)
    print *, "Array Creates returned"

    !! TODO: set & get the axis info here.  These need to be
    !!  different on each DE.
    call ESMF_ArraySetAxisIndex(array1, indexlist1, rc)
    call ESMF_ArraySetAxisIndex(array2, indexlist2, rc)
    call ESMF_ArraySetAxisIndex(array3, indexlist3, rc)

    ! Generate global cell numbers, each DE has a contiguous 
    ! chunk of numbers.
    base = de_id * ni* nj
    do i=1, ni
      do j=1, nj
       srcdata(i, j) = i + (j*ni) + base   !! TODO: see if this is right
      enddo
    enddo

    print *, "Initial data, before Transpose:"
    call ESMF_ArrayPrint(array1, "foo", rc);

    ! No deallocate() is needed for array data, it will be freed when the
    !  Array is destroyed. 

    call ESMF_StateAddData(state1, array1, rc=rc)
    print *, "Source Array added to state"


    print *, "Init section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    call ESMF_StateGetData(state1, "default array name", array1a, rc=rc)
    print *, "Source Array retrieved from state"
    
    timestep = 1
    !! TODO: fool with clocks here

    !! TODO: call transpose method here, output ends up in array2

    print *, "Array contents after Transpose:"
    call ESMF_ArrayPrint(array2, "", rc)

    !! Transpose back so we can compare contents
    !! TODO: call transpose method again here, output ends up in array3

    print *, "Array contents after second Transpose, should match original:"
    call ESMF_ArrayPrint(array3, "", rc)

    print *, "Run section finished"


!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Print result

    call ESMF_LayoutGetDEId(layout1, de_id, rc)

    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"
    print *, "Result from DE ", de_id
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    call ESMF_ArrayGetData(array1, srcptr, rc=rc)
    call ESMF_ArrayGetData(array3, resptr, rc=rc)
    match = .true.
    do i=1, ni
      do j=1, nj
        if (srcptr(i,j) .ne. resptr(i,j)) then
          print *, "array contents do not match: ", &
                     srcptr(i,j), ".ne.", resptr(i,j), "at", i,j
          match = .false.
        endif
      enddo
    enddo
    if (match) print *, "Array contents matched correctly!!"

    print *, "Finalize section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    call ESMF_StateDestroy(state1, rc)
    call ESMF_ArrayDestroy(array1, rc)
    call ESMF_ArrayDestroy(array2, rc)
    call ESMF_ArrayDestroy(array3, rc)
    !! TODO: comment this in when it exists
    !call ESMF_ClockDestroy(clock1, rc)
    call ESMF_LayoutDestroy(layout1, rc)
    print *, "All Destroy routines done"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    print *, "System Test #70384 complete!"

    

    end program ESMF_SysTest70384
    
!\end{verbatim}
    
