! $Id: ESMF_SysTest70384.F90,v 1.1 2003/02/13 21:12:45 nscollins Exp $
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
    integer(ESMF_IKIND_I4), dimension(:,:), pointer :: srcdata, dstdata
    character(len=ESMF_MAXSTR) :: cname, sname, gname, fname
    type(ESMF_Clock) :: clock1
    integer :: timestep
    type(ESMF_Layout) :: layout1 
    type(ESMF_Array) :: array1, array1a, array2, array2a
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
    ni = 400
    nj = 200
    allocate(srcdata(ni, nj))
    allocate(dstdata(ni, nj))    !! TODO: what are these?

    !! TODO: Get our local DE here somehow and put it in de_id
    base = 0

    ! Generate global cell numbers, each DE has a contiguous 
    ! chunk of numbers.
    do i=1, ni
      do j=1, nj
       srcdata(i, j) = i + base
      enddo
    enddo

    array1 = ESMF_ArrayCreate(srcdata, ESMF_NO_COPY, rc)
    array2 = ESMF_ArrayCreate(dstdata, ESMF_NO_COPY, rc)
    print *, "Array Creates returned"

    print *, "Initial data, before Transpose:"
    call ESMF_ArrayPrint(array1, "foo", rc);

    ! No deallocate() is needed for array data, it will be freed when the
    !  Array is destroyed. 

    call ESMF_StateAddData(state1, array1, rc=rc)
    print *, "Source Array added to state"

    !! TODO: set the axis info here.


    print *, "Init section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Run section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!

    timestep = 1

    call ESMF_StateGetData(state1, "default array name", array1a, rc=rc)
    print *, "Source Array retrieved from state"
    

    !! TODO: call transpose method here, output ends up in array2



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

    print *, "Destination array contents:"
    call ESMF_ArrayPrint(array2, " ", rc=rc)

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
    !! TODO: comment this in when it exists
    !call ESMF_ClockDestroy(clock1, rc)
    call ESMF_LayoutDestroy(layout1, rc)
    print *, "All Destroy routines done"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    print *, "System Test #70384 complete!"

    

    end program ESMF_SysTest70384
    
!\end{verbatim}
    
