! $Id: ESMF_SysTest70384.F90,v 1.20 2003/07/29 16:44:55 jwolfe Exp $
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

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    implicit none
    
    ! Local variables
    integer :: nx, ny, nz, i, j, k, ni, nj, nk, nj2, nk2, rc, ii, jj, kk
    integer, dimension(:), allocatable :: delist
    integer, dimension(3) :: global_start, global_position
    integer :: result, len, base, de_id, de_x, de_y
    integer :: i_max, j_max, k_max, miscount
    integer :: status
    integer :: ndex, ndey, ndes
    logical :: match
    integer(ESMF_IKIND_I4), dimension(:,:,:), pointer :: srcdata, dstdata, resdata
    integer(ESMF_IKIND_I4), dimension(:,:,:), pointer :: srcptr, dstptr, resptr
    integer, dimension(3) :: global_counts, decompids1, decompids2, rank_trans, &
                             global_dimlengths
    character(len=ESMF_MAXSTR) :: cname, sname, gname, fname
    type(ESMF_DELayout) :: layout0, layout1, layout2
    type(ESMF_Array) :: array1, array1a, array2, array2a, array3
    type(ESMF_AxisIndex) :: indexlist1(3), indexlist2(3), indexlist3(3)
    type(ESMF_State) :: state1
        
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: testresult = 0

    ! individual test name
    character(ESMF_MAXSTR) :: testname

    ! individual test failure message and final status msg
    character(ESMF_MAXSTR) :: failMsg, finalMsg

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
    call ESMF_FrameworkInitialize(rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! Create a default 1xN DELayout
    layout0 = ESMF_DELayoutCreate(rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutGetNumDES(layout0, ndes, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    if (ndes .eq. 1) then
       print *, "This test must run with > 1 processor"
       goto 20
    endif

    ndex = 2
    ndey = ndes/2
    allocate(delist(ndes))
    delist = (/ (i, i=0, ndes-1) /)
    layout1 = ESMF_DELayoutCreate(layout0, 2, (/ ndex, ndey /), (/ 0, 0 /), &
                                                      de_indices=delist, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "DELayout Create finished, rc =", rc
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! Create the State
    cname = "Atmosphere"
    sname = "Atmosphere Export State"
    state1 = ESMF_StateCreate(sname, ESMF_STATEEXPORT, cname, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "State Create finished, name = ", trim(sname), " rc =", rc

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
    !  Data is type Integer, 3D.

    ! Allocate and set initial data values.  These are different on each DE.
    ni = 10
    nj = 06
    nk = 12
    allocate(srcdata(ni, nj, nk))
    allocate(resdata(ni, nj, nk))
    nj2 = 18
    nk2 = 04
    allocate(dstdata(ni, nj2, nk2))

    ! Get our local DE id
    call ESMF_DELayoutGetDEID(layout1, de_id, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! Get our position in the layout
    call ESMF_DELayoutGetDEPosition(layout1, de_x, de_y, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    de_x = de_x + 1  ! TODO:  Should not have to translate from C
    de_y = de_y + 1

    ! Calculate global_positions -- would otherwise come from a grid call
    global_position(1) = ni*(de_x-1)
    global_position(2) = nj*(de_y-1)
    global_position(3) = 0

    ! Create arrays, set and get axis info here before initializing
    ! the data.

    array1 = ESMF_ArrayCreate(srcdata, ESMF_DATA_REF, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    array2 = ESMF_ArrayCreate(dstdata, ESMF_DATA_REF, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    array3 = ESMF_ArrayCreate(resdata, ESMF_DATA_REF, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "Array Creates returned"

    ! Create axis indices  TODO:  move to an array method - ArrayDist?
    global_counts(1) = ni*ndex
    global_counts(2) = nj*ndey
    global_counts(3) = nk
    decompids1(1) = 1
    decompids1(2) = 2
    decompids1(3) = 0
    call ESMF_DELayoutSetAxisIndex(layout1, global_counts, decompids1, &
                                   indexlist1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutSetAxisIndex(layout1, global_counts, decompids1, &
                                   indexlist3, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    global_counts(1) = ni*ndex
    global_counts(2) = nj*ndey
    global_counts(3) = nk
    decompids2(1) = 1
    decompids2(2) = 0
    decompids2(3) = 2
    call ESMF_DELayoutSetAxisIndex(layout1, global_counts, decompids2, &
                                   indexlist2, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    !! TODO: set & get the axis info here.  These need to be
    !!  different on each DE.
    call ESMF_ArraySetAxisIndex(array1, totalindex=indexlist1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArraySetAxisIndex(array2, totalindex=indexlist2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArraySetAxisIndex(array3, totalindex=indexlist3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! Generate global cell numbers, where cell numbering scheme goes
    ! across the global mesh, rows first
    i_max = indexlist1(1)%max
    j_max = indexlist1(2)%max
    do k=1,indexlist1(3)%max-indexlist1(3)%min+1
      kk = k+global_position(3)
      do j=1,indexlist1(2)%max-indexlist1(2)%min+1
        jj = j+global_position(2)
        do i=1,indexlist1(1)%max-indexlist1(1)%min+1
          ii = i+global_position(1)
          srcdata(i, j, k) = i_max*j_max*(kk-1) + i_max*(jj-1) + ii
        enddo
      enddo
    enddo

    print *, "Initial data, before Transpose:"
    call ESMF_ArrayPrint(array1, "foo", rc);
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! No deallocate() is needed for array data, it will be freed when the
    !  Array is destroyed. 

    call ESMF_StateAddData(state1, array1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
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
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "Source Array retrieved from state"
    
    !! Call transpose method here, output ends up in array2
    global_dimlengths(1) = ni
    global_dimlengths(2) = nj
    global_dimlengths(3) = nk
    do i=1,3
      global_start(i) = global_position(i)
      if(decompids1(i).ne.0) then
        global_dimlengths(i) = global_counts(decompids1(i))
        global_start(i) = global_position(decompids1(i))
      endif
    enddo
    rank_trans(1) = 1
    rank_trans(2) = 2
    rank_trans(3) = 3
    call ESMF_ArrayRedist(array1, layout1, global_start, global_dimlengths, &
                          rank_trans, decompids1, decompids2, array2, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after Transpose:"
    call ESMF_ArrayPrint(array2, "", rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    !! Transpose back so we can compare contents
    !! Call transpose method again here, output ends up in array3
    global_dimlengths(1) = ni
    global_dimlengths(2) = nj
    global_dimlengths(3) = nk
    do i=1,3
      global_start(i) = global_position(i)
      if(decompids2(i).ne.0) then
        global_dimlengths(i) = global_counts(decompids2(i))
        global_start(i) = global_position(decompids2(i))
      endif
    enddo
    rank_trans(1) = 1
    rank_trans(2) = 2
    rank_trans(3) = 3
    call ESMF_ArrayRedist(array2, layout1, global_start, global_dimlengths, &
                          rank_trans, decompids2, decompids1, array3, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after second Transpose, should match original:"
    call ESMF_ArrayPrint(array3, "", rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Run section finished"


!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Finalize section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Print result

    call ESMF_DELayoutGetDEID(layout1, de_id, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"
    print *, "Result from DE_id ", de_id
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    call ESMF_ArrayGetData(array1, srcptr, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArrayGetData(array3, resptr, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    match = .true.
    miscount = 0
    do i=1, ni
      do j=1, nj
        do k=1, nk
          if (srcptr(i,j,k) .ne. resptr(i,j,k)) then
            print *, "array contents do not match at: (", i,j,k, ") on DE ", &
                     de_id, ".  src=", srcptr(i,j,k), "dst=", resptr(i,j,k)
            match = .false.
            miscount = miscount + 1
            if (miscount .gt. 40) then
              print *, "more than 40 mismatches, skipping rest of loop"
              goto 10
            endif
          endif
        enddo
      enddo
    enddo
    if (match) print *, "Array contents matched correctly!! DE_id = ", de_id
10  continue

    print *, "Finalize section finished"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Destroy section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!   Clean up

    call ESMF_StateDestroy(state1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArrayDestroy(array1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArrayDestroy(array2, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArrayDestroy(array3, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_DELayoutDestroy(layout1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "All Destroy routines done"

    call ESMF_FrameworkFinalize(rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
20    print *, "System Test #70384 complete!"

    if ((de_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
      write(failMsg, *)  "Transposed transpose not same as original"
      write(testname, *) "System Test 70384: Array Transpose/Redistribute"

      call ESMF_Test((miscount.eq.0) .and. (rc.eq.ESMF_SUCCESS), &
                        testname, failMsg, testresult, ESMF_SRCLINE)

      ! Separate message to console, for quick confirmation of success/failure
      if (rc .eq. ESMF_SUCCESS) then
        write(finalMsg, *) "SUCCESS!! Data transposed twice same as original"
      else
        write(finalMsg, *) "System Test did not succeed. ", &
        "Data transpose does not match expected values, or error code set ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
    

    end program ESMF_SysTest70384
    
!\end{verbatim}
    
