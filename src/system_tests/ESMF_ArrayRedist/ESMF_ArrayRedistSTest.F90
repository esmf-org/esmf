! $Id: ESMF_ArrayRedistSTest.F90,v 1.12 2004/06/04 08:47:06 nscollins Exp $
!
! System test ArrayRedist
!  Description on Sourceforge under System Test #70384

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!BOP
!
! !DESCRIPTION:
! System test ArrayRedist.
!
!
!\begin{verbatim}

    program ArrayRedist

#include <ESMF_Macros.inc>

    ! ESMF Framework module
    use ESMF_Mod
    use ESMF_TestMod
    
    implicit none
    
    ! Local variables
    integer :: i, j, k, rc, ii, jj, kk
    integer, dimension(3) :: global_start, global_position, counts1, counts2, de_pos
    integer :: de_id, npets
    integer :: i_max, j_max, miscount
    integer :: nde(2)
    logical :: match
    integer(ESMF_KIND_I4), dimension(:,:,:), pointer :: srcdata, dstdata, resdata
    integer(ESMF_KIND_I4), dimension(:,:,:), pointer :: srcptr, resptr
    integer, dimension(3) :: global_counts, decompids1, decompids2, rank_trans
    character(len=ESMF_MAXSTR) :: sname, aname
    type(ESMF_DELayout) :: delayout1
    type(ESMF_VM) :: vm
    type(ESMF_Array) :: array1, array1a, array2, array3
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

    print *, "System Test ArrayRedist:"

!
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!  Create section
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
!
    ! Initialize the framework and get back the default global VM
    call ESMF_Initialize(vm=vm, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! Get number of PETs and my PET id
    call ESMF_VMGet(vm, petCount=npets, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    if (npets .eq. 1) then
       print *, "This test must run with > 1 processor"
       goto 20
    endif

    ! Create a 2 x N layout
    nde(1) = 2
    nde(2) = npets/2
    delayout1 = ESMF_DELayoutCreate(vm, (/ nde(1), nde(2) /), rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "DELayout Create finished, rc =", rc

    ! Create the State
    sname = "Atmosphere Export State"
    state1 = ESMF_StateCreate(sname, ESMF_STATEEXPORT, rc=rc)
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
    counts1(1) = 10
    counts1(2) = 06
    counts1(3) = 12
    allocate(srcdata(counts1(1), counts1(2), counts1(3)))
    allocate(resdata(counts1(1), counts1(2), counts1(3)))
    counts2(1) = 10
    counts2(2) = 18
    counts2(3) = 04
    allocate(dstdata(counts2(1), counts2(2), counts2(3)))

    ! Get our position in the layout
    do i = 1,3
      de_pos(i) = 1
    enddo
    call ESMF_DELayoutGet(delayout1, deCountPerDim=de_pos, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    ! Calculate global_positions -- would otherwise come from a grid call
    do i = 1,3
      global_position(i) = counts1(i)*(de_pos(i)-1)
    enddo

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
    decompids1(1) = 1
    decompids1(2) = 2
    decompids1(3) = 0
    do i = 1,3
      global_counts(i) = counts1(i)
      if (decompids1(i).ne.0) global_counts(i) = counts1(i)*nde(decompids1(i))
    enddo
    call ESMF_ArrayComputeAxisIndex(array1, delayout1, decompids1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArrayComputeAxisIndex(array3, delayout1, decompids1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    decompids2(1) = 1
    decompids2(2) = 0
    decompids2(3) = 2
    call ESMF_ArrayComputeAxisIndex(array2, delayout1, decompids2, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    call ESMF_ArrayGetAxisIndex(array1, compindex=indexlist1, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArrayGetAxisIndex(array2, compindex=indexlist2, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    call ESMF_ArrayGetAxisIndex(array3, compindex=indexlist3, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20


    ! Generate global cell numbers, where cell numbering scheme goes
    ! across the global mesh, rows first
    i_max = global_counts(1)
    j_max = global_counts(2)
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
    call ESMF_ArrayGet(array1, name=aname, rc=rc)
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

    call ESMF_StateGetData(state1, aname, array1a, rc=rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "Source Array retrieved from state"
    
    !! Call transpose method here, output ends up in array2
    do i=1,3
      rank_trans(i) = i
      global_start(i) = 0
      if(decompids2(i).ne.0) then
        global_start(i) = counts2(i)*(de_pos(decompids2(i))-1)
      endif
    enddo
    call ESMF_ArrayRedist(array1, delayout1, global_start, global_counts, &
                          rank_trans, decompids1, decompids2, array2, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    print *, "Array contents after Transpose:"
    call ESMF_ArrayPrint(array2, "", rc)
    if (rc .ne. ESMF_SUCCESS) goto 20

    !! Transpose back so we can compare contents
    !! Call transpose method again here, output ends up in array3

    do i=1,3
      rank_trans(i) = i
      global_start(i) = 0
      if(decompids1(i).ne.0) then
        global_start(i) = counts1(i)*(de_pos(decompids1(i))-1)
      endif
    enddo
    call ESMF_ArrayRedist(array2, delayout1, global_start, global_counts, &
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

    call ESMF_DELayoutGet(delayout1, localDE=de_id, rc=rc)
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
    do i=1, counts1(1)
      do j=1, counts1(2)
        do k=1, counts1(3)
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
    if (match) print *, "Array contents matched correctly: DE_id = ", de_id
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
    call ESMF_DELayoutDestroy(delayout1, rc)
    if (rc .ne. ESMF_SUCCESS) goto 20
    print *, "All Destroy routines done"

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
20    print *, "System Test ArrayRedist complete"

    if ((de_id .eq. 0) .or. (rc .ne. ESMF_SUCCESS)) then
      write(failMsg, *)  "Transposed transpose not same as original"
      write(testname, *) "System Test ArrayRedist: Array Transpose/Redistribute"

      call ESMF_Test((miscount.eq.0) .and. (rc.eq.ESMF_SUCCESS), &
                        testname, failMsg, testresult, ESMF_SRCLINE)

      ! Separate message to console, for quick confirmation of success/failure
      if ((miscount.eq.0) .and. (rc .eq. ESMF_SUCCESS)) then
        write(finalMsg, *) "SUCCESS: Data transposed twice same as original."
      else
        write(finalMsg, *) "System Test did not succeed. ", &
        "Data transpose does not match expected values, or error code set ", rc
      endif
      write(0, *) ""
      write(0, *) trim(testname)
      write(0, *) trim(finalMsg)
      write(0, *) ""

    endif
    
    call ESMF_Finalize(rc)

    end program ArrayRedist
    
!\end{verbatim}
    
