! $Id: ESMF_SysTest70384.F90,v 1.6 2003/02/21 15:47:24 nscollins Exp $
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
    use ESMF_ArrayMod
    use ESMF_StateMod
    
    implicit none
    
    ! Local variables
    integer :: nx, ny, nz, i, j, k, ni, nj, nk, nj2, nk2, rc, ii, jj, kk
    integer, dimension(6) :: delist
    integer :: result, len, base, de_id
    integer :: i_max, j_max, k_max, miscount
    integer :: status
    integer :: ndex, ndey
    logical :: match
    integer(ESMF_IKIND_I4), dimension(:,:,:), pointer :: srcdata, dstdata, resdata
    integer(ESMF_IKIND_I4), dimension(:,:,:), pointer :: srcptr, dstptr, resptr
    integer, dimension(3) :: global_counts, decompids1, decompids2, rank_trans
    character(len=ESMF_MAXSTR) :: cname, sname, gname, fname
    type(ESMF_Layout) :: layout1 
    type(ESMF_Array) :: array1, array1a, array2, array2a, array3
    type(ESMF_AxisIndex) :: indexlist1(3), indexlist2(3), indexlist3(3)
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
    ndex = 2
    ndey = 3
    layout1 = ESMF_LayoutCreate(ndex, ndey, delist, ESMF_XFAST, rc)
    print *, "Layout Create finished, rc =", rc

    ! Create the State
    cname = "Atmosphere"
    sname = "Atmosphere Export State"
    state1 = ESMF_StateCreate(cname, ESMF_STATEEXPORT, sname, rc=rc)
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
    call ESMF_LayoutGetDEId(layout1, de_id, rc)

    ! Create arrays, set and get axis info here before initializing
    ! the data.

    array1 = ESMF_ArrayCreate(srcdata, ESMF_NO_COPY, rc)
    array2 = ESMF_ArrayCreate(dstdata, ESMF_NO_COPY, rc)
    array3 = ESMF_ArrayCreate(resdata, ESMF_NO_COPY, rc)
    print *, "Array Creates returned"

    ! Create axis indices  TODO:  move to an array method - ArrayDist?
    global_counts(1) = ni*ndex
    global_counts(2) = nj*ndey
    global_counts(3) = nk
    decompids1(1) = 1
    decompids1(2) = 2
    decompids1(3) = 0
    call ESMF_LayoutSetAxisIndex(layout1, global_counts, decompids1, &
                                 indexlist1, rc)
    call ESMF_LayoutSetAxisIndex(layout1, global_counts, decompids1, &
                                 indexlist3, rc)
    global_counts(1) = ni*ndex
    global_counts(2) = nj*ndey
    global_counts(3) = nk
    decompids2(1) = 1
    decompids2(2) = 0
    decompids2(3) = 2
    call ESMF_LayoutSetAxisIndex(layout1, global_counts, decompids2, &
                                 indexlist2, rc)
    !! TODO: set & get the axis info here.  These need to be
    !!  different on each DE.
    call ESMF_ArraySetAxisIndex(array1, indexlist1, rc)
    call ESMF_ArraySetAxisIndex(array2, indexlist2, rc)
    call ESMF_ArraySetAxisIndex(array3, indexlist3, rc)

    ! Generate global cell numbers, each DE has a contiguous 
    ! chunk of numbers.
    i_max = indexlist1(1)%max
    j_max = indexlist1(2)%max
    do k=1,indexlist1(3)%r-indexlist1(3)%l+1
      kk = k+indexlist1(3)%l
      do j=1,indexlist1(2)%r-indexlist1(2)%l+1
        jj = j+indexlist1(2)%l
        do i=1,indexlist1(1)%r-indexlist1(1)%l+1
          ii = i+indexlist1(1)%l
          srcdata(i, j, k) = i_max*j_max*(kk-1) + i_max*(jj-1) + ii
        enddo
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
    
    !! Call transpose method here, output ends up in array2
    rank_trans(1) = 1
    rank_trans(2) = 2
    rank_trans(3) = 3
    call ESMF_ArrayRedist(array1, layout1, rank_trans, decompids1, &
                          decompids2, array2, rc)

    print *, "Array contents after Transpose:"
    call ESMF_ArrayPrint(array2, "", rc)

    !! Transpose back so we can compare contents
    !! Call transpose method again here, output ends up in array3
    rank_trans(1) = 1
    rank_trans(2) = 2
    rank_trans(3) = 3
    call ESMF_ArrayRedist(array2, layout1, rank_trans, decompids2, &
                          decompids1, array3, rc)

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
    print *, "Result from DE_id ", de_id
    print *, "-----------------------------------------------------------------"
    print *, "-----------------------------------------------------------------"

    call ESMF_ArrayGetData(array1, srcptr, rc=rc)
    call ESMF_ArrayGetData(array3, resptr, rc=rc)
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
    call ESMF_ArrayDestroy(array1, rc)
    call ESMF_ArrayDestroy(array2, rc)
    call ESMF_ArrayDestroy(array3, rc)
    call ESMF_LayoutDestroy(layout1, rc)
    print *, "All Destroy routines done"


!-------------------------------------------------------------------------
!-------------------------------------------------------------------------
    print *, "System Test #70384 complete!"

    

    end program ESMF_SysTest70384
    
!\end{verbatim}
    
