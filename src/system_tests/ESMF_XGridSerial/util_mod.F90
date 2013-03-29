! $Id$
!
! Example/test code which shows User Component calls.

!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

!
! !DESCRIPTION:
!  Utility module for concurrent XGrid system test
!
!
!\begin{verbatim}

module util_mod

  ! ESMF Framework module
  use ESMF

  implicit none
  private
    
  public make_grid_sph
        
  contains

  function make_grid_sph(atm_nx, atm_ny, atm_dx, atm_dy, atm_sx, atm_sy, &
    msx, msy, mex, mey, maskvalue, isfull, rc)

    ! return value
    type(ESMF_Grid)                           :: make_grid_sph
    ! arguments
    integer, intent(in)                       :: atm_nx, atm_ny
    real(ESMF_KIND_R4), intent(in)            :: atm_dx, atm_dy
    real(ESMF_KIND_R4), intent(in)            :: atm_sx, atm_sy
    real(ESMF_KIND_R4), intent(in), optional  :: msx, msy, mex, mey
    integer(ESMF_KIND_I4), intent(in), optional :: maskvalue
    logical, intent(in) , optional            :: isfull
    integer, intent(out), optional            :: rc

    ! local variables
    integer                                   :: localrc, i, j
    real(ESMF_KIND_R8), pointer               :: coordX(:,:), coordY(:,:)
    integer(ESMF_KIND_I4), pointer            :: maskptr(:,:)
    real(ESMF_KIND_R8)                        :: startx, starty
    logical                                   :: l_isfull

    l_isfull = .true.
    if(present(isfull)) l_isfull = isfull

    if(l_isfull) then
      make_grid_sph = ESMF_GridCreate1PeriDim(maxIndex=(/atm_nx, atm_ny/), &
        coordSys=ESMF_COORDSYS_SPH_DEG, &
        indexflag=ESMF_INDEX_GLOBAL, &
        rc=localrc)
    else
      make_grid_sph = ESMF_GridCreateNoPeriDim(maxIndex=(/atm_nx, atm_ny/), &
        indexflag=ESMF_INDEX_GLOBAL, &
        gridEdgeLWidth=(/0,0/), gridEdgeUWidth=(/1,1/), &
        rc=localrc)
    endif 
    if(localrc /= ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_GridAddItem(make_grid_sph, staggerloc=ESMF_STAGGERLOC_CENTER, &
           itemflag=ESMF_GRIDITEM_MASK, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_GridGetItem(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
                          itemflag=ESMF_GRIDITEM_MASK, farrayPtr=maskptr, rc=localrc)
    if (localrc /=ESMF_SUCCESS) then
       rc=ESMF_FAILURE
       return
    endif
    maskptr = 0

    call ESMF_GridAddCoord(make_grid_sph, staggerloc=ESMF_STAGGERLOC_CENTER, &
        rc=localrc)
    if(localrc /= ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    call ESMF_GridAddCoord(make_grid_sph, staggerloc=ESMF_STAGGERLOC_CORNER, &
        rc=localrc)
    if(localrc /= ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    ! global indexing
    ! atm grid is not decomposed in the y direction
    !startx = lpet*atm_nx/npet*atm_dx
    startx = atm_sx
    starty = atm_sy
    ! compute coord
    ! X center
    call ESMF_GridGetCoord(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if(localrc /= ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    ! Y center
    call ESMF_GridGetCoord(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CENTER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if(localrc /= ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordY, 2), ubound(coordY, 2)
        coordX(i,j) = startx + atm_dx/2. + (i-1)*atm_dx
        coordY(i,j) = starty + atm_dy/2. + (j-1)*atm_dy
      enddo
    enddo
    if(present(maskvalue) .and. present(msx) .and. present(mex) .and. &
       present(msy) .and. present(mey)) then
      do i = lbound(coordX,1), ubound(coordX,1)
        do j = lbound(coordY, 2), ubound(coordY, 2)
          if((coordX(i,j) .ge. msx .and. coordX(i,j) .le. mex) .and. &
             (coordY(i,j) .ge. msy .and. coordY(i,j) .le. mey) ) then
            maskptr(i,j) = maskvalue
          endif
        enddo
      enddo
    endif
    !print *, 'startx: ', startx, lbound(coordX, 1), ubound(coordX, 1), 'coordX: ', coordX(:,1)
    ! X corner
    call ESMF_GridGetCoord(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=1, farrayPtr=coordX, rc=localrc)
    if(localrc /= ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    ! Y corner
    call ESMF_GridGetCoord(make_grid_sph, localDE=0, staggerLoc=ESMF_STAGGERLOC_CORNER, &
        coordDim=2, farrayPtr=coordY, rc=localrc)
    if(localrc /= ESMF_SUCCESS) then
      rc=ESMF_FAILURE
      return
    endif

    do i = lbound(coordX,1), ubound(coordX,1)
      do j = lbound(coordX, 2), ubound(coordX, 2)
        coordX(i,j) = startx + (i-1)*atm_dx
      enddo
    enddo
    do i = lbound(coordY,1), ubound(coordY,1)
      do j = lbound(coordY, 2), ubound(coordY, 2)
        coordY(i,j) = starty + (j-1)*atm_dy
      enddo
    enddo

    if(present(rc)) rc = ESMF_SUCCESS

  end function make_grid_sph

end module util_mod
    
!\end{verbatim}
