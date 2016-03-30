    program test

    use ESMF_CFIOGridMOD
    use ESMF_CFIOVarInfoMOD
    use ESMF_CFIOFileMOD
    use ESMF_CFIOSdfMOD
    use ESMF_CFIOGrADSMOD
    use ESMF_CFIOMOD

!    use ESMF_CFIOMOD

! define ESMF_CFIO, ESMF_CFIOVarInfo, and ESMF_CFIOGrid objects
    type(ESMF_CFIO) :: cfio, cfio_out
    type(ESMF_CFIOVarInfo), pointer :: vars(:)
    type(ESMF_CFIOGrid) :: grid
    type(ESMF_CFIOGrid), pointer :: ggrid

    character(len=256) :: vName, standardName
    character(len=256) :: title, source
    real, pointer :: tmpu(:,:,:)
    integer :: fmode=1, rc, im, jm, km, km_out, date, begTime, timeInc, curTime
    integer :: nSteps, t, k
    real :: amiss
    logical :: twoD
    real, pointer :: lev(:), lat(:), lon(:)  
    real, pointer :: lev_out(:)
    real :: ak(19)=(/291.70,  792.92,  2155.39,  4918.34,  8314.25,      &
               7993.08, 7577.38,  7057.52,  6429.63,  5698.38,      &
               4879.13, 3998.95,  3096.31,  2219.02,  1420.39,      &
               754.13,  268.38,   0.0000,   0.0000 /)
    real :: bk(19)=(/0.0000,    0.0000,    0.0000,   0.0000,   0.0000,   &
                0.0380541, 0.0873088, 0.1489307, 0.2232996,         &
                0.3099406, 0.4070096, 0.5112977, 0.6182465,         &
                0.7221927, 0.8168173, 0.8957590, 0.9533137,         &
                0.9851122, 1.0  /)
    real :: range(2) = (/-1.E10, 1.E10/)
    logical :: isGrADS = .false.


    integer :: fid
    integer :: argc
    integer, external :: iargc
    logical :: passed = .true.
    logical :: new_file

    character(len=256) :: inFile

    argc = iargc()
    if ( argc < 1 ) then 
       inFile = "GEOS5.ana.hdf"
    else
       call GetArg ( 1, inFile )
    end if

! Create a CFIO object
    cfio =  ESMF_CFIOCreate(cfioObjName='ana')
    cfio_out =  ESMF_CFIOCreate(cfioObjName='test')

! Set file name to the cfio obj
    call ESMF_CFIOSet(cfio, fName=inFile)

! open the file
    call ESMF_CFIOFileOpen(cfio, fmode, rc=rc)
    call ESMF_CFIOGet(cfio, date=date, begTime=begTime, timeInc=timeInc)
    call ESMF_CFIOGet(cfio, title=title, source=source)

! read the data if you know date, time, and variable dimension
    call ESMF_CFIOGet(cfio, nVars=nVars, nSteps=nSteps)
    allocate(vars(nVars))
    do i = 1, nVars
      vars(i) = ESMF_CFIOVarInfoCreate()
    enddo

!   read variable objects
    call ESMF_CFIOGet(cfio, varObjs=vars)

    allocate(ggrid)
    ggrid = ESMF_CFIOGridCreate()
    call ESMF_CFIOVarInfoGet(vars(nVars), vName=vName, grid=ggrid)
    call ESMF_CFIOGridGet(ggrid, im=im, jm=jm, km=km_out)
    allocate(lev(km_out), lon(im), lat(jm))
    call ESMF_CFIOGridGet(ggrid, lev=lev,lon=lon,lat=lat)

    new_file = .true.
    do t = 1, nSteps
    curTime = begTime + (t-1)*timeInc
    do i = 1, nVars
!      get variable name and grid
       call ESMF_CFIOVarInfoGet(vars(i), vName=vName, grid=grid, amiss=amiss, &
                                twoDimVar=twoD, standardName=standardName)

!      get dimensions 
       call ESMF_CFIOGridGet(grid, im=im, jm=jm, km=km)
!       allocate(lev(km), lon(im), lat(jm))
!       call ESMF_CFIOGridGet(grid, lev=lev,lon=lon,lat=lat)

       if ( twoD ) then 
          allocate(tmpu(im,jm,1))
          km=1
       else
         allocate(tmpu(im,jm,km))
       end if

       if (new_file) then
         allocate(lev_out(km_out))
         lev_out = lev 
!        allocate(ggrid)
!         ggrid = ESMF_CFIOGridCreate(gName='prs_grid')
         call ESMF_CFIOGridSet(ggrid,levUnit='hPa')

!         call ESMF_CFIOVarInfoSet(vars(1), vName='ps', grid=ggrid, twoDimVar=.true.)
!         call ESMF_CFIOVarInfoSet(vars(2), vName='ts', grid=ggrid, twoDimVar=.true.)
!         call ESMF_CFIOVarInfoSet(vars(3), vName='tmpu', grid=ggrid)
!         call ESMF_CFIOVarInfoSet(vars(4), vName='u', grid=ggrid)
!         call ESMF_CFIOVarInfoSet(vars(5), vName='v', grid=ggrid)
         call ESMF_CFIOSet(cfio_out, fName='GEOS5.ana.new.hdf', varObjs=vars, &
                   grid=ggrid, date=date, BegTime=BegTime, timeInc=timeInc)
!                   grid=grid, date=date, BegTime=BegTime, timeInc=timeInc)
         call ESMF_CFIOSet(cfio_out, title=title, source=source)
!	 call ESMF_CFIOFileCreate(cfio_out)
	 call ESMF_CFIOFileCreate(cfio_out, format='GrADS')
         new_file = .false.
       end if
         
!      read the data
          call ESMF_CFIOVarRead(cfio, vName, tmpu, date, curTime, rc=rc)
          call ESMF_CFIOVarWrite(cfio_out, vName, tmpu, date, curTime, rc=rc)
!          if ( twoD ) then
!            print *, "vName: ", tmpu(10,:,1)
!          else 
!             print *, "vName: ", tmpu(10,:,2)
!          end if
          if (rc .ne. 0) passed = .false.
       deallocate(tmpu)

    end do
    end do

! close the file
    call ESMF_CFIOFileClose(cfio)
    call ESMF_CFIOFileClose(cfio_out)

    if ( passed ) then
       print *, "testr_gd_wr: Passed"
    else
       print *, "testr_gd_wr: NOT Passed"
    end if
  end 
