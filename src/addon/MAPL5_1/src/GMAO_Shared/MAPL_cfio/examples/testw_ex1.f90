    program test

    use ESMF_CFIOGridMOD
    use ESMF_CFIOVarInfoMOD
    use ESMF_CFIOFileMOD
    use ESMF_CFIOSdfMOD
    use ESMF_CFIOMOD

!   define ESMF_CFIO,  ESMF_CFIOVarInfo, and ESMF_CFIOGrid objects
    type(ESMF_CFIO) :: cfio
    type(ESMF_CFIOVarInfo), pointer :: vars(:)
    type(ESMF_CFIOGrid), pointer :: grid

    character(len=20) :: fileName
    real, pointer :: ts(:,:,:), ps(:,:,:)
    integer :: fmode, rc
    integer :: date, curTime, timeInc
    real :: range(2) = (/-1.E10, 1.E10/)
    logical :: twoD

    integer :: hhmmss(2) = (/0, 60000/)
    integer :: i,j, t
    real :: lon(72), lat(46), dlat, dlon
    integer :: im=72, jm=46
    logical :: passed = .true.

    fmode = 0

    dlat = 180./(jm-1)
    dlon = 360./im
    do i =1, im
      lon(i) = -180 + (i-1)*dlon
    end do
    do j =1, jm
      lat(j) = -90 + (j-1)*dlat
    end do

! Create grid and set grid attributes
    allocate(grid)
    grid = ESMF_CFIOGridCreate(gName='test2d')
    call ESMF_CFIOGridSet(grid, im=72, jm=46, lon=lon, lat=lat)

!   read back im, jm
    call ESMF_CFIOGridGet(grid, im=im, jm=jm)

! Create variable object and set variable attributes
    allocate(vars(2))
    vars(1) = ESMF_CFIOVarInfoCreate(vName='ps')
    call ESMF_CFIOVarInfoSet(vars(1), vName='ps', vTitle='surface pressure', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(1), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(1), standardName='ps', twoDimVar=.true.)
    call ESMF_CFIOVarInfoSet(vars(1), validRange=range, vUnits='K')

    vars(2) = ESMF_CFIOVarInfoCreate(vName='ts')
    call ESMF_CFIOVarInfoSet(vars(2),vName='ts',vTitle='skin temperature', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(2), standardName='tskin', twoDimVar=.true.)
    call ESMF_CFIOVarInfoSet(vars(2), validRange=range, vUnits='K')

! Create CFIO object and set global attributes

    cfio =  ESMF_CFIOCreate(cfioObjName='ana')
    call ESMF_CFIOSet(cfio, fName='GEOS5.ana.hdf', varObjs=vars, &
                   grid=grid, date=20011201, BegTime=0, timeInc=60000)
    call ESMF_CFIOSet(cfio, title="c403_cer_01: FVGCM Diagnostics",  &
                   source="Global Modeling and Assimilation Office", &
                   contact="data@gmao.gsfc.nasa.gov")
    call ESMF_CFIOSet(cfio, history='File written by CFIO v1.0.0', &
           convention='ESMF', institution="Global Modeling and Assimilation Office")
    call ESMF_CFIOSet(cfio, references='see ESMF', comment='First CFIO test version',prec=0) 

! All metadata are set. Get some metadata to see what in the CFIO Obj.

    call ESMF_CFIOGet(cfio, nVars=nVars, date=date, begTime=curTime, &
                   timeInc=timeInc, fName=fileName)

! Create output file
    call ESMF_CFIOFileCreate(cfio)

! put some test data into the variables
    allocate(ps(im,jm,1), ts(im,jm,1))

    do t = 1, 2
       do j = 1, jm
         do i = 1, im
            ps(i,j,1) = 100000 + (t-1)*cos(2*3.14*lat(j)/180.)*sin(2*3.14*2*lon(i)/360.)   
            ts(i,j,1) = 273 - (t-1)*sin(2*3.14*lat(j)/180.)*cos(2*3.14*2*lon(i)/360.)
         enddo
       enddo

! Write data to the output file

       call ESMF_CFIOVarWrite(cfio, 'ps', ps, date, hhmmss(t), rc=rc)
       if ( rc .ne. 0 ) passed = .false.
       call ESMF_CFIOVarWrite(cfio, 'ts', ts, date, hhmmss(t), rc=rc)
       if ( rc .ne. 0 ) passed = .false.
    end do
! Close the file
    call ESMF_CFIOFileClose(cfio)

    if ( passed ) then
       print *, "testw_ex1: Passed"
    else
       print *, "testw_ex1: NOT Passed"
    end if

    stop
   end 
