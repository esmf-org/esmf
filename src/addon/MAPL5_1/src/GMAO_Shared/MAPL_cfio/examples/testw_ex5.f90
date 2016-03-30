    program test
    use ESMF_CFIOGridMOD
    use ESMF_CFIOVarInfoMOD
    use ESMF_CFIOFileMOD
    use ESMF_CFIOSdfMOD
    use ESMF_CFIOMOD

!   define ESMF_CFIO,  ESMF_CFIOVarInfo, and ESMF_CFIOGrid objects
    type(ESMF_CFIO) :: cfio
    type(ESMF_CFIOVarInfo), pointer :: vars(:)
    type(ESMF_CFIOGrid), pointer :: grids(:)

    character(len=20) :: fileName
    real, pointer :: ts(:,:,:), tmpu(:,:,:), u(:,:,:), v(:,:,:), ps(:,:,:)
    integer :: fmode, rc, im=72, jm=46, km=18
    integer :: date, curTime, timeInc, t
    integer :: hhmmss(2) = (/0, 60000/)
    real :: lev(18) = (/100.5145 , 118.2502 , 139.115 , 163.6615 , 192.541 , 226.5135 , &
    266.4789 , 313.4988 , 368.8161 , 433.8927 , 510.4555 , 600.5238 , &
    696.793 , 787.7 , 867.1572 , 929.6454 , 970.5525 , 992.555/)
    real :: range(2) = (/-1.E10, 1.E10/)
    logical :: twoD
    real :: ak(19)=(/291.70,  792.92,  2155.39,  4918.34,  8314.25,      &
               7993.08, 7577.38,  7057.52,  6429.63,  5698.38,      &
               4879.13, 3998.95,  3096.31,  2219.02,  1420.39,      &
               754.13,  268.38,   0.0000,   0.0000 /)
    real :: bk(19)=(/0.0000,    0.0000,    0.0000,   0.0000,   0.0000,   &
                0.0380541, 0.0873088, 0.1489307, 0.2232996,         &
                0.3099406, 0.4070096, 0.5112977, 0.6182465,         &
                0.7221927, 0.8168173, 0.8957590, 0.9533137,         &
                0.9851122, 1.0  /)

    integer :: i
    real :: kappa = 0.28571428571428575
    real :: lon(72), lat(46), dlat, dlon
    character(len=50) :: nameLat
    character :: str
    integer :: ig=2
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
    allocate(grids(3))
    grids(1) = ESMF_CFIOGridCreate(gName='test3d')
    call ESMF_CFIOGridSet(grids(1), im=72, jm=46, lon=lon, lat=lat, lev=lev, levUnit='layer')
    call ESMF_CFIOGridSet(grids(1), ak=ak, bk=bk)
    call ESMF_CFIOGridSet(grids(1),standardName='atmosphere_hybrid_sigma_pressure_coordinate')
    call ESMF_CFIOGridSet(grids(1), coordinate = 'eta')
    call ESMF_CFIOGridSet(grids(1),formulaTerm='a: ak0 b: bk0 ps: ps')

    grids(2) = ESMF_CFIOGridCreate(gName='test3d1')
    call ESMF_CFIOGridSet(grids(2), im=72, jm=46, lon=lon, lat=lat, lev=lev, levUnit='layer')
    call ESMF_CFIOGridSet(grids(2), ak=ak, bk=bk)
    call ESMF_CFIOGridSet(grids(2),standardName='atmosphere_hybrid_sigma_pressure_coordinate')
    call ESMF_CFIOGridSet(grids(2), coordinate = 'eta')
    call ESMF_CFIOGridSet(grids(2),formulaTerm='a: ak1 b: bk1 ps: ps')

    grids(3) = ESMF_CFIOGridCreate(gName='test3d2')
    call ESMF_CFIOGridSet(grids(3), im=72, jm=46, lon=lon, lat=lat, lev=lev, levUnit='layer')
    call ESMF_CFIOGridSet(grids(3), ak=ak, bk=bk)
    call ESMF_CFIOGridSet(grids(3),standardName='atmosphere_hybrid_sigma_pressure_coordinate')
    call ESMF_CFIOGridSet(grids(3), coordinate = 'eta')
    call ESMF_CFIOGridSet(grids(3),formulaTerm='a: ak2 b: bk2 ps: ps')


! Create variable object and set variable attributes
    allocate(vars(5))
    vars(1) = ESMF_CFIOVarInfoCreate(vName='ps')
    call ESMF_CFIOVarInfoSet(vars(1), vName='ps', vTitle='surface pressure', grid=grids(1))
    call ESMF_CFIOVarInfoSet(vars(1), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(1), standardName='ps', twoDimVar=.true.)
    call ESMF_CFIOVarInfoSet(vars(1), validRange=range, vUnits='K')
    call ESMF_CFIOVarInfoSet(vars(1), timAve=.true., cellMthd='mean')
                                                                                   
    vars(2) = ESMF_CFIOVarInfoCreate(vName='ts')
    call ESMF_CFIOVarInfoSet(vars(2),vName='ts',vTitle='skin temperature', grid=grids(1))
    call ESMF_CFIOVarInfoSet(vars(2), standardName='tskin', twoDimVar=.true.)
    call ESMF_CFIOVarInfoSet(vars(2), validRange=range, vUnits='K')
    call ESMF_CFIOVarInfoSet(vars(2), timAve=.true., cellMthd='mean')
                                                                                   
    vars(3) = ESMF_CFIOVarInfoCreate(vName='tmpu')
    call ESMF_CFIOVarInfoSet(vars(3), vName='tmpu', vTitle='temperature', grid=grids(1))
    call ESMF_CFIOVarInfoSet(vars(3), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(3), standardName='tmpu')
    call ESMF_CFIOVarInfoSet(vars(3), validRange=range, vUnits='K')
    call ESMF_CFIOVarInfoSet(vars(3), timAve=.true., cellMthd='mean')
     
    vars(4) = ESMF_CFIOVarInfoCreate(vName='u')
    call ESMF_CFIOVarInfoSet(vars(4), vName='u', vTitle='zonal wind', grid=grids(2))
    call ESMF_CFIOVarInfoSet(vars(4), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(4), standardName='uwnd')
    call ESMF_CFIOVarInfoSet(vars(4), validRange=range, vUnits='m/s')
    call ESMF_CFIOVarInfoSet(vars(4), timAve=.true., cellMthd='mean')

    vars(5) = ESMF_CFIOVarInfoCreate(vName='v')
    call ESMF_CFIOVarInfoSet(vars(5), vName='v', vTitle='V wind', grid=grids(3))
    call ESMF_CFIOVarInfoSet(vars(5), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(5), standardName='vwnd')
    call ESMF_CFIOVarInfoSet(vars(5), validRange=range, vUnits='m/s')
    call ESMF_CFIOVarInfoSet(vars(5), timAve=.true., cellMthd='max')

! Create CFIO object and set global attributes

    cfio =  ESMF_CFIOCreate(cfioObjName='ana')
    call ESMF_CFIOSet(cfio, fName='GEOS5.ana.hdf', varObjs=vars, &
                   grids=grids, date=20011201, BegTime=0, timeInc=60000)
    call ESMF_CFIOSet(cfio, title="c403_cer_01: FVGCM Diagnostics",  &
                   source="Global Modeling and Assimilation Office", &
                   contact="data@gmao.gsfc.nasa.gov")
    call ESMF_CFIOSet(cfio, history='File written by CFIO v1.0.0', &
           convention='ESMF', institution="Global Modeling and Assimilation Office,&
       &     NASA Goddard Space Flight Center, Greenbelt, MD 20771") 
    call ESMF_CFIOSet(cfio, references='see ESMF', comment='First CFIO test version',prec=0) 
!    call ESMF_CFIOSet(cfio, attRealName='ak', attReal=ak )
!    call ESMF_CFIOSet(cfio, attRealName='bk', attReal=bk )

! All metadata are set. Get some metadata to see what in the CFIO Obj.

    call ESMF_CFIOGet(cfio, nVars=nVars, date=date, begTime=curTime, &
                   timeInc=timeInc, fName=fileName)

! Create output file
    call ESMF_CFIOFileCreate(cfio)

! put some test data into the variables
    allocate(tmpu(im,jm,km), ts(im,jm,1),ps(im,jm,1),u(im,jm,km),v(im,jm,km))

  do t = 1, 2
    do j = 1, jm
         do i = 1, im
            ps(i,j,1) = 100000 + (t-1)*cos(2*3.14*lat(j)/180.)*sin(2*3.14*2*lon(i)/360.)
            ts(i,j,1) = 273 - (t-1)*sin(2*3.14*lat(j)/180.)*cos(2*3.14*2*lon(i)/360.)
         enddo
       enddo
                                                                                   
    do k =1, km
     do j = 1, jm
       do i = 1, im
          tmpu(i,j,k) = ts(i,j,1)*(lev(k)/1000.)**kappa
          u(i,j,k) = 10*(t-1)*cos(2*3.14*lat(j)/180.)*sin(2*3.14*lon(i)/360.)*exp(lev(k)/1000.)
          v(i,j,k) = 10*(t-1)*cos(2*3.14*lat(j)/180.)*cos(2*3.14*lon(i)/360.)*exp(lev(k)/1000.)
       enddo
     enddo
    enddo


! Write data to the output file

    call ESMF_CFIOVarWrite(cfio, 'ps', ps,  date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'ts', ts, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'tmpu', tmpu,  date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'u', u, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'v', v, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.

  end do
! Close the file
    call ESMF_CFIOFileClose(cfio)

    if ( passed ) then
       print *, "testw_ex5: Passed"
    else
       print *, "testw_ex5: NOT Passed"
    end if

    stop
   end 
