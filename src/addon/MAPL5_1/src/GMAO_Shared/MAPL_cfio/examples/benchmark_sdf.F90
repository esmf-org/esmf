!
!   Since code for benchmarking CFIO compared to FORTRAN and C flat binaries;
!   see benchmark_bin.F90 and benchmark_fwrite.c.
!

#include "benchmark.h"

    program test

    use ESMF_CFIOMOD

!   define ESMF_CFIO,  ESMF_CFIOVarInfo, and ESMF_CFIOGrid objects

    integer, parameter :: im=IM_, jm=JM_, km=KM_, tm=TM_

    type(ESMF_CFIO) :: cfio
    type(ESMF_CFIOVarInfo), pointer :: vars(:)
    type(ESMF_CFIOGrid), pointer :: grid

    character(len=20) :: fileName
    integer :: i, fmode, rc
    integer :: date, curTime, timeInc, t
    integer :: hhmmss(tm) = (/0, 30000,  60000,  90000, 120000, &
                               150000, 180000, 210000 /)
    logical :: twoD

    integer :: im1, jm1, km1
 
    logical :: passed = .true.

!   variables and coordinates

    real :: ps(im,jm), ts(im,jm), tmpu(im,jm,km) 
    real :: u(im,jm,km), v(im,jm,km), q(im,jm,km) 
    real :: lon(im), lat(jm), lev(km)
    real :: dlat, dlon, sig, dsig

    fmode = 0

!   Define Coordinate variables

    dlat = 180./(jm-1)
    dlon = 360./im
    dsig = 1. / (km-1)
    do i =1, im
      lon(i) = -180 + (i-1)*dlon
    end do
    do j =1, jm
      lat(j) = -90 + (j-1)*dlat
    end do
    do k = 1, km
       sig = (k-1) * dsig
       lev(k) = 0.01 + sig * (1000.-0.01)
    end do

! Create grid and set grid attributes

    allocate(grid)
    grid = ESMF_CFIOGridCreate(gName='test3d')
    call ESMF_CFIOGridSet(grid,im=im,jm=jm,km=km,lon=lon,lat=lat,lev=lev,levUnit='hPa')
    call ESMF_CFIOGridSet(grid, standardName='atmosphere_pressure_coordinate')
    call ESMF_CFIOGridSet(grid, coordinate = 'pressure')

!   read back im, jm, km
    call ESMF_CFIOGridGet(grid, im=im1, jm=jm1, km=km1)

! Create variable object and set variable attributes
    allocate(vars(6))

    vars(1) = ESMF_CFIOVarInfoCreate(vName='ps')
    call ESMF_CFIOVarInfoSet(vars(1), vName='ps', vTitle='surface pressure', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(1), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(1), standardName='ps', twoDimVar=.true.)
    call ESMF_CFIOVarInfoSet(vars(1),  vUnits='K')
                                                                                   
    vars(2) = ESMF_CFIOVarInfoCreate(vName='ts')
    call ESMF_CFIOVarInfoSet(vars(2),vName='ts',vTitle='skin temperature', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(2), standardName='tskin', twoDimVar=.true.)
    call ESMF_CFIOVarInfoSet(vars(2),  vUnits='K')
                                                                                   
    vars(3) = ESMF_CFIOVarInfoCreate(vName='tmpu')
    call ESMF_CFIOVarInfoSet(vars(3), vName='tmpu', vTitle='temperature', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(3), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(3), standardName='tmpu')
    call ESMF_CFIOVarInfoSet(vars(3),  vUnits='K')

    vars(4) = ESMF_CFIOVarInfoCreate(vName='u')
    call ESMF_CFIOVarInfoSet(vars(4), vName='u', vTitle='zonal wind', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(4), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(4), standardName='uwnd')
    call ESMF_CFIOVarInfoSet(vars(4),  vUnits='m/s')

    vars(5) = ESMF_CFIOVarInfoCreate(vName='v')
    call ESMF_CFIOVarInfoSet(vars(5), vName='v', vTitle='V wind', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(5), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(5), standardName='vwnd')
    call ESMF_CFIOVarInfoSet(vars(5),  vUnits='m/s')

    vars(6) = ESMF_CFIOVarInfoCreate(vName='q')
    call ESMF_CFIOVarInfoSet(vars(6), vName='q', vTitle='humidity', grid=grid)
    call ESMF_CFIOVarInfoSet(vars(6), amiss=1.E15, scaleFactor=1., addOffSet=0.)
    call ESMF_CFIOVarInfoSet(vars(6), standardName='sphu')
    call ESMF_CFIOVarInfoSet(vars(6),  vUnits='g/km')

! Create CFIO object and set global attributes

    cfio =  ESMF_CFIOCreate(cfioObjName='ana')
    call ESMF_CFIOSet(cfio, fName='benchmark_out.hdf', varObjs=vars, &
                   grid=grid, date=20011201, BegTime=0, timeInc=30000)
    call ESMF_CFIOSet(cfio, title="c403_cer_01: FVGCM Diagnostics",  &
                   source="Global Modeling and Assimilation Office", &
                   contact="data@gmao.gsfc.nasa.gov")
    call ESMF_CFIOSet(cfio, history='File written by CFIO v1.0.0', &
           convention='ESMF', institution="Global Modeling and Assimilation Office,&
       &     NASA Goddard Space Flight Center, Greenbelt, MD 20771") 
    call ESMF_CFIOSet(cfio, references='see ESMF', comment='First CFIO test version',prec=0) 

! All metadata are set. Get some metadata to see what in the CFIO Obj.

    call ESMF_CFIOGet ( cfio, nVars=nVars, date=date, begTime=curTime, &
                        timeInc=timeInc, fName=fileName)

! Create output file
    call ESMF_CFIOFileCreate(cfio)

! put some test data into the variables

     ps = 1000.
     ts = 273.
     tmpu = 300.
     u = 10.
     v = 5.
     q = 20.

! Write data to the output file

    do t = 1, tm

    call ESMF_CFIOVarWrite(cfio, 'ps', ps, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'ts', ts, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'tmpu', tmpu, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'u', u, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'v', v, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.
    call ESMF_CFIOVarWrite(cfio, 'q', q, date, hhmmss(t), rc=rc)
    if ( rc .ne. 0 ) passed = .false.

  end do

! Close the file
    call ESMF_CFIOFileClose(cfio)

    if ( passed ) then
       print *, "benchmark_sdf: Passed"
    else
       print *, "benchmark_sdf: NOT Passed"
    end if

    stop

   end 
