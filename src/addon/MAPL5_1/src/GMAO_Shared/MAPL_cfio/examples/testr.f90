    program test

    use ESMF_CFIOGridMOD
    use ESMF_CFIOVarInfoMOD
    use ESMF_CFIOFileMOD
    use ESMF_CFIOSdfMOD
    use ESMF_CFIOMOD

! define ESMF_CFIO, ESMF_CFIOVarInfo, and ESMF_CFIOGrid objects
    type(ESMF_CFIO) :: cfio
    type(ESMF_CFIOVarInfo), pointer :: vars(:)
    type(ESMF_CFIOGrid) :: grid

    character(len=256) :: vName
    real, pointer :: tmpu(:,:,:)
    integer :: fmode=0, rc, im, jm, km, date, begTime, timeInc, curTime
    integer :: nSteps, t
    logical :: twoD
    real, pointer :: ts(:,:)  

    integer :: fid
    integer :: argc
    integer, external :: iargc
    logical :: passed = .true.

    character(len=256) :: inFile

    argc = iargc()
    if ( argc < 1 ) then 
       inFile = "GEOS5.ana.hdf"
    else
       call GetArg ( 1, inFile )
    end if

! Create a CFIO object
    cfio =  ESMF_CFIOCreate(cfioObjName='ana')

! Set file name to the cfio obj
    call ESMF_CFIOSet(cfio, fName=inFile)

! open the file
    call ESMF_CFIOFileOpen(cfio, fmode)

! read the data if you know date, time, and variable dimension
!    allocate(ts(72,46,5))
!    call ESMF_CFIOVarRead(cfio, 'tmpu', 20011201, 60000, ts)
!    print *, "ts: ", ts(1,1,1)

! Get meta data
!    call ESMF_CFIOSet(cfio, date=20011201, begTime=0, timeInc=60000)
    call ESMF_CFIOGet(cfio, date=date, begTime=begTime, timeInc=timeInc)
    call ESMF_CFIOGet(cfio, nVars=nVars, nSteps=nSteps)
!    print *, "nVars, date, begTime, timeInc, nSteps: ", nVars, date, begTime, timeInc, nSteps
    allocate(vars(nVars))

!   read variable objects
    call ESMF_CFIOGet(cfio, varObjs=vars)

    do i = 1, nVars
!      get variable name and grid
       call ESMF_CFIOVarInfoGet(vars(i), vName=vName, grid=grid, twoDimVar=twoD)

!      get dimensions 
       call ESMF_CFIOGridGet(grid, im=im, jm=jm, km=km)

       if ( twoD ) then 
          allocate(tmpu(im,jm,1))
       else
         allocate(tmpu(im,jm,km))
       end if

!      read the data
       do t = 1, nSteps
          curTime = begTime + (t-1)*timeInc
          call ESMF_CFIOVarRead(cfio, vName, tmpu, date, curTime, rc=rc)
          if (rc .ne. 0) passed = .false.
       end do
       deallocate(tmpu)

    end do

! close the file
    call ESMF_CFIOFileClose(cfio)

    if ( passed ) then
       print *, "testr_gd: Passed"
    else
       print *, "testr_gd: NOT Passed"
    end if
  end 
