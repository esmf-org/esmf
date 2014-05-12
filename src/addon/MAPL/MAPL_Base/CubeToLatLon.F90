! +-======-+ 
!  Copyright (c) 2003-2007 United States Government as represented by 
!  the Admistrator of the National Aeronautics and Space Administration.  
!  All Rights Reserved.
!  
!  THIS OPEN  SOURCE  AGREEMENT  ("AGREEMENT") DEFINES  THE  RIGHTS  OF USE,
!  REPRODUCTION,  DISTRIBUTION,  MODIFICATION AND REDISTRIBUTION OF CERTAIN 
!  COMPUTER SOFTWARE ORIGINALLY RELEASED BY THE UNITED STATES GOVERNMENT AS 
!  REPRESENTED BY THE GOVERNMENT AGENCY LISTED BELOW ("GOVERNMENT AGENCY").  
!  THE UNITED STATES GOVERNMENT, AS REPRESENTED BY GOVERNMENT AGENCY, IS AN 
!  INTENDED  THIRD-PARTY  BENEFICIARY  OF  ALL  SUBSEQUENT DISTRIBUTIONS OR 
!  REDISTRIBUTIONS  OF THE  SUBJECT  SOFTWARE.  ANYONE WHO USES, REPRODUCES, 
!  DISTRIBUTES, MODIFIES  OR REDISTRIBUTES THE SUBJECT SOFTWARE, AS DEFINED 
!  HEREIN, OR ANY PART THEREOF,  IS,  BY THAT ACTION, ACCEPTING IN FULL THE 
!  RESPONSIBILITIES AND OBLIGATIONS CONTAINED IN THIS AGREEMENT.
!  
!  Government Agency: National Aeronautics and Space Administration
!  Government Agency Original Software Designation: GSC-15354-1
!  Government Agency Original Software Title:  GEOS-5 GCM Modeling Software
!  User Registration Requested.  Please Visit http://opensource.gsfc.nasa.gov
!  Government Agency Point of Contact for Original Software:  
!  			Dale Hithon, SRA Assistant, (301) 286-2691
!  
! +-======-+ 
!  $Id: CubeToLatLon.F90,v 1.2.12.5 2013-06-20 11:05:08 mathomp4 Exp $

#define SUCCESS 0
#define VERIFY_(A) if((A)/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define ASSERT_(A) if(.not.(A)) then; if(present(rc)) rc=1; PRINT *, Iam, __LINE__; return; endif
#define RETURN_(A) if(present(rc)) rc=A; return

#define DEALOC_(A) if(associated(A)) then; call MAPL_DeAllocNodeArray(A,rc=STATUS); if(STATUS==MAPL_NoShm) deallocate(A, stat=STATUS); VERIFY_(STATUS); NULLIFY(A); endif

#define DEALOC2_(A) if(associated(A)) then; deallocate(A, stat=STATUS); VERIFY_(STATUS); NULLIFY(A); endif

#ifdef TAU_PROFILE
#undef ASSERT_
#define ASSERT_(A)

#undef VERIFY_
#define VERIFY_(A)

#undef RETURN_
#define RETURN_(A)
#endif

Module CubeLatLonTransformMod
  
  use ESMF
  use MAPL_BaseMod
  use MAPL_LocStreamMod
  use MAPL_IOMod
  use MAPL_CommsMod
  use MAPL_ShmemMod

  implicit none
  private

  public T_CubeLatLonTransform
  public T_CubeCubeTransform
  public CubeLatLonIsCreated
  public CubeLatLonSubset
  public CubeLatLonCreate
  public CubeLatLonDestroy
  public CubeToLatLon
  public CubeCubeIsCreated
  public CubeCubeCreate
  public CubeCubeDestroy
  public CubeToCube
  public LatLonToCube
  public CartesianToSpherical
  public SphericalToCartesian
  public RunTileTransform
  public MAPL_RegridLSCreate
  public TileTransformTranspose
  public get_conservative_weights !JK patch for conservative interp

  include 'mpif.h'

  character*30 :: Iam="CubeToLatLon in line "

#define R8  8

 type T_CubeLatLonTransform
     private
     real(R8),pointer    :: weight(:,:,:),l2c(:,:,:)  
     integer, pointer    :: index (:,:,:)
     integer, pointer    :: id1(:,:), id2(:,:), jdc(:,:)
     logical             :: Created=.false.
     character(len=120)  :: name
     integer             :: npx, npy, nlon, nlat
     real(R8), pointer   :: ee1(:,:,:) 
     real(R8), pointer   :: ee2(:,:,:) 
     real(R8), pointer   :: ff1(:,:,:) 
     real(R8), pointer   :: ff2(:,:,:) 
     real(R8), pointer   :: gg1(:,:,:) 
     real(R8), pointer   :: gg2(:,:,:) 
     real(R8), pointer   :: elon(:,:,:)
     real(R8), pointer   :: elat(:,:,:)
!
     logical             :: lsCreated = .false.
     type(MAPL_LocStream) :: locStIn
     type(MAPL_LocStream) :: locStOut
     type(MAPL_LocStreamXform) :: XformInOut
     type(MAPL_LocStreamXform) :: XformOutIn
     logical             :: subset = .false.
  end type T_CubeLatLonTransform

 type T_CubeCubeTransform
     private
     real(R8),pointer    :: weight(:,:,:,:)
     integer, pointer    :: index (:,:,:,:)
     logical             :: Created=.false.
     character(len=120)  :: name
     integer             :: npx, npy, npxout, npyout
  end type T_CubeCubeTransform

  interface CubeToLatLon
     module procedure CubeToLatLonr8
     module procedure CubeToLatLonr4
  end interface

  interface LatLonToCube
     module procedure LatLonToCuber8
     module procedure LatLonToCuber4
  end interface

  interface SphericalToCartesian
     module procedure SphericalToCartesianR4
     module procedure SphericalToCartesianR8
  end interface

  interface CartesianToSpherical
     module procedure CartesianToSphericalR4
     module procedure CartesianToSphericalR8
  end interface

  integer, parameter :: ntiles=6
  integer, parameter :: ndims=2
  integer, parameter :: r8=R8
  integer, parameter :: maxstring=120

  real(R8), parameter :: PI=3.14159265358979323846

! This EXTERNAL subroutine is in the fv directory
!  and has real*8 interfaces

  interface
     subroutine GetWeights(npx, npy, nlat, nlon, &
          index, weight, id1, id2, jdc, l2c,     &
          ee1, ee2, ff1, ff2, gg1, gg2, sublons, sublats) 
       integer,  intent(in   ) :: npx,  npy
       integer,  intent(in   ) :: nlon, nlat
       integer,  intent(inout) :: index(3,nlon,nlat)
       real(R8), intent(inout) :: weight(4,nlon,nlat)
       integer,  intent(inout) :: id1(npx,npy)
       integer,  intent(inout) :: id2(npx,npy)
       integer,  intent(inout) :: jdc(npx,npy)
       real(R8), intent(inout) :: l2c(4,npx,npy)
       real(R8), pointer       :: ee1(:,:,:) 
       real(R8), pointer       :: ee2(:,:,:) 
       real(R8), pointer       :: ff1(:,:,:) 
       real(R8), pointer       :: ff2(:,:,:) 
       real(R8), pointer, optional :: gg1(:,:,:) 
       real(R8), pointer, optional :: gg2(:,:,:) 
       real(R8), optional      :: sublons(:)
       real(R8), optional      :: sublats(:)
     end subroutine GetWeights
  end interface

  interface
     subroutine GetWeightsC2C(npx, npy, npxout, npyout, index, weight) 
       integer,  intent(in   ) :: npx,  npy
       integer,  intent(in   ) :: npxout, npyout
       integer,  intent(  out) :: index(:,:,:,:)
       real(R8), intent(  out) :: weight(:,:,:,:)
     end subroutine GetWeightsC2C
  end interface

!JK patch for conservative interp----------------------------
integer , save   :: NT_tiles=-1
integer , allocatable, dimension(:) :: Tile_LL_ii, Tile_LL_jj
integer , allocatable, dimension(:) :: Tile_CS_nx, Tile_CS_ny
real(R8), allocatable, dimension(:) :: Tile_LL_wfrac, Tile_CS_wfrac
real(R8), allocatable, dimension(:) :: Tile_area, Tile_x, Tile_y
logical, save                 :: DO_CONSERVATIVE=.false.
!JK patch for conservative interp----------------------------

contains

  subroutine CubeLatLonDestroy( Trans, rc)
    type(T_CubeLatLonTransform), intent(inout) :: Trans
    integer, optional,           intent(  out) :: rc

    integer :: status

    DEALOC_(Trans%index)
    DEALOC_(Trans%weight)
    DEALOC_(Trans%l2c   )
    DEALOC_(Trans%id1   )
    DEALOC_(Trans%id2   )
    DEALOC_(Trans%jdc   )
    DEALOC_(Trans%elon  )
    DEALOC_(Trans%elat  )
    DEALOC2_(Trans%ee1   )
    DEALOC2_(Trans%ee2   )
    DEALOC2_(Trans%ff1   )
    DEALOC2_(Trans%ff2   )
    DEALOC2_(Trans%gg1   )
    DEALOC2_(Trans%gg2   )

    Trans%Created = .false.

    if (Trans%lsCreated) then
!ALT: if we created LocStream tile transforms we should destroy them
!     unfortunately MAPL does not destroy LocationStreams yet 
    end if

    RETURN_(SUCCESS)
  end subroutine CubeLatLonDestroy

  logical function CubeLatLonIsCreated(Trans)

    type(T_CubeLatLonTransform), intent(in ) :: Trans

    CubeLatLonIsCreated = Trans%Created

  end function CubeLatLonIsCreated

  subroutine CubeLatLonSubset(trans,doSubset)
    logical,                     intent(in ) :: doSubset
    type(T_CubeLatLonTransform)              :: Trans
    trans%subset=dosubset
  end subroutine CubeLatLonSubset

  function CubeLatLonCreate( npx, npy, nlon, nlat, lons, lats, doSubset, rc ) result(Trans)

    integer,                     intent(in ) :: npx,  npy
    integer,                     intent(in ) :: nlon, nlat
    real(R8),                    intent(in ) :: lons(:), lats(:)
    logical,                     intent(in ) :: doSubset
    type(T_CubeLatLonTransform)              :: Trans
    integer, optional,           intent(out) :: rc

! Locals
!-------

    integer :: npts, status, n, l
    integer :: i, j
    real(R8), allocatable :: slon(:), slat(:)
    real(R8), allocatable :: clon(:), clat(:)

! Real*8 are needed to make fv calls.
!-----------------------------------

! Begin
!------

    ASSERT_(.not.Trans%Created)

    npts = npx + 1

    write(Trans%name,'(i5.5,"x",i5.5,"_c2l_",i5.5,"x",i5.5)') npx,npy,nlon,nlat
!    write(*,'(i5.5,"x",i5.5,"_c2l_",i5.5,"x",i5.5)') npx,npy,nlon,nlat

    Trans%npx  = npx
    Trans%npy  = npy
    Trans%nlon = nlon
    Trans%nlat = nlat

  ! allocate storage for weights and indeces for C2L
  !-------------------------------------------------

    DEALOC_(Trans%index)
    DEALOC_(Trans%weight)
    DEALOC_(Trans%l2c   )
    DEALOC_(Trans%id1   )
    DEALOC_(Trans%id2   )
    DEALOC_(Trans%jdc   )
    DEALOC_(Trans%elon  )
    DEALOC_(Trans%elat  )
    DEALOC2_(Trans%ee1   )
    DEALOC2_(Trans%ee2   )
    DEALOC2_(Trans%ff1   )
    DEALOC2_(Trans%ff2   )
    DEALOC2_(Trans%gg1   )
    DEALOC2_(Trans%gg2   )

    call MAPL_AllocNodeArray(Trans%index,(/3,nlon,nlat/),rc=STATUS)
    if(STATUS==MAPL_NoShm) allocate(Trans%index(3,nlon,nlat),stat=status)
    VERIFY_(STATUS)

    call MAPL_AllocNodeArray(Trans%weight,(/4,nlon,nlat/),rc=STATUS)
    if(STATUS==MAPL_NoShm) allocate(Trans%weight(4,nlon,nlat),stat=status)
    VERIFY_(STATUS)

    call MAPL_AllocNodeArray(Trans%l2c,(/4,npx,npy/),rc=STATUS)
    if(STATUS==MAPL_NoShm) allocate(Trans%l2c(4,npx,npy),stat=status)
    VERIFY_(STATUS)

    call MAPL_AllocNodeArray(Trans%id1,(/npx,npy/),rc=STATUS)
    if(STATUS==MAPL_NoShm) allocate(Trans%id1(npx,npy),stat=status)
    VERIFY_(STATUS)

    call MAPL_AllocNodeArray(Trans%id2,(/npx,npy/),rc=STATUS)
    if(STATUS==MAPL_NoShm) allocate(Trans%id2(npx,npy),stat=status)
    VERIFY_(STATUS)

    call MAPL_AllocNodeArray(Trans%jdc,(/npx,npy/),rc=STATUS)
    if(STATUS==MAPL_NoShm) allocate(Trans%jdc(npx,npy),stat=status)
    VERIFY_(STATUS)

    call MAPL_AllocNodeArray(Trans%elon,(/size(lons),size(lats),3/),rc=STATUS)
    if(STATUS==MAPL_NoShm) allocate(Trans%elon(size(lons),size(lats),3),stat=status)
    VERIFY_(STATUS)

    call MAPL_AllocNodeArray(Trans%elat,(/size(lons),size(lats),3/),rc=STATUS)
    if(STATUS==MAPL_NoShm) allocate(Trans%elat(size(lons),size(lats),3),stat=status)
    VERIFY_(STATUS)

    if (doSubSet) then
       call GetWeights(npx, npy, nlat, nlon, Trans%index, Trans%weight, &
            Trans%id1, Trans%id2, Trans%jdc, Trans%l2c,  &
            Trans%ee1, Trans%ee2, Trans%ff1, Trans%ff2, Trans%gg1, Trans%gg2, lons, lats)
    else
       call GetWeights(npx, npy, nlat, nlon, Trans%index, Trans%weight, &
            Trans%id1, Trans%id2, Trans%jdc, Trans%l2c,  &
            Trans%ee1, Trans%ee2, Trans%ff1, Trans%ff2, Trans%gg1, Trans%gg2)
    endif

!cartesian to latlon spherical on latlon grid

       allocate(slat(size(lats)),clat(size(lats)))
       allocate(slon(size(lons)),clon(size(lons)))

       do j=1,size(lats)
          SLAT(j) = SIN(lats(j))
          CLAT(j) = COS(lats(j))
       end do

       do I=1,size(lons)
          SLON(I) = sin(lons(i) - PI)
          CLON(I) = cos(lons(i) - PI)
       end DO

       do j=1,size(lats)
          do I=1,size(lons)
             Trans%elon(I,J,1) = -SLON(I)
             Trans%elon(I,J,2) =  CLON(I)
             Trans%elon(I,J,3) =  0.0
             Trans%elat(I,J,1) = -SLAT(J)*CLON(I)
             Trans%elat(I,J,2) = -SLAT(J)*SLON(I)
             Trans%elat(I,J,3) =  CLAT(J)
          end do
       end do

       deallocate(slon,clon,slat,clat)

    Trans%Created=.true.

    RETURN_(SUCCESS)
  end function CubeLatLonCreate

  subroutine CubeCubeDestroy( Trans, rc)
    type(T_CubeCubeTransform), intent(inout) :: Trans
    integer, optional,         intent(  out) :: rc

    if(associated(Trans%weight)) deallocate(Trans%weight)
    if(associated(Trans%index)) deallocate(Trans%index)

    Trans%Created = .false.


    RETURN_(SUCCESS)
  end subroutine CubeCubeDestroy

  logical function CubeCubeIsCreated(Trans)

    type(T_CubeCubeTransform), intent(in ) :: Trans

    CubeCubeIsCreated = Trans%Created

  end function CubeCubeIsCreated

  function CubeCubeCreate( npx, npy, npxout, npyout, rc ) result(Trans)

    integer,                     intent(in ) :: npx,  npy
    integer,                     intent(in ) :: npxout,  npyout
    type(T_CubeCubeTransform)                :: Trans
    integer, optional,           intent(out) :: rc

! Locals
!-------

    integer :: npts, status, n, l
    integer :: i, j
    integer, parameter :: ntiles=6

! Real*8 are needed to make fv calls.
!-----------------------------------

! Begin
!------

    ASSERT_(.not.Trans%Created)

!ALT    npts = npx + 1
    npts = npxout ! + 1

    write(Trans%name,'(i5.5,"x",i5.5,"_c2l_",i5.5,"x",i5.5)') npx,npy,npxout,npyout

    Trans%npx  = npx
    Trans%npy  = npy
    Trans%npxout  = npxout
    Trans%npyout  = npyout

  ! allocate storage for weights and indeces for C2C
  !-------------------------------------------------

    if(associated(Trans%index )) deallocate(Trans%index )
    if(associated(Trans%weight)) deallocate(Trans%weight)
    
    allocate(Trans%index(3,npts,npts,ntiles),Trans%weight(4,npts,npts,ntiles),stat=status)
    VERIFY_(STATUS)


    call GetWeightsC2C(npx, npy, npxout, npyout, Trans%index, Trans%weight )

       
    Trans%Created=.true.

    RETURN_(SUCCESS)
  end function CubeCubeCreate

  subroutine CubeToCube(Trans, data_cs_in, data_cs_out, rc)

    type(T_CubeCubeTransform),    intent(in )   :: Trans
    real,                     intent(inout) :: data_cs_in(:,:)
    real,                     intent(inout) :: data_cs_out(:,:)
    integer, optional,            intent(out)   :: rc

! Locals
!-------

    integer               :: npx,npy,nlon,nlat
    integer               :: nx,j1,j2,status,itile
    real(R8), allocatable :: var_cs_in(:,:,:), var_cs_out(:,:,:)

    ASSERT_(Trans%Created)

    nx   = Trans%npx

    !--------------------------------------------------------------------!
    ! perform interpolation                                              !
    !--------------------------------------------------------------------!

    allocate ( var_cs_in(0:nx+1,0:nx+1,ntiles),stat=status)
    VERIFY_(STATUS)

    var_cs_in=0.0

    do itile=1,ntiles
       j1 = nx*(itile-1) + 1
       j2 = nx*(itile-1) + nx
       var_cs_in(1:nx,1:nx,itile)=data_cs_in(:,j1:j2)
    end do

    nx   = Trans%npxout
    allocate ( var_cs_out(0:nx+1,0:nx+1,ntiles),stat=status)
    VERIFY_(STATUS)
    var_cs_out=0.0

    call C2CInterp(var_cs_in, var_cs_out, Trans%index, Trans%weight)

    do itile=1,ntiles
       j1 = nx*(itile-1) + 1
       j2 = nx*(itile-1) + nx
       data_cs_out(:,j1:j2) = var_cs_out(1:nx,1:nx,itile)
    end do
       
    deallocate ( var_cs_in, var_cs_out, stat=status)
    VERIFY_(STATUS)

    RETURN_(SUCCESS)
  end subroutine CubeToCube

  subroutine C2CInterp(var_in, var_out, index_c2c, weight_c2c)

    !------------------------------------------------------------------!
    ! do bilinear interpolation from cubed sphere to latlon grid       !
    ! using precalculated weights from get_weight                  !
    !------------------------------------------------------------------!

    real(R8), dimension(0:,0:,:), intent(in out) :: var_in
    real(R8), dimension(0:,0:,:), intent(inout) :: var_out
    real(R8), dimension(:,:,:,:), intent(in   ) :: weight_c2c
    integer,  dimension(:,:,:,:), intent(in   ) :: index_c2c

    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!

    integer           :: i, j, l, ic, jc, lc, nx, ny

    nx   = size(var_out,1)-2
    ny   = size(var_out,2)-2


    call GhostCube(var_in)

    FACES: do l=1,ntiles
       JLOOP: do j=1,ny
          ILOOP: do i=1,nx
             ic=index_c2c(1,i,j,l)
             jc=index_c2c(2,i,j,l)
             lc=index_c2c(3,i,j,l)

             var_out(i,j,l)=weight_c2c(1,i,j,l)*var_in(ic  ,jc  , lc)  &
                           +weight_c2c(2,i,j,l)*var_in(ic  ,jc+1, lc)  &
                           +weight_c2c(3,i,j,l)*var_in(ic+1,jc+1, lc)  &
                           +weight_c2c(4,i,j,l)*var_in(ic+1,jc  , lc)
          enddo ILOOP
       enddo JLOOP
    enddo FACES

    return
  end subroutine C2CInterp


  subroutine get_conservative_weights(name)

    real(R8), allocatable :: FF_LL(:), FF_CS(:), CS_data(:,:), LL_data(:,:)
    character(len=120)  :: name
    integer             :: n, n_temp, nlon_, nlat_, nx_g, ny_g, UNIT
    integer             :: STATUS
    type (ESMF_VM)      :: vm

    DO_CONSERVATIVE = .true.
    call ESMF_VMGetCurrent(vm, rc=status)

!---read the weight fractions and connectivities---------------------------
    UNIT = GETFILE(name, DO_OPEN=0, ALL_PES=.true., RC=STATUS)
    open (UNIT=UNIT, FILE=NAME)
    if ( MAPL_am_I_root() ) read(UNIT,*) NT_tiles, n_temp, n_temp
    if ( MAPL_am_I_root() ) read(UNIT,*) n_temp
    if ( MAPL_am_I_root() ) read(UNIT,*) name
    if ( MAPL_am_I_root() ) read(UNIT,*) nlon_
    if ( MAPL_am_I_root() ) read(UNIT,*) nlat_
    if ( MAPL_am_I_root() ) read(UNIT,*) name
    if ( MAPL_am_I_root() ) read(UNIT,*) nx_g
    if ( MAPL_am_I_root() ) read(UNIT,*) ny_g
    call MAPL_CommsBcast(vm, DATA=NT_tiles, N=1, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=nlon_, N=1, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=nlat_, N=1, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=nx_g , N=1, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=ny_g , N=1, ROOT=0, RC=status)
    allocate (Tile_LL_ii(NT_tiles), Tile_LL_jj(NT_tiles))
    allocate (Tile_CS_nx(NT_tiles), Tile_CS_ny(NT_tiles))
    allocate (Tile_LL_wfrac(NT_tiles), Tile_CS_wfrac(NT_tiles))
    allocate (Tile_area(NT_tiles), Tile_x(NT_tiles), Tile_y(NT_tiles))
    do n=1,NT_tiles
       if ( MAPL_am_I_root() ) then
       read(UNIT,*) n_temp, Tile_area(n),  Tile_x(n),     Tile_y(n),       &
                            Tile_LL_ii(n), Tile_LL_jj(n), Tile_LL_wfrac(n),&
                    n_temp, Tile_CS_nx(n), Tile_CS_ny(n), Tile_CS_wfrac(n),&
                    n_temp 
       endif
    enddo
    call MAPL_CommsBcast(vm, DATA=Tile_LL_ii, N=NT_tiles, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=Tile_LL_jj, N=NT_tiles, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=Tile_CS_nx, N=NT_tiles, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=Tile_CS_ny, N=NT_tiles, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=Tile_LL_wfrac, N=NT_tiles, ROOT=0, RC=status)
    call MAPL_CommsBcast(vm, DATA=Tile_CS_wfrac, N=NT_tiles, ROOT=0, RC=status)
    call FREE_FILE(UNIT)

!---normalizing the weight fractions--------------------------------------
    allocate(LL_data(nlon_, nlat_), CS_data(nx_g, ny_g))
    allocate(FF_CS(nt_tiles), FF_LL(nt_tiles))
    CS_data(:,:)=0.; LL_data(:,:)=0.
    do n=1,ntiles
      CS_data(Tile_CS_nx(n),Tile_CS_ny(n))=&
      CS_data(Tile_CS_nx(n),Tile_CS_ny(n))+Tile_CS_wfrac(n)
      LL_data(Tile_LL_ii(n),Tile_LL_jj(n))=&
      LL_data(Tile_LL_ii(n),Tile_LL_jj(n))+Tile_LL_wfrac(n)
    enddo
    FF_CS=0.; FF_LL=0.
    do n=1,ntiles
      FF_CS(n)=CS_data(Tile_CS_nx(n),Tile_CS_ny(n))
      FF_LL(n)=LL_data(Tile_LL_ii(n),Tile_LL_jj(n))
    enddo
    do n=1,ntiles
      Tile_CS_wfrac(n)=Tile_CS_wfrac(n)/FF_CS(n)
      Tile_LL_wfrac(n)=Tile_LL_wfrac(n)/FF_LL(n)
    enddo
    deallocate(FF_CS, FF_LL)
    deallocate(LL_data, CS_data)

  end subroutine get_conservative_weights


  subroutine CubeToLatLonr8( Trans, data_cs, data_ll, transpose, misval, rc)

    type(T_CubeLatLonTransform),  intent(in )   :: Trans
    real(R8),                     intent(inout) :: data_cs(:,:)
    real(R8),                     intent(inout) :: data_ll(:,:)
    logical, optional,            intent(in )   :: transpose
    real,    optional,            intent(in )   :: misval
    integer, optional,            intent(out)   :: rc

! Locals
!-------

    integer               :: npx,npy,nlon,nlat
    integer               :: nx,j1,j2,status,itile
    real(R8), allocatable :: var_cs(:,:,:)
    real(R8)              :: misval_

    ASSERT_(Trans%Created)

    if(present(misval)) then
       misval_ = misval
    else
       misval_ = 1.0
    end if

    nx   = Trans%npx

    !--------------------------------------------------------------------!
    ! perform interpolation                                              !
    !--------------------------------------------------------------------!

    allocate ( var_cs(0:nx+1,0:nx+1,ntiles),stat=status)
    VERIFY_(STATUS)

    ASSERT_(.not.transpose .or. misval_==1.0)

    var_cs=0.0

    if(.not.transpose) then
       data_ll=0.0
       do itile=1,ntiles
          j1 = nx*(itile-1) + 1
          j2 = nx*(itile-1) + nx
          var_cs(1:nx,1:nx,itile)=data_cs(:,j1:j2)
       enddo
    end if

    call C2LInterp(var_cs, data_ll, Trans%index, Trans%weight,&
                   misval_,  Trans%subset, transpose)

    if(transpose) then
       do itile=1,ntiles
          j1 = nx*(itile-1) + 1
          j2 = nx*(itile-1) + nx
          data_cs(:,j1:j2) = var_cs(1:nx,1:nx,itile)
       enddo
    end if

    deallocate ( var_cs ,stat=status)
    VERIFY_(STATUS)

    RETURN_(SUCCESS)
  end subroutine CubeToLatLonr8

  subroutine CubeToLatLonr4( Trans, data_cs, data_ll, transpose, misval, rc)

    type(T_CubeLatLonTransform),  intent(in )   :: Trans
    real,                         intent(inout) :: data_cs(:,:)
    real,                         intent(inout) :: data_ll(:,:)
    logical, optional,            intent(in )   :: transpose
    real,    optional,            intent(in )   :: misval
    integer, optional,            intent(out)   :: rc

! Locals
!-------

    integer               :: npx,npy,nlon,nlat
    integer               :: nx,j1,j2,status,itile
    real(R8), allocatable :: var_cs(:,:,:), data_ll8(:,:)
    real(R8)              :: misval_

!JK patch for conservative interp---------------
    real(R8), allocatable :: data_cs8(:,:)


    ASSERT_(Trans%Created)

    if(present(misval)) then
       misval_ = misval
    else
       misval_ = 1.0
    end if

    nx   = Trans%npx

    !--------------------------------------------------------------------!
    ! perform interpolation                                              !
    !--------------------------------------------------------------------!

    allocate ( var_cs(0:nx+1,0:nx+1,ntiles),stat=status )
    VERIFY_(STATUS)
    allocate ( data_ll8(size(data_ll,1),size(data_ll,2)),stat=status)
    VERIFY_(STATUS)

    if (DO_CONSERVATIVE) then
       allocate ( data_cs8(size(data_cs,1),size(data_cs,2)),stat=status )
       VERIFY_(STATUS)
    endif

    ASSERT_(.not.transpose .or. misval_==1.0)

    var_cs=0.0

    if(.not.transpose) then
       do itile=1,ntiles
          j1 = nx*(itile-1) + 1
          j2 = nx*(itile-1) + nx
          var_cs(1:nx,1:nx,itile)=data_cs(:,j1:j2)
       enddo
       if (DO_CONSERVATIVE) data_cs8=data_cs  !JK for conservative interp---
    else
       data_ll8=data_ll
    end if

    if (DO_CONSERVATIVE) then  !JK for conservative interp---

    if(.not.transpose) then
      call CToL_interp  &
         (data_ll8, data_cs8, NT_Tiles, Trans%nlat, Trans%nlon, Trans%npx, Trans%npy)
    else    
      call CToL_interp_b&
         (data_ll8, data_cs8, NT_Tiles, Trans%nlat, Trans%nlon, Trans%npx, Trans%npy)
    endif

    else                       !JK for conservative interp---

    call C2LInterp(var_cs, data_ll8, Trans%index, Trans%weight,&
                   misval_,  trans%subset, transpose)

    endif                      !JK for conservative interp---

    if(transpose) then
       do itile=1,ntiles
          j1 = nx*(itile-1) + 1
          j2 = nx*(itile-1) + nx
          data_cs(:,j1:j2) = var_cs(1:nx,1:nx,itile)
       enddo
       if (DO_CONSERVATIVE) data_cs=data_cs8  !JK for conservative interp---
    else
       data_ll=data_ll8
    end if

    if (DO_CONSERVATIVE) deallocate ( data_cs8 ) !JK for conservative interp---

    deallocate ( var_cs, data_ll8 )

    RETURN_(SUCCESS)
  end subroutine CubeToLatLonr4

  subroutine LatLonToCuber8( Trans, data_ll, data_cs, transpose, misval, rc)

    type(T_CubeLatLonTransform),  intent(in )   :: Trans
    real(R8),                     intent(inout) :: data_ll(:,:)
    real(R8),                     intent(inout) :: data_cs(:,:)
    logical, optional,            intent(in )   :: transpose
    real,    optional,            intent(in )   :: misval
    integer, optional,            intent(out)   :: rc

! Locals
!-------

    integer               :: nx,j1,j2,status,itile
    real(R8), allocatable :: var_cs(:,:,:)
    real(R8)              :: misval_

    ASSERT_(Trans%Created)

    if(present(misval)) then
       misval_ = misval
    else
       misval_ = 1.0
    end if

    nx   = Trans%npx

    !--------------------------------------------------------------------!
    ! perform interpolation                                              !
    !--------------------------------------------------------------------!

    allocate ( var_cs(0:nx+1,0:nx+1,ntiles),stat=status)
    VERIFY_(STATUS)

    var_cs=0.

    if(transpose) then
       data_ll=0.
       do itile=1,ntiles
          j1 = nx*(itile-1) + 1
          j2 = nx*(itile-1) + nx
          var_cs(1:nx,1:nx,itile) = data_cs(:,j1:j2)
       enddo
    end if

    call L2CInterp(data_ll, var_cs, Trans%id1,  Trans%id2,  Trans%jdc, &
                   Trans%l2c, misval_, transpose)

    if(.not.transpose) then
       do itile=1,ntiles
          j1 = nx*(itile-1) + 1
          j2 = nx*(itile-1) + nx
          data_cs(:,j1:j2) = var_cs(1:nx,1:nx,itile)
       enddo
    end if

    deallocate ( var_cs ,STAT=STATUS)
    VERIFY_(STATUS)

    RETURN_(SUCCESS)
  end subroutine LatLonToCuber8

  subroutine LatLonToCuber4( Trans, data_ll, data_cs, transpose, misval, rc)

    type(T_CubeLatLonTransform),  intent(in )   :: Trans
    real,                         intent(inout) :: data_ll(:,:)
    real,                         intent(inout) :: data_cs(:,:)
    logical, optional,            intent(in )   :: transpose
    real,    optional,            intent(in )   :: misval
    integer, optional,            intent(out)   :: rc

! Locals
!-------

    integer               :: nx,j1,j2,status,itile
    real(R8)              :: misval_

    real(R8), allocatable :: data_cs8(:,:,:), data_ll8(:,:)
!JK for conservative interp--------------
    real(R8), allocatable :: cs8_data(:,:)
!JK for conservative interp--------------


    ASSERT_(Trans%Created)

    if(present(misval)) then
       misval_ = misval
    else
       misval_ = 1.0
    end if

    nx   = Trans%npx

    allocate ( data_ll8(size(data_ll,1),size(data_ll,2)),stat=status)
    VERIFY_(STATUS)
    allocate ( data_cs8(0:nx+1,0:nx+1,ntiles),stat=status)
    VERIFY_(STATUS)

    if (DO_CONSERVATIVE) then   !JK for conservative interp--------------
      allocate ( cs8_data(size(data_cs,1),size(data_cs,2)),stat=status )
      VERIFY_(STATUS)
    endif

    !--------------------------------------------------------------------!
    ! perform interpolation                                              !
    !--------------------------------------------------------------------!

    data_cs8=0.

    if(.not.transpose) then
       data_ll8 = data_ll
    end if

    if(transpose) then
       data_ll8=0.
       do itile=1,ntiles
          j1 = nx*(itile-1) + 1
          j2 = nx*(itile-1) + nx
          data_cs8(1:nx,1:nx,itile) = data_cs(:,j1:j2)
       enddo

       if (DO_CONSERVATIVE) cs8_data=data_cs !JK for conservative interp---

    end if

    if (DO_CONSERVATIVE) then !JK for conservative interp---

    if(.not.transpose) then
      call LToC_interp &
         (cs8_data, data_ll8, NT_Tiles, Trans%nlat, Trans%nlon, Trans%npx, Trans%npy)
    else
      call LToC_interp_b&
         (cs8_data, data_ll8, NT_Tiles, Trans%nlat, Trans%nlon, Trans%npx, Trans%npy)
    endif

    else                     !JK for conservative interp---

    call L2CInterp(data_ll8, data_cs8, Trans%id1,  Trans%id2,  Trans%jdc, &
                   Trans%l2c, misval_, transpose)

    endif                    !JK for conservative interp---

    if(.not.transpose) then
       do itile=1,ntiles
          j1 = nx*(itile-1) + 1
          j2 = nx*(itile-1) + nx
          data_cs(:,j1:j2) = data_cs8(1:nx,1:nx,itile)
       enddo

       if (DO_CONSERVATIVE) data_cs=cs8_data !JK for conservative interp---

    end if

    if(transpose) then
       data_ll = data_ll8
    end if

    if (DO_CONSERVATIVE) deallocate ( cs8_data )

    deallocate ( data_cs8 )
    deallocate ( data_ll8 )

    RETURN_(SUCCESS)
  end subroutine LatLonToCuber4

  subroutine C2LInterp(cubsph, latlon, index, weight, misval, subset, transpose)

    !------------------------------------------------------------------!
    ! do bilinear interpolation from cubed sphere to latlon grid       !
    ! using precalculated weights from get_weight                  !
    !------------------------------------------------------------------!

    real(R8), dimension(0:,0:,:), intent(inout) :: cubsph
    real(R8), dimension(:,:),     intent(inout) :: latlon
    real(R8), dimension(:,:,:),   intent(in)    :: weight
    integer,  dimension(:,:,:),   intent(in)    :: index
    real(R8),                     intent(in)    :: misval 
    logical,                      intent(in)    :: subset
    logical,                      intent(in)    :: transpose

    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!

    integer           :: i, j, ic, jc, nx, ny, nlon, nlat, tile, ii
    real(R8)          :: ww

    if(transpose .and. misval/=1) then
       print *, 'Trying to do C2L-transpose with missing value'
       return
    end if

    nx   = size(cubsph,1)-2
    ny   = size(cubsph,2)-2


    if(.not.transpose) then
       
       call GhostCube(cubsph)

    else

       cubsph = 0.0

    endif ! not transpose

    nlon = size(latlon,1)
    nlat = size(latlon,2)

    if( (mod(nlon,2)/=0) .and. (.not.subset) ) then
       print *, "NLON not even in cubetolatlon. Stopping."
       stop
    endif

    FACES: do tile = 1,ntiles
       ILOOP: do i=1,nlon
          ! if subsetting do not flip as this can make no sense
          if (.not.subset) then 
             ii = mod(i - 1 + nlon/2,nlon) + 1
          else
             ii = i
          end if
          JLOOP: do j=1,nlat

             HAVE_POINT: if (tile==index(3,i,j)) then

                ic=index(1,i,j)
                jc=index(2,i,j)

                ADJOINT: if(.not.transpose) then
                   UNDEF: if(misval==1.D0) then
                      latlon(ii,j) = weight(1,i,j)*cubsph(ic  ,jc  ,tile)  &
                                   + weight(2,i,j)*cubsph(ic  ,jc+1,tile)  &
                                   + weight(3,i,j)*cubsph(ic+1,jc+1,tile)  &
                                   + weight(4,i,j)*cubsph(ic+1,jc  ,tile)
                   else
                      ww          = 0.0
                      latlon(ii,j) = 0.0

                      if(cubsph(ic  ,jc  ,tile)/=misval) then
                         latlon(ii,j) = latlon(ii,j) + weight(1,i,j)*cubsph(ic  ,jc  ,tile)
                         ww           = ww           + weight(1,i,j)
                      end if

                      if(cubsph(ic  ,jc+1,tile)/=misval) then
                         latlon(ii,j) = latlon(ii,j) + weight(2,i,j)*cubsph(ic  ,jc+1,tile)
                         ww           = ww           + weight(2,i,j)
                      end if

                      if(cubsph(ic+1,jc+1,tile)/=misval) then
                         latlon(ii,j) = latlon(ii,j) + weight(3,i,j)*cubsph(ic+1,jc+1,tile)
                         ww           = ww           + weight(3,i,j)
                      end if

                      if(cubsph(ic+1,jc  ,tile)/=misval) then
                         latlon(ii,j) = latlon(ii,j) + weight(4,i,j)*cubsph(ic+1,jc  ,tile)
                         ww           = ww           + weight(4,i,j)
                      end if

                      if(ww==0.0) then
                         latlon(ii,j) = misval
                      else
                         latlon(ii,j) = latlon(ii,j) / ww
                      end if
                   end if UNDEF

                else  ! Transpose
                   
                   cubsph(ic  ,jc  ,tile)=cubsph(ic  ,jc  ,tile)+weight(1,i,j)*latlon(ii,j)
                   cubsph(ic  ,jc+1,tile)=cubsph(ic  ,jc+1,tile)+weight(2,i,j)*latlon(ii,j)
                   cubsph(ic+1,jc+1,tile)=cubsph(ic+1,jc+1,tile)+weight(3,i,j)*latlon(ii,j)
                   cubsph(ic+1,jc  ,tile)=cubsph(ic+1,jc  ,tile)+weight(4,i,j)*latlon(ii,j)

                end if ADJOINT
             endif HAVE_POINT

          enddo JLOOP
       enddo ILOOP
    end do FACES


    if(transpose) then
       call GhostCubeT(cubsph)
    endif

    return
  end subroutine C2LInterp

  subroutine L2CInterp(latlon, cubsph, id1, id2, jdc, weight, misval, transpose)

    !------------------------------------------------------------------!
    ! do bilinear interpolation from cubed sphere to latlon grid       !
    ! using precalculated weights from get_weight                  !
    !------------------------------------------------------------------!

    real(R8), dimension(0:,0:,:), intent(inout) :: cubsph
    real(R8), dimension(:,:),   intent(inout) :: latlon
    real(R8), dimension(:,:,:), intent(in)    :: weight
    integer,  dimension(:,:),   intent(in)    :: id1, id2, jdc
    real(R8),                   intent(in)    :: misval 
    logical,                    intent(in)    :: transpose

    !------------------------------------------------------------------!
    ! local variables                                                  !
    !------------------------------------------------------------------!

    integer       :: i, j, i1, i2, j1, nx, ny, nz, jx, k, nlon
    real(R8)      :: ww

    nx   = size(cubsph,1)-2
    ny   = size(cubsph,2)-2
    nz   = size(cubsph,3)
    nlon = size(latlon,1)

    if(transpose) latlon = 0.0    

    FACES: do k=1,nz
       FACE_Y: do jx=1,ny
          FACE_X: do i=1,nx

             j  = (k-1)*ny + jx
             j1 = jdc(i,j)

             i1 = mod(id1(i,j)-1+nlon/2,nlon) + 1
             i2 = mod(id2(i,j)-1+nlon/2,nlon) + 1

             ADJOINT: if(.not.transpose) then    

                UNDEF: if(misval==1.D0) then
                   cubsph(i,jx,k) = weight(1,i,j)*latlon(i1,j1  )   &
                                  + weight(2,i,j)*latlon(i2,j1  )   &
                                  + weight(3,i,j)*latlon(i2,j1+1)   &
                                  + weight(4,i,j)*latlon(i1,j1+1)
                else
                   ww          = 0.0
                   cubsph(i,jx,k) = 0.0

                   if(latlon(i1,j1  )/=misval) then
                      cubsph(i,jx,k) = cubsph(i,jx,k) + weight(1,i,j)*latlon(i1,j1  )
                      ww             = ww             + weight(1,i,j)
                   end if
                   if(latlon(i2,j1  )/=misval) then
                      cubsph(i,jx,k) = cubsph(i,jx,k) + weight(2,i,j)*latlon(i2,j1  )
                      ww             = ww             + weight(2,i,j)
                   end if
                   if(latlon(i1,j1+1)/=misval) then
                      cubsph(i,jx,k) = cubsph(i,jx,k) + weight(3,i,j)*latlon(i1,j1+1)
                      ww             = ww             + weight(3,i,j)
                   end if
                   if(latlon(i2,j1+1)/=misval) then
                      cubsph(i,jx,k) = cubsph(i,jx,k) + weight(4,i,j)*latlon(i2,j1+1)
                      ww             = ww             + weight(4,i,j)
                   end if

                   if(ww==0.0) then
                      cubsph(i,jx,k) = misval
                   else
                      cubsph(i,jx,k) = cubsph(i,jx,k) / ww
                   end if

                end if UNDEF

             else

                latlon(i1,j1  ) = latlon(i1,j1  ) + weight(1,i,j)*cubsph(i,jx,k)
                latlon(i2,j1  ) = latlon(i2,j1  ) + weight(2,i,j)*cubsph(i,jx,k)
                latlon(i2,j1+1) = latlon(i2,j1+1) + weight(3,i,j)*cubsph(i,jx,k)
                latlon(i1,j1+1) = latlon(i1,j1+1) + weight(4,i,j)*cubsph(i,jx,k)

             end if ADJOINT

          enddo FACE_X
       enddo FACE_Y
    end do FACES

    return
  end subroutine L2CInterp

  
  subroutine GhostCube(x)
    real(R8), intent(INOUT) :: x(0:,0:,:)

    integer :: nx, ny
    
    nx = size(x,1)-2
    ny = nx

    x(1:nx,0,   1) = x(1:nx,ny,   6)
    x(1:nx,ny+1,1) = x(1,ny:1:-1, 3)
    x(0,1:ny,   1) = x(nx:1:-1,ny,5)
    x(nx+1,1:ny,1) = x(1,1:ny,    2)

    x(1:nx,0,   2) = x(nx,ny:1:-1,6)
    x(1:nx,ny+1,2) = x(1:nx,1,    3)
    x(0,1:ny,   2) = x(nx,1:ny,   1)
    x(nx+1,1:ny,2) = x(nx:1:-1,1, 4)

    x(1:nx,0,   3) = x(1:nx,ny,   2)
    x(1:nx,ny+1,3) = x(1,ny:1:-1, 5)
    x(0,1:ny,   3) = x(nx:1:-1,ny,1)
    x(nx+1,1:ny,3) = x(1,1:ny,    4)

    x(1:nx,0,   4) = x(nx,ny:1:-1,2)
    x(1:nx,ny+1,4) = x(1:nx,1,    5)
    x(0,1:ny,   4) = x(nx,1:ny,   3)
    x(nx+1,1:ny,4) = x(nx:1:-1,1, 6)

    x(1:nx,0,   5) = x(1:nx,ny,   4)
    x(1:nx,ny+1,5) = x(1,ny:1:-1, 1)
    x(0,1:ny,   5) = x(nx:1:-1,ny,3)
    x(nx+1,1:ny,5) = x(1,1:ny,    6)

    x(1:nx,0,   6) = x(nx,ny:1:-1,4)
    x(1:nx,ny+1,6) = x(1:nx,1,    1)
    x(0,1:ny,   6) = x(nx,1:ny,   5)
    x(nx+1,1:ny,6) = x(nx:1:-1,1, 2)

!  Zero corners

    x(0,   ny+1,:) = 0.0
    x(0,      0,:) = 0.0
    x(nx+1,   0,:) = 0.0
    x(nx+1,ny+1,:) = 0.0

  end subroutine GhostCube

  subroutine GhostCubeT(x)
    real(R8), intent(INOUT) :: x(0:,0:,:)

    integer :: nx, ny
    
    nx = size(x,1)-2
    ny = nx

    x(1:nx,ny,   6) = x(1:nx,ny,   6) + x(1:nx,0,   1)
    x(1,ny:1:-1, 3) = x(1,ny:1:-1, 3) + x(1:nx,ny+1,1)
    x(nx:1:-1,ny,5) = x(nx:1:-1,ny,5) + x(0,1:ny,   1)
    x(1,1:ny,    2) = x(1,1:ny,    2) + x(nx+1,1:ny,1)
                                                                            
    x(nx,ny:1:-1,6) = x(nx,ny:1:-1,6) + x(1:nx,0,   2)
    x(1:nx,1,    3) = x(1:nx,1,    3) + x(1:nx,ny+1,2)
    x(nx,1:ny,   1) = x(nx,1:ny,   1) + x(0,1:ny,   2)
    x(nx:1:-1,1, 4) = x(nx:1:-1,1, 4) + x(nx+1,1:ny,2)
                                                                            
    x(1:nx,ny,   2) = x(1:nx,ny,   2) + x(1:nx,0,   3)
    x(1,ny:1:-1, 5) = x(1,ny:1:-1, 5) + x(1:nx,ny+1,3)
    x(nx:1:-1,ny,1) = x(nx:1:-1,ny,1) + x(0,1:ny,   3)
    x(1,1:ny,    4) = x(1,1:ny,    4) + x(nx+1,1:ny,3)
                                                                            
    x(nx,ny:1:-1,2) = x(nx,ny:1:-1,2) + x(1:nx,0,   4)
    x(1:nx,1,    5) = x(1:nx,1,    5) + x(1:nx,ny+1,4)
    x(nx,1:ny,   3) = x(nx,1:ny,   3) + x(0,1:ny,   4)
    x(nx:1:-1,1, 6) = x(nx:1:-1,1, 6) + x(nx+1,1:ny,4)
                                                                            
    x(1:nx,ny,   4) = x(1:nx,ny,   4) + x(1:nx,0,   5)
    x(1,ny:1:-1, 1) = x(1,ny:1:-1, 1) + x(1:nx,ny+1,5)
    x(nx:1:-1,ny,3) = x(nx:1:-1,ny,3) + x(0,1:ny,   5)
    x(1,1:ny,    6) = x(1,1:ny,    6) + x(nx+1,1:ny,5)
                                                                            
    x(nx,ny:1:-1,4) = x(nx,ny:1:-1,4) + x(1:nx,0,   6)
    x(1:nx,1,    1) = x(1:nx,1,    1) + x(1:nx,ny+1,6)
    x(nx,1:ny,   5) = x(nx,1:ny,   5) + x(0,1:ny,   6)
    x(nx:1:-1,1, 2) = x(nx:1:-1,1, 2) + x(nx+1,1:ny,6)

! Zero Halo

    x(   :,   0,:) = 0.0
    x(   :,ny+1,:) = 0.0
    x(   0,   :,:) = 0.0
    x(nx+1,   :,:) = 0.0

  end subroutine GhostCubeT

! Transforms a 2D vector on the surface of the sphere to
! a 3D Cartesian vector or 3D to 2D when (Inverse == True).
! It can also apply the transpose of either operation (Transpose==True).
! It can deal with 2D vectors defined either along Lat-Lon coordinates
! or along Cube-sphere grid lines (Cube==True) as defined by
! the FVCube dynamical core. The transforming coefficients for 
! all of these operations are kept in Trans and are precomputed when
! it is initialized for a particular pair of grids when one grid is cube
! and the other is lat-lon.


  subroutine SphericalToCartesianR4(Tr, U, V, Uxyz, Transpose, SphIsLL)
    type(T_CubeLatLonTransform), intent(IN ) :: Tr
    real,                        intent(IN ) :: U(:,:,:), V(:,:,:)
    real,                        intent(OUT) :: Uxyz(:,:,:)
    logical,                     intent(IN ) :: Transpose
    logical,                     intent(IN ) :: SphIsLL

    integer           :: K, LM
    real(R8), pointer :: e1(:,:,:), e2(:,:,:) 

    if(SphIsLL) then
       e1=>Tr%elon
       e2=>Tr%elat
    else
       if(.not.Transpose) then
          e1=>Tr%ff1
          e2=>Tr%ff2
       else
          e1=>Tr%ee1
          e2=>Tr%ee2
       end if
    end if

    LM = size(U,3)

    do k=1,LM
       Uxyz(:,:,k     ) = U(:,:,k)*e1(:,:,1) + V(:,:,k)*e2(:,:,1)
       Uxyz(:,:,k+  LM) = U(:,:,k)*e1(:,:,2) + V(:,:,k)*e2(:,:,2)
       Uxyz(:,:,k+2*LM) = U(:,:,k)*e1(:,:,3) + V(:,:,k)*e2(:,:,3)
    end do

    return
  end subroutine SphericalToCartesianR4

  subroutine SphericalToCartesianR8(Tr, U, V, Uxyz, Transpose, SphIsLL)
    type(T_CubeLatLonTransform), intent(IN ) :: Tr
    real(R8),                    intent(IN ) :: U(:,:,:), V(:,:,:)
    real(R8),                   intent(OUT) :: Uxyz(:,:,:)
    logical,                     intent(IN ) :: Transpose
    logical,                     intent(IN ) :: SphIsLL

    integer           :: K, LM
    real(R8), pointer :: e1(:,:,:), e2(:,:,:) 

    if(SphIsLL) then
       e1=>Tr%elon
       e2=>Tr%elat
    else
       if(.not.Transpose) then
          e1=>Tr%ff1
          e2=>Tr%ff2
       else
          e1=>Tr%ee1
          e2=>Tr%ee2
       end if
    end if

    LM = size(U,3)

    do k=1,LM
       Uxyz(:,:,k     ) = U(:,:,k)*e1(:,:,1) + V(:,:,k)*e2(:,:,1)
       Uxyz(:,:,k+  LM) = U(:,:,k)*e1(:,:,2) + V(:,:,k)*e2(:,:,2)
       Uxyz(:,:,k+2*LM) = U(:,:,k)*e1(:,:,3) + V(:,:,k)*e2(:,:,3)
    end do

    return
  end subroutine SphericalToCartesianR8

  subroutine CartesianToSphericalR4(Tr, Uxyz, U, V, Transpose, SphIsLL, Rotate, RC)
    type(T_CubeLatLonTransform), intent(IN ) :: Tr
    real,                        intent(OUT) :: U(:,:,:), V(:,:,:)
    real,                        intent(IN ) :: Uxyz(:,:,:)
    logical,                     intent(IN ) :: Transpose
    logical,                     intent(IN ) :: SphIsLL
    logical, optional,           intent(IN ) :: Rotate
    integer, optional,           intent(OUT) :: RC

    logical           :: Rotate_
    integer           :: K, LM
    real(R8), pointer :: e1(:,:,:), e2(:,:,:) 

    Rotate_ = .true.
    if(present(Rotate)) then
       if(.not.Rotate) then
          ASSERT_(.not.Transpose .and. .not.SphIsLL)
          Rotate_ = Rotate
       end if
    end if

    if(SphIsLL) then
       e1=>Tr%elon
       e2=>Tr%elat
    else
       if(.not.Rotate_) then
          e1=>Tr%gg1
          e2=>Tr%gg2
       elseif(Transpose) then
          e1=>Tr%ff1
          e2=>Tr%ff2
       else
          e1=>Tr%ee1
          e2=>Tr%ee2
       end if
    end if

    LM = size(U,3)
    
    do k=1,LM
       U(:,:,k) = Uxyz(:,:,k     )*e1(:,:,1) + &
                  Uxyz(:,:,k+  LM)*e1(:,:,2) + &
                  Uxyz(:,:,k+2*LM)*e1(:,:,3)

       V(:,:,k) = Uxyz(:,:,k     )*e2(:,:,1) + &
                  Uxyz(:,:,k+  LM)*e2(:,:,2) + &
                  Uxyz(:,:,k+2*LM)*e2(:,:,3)
    end do

    return
  end subroutine CartesianToSphericalR4

  subroutine CartesianToSphericalR8(Tr, Uxyz, U, V, Transpose, SphIsLL, Rotate, RC)
    type(T_CubeLatLonTransform), intent(IN ) :: Tr
    real(R8),                    intent(OUT) :: U(:,:,:), V(:,:,:)
    real(R8),                    intent(IN ) :: Uxyz(:,:,:)
    logical,                     intent(IN ) :: Transpose
    logical,                     intent(IN ) :: SphIsLL
    logical, optional,           intent(IN ) :: Rotate
    integer, optional,           intent(OUT) :: RC

    logical           :: Rotate_
    integer           :: K, LM
    real(R8), pointer :: e1(:,:,:), e2(:,:,:) 

    Rotate_ = .true.
    if(present(Rotate)) then
       if(.not.Rotate) then
          ASSERT_(.not.Transpose .and. .not.SphIsLL)
          Rotate_ = Rotate
       end if
    end if

    if(SphIsLL) then
       e1=>Tr%elon
       e2=>Tr%elat
    else
       if(.not.Rotate_) then
          e1=>Tr%gg1
          e2=>Tr%gg2
       elseif(Transpose) then
          e1=>Tr%ff1
          e2=>Tr%ff2
       else
          e1=>Tr%ee1
          e2=>Tr%ee2
       end if
    end if

    LM = size(U,3)
    
    do k=1,LM
       U(:,:,k) = Uxyz(:,:,k     )*e1(:,:,1) + &
                  Uxyz(:,:,k+  LM)*e1(:,:,2) + &
                  Uxyz(:,:,k+2*LM)*e1(:,:,3)

       V(:,:,k) = Uxyz(:,:,k     )*e2(:,:,1) + &
                  Uxyz(:,:,k+  LM)*e2(:,:,2) + &
                  Uxyz(:,:,k+2*LM)*e2(:,:,3)
    end do

    return
  end subroutine CartesianToSphericalR8

! create LS and grindIn
! create LS and grindOut
! create Xform
  subroutine MAPL_RegridLSCreate(TR, GridIn, GridOut, TILINGFILE, RC)
    type(T_CubeLatLonTransform), intent(INOUT) :: Tr
    type(ESMF_Grid),             intent(INout) :: GridIn
    type(ESMF_Grid),             intent(INout) :: GridOut
    character(len=*),            intent(IN   ) :: TILINGFILE
    integer, optional,           intent(  OUT) :: RC


    type(MAPL_LocStream) :: LocStIn
    type(MAPL_LocStream) :: LocStOut
    type(ESMF_DistGrid)  :: distgrid
    type(ESMF_DELayout)  :: layout
    integer :: status


    call ESMF_GridGet(GridIn, DistGrid=distgrid, rc=status)
    VERIFY_(STATUS)
    call ESMF_DistGridGet(distGRID, deLayout=layout, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_LocStreamCreate(locStIn, LAYOUT=layout, FILENAME=TILINGFILE, &
                              NAME='LocStIn',                           &
                              grid=GridIn, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_LocStreamCreate(locStOut, LAYOUT=layout, FILENAME=TILINGFILE, &
                              NAME='LocStOut',                           &
                              grid=GridOut, RC=STATUS)
    VERIFY_(STATUS)

   call MAPL_LocStreamCreateXform ( XFORM=TR%XFORMinout, &
                                    LocStreamOut=locStOut, &
                                    LocStreamIn=locstIn, &
                                    NAME='XFORM_InOut', &
                                    RC=STATUS )
   VERIFY_(STATUS)

   call MAPL_LocStreamCreateXform ( XFORM=TR%XFORMoutin, &
                                    LocStreamOut=locStIn, &
                                    LocStreamIn=locstOut, &
                                    NAME='XFORM_OutIn', &
                                    RC=STATUS )
   VERIFY_(STATUS)

   TR%lsCreated = .true.
   TR%locStIn = locStIn
   TR%locStOut = locStOut

   RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_RegridLSCreate


  subroutine RunTileTransform(TR, PTR2d_IN, PTR2d_OUT, TRANSPOSE, RC)
    type(T_CubeLatLonTransform), target, intent(IN ) :: Tr
    real                                     :: ptr2d_in(:,:)
    real                                     :: ptr2d_out(:,:)
    logical                                  :: transpose
    integer, optional        , intent(  OUT) :: RC
    

! Errlog vars
    integer :: status

! Local vars
    type(MAPL_LocStreamXform), pointer :: XFORM
    type(MAPL_LocStream),      pointer :: LS_IN, LS_OUT
    integer                            :: NTILES_IN, NTILES_OUT
    real, allocatable, dimension(:)    :: tile_in, tile_out

    LS_IN => TR%LocStIn
    LS_OUT => TR%LocStOut
    XFORM => TR%XFORMinout

! query ntiles
    call MAPL_LocStreamGet(LS_In, NT_LOCAL = ntiles_in, rc=status)
    VERIFY_(STATUS)
    call MAPL_LocStreamGet(LS_Out, NT_LOCAL = ntiles_out, rc=status)
    VERIFY_(STATUS)

    allocate(tile_in (ntiles_in ), stat=status)
    VERIFY_(STATUS)
    allocate(tile_out(ntiles_out), stat=status)
    VERIFY_(STATUS)

    if (.not.transpose) then
! forward run
! G2T
       call MAPL_LocStreamTransform(LS_IN, TILE_IN, PTR2d_IN, RC=STATUS)
       VERIFY_(STATUS)
! T2T
       call MAPL_LocStreamTransform(tile_out, XFORM, tile_in, RC=STATUS ) 
       VERIFY_(STATUS)
! T2G
       call MAPL_LocStreamTransform(LS_OUT, PTR2d_OUT, TILE_OUT, RC=STATUS)
       VERIFY_(STATUS)
    else 
! adjoint run
! T2G adj
       call MAPL_LocStreamTransform(LS_IN, PTR2d_IN, TILE_IN, &
            TRANSPOSE=transpose, RC=STATUS)
       VERIFY_(STATUS)
! T2T adj
       call MAPL_LocStreamTransform(tile_out, XFORM, tile_in, RC=STATUS ) 
       VERIFY_(STATUS)
! G2T adj
       call MAPL_LocStreamTransform(LS_OUT, TILE_OUT, PTR2d_OUT, &
            TRANSPOSE=transpose, RC=STATUS)
       VERIFY_(STATUS)
    endif

    deallocate(tile_out)
    deallocate(tile_in )

    RETURN_(ESMF_SUCCESS)
  end subroutine RunTileTransform

  subroutine TileTransformTranspose(TR, RC)
    type(T_CubeLatLonTransform), intent(INOUT) :: TR
    integer, optional          , intent(  OUT) :: RC

    integer :: status
    type(MAPL_LocStream) :: ls
    type(MAPL_LocStreamXform) :: Xform

    TR%lsCreated = .false.

    ls = TR%locStIn
    TR%locStIn = TR%locStOut
    TR%locStOut = ls

    xform = TR%xformInOut
    TR%xformInOut = TR%xformOutIn
    TR%xformOutIn = xform

    RETURN_(ESMF_SUCCESS)
  end subroutine TileTransformTranspose

subroutine LToC_interp(CS_data, LL_data, NT_Tiles, nlat, nlon, nxg, nyg)
integer n, NT_Tiles, nlat, nlon, nxg, nyg
real (R8) :: LL_data(nlon,nlat), CS_data(nxg,nyg)
real (R8), allocatable :: T_data(:)
allocate(T_data(NT_tiles))
!LL2Tile-----------------------------------------------------
do n=1,NT_tiles
  T_data(n)=LL_data(Tile_LL_ii(n),Tile_LL_jj(n))
enddo
!Tile2Cube---------------------------------------------------
CS_data(:,:)=0.
do n=1,NT_tiles
  CS_data(Tile_CS_nx(n),Tile_CS_ny(n))=CS_data(Tile_CS_nx(n),Tile_CS_ny(n))+&
                                       T_data(n)*Tile_CS_wfrac(n)
enddo
deallocate(T_data)
end subroutine LToC_interp

subroutine CToL_interp(LL_data, CS_data, NT_Tiles, nlat, nlon, nxg, nyg)
integer n, NT_Tiles, nlat, nlon, nxg, nyg
real (R8) :: LL_data(nlon,nlat), CS_data(nxg,nyg)
real (R8), allocatable :: T_data(:)

allocate(T_data(NT_tiles))
!Cube2Tile---------------------------------------------------
T_data=0.
do n=1,NT_tiles
  T_data(n)=CS_data(Tile_CS_nx(n),Tile_CS_ny(n))
enddo
!Tile2LL-----------------------------------------------------
LL_data(:,:)=0.
do n=1,NT_tiles
  LL_data(Tile_LL_ii(n),Tile_LL_jj(n))=LL_data(Tile_LL_ii(n),Tile_LL_jj(n))+&
                                       T_data(n)*Tile_LL_wfrac(n)
enddo
deallocate(T_data)
end subroutine CToL_interp
SUBROUTINE LToC_INTERP_B(cs_datab, ll_datab, NT_tiles, nlat, nlon, nxg, nyg)
  IMPLICIT NONE
  INTEGER :: NT_tiles, nlat, nlon, nxg, nyg
  REAL (R8) :: ll_datab(nlon, nlat), cs_datab(nxg, nyg)
  REAL (R8) :: t_datab(NT_tiles)
  INTEGER :: n
  t_datab = 0.0
  DO n=1,NT_tiles
    t_datab(n)=t_datab(n)+tile_cs_wfrac(n)*cs_datab(tile_cs_nx(n),tile_cs_ny(n))
  END DO
  ll_datab = 0.0
  DO n=1,NT_tiles
    ll_datab(tile_ll_ii(n),tile_ll_jj(n))=ll_datab(tile_ll_ii(n),tile_ll_jj(n))+&
                                          t_datab(n)
    t_datab(n) = 0.0
  END DO
  cs_datab = 0.0
END SUBROUTINE LToC_INTERP_B
SUBROUTINE CToL_INTERP_B(ll_datab, cs_datab, NT_tiles, nlat, nlon, nxg, nyg)
  IMPLICIT NONE
  INTEGER :: NT_tiles, nlat, nlon, nxg, nyg
  REAL (R8) :: ll_datab(nlon, nlat), cs_datab(nxg, nyg)
  REAL (R8) :: t_datab(NT_tiles)
  INTEGER :: n
  t_datab = 0.0
  DO n=1,NT_tiles
    t_datab(n)=t_datab(n)+tile_ll_wfrac(n)*ll_datab(tile_ll_ii(n),tile_ll_jj(n))
  END DO
  cs_datab = 0.0
  DO n=1,NT_tiles
    cs_datab(tile_cs_nx(n),tile_cs_ny(n))=cs_datab(tile_cs_nx(n),tile_cs_ny(n))+&
                                          t_datab(n)
    t_datab(n) = 0.0
  END DO
  ll_datab = 0.0
END SUBROUTINE CToL_INTERP_B

end Module CubeLatLonTransformMod
