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

#define VERIFY_(A)   if(  A/=0) then; if(present(rc)) rc=A; PRINT *, Iam, __LINE__; return; endif
#define ASSERT_(A)   if(.not.A) then; if(present(rc)) rc=1; PRINT *, Iam, __LINE__; return; endif
#define NEGASSERT_(A)   if(A) then; if(present(rc)) rc=1; PRINT *, Iam, __LINE__; return; endif
#define RETURN_(A)   if(present(rc)) rc=A; return
#define SUCCESS      0
#define DEALOC_(A)   if(associated(A)) deallocate(A)
#define DEALOCS_(A) if(associated(A)) then;A=0;call MAPL_DeAllocNodeArray(A,rc=STATUS);if(STATUS==MAPL_NoShm) deallocate(A);NULLIFY(A);endif


#define R8  8

#ifdef TAU_PROFILE
#undef ASSERT_
#define ASSERT_(A)

#undef VERIFY_
#define VERIFY_(A)

#undef RETURN_
#define RETURN_(A)
#endif

!BOP

! !MODULE: MAPL_HorzTransMod
!    A Module to do linear transformations on 2-dimensional arrays


! !INTERFACE:

module MAPL_HorzTransformMod

!  $Id: MAPL_HorzTransform.F90,v 1.12.12.6.4.1.4.4.4.2 2014-02-06 20:52:20 atrayano Exp $


  use ESMF
  use MAPL_BaseMod
  use MAPL_CommsMod
  use MAPL_IOMod
  use MAPL_LocStreamMod
  use CubeLatLonTransformMod
  use MAPL_ShmemMod
  use MAPL_CommsMod

  implicit none
  private

! PUBLIC TYPES:

  public MAPL_HorzTransform

! PUBLIC MEMBER FUNCTIONS:

  public MAPL_HorzTransformIsCreated
  public MAPL_HorzTransformCreate
  public MAPL_HorzTransformTranspose
  public MAPL_HorzTransformDestroy
  public MAPL_HorzTransformRun
  public MAPL_HorzTransformIsTranspose
  public MAPL_HorzTransformGet
  public MAPL_HorzTransformSet

  public MAPL_DimTopoCyclic
  public MAPL_DimTopoEdge
  public MAPL_DimTopoCenter
  public Get_conservative_transform !JK patch for conservative transform

!  public MAPL_RegridConserv
!  public MAPL_RegridConservativeCreate
!  public MAPL_RegridConservativeDestroy
!  public MAPL_RegridConservativeRun

! !DESCRIPTION:

! This package performs a serial linear transformation with a
!   previously computed sparse matrix stored in a MAPL_HorzBinTransform
!   object.  Currently, the Transform objects that can be created
!   are limited to a conservative ``binning'',
!   bilinear intepolation, or biquadratic interpolation
!   of uniform-resolution, 2-dimensional
!   (LogicalRectangular) grids that span the same domain. 
!
! Either dimension of the array can be cyclic or bounded. If bounded
!   the grids can have either an edge or a center at the domain boundary,
!   the only difference being that an "edge" boundary does not allow
!   any extrapolation on input values below the first or above the last
!   point. Edge is useful only for meridional interpolation between the
!   two poles.
!   
! To use the pachage, a transform is first created using {\bf MAPL_HorzTransformCreate} and
!   then used repeatedly by passing it to the interpolation routine {\bf MAPL_HorzTransformRun}
!   together with input and output arrays. When a transfor is no longer needed, it can be
!   destroyed with {\bf MAPL_HorzTransformDestroy}. 
!
! In addition to these three methods and the type of the class object {\bf MAPL_HorzTransform},
!   the only puclic quantities are the allowed values of an enumeration that   
!   specifies the 'topology' of a dimension (cyclic, edge, or centered).
!   
! The package is not parallelized, so all the data must be on a single processor. It  
!   does not rely on an other modules or libraries.
!   
!EOP

! Topology enumeration:
!   Edge and Center refer to the location of the 
!   first and last values in a dimension. The distance between points
!   is always the same, but with edge the first and last points represent 
!   only half a grid box, as with the meridional dimension in the grid
!   used by FV.

  integer, parameter :: MAPL_DimTopoCyclic = 0
  integer, parameter :: MAPL_DimTopoEdge   = -1
  integer, parameter :: MAPL_DimTopoCenter = 1

! The code is mostly general for N dimensions, but in fact works only
! for transformations of 2-dimensional arrays.

  integer, parameter :: NUMDIMS = 2
  character*30 :: Iam="MAPL_HorzTransform in line "

  type Weights
     real, pointer :: f(:)
  end type Weights

  type Mapping
     type(Weights), pointer :: WeightList(:)
  end type Mapping

  type LocStream_GridInfo
     private
     character(ESMF_MAXSTR) :: GridName
     integer                :: IM
     integer                :: JM
  end type LocStream_GridInfo

  type TileInfo
     private
     integer, pointer       :: II(:) => null()
     integer, pointer       :: JJ(:) => null()
     real,    pointer       :: W (:) => null()
  end type TileInfo

  type TileTrans
     private
     type(TileInfo)         :: IN
     type(TileInfo)         :: OUT
  end type TileTrans

  type GlobalTileTrans
     private
     integer                  :: NT
     integer, pointer         :: tilesPerDE(:) => NULL()
     type(LocStream_GridInfo) :: GInfo(2)
     type(TileTrans)          :: LOCAL
     type(TileTrans)          :: GLOBAL
  end type GlobalTileTrans

  type DistributedTileTrans
     private
     logical             :: lsCreated = .false.
     type(MAPL_LocStream) :: locStIn
     type(MAPL_LocStream) :: locStOut
     type(MAPL_LocStreamXform) :: XformInOut
     type(MAPL_LocStreamXform) :: XformOutIn
  end type DistributedTileTrans

  type MAPL_RegridConserv
     private
     logical                :: created=.false.
     logical                :: runDistributed=.false.
     logical                :: tileDataGlobal=.false.
     character(ESMF_MAXSTR) :: TileFile
     type(GlobalTileTrans)  :: GlobalTrans
!ALT: to be incorporated     type(DistributedTileTrans) :: DistTrans
  end type MAPL_RegridConserv

  type MAPL_HorzTransform
     private
     logical                :: created=.false.
     integer                :: Order
     integer                :: N_in  (NUMDIMS), N_out  (NUMDIMS)
     integer                :: topoIN(NUMDIMS), topoOUT(NUMDIMS)
     real                   :: XmaxIn(NUMDIMS), XmaxOut(NUMDIMS)
     real                   :: XminIn(NUMDIMS), XminOut(NUMDIMS)
     type(Mapping)          :: DimMapping(NUMDIMS)
     character(len=64)      :: gridtypeIN, gridtypeOUT
     type(ESMF_Grid)        :: GridIn, GridOut
     logical                :: Parallel=.false.
     logical                :: Transpose=.false.
     logical                :: runTile=.false.
     type(T_CubeLatLonTransform) :: CubeTrans
     type(T_CubeCubeTransform)   :: C2CTrans
     type(MAPL_RegridConserv) :: ConsrvTrans
     integer                 :: subset(4)
  end type MAPL_HorzTransform

!==========

  interface MAPL_HorzTransformCreate
     module procedure MAPL_HorzTransformCreateBySize
     module procedure MAPL_HorzTransformCreateGEOS
     module procedure MAPL_HorzTransformCreateFromGrids
     module procedure MAPL_HorzTransformCreateConservative
  end interface

  interface MAPL_HorzTransformRun
     module procedure MAPL_HorzTransformRun2
     module procedure MAPL_HorzTransformRun3
     module procedure MAPL_HorzTransformRunV3
#ifdef OVERLOAD_R8
     module procedure MAPL_HorzTransformRun2R8
     module procedure MAPL_HorzTransformRun3R8
     module procedure MAPL_HorzTransformRunV3R8
#endif
     module procedure MAPL_HorzTransformRunParallelFromFields
  end interface

  real, parameter :: wc = 0.7

! Static list of previously created transforms

  integer, parameter :: MAX_AvailableTransforms=20
  type (MAPL_HorzTransform) :: AvailableTransforms(MAX_AvailableTransforms)

contains

!==============================================================

  logical function MAPL_HorzTransformIsCreated(Trans)
    type (MAPL_HorzTransform), intent(IN ) :: Trans

    MAPL_HorzTransformIsCreated = Trans%created

  end function MAPL_HorzTransformIsCreated

!==============================================================

  logical function MAPL_HorzTransformIsTranspose(Trans)
    type (MAPL_HorzTransform), intent(IN ) :: Trans

    MAPL_HorzTransformIsTranspose = Trans%Transpose

  end function MAPL_HorzTransformIsTranspose

!==============================================================

  subroutine MAPL_HorzTransformGet  (Trans,GridTypeIn, GridTypeOut, order)

    type (MAPL_HorzTransform), intent(INout) :: Trans
    character*(*), optional :: GridTypeIn, GridTypeOut
    integer, optional,         intent(  OUT) :: order

    if(present(GridTypeIn )) GridTypeIn  = Trans%GridTypeIn
    if(present(GridTypeOut)) GridTypeOut = Trans%GridTypeOut
    if(present(order)) order = Trans%order

  end subroutine MAPL_HorzTransformGet

!==============================================================

  subroutine MAPL_HorzTransformSet  (Trans, order, rc)

    type (MAPL_HorzTransform), intent(INout) :: Trans
    integer, optional,         intent(IN   ) :: order
    integer, optional,         intent(  OUT) :: rc

    if (present(order)) then
       ASSERT_(Trans%runTile)
       Trans%order = order
    end if

  end subroutine MAPL_HorzTransformSet

!==============================================================

  type (MAPL_HorzTransform) function MAPL_HorzTransformTranspose(Trans, rc) &
       result(Transpose)
    type (MAPL_HorzTransform), intent(IN   ) :: Trans
    integer, optional,         intent(  OUT) :: rc

    integer :: N(2)
    type(ESMF_GRID) :: Grid
    character*(60) :: GridType

    ASSERT_(CubeLatLonIsCreated(Trans%CubeTrans)) ! Only Cube can be transposed
    ASSERT_((.not.Trans%Created))      ! Only Cube can be transposed

    Transpose           = Trans
    Transpose%Transpose = .not.Transpose%Transpose

    N = Trans%N_Out
    Transpose%N_Out = Transpose%N_In
    Transpose%N_In  = N

    GridType = Transpose%GridTypeOut
    Transpose%GridTypeOut = Transpose%GridTypeIn
    Transpose%GridTypeIn  = GridType

    Grid = Transpose%GridOut
    Transpose%GridOut = Transpose%GridIn
    Transpose%GridIn  = Grid

    if (Transpose%runTile) then
       call TileTransformTranspose(Transpose%CubeTrans, RC)
    end if

  end function MAPL_HorzTransformTranspose

!==============================================================

  subroutine MAPL_HorzTransformCreateFromGrids  (Trans, &
                                       Gridin, Gridout, &
                                       XYOFFSET, Order, &
                                       TILINGFILE,      &
                                                      rc)

    type (MAPL_HorzTransform), intent(OUT) :: Trans
    type (ESMF_Grid)         , intent(INout ) :: gridin, gridout
    integer, optional,    intent(IN ) :: XYOFFSET
    integer, optional,    intent(IN ) :: Order
    character(len=*), optional, intent(IN) :: TILINGFILE    
    integer, optional,    intent(OUT) :: rc

    integer        :: im_out,jm_out,im_in,jm_in,DIMS(5)
    character*(60) :: GridTypeIn,GridTypeOut
    integer        :: STATUS
    real(R8), pointer     :: centerX(:,:)
    real(R8), pointer     :: centerY(:,:)


    call MAPL_GridGet(Gridin , globalCellCountPerDim=DIMS, rc=STATUS)
    VERIFY_(STATUS)
    im_in = DIMS(1)
    jm_in = DIMS(2)
    call MAPL_GridGet(Gridout, globalCellCountPerDim=DIMS, rc=STATUS)
    VERIFY_(STATUS)
    im_out = DIMS(1)
    jm_out = DIMS(2)

    call ESMF_AttributeGet(GRIDin , name="GridType", value=GridTypeIn, RC=STATUS)
    if (STATUS /= ESMF_SUCCESS) then
       GridTypeIn = 'UNKNOWN'
    endif
    call ESMF_AttributeGet(GRIDout, name="GridType", value=GridTypeOut, RC=STATUS)
    if (STATUS /= ESMF_SUCCESS) then
       GridTypeOut = 'UNKNOWN'
    endif

    if    ( GridTypeIn =='Cubed-Sphere') then
       call ESMF_GridGetCoord (GridOut, coordDim=1, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=centerX, rc=status)
       VERIFY_(STATUS)
   
       call ESMF_GridGetCoord (GridOut, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=centerY, rc=status)
       VERIFY_(STATUS)

       Trans%CubeTrans =  CubeLatLonCreate( im_in, jm_in, im_out, jm_out, &
            centerx(:,1), centery(1,:), .false., rc=STATUS )
       VERIFY_(STATUS)
    elseif( GridTypeOut =='Cubed-Sphere') then
       call ESMF_GridGetCoord (GridIn, coordDim=1, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=centerX, rc=status)
       VERIFY_(STATUS)
   
       call ESMF_GridGetCoord (GridIn, coordDim=2, localDE=0, &
            staggerloc=ESMF_STAGGERLOC_CENTER, &
            farrayPtr=centerY, rc=status)
       VERIFY_(STATUS)

       Trans%CubeTrans =  CubeLatLonCreate( im_out, jm_out, im_in, jm_in, &
            centerx(:,1), centery(1,:), .false. ,rc=STATUS )
       VERIFY_(STATUS)
    else
       call MAPL_HorzTransformCreateGEOS (Trans,           &
                                          im_in,  jm_in,   &
                                          im_out, jm_out,  &
                                          XYOFFSET, Order, &
                                          gridtypeIn,      &
                                          gridtypeOUT,     &
                                          rc=status)
       VERIFY_(STATUS)
    endif

    Trans%N_in    = (/im_in , jm_in /)
    Trans%N_out   = (/im_out, jm_out/)
    Trans%Gridin  = Gridin
    Trans%Gridout = Gridout
    Trans%GridTypeIn  = GridTypeIn
    Trans%GridTypeOut = GridTypeOut
    Trans%Parallel=.true.
    Trans%subset  = -1

    if (present(TILINGFILE)) then
       Trans%Parallel=.false.
       Trans%RunTile=.true.
       Trans%ConsrvTrans%runDistributed = .true.

       call MAPL_RegridLSCreate(Trans%CubeTrans, GridIn, GridOut, TilingFile, RC=STATUS) 
       VERIFY_(STATUS)
    endif

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_HorzTransformCreateFromGrids

!==============================================================

  subroutine MAPL_HorzTransformCreateGEOS  (Trans,           &
                                       im_in,  jm_in,   &
                                       im_out, jm_out,  &
                                       XYOFFSET, Order, &
                                       gridtypeIn,      &
                                       gridtypeOUT,     &
                                       subset,          &
                                                      rc)

    type (MAPL_HorzTransform), intent(OUT) :: Trans
    integer,              intent(IN ) :: im_in, jm_in, im_out, jm_out
    integer, optional,    intent(IN ) :: XYOFFSET
    integer, optional,    intent(IN ) :: Order
    character(len=*),optional, intent(IN ) :: gridtypeIN
    character(len=*),optional, intent(IN ) :: gridtypeOut
    integer, optional                      :: subset(4)
    integer, optional,    intent(OUT) :: rc

    integer :: xyoffset_
    integer :: order_
    integer :: SUBSET_(4)
    character(64):: gridtypeIN_
    character(64):: gridtypeOut_

    integer :: STATUS

! Specialized interface for use in GEOS5. Input is always on an FV grid;
! Output grid is determined by xyoffset=[0-3], which denote DCPC, DEPC, 
! The order of the interpolation can be 0=binning or 1=bilinear.

    if(present(GridTypeIn)) then
       GridTypeIn_ = trim(GridTypeIn)
    else if (jm_in==6*im_in) then
       GridTypeIn_ = "Cubed-Sphere"
    else
       GridTypeIn_ = 'UNKNOWN'
    endif

    if(present(GridTypeOut)) then
       GridTypeOut_ = trim(GridTypeOut)
    else if (jm_out==6*im_out) then
       GridTypeOut_ = "Cubed-Sphere"
    else
       GridTypeOut_ = 'UNKNOWN'
    endif

    order_ = 0
    if(present(Order)) order_ =Order

    Xyoffset_ = 0
    if(present(Xyoffset)) Xyoffset_ = Xyoffset

    if(present(SUBSET)) then
      SUBSET_=SUBSET
    else
      SUBSET_=-1
    endif

    select case(Xyoffset_)
    case(0)  ! FV to FV (DCPC)
       call MAPL_HorzTransformCreateBySize(Trans,                      &
                     (/ im_in              , jm_in            /), &
                     (/ im_out             , jm_out           /), &
                     (/ MAPL_DimTopoCyclic , MAPL_DimTopoEdge /), &
                     (/ MAPL_DimTopoCyclic , MAPL_DimTopoEdge /), &
                     (/ -180.              , -90.             /), &
                     (/ -180.              , -90.             /), &
                     (/  180.-(360./IM_in ),  90.             /), &
                     (/  180.-(360./IM_out),  90.             /), &
                     gridtypeIN_,                                 &
                     gridtypeOUT_,                                &
                     order_,                                      &
                     SUBSET_,                                     &
                                                        rc=STATUS )
       VERIFY_(STATUS)
    case(1)  ! FV to FV with dateline edge (DEPC)
       call MAPL_HorzTransformCreateBySize(Trans,                      &
                     (/im_in              , jm_in             /), &
                     (/im_out             , jm_out            /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoEdge  /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoEdge  /), &
                     (/-180.              , -90.              /), &
                     (/-180.+(180./im_out), -90.              /), &
                     (/ 180.-(360./IM_in ),  90.              /), &
                     (/ 180.-(180./IM_out),  90.              /), &
                     gridtypeIN_,                                 &
                     gridtypeOUT_,                                &
                     order_,                                      &
                     SUBSET_,                                     &
                                                        rc=STATUS )
       VERIFY_(STATUS)
    case(2)  !  FV to DCPE
       call MAPL_HorzTransformCreateBySize(Trans,                      &
                     (/im_in              , jm_in             /), &
                     (/im_out             , jm_out            /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoEdge  /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoCenter/), &
                     (/-180.              , -90.              /), &
                     (/-180.              , -90.+(90./jm_out) /), &
                     (/ 180.-(360./IM_in ),  90.              /), &
                     (/ 180.-(360./IM_out),  90.-(90./jm_out) /), &
                     gridtypeIN_,                                 &
                     gridtypeOUT_,                                &
                     order_,                                      &
                     SUBSET_,                                     &
                                                        rc=STATUS )
       VERIFY_(STATUS)
    case(3)  !  FV to DEPE
       call MAPL_HorzTransformCreateBySize(Trans,                      &
                     (/im_in              , jm_in             /), &
                     (/im_out             , jm_out            /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoEdge  /), &
                     (/MAPL_DimTopoCyclic , MAPL_DimTopoCenter/), &
                     (/-180.              , -90.              /), &
                     (/-180.+(180./im_out), -90.+(90./jm_out) /), &
                     (/ 180.-(360./IM_in ),  90.              /), &
                     (/ 180.-(180./IM_out),  90.-(90./jm_out) /), &
                     gridtypeIN_,                                 &
                     gridtypeOUT_,                                &
                     order_,                                      &
                     SUBSET_,                                     &
                                                        rc=STATUS )
       VERIFY_(STATUS)
    case default
       ASSERT_(.false.)
    end select

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_HorzTransformCreateGEOS



  subroutine MAPL_HorzTransformCreateBySize(Trans,           &
                                       N_in ,  N_out,   &
                                       topoIN, topoOUT, &
                                       FirstIN,FirstOUT,&
                                       LastIN, LastOUT, &
                                       gridtypeIN,      &
                                       gridtypeOUT,     &
                                       order,           &
                                       subset,          &
                                                    rc)
    type (MAPL_HorzTransform), intent(OUT) :: Trans
    integer,              intent(IN ) :: N_in    (NUMDIMS)
    integer,              intent(IN ) :: N_out   (NUMDIMS)
    integer,              intent(IN ) :: topoIN  (NUMDIMS) 
    integer,              intent(IN ) :: topoOUT (NUMDIMS) 
    real,                 intent(IN ) :: FirstIN (NUMDIMS)
    real,                 intent(IN ) :: LastIN  (NUMDIMS)
    real,                 intent(IN ) :: FirstOUT(NUMDIMS)
    real,                 intent(IN ) :: LastOUT (NUMDIMS)
    integer,              intent(IN ) :: order
    character(len=*),     intent(IN ) :: gridtypeIN
    character(len=*),     intent(IN ) :: gridtypeOUT
    integer,              intent(IN ) :: subset(4)
    integer, optional,    intent(OUT) :: rc

    integer :: status
    integer :: i, j
    real    :: rngIn, rngOut

    real, allocatable :: xin(:), xout(:)
    real*8, allocatable :: xll(:), yll(:)
    logical           :: isCube2Cube

    ! stuff for subset
    logical :: doSubset
    integer :: IMSUB,JMSUB,IMBEG,IMEND,JMBEG,JMEND
    integer :: Nout(NUMDIMS)
    real*8, allocatable :: xsub(:), ysub(:)
    real*8  :: dx,dy
    if(present(rc)) rc = 0

    isCube2Cube = (N_in(2) == N_in(1)*6) .and. (N_out(2) == N_out(1)*6)

    ! check if we are subsetting
    DoSubset = .true.
       
    if ( all(subset == -1) ) DoSubset = .false.
    if (.not. doSubset) then
       Nout = N_out
    else
!ALT we should assert that we are going to LatLon
       IMBEG = SUBSET(1)
       IMEND = SUBSET(2)
       JMBEG = SUBSET(3)
       JMEND = SUBSET(4)
       IMSUB = IMEND-IMBEG+1
       JMSUB = JMEND-JMBEG+1

       Nout(1) = IMSUB
       Nout(2) = JMSUB
    end if

    do i=1,MAX_AvailableTransforms
       if(AvailableTransforms(i)%created) then
          Trans = AvailableTransforms(i)

          if(Trans%runTile) cycle

          if(isCube2Cube) then
             if(all(Trans%N_in       ==N_in  )  .and. &
                all(Trans%N_out      ==N_out )      ) return
          else
             if(  all(Trans%N_in       ==N_in    )  .and. &
                  all(Trans%N_out      ==Nout    )  .and. &
                  all(Trans%topoIN     ==topoIN  )  .and. &
                  all(Trans%topoOUT    ==topoOUT )  .and. &
                  all(Trans%XminIn     ==FirstIN )  .and. &
                  all(Trans%XmaxIn     ==LastIN  )  .and. &
                  all(Trans%XminOut    ==FirstOUT)  .and. &
                  all(Trans%XmaxOut    ==LastOUT )  .and. &
                  all(Trans%subset     ==subset  )  .and. &
                  Trans%GridTypeIn ==GridTypeIn .and. &
                  Trans%GridTypeOut==GridTypeOut     ) return
          end if
       end if
    enddo

    Trans%runTile = .false.

    Trans%N_in    = N_in
    Trans%N_out   = N_out
    Trans%topoIN  = topoIN
    Trans%topoOUT = topoOUT
    Trans%XminIn  = FirstIN
    Trans%XmaxIn  = LastIN  
    Trans%XminOut = FirstOUT
    Trans%XmaxOut = LastOUT
    Trans%Order   = order
    Trans%GridTypeIn = GridTypeIn
    Trans%GridTypeOut= GridTypeOut
    Trans%subset  = subset

    ASSERT_(all(Trans%N_in >1))
    ASSERT_(all(Trans%N_out>1))

    if (isCube2Cube) then

       Trans%C2CTrans =  CubeCubeCreate( N_in(1), N_in(2), N_out(1), N_out(2), rc=STATUS )
       VERIFY_(STATUS)
       Trans%created = .true.

#ifndef USEBILLS 
    elseif(GridTypeIn=="Cubed-Sphere") then

       allocate(xll(N_out(1)),yll(N_out(2)), stat=STATUS)
       VERIFY_(STATUS)

       do i=1,N_out(1)
          xll(i) = FirstOut(1) + (i-1)*(Lastout(1)-FirstOut(1))/(N_out(1)-1)
       enddo
       do i=1,N_out(2)
          yll(i) = FirstOut(2) + (i-1)*(Lastout(2)-FirstOut(2))/(N_out(2)-1)
       enddo

       if (doSubset) then
          allocate(xsub(N_out(1)),stat=status)
          VERIFY_(STATUS)
          allocate(ysub(N_out(2)),stat=status)
          VERIFY_(STATUS)
          dx = 360.0_8/real(N_out(1))
          dy = 180.0_8/real(N_out(2)-1)
          do i=1,N_out(1)
             xsub(i) = -180.0_8+real(i-1)*dx
          enddo
          do i=1,N_out(2)
             ysub(i) = -90.0_8+real(i-1)*dy
          enddo

          Trans%N_out(1) = IMSUB
          Trans%N_out(2) = JMSUB
          Trans%CubeTrans =  CubeLatLonCreate(N_in(1), N_in(2), IMSUB, JMSUB, xsub(IMBEG:IMEND), ysub(JMBEG:JMEND), .true.  , rc=STATUS)
          VERIFY_(STATUS)
          call CubeLatLonSubset(trans%cubetrans, .true.)
          deallocate(xsub,ysub)
       else
          Trans%CubeTrans =  CubeLatLonCreate(N_in(1), N_in(2), N_out(1), N_out(2), xll, yll, .false. , rc=STATUS)
          VERIFY_(STATUS)
       endif

       deallocate(xll,yll)
    elseif(GridTypeOut=="Cubed-Sphere") then

       allocate(xll(N_in(1)),yll(N_in(2)), stat=STATUS)
       VERIFY_(STATUS)

       do i=1,N_in(1)
          xll(i) = FirstIn(1) + (i-1)*(LastIn(1)-FirstIn(1))/(N_In(1)-1)
       enddo
       do i=1,N_in(2)
          yll(i) = FirstIn(2) + (i-1)*(LastIn(2)-FirstIn(2))/(N_in(2)-1)
       enddo

       Trans%CubeTrans =  CubeLatLonCreate(N_out(1), N_out(2), N_in(1), N_in(2), xll, yll, .false., rc=STATUS)
       VERIFY_(STATUS)

       deallocate(xll,yll)
#endif

    else
       
       do i=1,NUMDIMS

          if(topoIN(i)==MAPL_DimTopoEdge) then
             ASSERT_(Trans%XminIn(i)<=Trans%XminOut(i))
             ASSERT_(Trans%XmaxIn(i)>=Trans%XmaxOut(i))
          end if

          if(topoIN(i)==MAPL_DimTopoCyclic) then
             ASSERT_(topoOUT(i)==MAPL_DimTopoCyclic)
          end if

          if(topoOUT(i)==MAPL_DimTopoCyclic) then
             ASSERT_(topoIN(i)==MAPL_DimTopoCyclic)
          end if

          allocate(Trans%DimMapping(i)%WeightList(n_out(i)), stat=STATUS)
          VERIFY_(STATUS)

          if(topoIN(i)==MAPL_DimTopoCyclic) then
             allocate(xin (2*Trans%N_in (i)+1),stat=STATUS)
             VERIFY_(STATUS)
             allocate(xout(  Trans%N_out(i)+1),stat=STATUS)
             VERIFY_(STATUS)
          elseif(order==0) then
             allocate(xin (  Trans%N_in (i)+1),stat=STATUS)
             VERIFY_(STATUS)
             allocate(xout(  Trans%N_out(i)+1),stat=STATUS)
             VERIFY_(STATUS)
          else
             allocate(xin (  Trans%N_in (i)  ),stat=STATUS)
             VERIFY_(STATUS)
             allocate(xout(  Trans%N_out(i)  ),stat=STATUS)
             VERIFY_(STATUS)
          endif

          call GetX(xin , Trans%N_in(i) , Trans%XminIn (i), Trans%XmaxIn (i),  &
               Trans%TopoIn (i), order, rc=status)
          VERIFY_(STATUS)

          call GetX(xout, Trans%N_out(i), Trans%XminOut(i), Trans%XmaxOut(i),  &
               Trans%TopoOut(i), order, rc=status)
          VERIFY_(STATUS)

          if(topoIN(i)==MAPL_DimTopoCyclic) then
             rngIn  = ((Trans%XmaxIn (i)-Trans%XminIn (i))*Trans%N_in (i))/(Trans%N_in (i)-1)
             rngOut = ((Trans%XmaxOut(i)-Trans%XminOut(i))*Trans%N_out(i))/(Trans%N_out(i)-1)

             ASSERT_(abs( (rngIn-rngOut)/rngIn ) < 1.e-5)

             if(xout(1)<xin(1)) then
                xout  = xout + int((xin(1)-xout(1))/rngIn+1)*rngIn
             else
                xout  = xout + int((xin(1)-xout(1))/rngIn)*rngIn
             end if
          end if

          ASSERT_(xin(size(xin)) >= xout(size(xout)))
          ASSERT_(xin(        1) <= xout(         1))

          select case (order)
          case(0)
             call ComputeDimBinWeights(Trans%DimMapping(i)%WeightList,Xin,Xout, &
                  HasPoles=(topoIN(i)==MAPL_DimTopoEdge.and.topoOUT(i)==MAPL_DimTopoEdge),&
                  rc=status)
             VERIFY_(STATUS)
          case(1)
             call ComputeDimLinWeights(Trans%DimMapping(i)%WeightList,Xin,Xout,rc=status)
             VERIFY_(STATUS)
          case default
             ASSERT_(.false.)
          end select

          deallocate(Xin )
          deallocate(Xout)

       end do

    end if

! If latlon2cube or cube2latlon we mark it created without
!  creating anything because it is currently being created
!  on the fly

    Trans%created = .true.


    do i=1,MAX_AvailableTransforms
       if(AvailableTransforms(i)%created) then
          if(i==MAX_AvailableTransforms) then
             print *, "Error in HorzTransform: Out Of Space for new transform"
             ASSERT_(.false.)
          else
             cycle
          end if
       else
          AvailableTransforms(i)=Trans
          exit
       end if
    end do

    RETURN_(SUCCESS)

  end subroutine MAPL_HorzTransformCreateBySize

  subroutine GetX(X,N,xmin,xmax,topo,order,rc)

    real,                 intent(OUT) :: x(:)
    integer,              intent(IN ) :: N
    integer,              intent(IN ) :: topo
    integer,              intent(IN ) :: order
    real,                 intent(IN ) :: xmin
    real,                 intent(IN ) :: xmax
    integer, optional,    intent(OUT) :: rc

    integer :: status, j, jm
    real    :: dx

    jm = size(X)
    dx = (Xmax-xmin) / (N-1)

    if(order==1) then
       x(1)       = xmin
    else
       x(1)       = xmin-0.5*dx
    end if

    do j=2,JM
       x(j) = x(1) + (j - 1)*dx
    end do

    if(topo==MAPL_DimTopoEdge  ) then
       x( 1) = xmin
       x(JM) = xmax
    end if

    RETURN_(SUCCESS)
  end subroutine GetX


  subroutine ComputeDimLinWeights(Weight,Xin,Xout,rc)
    
    type(Weights),     intent(INOUT) :: Weight(:)
    real,              intent(IN   ) :: Xin(:), Xout(:)
    integer, optional, intent(OUT  ) :: rc


    ! Compute weights for binned interpolation along a dimension.
    ! Xout are the N_in + 1 input bin edges.
    ! Xin  are the N_out + 1 output bin edges
    ! Weigths are the mapping


    integer :: j_out, j0, j1
    integer :: N_in
    integer :: status
    real, pointer :: b(:)

    N_in  = size(Xin )

    do j_out=1,size(Weight)
       j0 = 1
       do           
          if(Xout(j_out  )<=Xin(j0+1)) exit
          j0=j0+1
          ASSERT_(j0 < N_in )
       end do
       j1 = j0 + 1

       allocate(b(j0:j1), stat=STATUS)
       VERIFY_(STATUS)

       b(j0  ) = (Xin(j1)-Xout(j_out))/(Xin(j1)-Xin(j0))
       b(j0+1) = 1.0 - b(j0)

       Weight(j_out)%f  => b

    end do

  end subroutine ComputeDimLinWeights

  subroutine ComputeDimBinWeights(Weight,Xin,Xout,HasPoles,rc)
    
    type(Weights),     intent(INOUT) :: Weight(:)
    real,              intent(IN   ) :: Xin(:), Xout(:)
    logical,           intent(IN   ) :: HasPoles
    integer, optional, intent(OUT  ) :: rc


    ! Compute weights for binned interpolation along a dimension.
    ! Xout are the N_in + 1 input bin edges.
    ! Xin  are the N_out + 1 output bin edges
    ! Weigths are the mapping


    integer :: j_out, j0, j1, j
    integer :: N_in, N_out
    integer :: status
    real    :: dx, ff
    real, pointer :: b(:)

    N_in  = size(Xin )-1
    N_out = size(Weight)

    do j_out=1,N_out
       j0 = 1
       do           
          if(Xout(j_out  )>=Xin(j0) .and. Xout(j_out  )<=Xin(j0+1)) exit
          j0=j0+1
          ASSERT_(j0 <= N_in )
       end do

       j1 = j0
       do
          if(Xout(j_out+1)>=Xin(j1) .and. Xout(j_out+1)<=Xin(j1+1)) exit
          j1=j1+1
          ASSERT_(j1 <= N_in )
       end do

       allocate(b(j0:j1), stat=STATUS)
       VERIFY_(STATUS)

       if(j0==j1) then
          b(j0) = 1.
       else
          dx    = Xin(j0+1)-Xout(j_out)
          ff    = dx
          b(j0) = dx
          do j=j0+1,j1-1
             dx   = Xin(j+1) - Xin(j)
             ff   = ff + dx
             b(j) = dx
          end do
          dx    = Xout(j_out+1)-Xin(j1)
          ff    = ff + dx
          b(j1) = dx
          b     = b/ff
       end if

       Weight(j_out)%f  => b

    end do

    if(HasPoles) then
       deallocate(Weight(    1)%f)
       deallocate(Weight(N_out)%f)
       allocate  (Weight(    1)%f(1   :1   ))
       allocate  (Weight(N_out)%f(N_in:N_in))
       Weight(    1)%f  =  1.
       Weight(N_out)%f  =  1.
    endif

  end subroutine ComputeDimBinWeights

  subroutine DestroyMapping(MAP, rc)
    type (Mapping),    intent(INOUT) :: Map
    integer, optional, intent(  OUT) :: rc

    integer :: i

    do i=1,size(MAP%WeightList)
       DEALOC_(MAP%WeightList(i)%f)
    end do

    DEALOC_(Map%Weightlist)

    RETURN_(SUCCESS)
  end subroutine DestroyMapping

  subroutine MAPL_HorzTransformDestroy(Trans, rc)
    type (MAPL_HorzTransform), target, intent(INOUT) :: Trans
    integer, optional,    intent(  OUT) :: rc

    integer :: status
    integer :: i, j
    logical :: isCube2Cube
    logical :: isConserv

    isCube2Cube = (Trans%N_in (2)==Trans%N_in (1)*6) .and. &
                  (Trans%N_out(2)==Trans%N_out(1)*6)

    isConserv = Trans%runTile ! might need more logic here

    do i=1,MAX_AvailableTransforms
       if(AvailableTransforms(i)%created) then
          if(isCube2Cube) then
             if(all(Trans%N_in ==AvailableTransforms(i)%N_in  )  .and. &
                all(Trans%N_out==AvailableTransforms(i)%N_out )      ) then
                Trans%created   = .false.
                Trans%parallel  = .false.
                Trans%transpose = .false.
                Trans%RunTile   = .false.
                RETURN_(SUCCESS)
             end if
          else if (isConserv) then

             if (MAPL_ConsrvTransMatch(Trans%ConsrvTrans, &
                  AvailableTransforms(i)%ConsrvTrans)) then

                ! table match: 
                ! mark it unused and leave it alone,
                ! except for global data in case of distributted tile
                if (.not. Trans%ConsrvTrans%tileDataGlobal) then
                   call MAPL_TileTransDestroy(Trans%ConsrvTrans%GlobalTrans%Global)
                end if
                   
                Trans%created   = .false.
                Trans%ConsrvTrans%created  = .false.

                RETURN_(SUCCESS)
             end if
          else
             if(all(Trans%N_in   ==AvailableTransforms(i)%N_in    )  .and. &
                all(Trans%N_out  ==AvailableTransforms(i)%N_out   )  .and. &
                all(Trans%topoIN ==AvailableTransforms(i)%topoIN  )  .and. &
                all(Trans%topoOUT==AvailableTransforms(i)%topoOUT )  .and. &
                all(Trans%XminIn ==AvailableTransforms(i)%XminIN )  .and. &
                all(Trans%XmaxIn ==AvailableTransforms(i)%XmaxIN  )  .and. &
                all(Trans%XminOut==AvailableTransforms(i)%XminOUT)  .and. &
                all(Trans%XmaxOut==AvailableTransforms(i)%XmaxOUT )  .and. &
                all(Trans%subset ==AvailableTransforms(i)%subset  )  .and. &
                Trans%GridTypeIn ==AvailableTransforms(i)%GridTypeIn .and. &
                Trans%GridTypeOut==AvailableTransforms(i)%GridTypeOut   ) then
                Trans%created   = .false.
                Trans%parallel  = .false.
                Trans%transpose = .false.
                Trans%RunTile   = .false.
                RETURN_(SUCCESS)
             end if
          end if
       end if
    enddo


    if(Trans%created) then
       if (CubeLatLonIsCreated(Trans%CubeTrans)) then
          call CubeLatLonDestroy(Trans%CubeTrans, rc=STATUS)
          VERIFY_(STATUS)
       endif

       if (CubeCubeIsCreated(Trans%C2CTrans)) then
          call CubeCubeDestroy(Trans%C2CTrans, rc=STATUS)
          VERIFY_(STATUS)
       endif

       if (isConserv) then
          call MAPL_RegridConservativeDestroy(Trans%ConsrvTrans, RC=STATUS)
          VERIFY_(STATUS)
       end if

       do I=1,NUMDIMS
          call DestroyMapping(Trans%DimMapping(i), RC=STATUS)
          VERIFY_(STATUS)
       enddo

       Trans%created   = .false.
       Trans%parallel  = .false.
       Trans%transpose = .false.
       Trans%RunTile   = .false.
    end if

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformDestroy

  subroutine MAPL_HorzTransformRun2(Trans, qin, qout, undef, rc)

!    Trans ....  Precomputed transform
!      qin ....  Input Variable
!      qout....  Output Variable
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform), intent(IN   ) :: Trans
    real,                    intent(INOUT) :: qin (:,:)
    real,                    intent(INOUT) :: qout(:,:)
    real,    optional,       intent(IN   ) :: undef
    integer, optional,       intent(  OUT) :: rc

    real            :: undef_
    real            :: q, w, f
    integer         :: STATUS
    integer         :: i,j
    integer         :: i0,i1,j0,j1
    integer         :: ii,jj,jx,ix
    real, pointer   :: fx(:), fy(:), X(:,:,:)
    
    logical :: doCube2Latlon, doLatlon2Cube, FillLL, NoCube, Cube2Cube
    integer :: npx, npy
    integer :: nlon, nlat

    if(present(rc)) rc = 0
    
    if (Trans%runTile)  then
       if (Trans%ConsrvTrans%runDistributed) then
          call RunTileTransform(TRANS%CubeTrans, QIN, QOUT, TRANS%TRANSPOSE, RC)
       else
          NEGASSERT_(.not. Trans%transpose) 

          if (Trans%order == MAPL_HorzTransOrderSample) then
             call MAPL_RegridConservativeRun(Trans%consrvTrans, QIN, QOUT, SAMPLE=.true., rc=STATUS )
          else
             call MAPL_RegridConservativeRun(Trans%consrvTrans, QIN, QOUT, rc=STATUS )
          endif
          VERIFY_(STATUS)

       end if
       return
    end if

    Cube2Cube = Trans%N_in(2) == Trans%N_in(1)*6 .and. Trans%N_out(2) == Trans%N_out(1)*6

    if (Cube2Cube) then
       call CubeToCube(Trans%C2CTrans, qin, qout, rc=status)
       VERIFY_(STATUS)

       RETURN_(SUCCESS)
    end if

    if(present(undef)) then
       undef_ = undef
    else
       undef_ = huge(undef_)
    end if

    ASSERT_(all(shape(qin )==Trans%N_in ))
    ASSERT_(all(shape(qout)==Trans%N_out))

    NoCube = Trans%N_in(2) /= Trans%N_in(1)*6 .and. Trans%N_out(2) /= Trans%N_out(1)*6
    FillLL = Trans%N_in(2) == Trans%N_in(1)*6

    if(.not.NoCube) then
       if    ( FillLL ) then 
          if(CubeLatLonIsCreated(Trans%CubeTrans)) then
             if(Trans%Transpose) then
                call LatLonToCube(Trans%CubeTrans,qout,qin,transpose=Trans%transpose, &
                     misval=undef,  rc=status)
                VERIFY_(STATUS)
             else
                call CubeToLatLon(Trans%CubeTrans,qin,qout,transpose=Trans%transpose, &
                     misval=undef,  rc=status)
                VERIFY_(STATUS)
             endif
          else

#ifdef USE_CUBEDSPHERE
             ASSERT_(.not.Trans%transpose)

             npx  = Trans%N_in (1)
             npy  = Trans%N_in (2)
             nlon = Trans%N_out(1)
             nlat = Trans%N_out(2)
             call cube2latlon(npx, npy, nlon, nlat, qin, qout)
#else
             print *,'MAPL is compiled without Cubed Sphere support'
             ASSERT_(.false.)
#endif
          end if

       else
          if(CubeLatLonIsCreated(Trans%CubeTrans)) then
             npx = size(qout,1)
             if(Trans%Transpose) then
                call CubeToLatLon(Trans%CubeTrans,qout,qin,&
                     transpose=Trans%transpose, misval=undef, rc=status)
                VERIFY_(STATUS)
             else
                call LatLonToCube(Trans%CubeTrans,qin,qout,&
                     transpose=Trans%transpose, misval=undef, rc=status)
                VERIFY_(STATUS)
             endif
          else

#ifdef USE_CUBEDSPHERE
             NEGASSERT_(.not.Trans%transpose)

             nlon = Trans%N_in (1)
             nlat = Trans%N_in (2)
             npx  = Trans%N_out(1)
             npy  = Trans%N_out(2)
             call Latlon2Cube(npx, npy, nlon, nlat, qin, qout)
#else
             print *,'MAPL is compiled without Cubed Sphere support'
             ASSERT_(.false.)
#endif
          end if
       end if
    else
       NEGASSERT_(.not.Trans%transpose)

       do j=1,Trans%N_out(2)
          j0 = lbound(Trans%DimMapping(2)%WeightList(j)%f,1)
          j1 = ubound(Trans%DimMapping(2)%WeightList(j)%f,1)
          fy =>Trans%DimMapping(2)%WeightList(j)%f

          do i=1,Trans%N_out(1)
             i0 = lbound(Trans%DimMapping(1)%WeightList(i)%f,1)
             i1 = ubound(Trans%DimMapping(1)%WeightList(i)%f,1)
             fx =>Trans%DimMapping(1)%WeightList(i)%f

             q = 0.0
             w = 0.0

             do jj=j0,j1
                if(jj>Trans%N_in(2)) then
                   jx = jj - Trans%N_in(2)
                else
                   jx = jj
                end if

                do ii=i0,i1
                   if(ii>Trans%N_in(1)) then
                      ix = ii - Trans%N_in(1)
                   else
                      ix = ii
                   end if

                   if(qin(ix,jx) /= undef_) then
                      f = fx(ii)*fy(jj)
                      q = q + f*qin(ix,jx)
                      w = w + f           
                   end if
                end do
             end do

             if ( w >= wc ) then
                qout(i,j) = q / w
             else
                qout(i,j) = undef_
             end if

          end do
       end do

    endif

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformRun2



  subroutine MAPL_HorzTransformRunV3(Trans, uin, vin, uout, vout, undef, rotate,  rc)

!    Trans ....  Precomputed transform
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform),intent(INout) :: Trans
    real,                     intent(INOUT) :: uin (:,:,:),vin (:,:,:)
    real,                     intent(INOUT) :: uout(:,:,:),vout(:,:,:)
    real,    optional,        intent(IN   ) :: undef
    logical, optional,        intent(IN   ) :: rotate
    integer, optional,        intent(  OUT) :: rc

    integer           :: STATUS
    real              :: Win (size(uin ,1),size(uin ,2),size(uin ,3)*3)
    real              :: Wout(size(uout,1),size(uout,2),size(uout,3)*3)
    logical           :: InputIsLL

    integer           :: IM, JM, LM
    logical           :: Cube2Cube

    if(present(rc)) rc = 0
   
    ASSERT_(size(uin,3)==size(uout,3))

    Cube2Cube = Trans%N_in(2) == Trans%N_in(1)*6 .and. Trans%N_out(2) == Trans%N_out(1)*6

    if (Cube2Cube) then
! ALT: Here we assume that the winds are on a D-grid, and therefore
!      we need to make D2A, rotate them, transform, rotate back, and do A2D
       if (present(Rotate)) then
          ASSERT_(Rotate)
       end if
       ASSERT_(CubeCubeIsCreated(Trans%C2CTrans))

       IM = size(Uin,1)
       JM = size(Uin,2)
       LM = size(Uin,3) ! This LM is the number of vertical levels passed in
                        ! and it may not be the same as LM in the model

       call RestaggerWindsCube(Uin, Vin, D2A=.true.)

       call SphericalToCartesian(Trans%C2CTrans, Uin, Vin, Win)

       call MAPL_HorzTransformRun3(Trans, Win, Wout, undef=undef, rc=status)
       VERIFY_(STATUS)

       call CartesianToSpherical(Trans%C2CTrans, Wout, Uout, Vout)

       call RestaggerWindsCube(Uout, Vout, D2A=.false.)
       
    else

       ASSERT_(CubeLatLonIsCreated(Trans%CubeTrans))

       InputIsLL = Trans%gridtypeOut == 'Cubed-Sphere'

       call SphericalToCartesian(Trans%CubeTrans, uin , vin , Win , &
            Trans%Transpose, SphIsLL=InputIsLL)

       call MAPL_HorzTransformRun3(Trans, Win, Wout, undef=undef, rc=status)
       VERIFY_(STATUS)

       call CartesianToSpherical(Trans%CubeTrans, Wout, uout, vout, &
            Trans%Transpose, SphIsLL=.not.InputIsLL, Rotate=Rotate)

    end if
    RETURN_(SUCCESS)

  end subroutine MAPL_HorzTransformRunV3

  recursive subroutine MAPL_HorzTransformRun3(Trans, qin, qout, undef, rc)

!    Trans ....  Precomputed transform
!      qin ....  Input Variable
!      qout....  Output Variable
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform), intent(INout) :: Trans
    real,                    intent(INout) :: qin (:,:,:)
    real,                    intent(inOUT) :: qout(:,:,:)
    real,    optional,       intent(IN   ) :: undef
    integer, optional,       intent(  OUT) :: rc

    real          :: undef_
    real          :: q(size(qin,3))
    real          :: w(size(qin,3))
    real          :: f
    integer       :: STATUS
    integer       :: i,j,k
    integer       :: i0,i1,j0,j1
    integer       :: ii,jj,jx,ix
    integer       :: sh(3)

    logical :: doCube2Latlon, doLatlon2Cube
    integer :: npx, npy, npz
    integer :: nlon, nlat

    real, pointer :: fx(:), fy(:)

    if(present(rc)) rc = 0

    ASSERT_(size(qin,3)==size(qout,3))
    
    if((size(qin,1)/=Trans%N_in(1) .or. size(qin,2)/=Trans%N_in(2)) &
         .and. .not. Trans%runTile)  then
       ASSERT_(Trans%Parallel)
       call MAPL_HorzTransformRunParallelFromArrays(Trans, qin, qout, undef, rc)
       return
    endif

    if(CubeLatLonIsCreated(Trans%CubeTrans) .or. &
         CubeCubeIsCreated(Trans%C2CTrans)) then
       do k=1,size(qin,3)
          call MAPL_HorzTransformRun2(Trans, qin(:,:,k), qout(:,:,k), undef, rc=status)
          VERIFY_(STATUS)
       end do
       return
    end if

    NEGASSERT_(.not.Trans%transpose)

    doCube2Latlon = Trans%gridtypeIN =='Cubed-Sphere'
    doLatlon2Cube = Trans%gridtypeOUT=='Cubed-Sphere'

    if (Trans%N_out(2) == Trans%N_out(1)*6 ) doLatlon2Cube = .true.
    if (Trans%N_in(2)  == Trans%N_in(1)*6  ) doCube2Latlon = .true.

    if (doCube2Latlon) then 
#ifdef USE_CUBEDSPHERE

       npx  = Trans%N_in(1)
       npy  = Trans%N_in(2)
       npz  = size(qin,3)
       nlon = Trans%N_out(1)
       nlat = Trans%N_out(2)
       do k=1,npz
          call cube2latlon(npx, npy, nlon, nlat, qin(:,:,k), qout(:,:,k))
       enddo
#else
       print *,'MAPL is compiled without Cubed Sphere support'
       ASSERT_(.false.)
#endif

    elseif(doLatlon2Cube) then

#ifdef USE_CUBEDSPHERE
       nlon = Trans%N_in (1)
       nlat = Trans%N_in (2)
       npz  = size(qin,3)
       npx  = Trans%N_out(1)
       npy  = Trans%N_out(2)
       do k=1,npz
        call Latlon2Cube(npx, npy, nlon, nlat, qin(:,:,k), qout(:,:,k))
       end do
#else
       print *,'MAPL is compiled without Cubed Sphere support'
       ASSERT_(.false.)
#endif
    else

    if(present(undef)) then
       undef_ = undef
    else
       undef_ = huge(undef_)
    end if

    sh = shape(qin )
    ASSERT_(all(sh(1:2)==Trans%N_in ))
    sh = shape(qout)
    ASSERT_(all(sh(1:2)==Trans%N_out))

    do j=1,Trans%N_out(2)
       j0 = lbound(Trans%DimMapping(2)%WeightList(j)%f,1)
       j1 = ubound(Trans%DimMapping(2)%WeightList(j)%f,1)
       fy =>Trans%DimMapping(2)%WeightList(j)%f

       do i=1,Trans%N_out(1)
          i0 = lbound(Trans%DimMapping(1)%WeightList(i)%f,1)
          i1 = ubound(Trans%DimMapping(1)%WeightList(i)%f,1)
          fx =>Trans%DimMapping(1)%WeightList(i)%f

          q = 0.0
          w = 0.0

          do jj=j0,j1
             if(jj>Trans%N_in(2)) then
                jx = jj - Trans%N_in(2)
             else
                jx = jj
             end if

             do ii=i0,i1
                if(ii>Trans%N_in(1)) then
                   ix = ii - Trans%N_in(1)
                else
                   ix = ii
                end if

                f = fx(ii)*fy(jj)

                where(qin(ix,jx,:) /= undef_)
                   q = q + f*qin(ix,jx,:)
                   w = w + f           
                end where
             end do
          end do

          where( w >= wc )
             qout(i,j,:) = q / w
          elsewhere
             qout(i,j,:) = undef_
          end where

       end do
    end do

    endif

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformRun3


  subroutine MAPL_HorzTransformRunParallelFromFields(Trans,In,Out,undef,rc)
    type (MAPL_HorzTransform),intent(INout) :: Trans
    type (ESMF_Field),        intent(INout) :: In
    type (ESMF_Field),        intent(inOUT) :: Out
    real,    optional,        intent(IN   ) :: undef
    integer, optional,        intent(  OUT) :: RC

! Locals
!-------

    integer                    :: STATUS
    character(len=ESMF_MAXSTR) :: IAm="MAPL_TransformRunParallel"

    real, pointer              :: InLoc3 (:,:,:), InLoc2  (:,:)
    real, pointer              :: OutLoc3(:,:,:), OutLoc2 (:,:)

    integer                    :: DIMS(7), RankIn, RankOut
    type (ESMF_Grid)           :: GridIn, GridOut
    type (ESMF_Array)          :: ArrayIn, ArrayOut

!  Begin
!-------

! Get grids and pointers from the ESMF Fields.
!--------------------------------------------

    call ESMF_FieldGet(In, ARRAY=arrayIn, grid=GridIn, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_ArrayGet(arrayIn, rank=RankIn,  RC=STATUS)
    VERIFY_(STATUS)

    call ESMF_FieldGet(Out, ARRAY=arrayOut, grid=GridOut, RC=STATUS)
    VERIFY_(STATUS)
    call ESMF_ArrayGet(arrayOut, rank=RankOut,  RC=STATUS)
    VERIFY_(STATUS)

    ASSERT_(RankIn==RankOut)

    if(RankIn==3) then
       call ESMF_FieldGet(In , 0, InLoc3 , rc=STATUS)
       VERIFY_(STATUS)
       call ESMF_FieldGet(Out, 0, OutLoc3, rc=STATUS)
       VERIFY_(STATUS)

       ASSERT_(size(InLoc3,3)==size(OutLoc3,3))
    elseif(RankIn==2) then
       call ESMF_FieldGet(In , 0, InLoc2 , rc=STATUS)
       VERIFY_(STATUS)
       call ESMF_FieldGet(Out, 0, OutLoc2, rc=STATUS)
       VERIFY_(STATUS)

       allocate( InLoc3(size( InLoc2,1),size( InLoc2,2),1), stat=STATUS)
       VERIFY_(STATUS)
       allocate(OutLoc3(size(OutLoc2,1),size(OutLoc2,2),1), stat=STATUS)
       VERIFY_(STATUS)

       InLoc3(:,:,1) = InLoc2
    else
       ASSERT_(.false.)
    end if

    call MAPL_GridGet(Gridin,  globalCellCountPerDim=DIMS, rc=STATUS)
    VERIFY_(STATUS)

    ASSERT_(Trans%N_in(1) ==DIMS(1) .and. Trans%N_in(2) ==DIMS(2))

    call MAPL_GridGet(Gridout, globalCellCountPerDim=DIMS, rc=STATUS)
    VERIFY_(STATUS)

    ASSERT_(Trans%N_out(2)==DIMS(1) .and. Trans%N_out(2)==DIMS(2))

    Trans%Gridin  = Gridin
    Trans%Gridout = Gridout

    call MAPL_HorzTransformRunParallelFromArrays(Trans, InLoc3, OutLoc3, undef, rc=status)
    VERIFY_(STATUS)


! Clean up.
!---------

    if(RankIn==2) then
       OutLoc2 = OutLoc3(:,:,1)

       deallocate( InLoc3, stat=STATUS)
       VERIFY_(STATUS)
       deallocate(OutLoc3, stat=STATUS)
       VERIFY_(STATUS)
    end if

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_HorzTransformRunParallelFromFields


  subroutine MAPL_HorzTransformRunParallelFromArrays(Trans,In,Out,undef,rc)
    type (MAPL_HorzTransform),intent(INout) :: Trans
    real,  target,            intent(INout) :: In(:,:,:)
    real,  target,            intent(inOUT) :: Out(:,:,:)
    real,    optional,        intent(IN   ) :: undef
    integer, optional,        intent(  OUT) :: RC

! Locals
!-------

    integer                    :: STATUS
    character(len=ESMF_MAXSTR) :: IAm="MAPL_TransformRunParallelFromArrays"
    real, pointer              :: InGlob (:,:,:)
    real, allocatable          :: OutGlob(:,:,:)

    ASSERT_(size(In,3)==size(Out,3))

! Everybody has to participate in the collective gather.
!   If we are a root of this gather we will have one or
!   more global layers in InGlob, and the third dimension
!   InGlob (which is allocated by the gather) is exactly
!   the number of layers allotted to us.
!--------------------------------------------------------

    nullify(InGlob)

    call MAPL_CollectiveGather3D(Trans%Gridin, In, InGlob, rc=STATUS)
    VERIFY_(STATUS)

! If we are one of the Roots of the gather, allocate the 3D global
!   output and then do the serial transform on the layers allotted to us.
!   We know we are root because the Gather call allocated a non-trivial
!   InGlob at the Roots, elsewhere it allocated a trivial (1,1,1) 3D array.
!--------------------------------------------------------------------------

    if(size(InGlob)>1) then
       allocate(OutGlob(Trans%N_out(1),Trans%N_out(2),size(InGlob,3)), stat=STATUS)
       VERIFY_(STATUS)

       call MAPL_HorzTransformRun3(Trans, InGlob, OutGlob, undef, rc=STATUS)
       VERIFY_(STATUS)
    endif

    deallocate(InGlob)

! Everybody has to participate in the collective scatter
!-------------------------------------------------------

    call MAPL_CollectiveScatter3D(Trans%Gridout, OutGlob, Out, rc=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_HorzTransformRunParallelFromArrays

#ifdef OVERLOAD_R8
  subroutine MAPL_HorzTransformRun2R8(Trans, qin, qout, undef, rc)

!    Trans ....  Precomputed transform
!      qin ....  Input Variable
!      qout....  Output Variable
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform), intent(IN   ) :: Trans
    real(R8),                    intent(INOUT) :: qin (:,:)
    real(R8),                    intent(INOUT) :: qout(:,:)
    real,    optional,       intent(IN   ) :: undef
    integer, optional,       intent(  OUT) :: rc

    real            :: undef_
    real            :: q, w, f
    integer         :: STATUS
    integer         :: i,j
    integer         :: i0,i1,j0,j1
    integer         :: ii,jj,jx,ix
    real, pointer   :: fx(:), fy(:), X(:,:,:)
    
    logical :: doCube2Latlon, doLatlon2Cube, FillLL, NoCube
    integer :: npx, npy
    integer :: nlon, nlat

    if(present(rc)) rc = 0
    
    NoCube = Trans%N_in(2) /= Trans%N_in(1)*6 .and. Trans%N_out(2) /= Trans%N_out(1)*6
    FillLL = Trans%N_in(2) == Trans%N_in(1)*6

    if(.not.NoCube) then
       if    ( FillLL ) then 
          if(CubeLatLonIsCreated(Trans%CubeTrans)) then
             if(Trans%Transpose) then
                call LatLonToCube(Trans%CubeTrans,qout,qin,transpose=Trans%transpose, &
                     misval=undef,  rc=status)
                VERIFY_(STATUS)
             else
                call CubeToLatLon(Trans%CubeTrans,qin,qout,transpose=Trans%transpose, &
                     misval=undef,  rc=status)
                VERIFY_(STATUS)
             endif
          else

#ifdef USE_CUBEDSPHERE
             NEGASSERT_(.not.Trans%transpose)

             npx  = Trans%N_in (1)
             npy  = Trans%N_in (2)
             nlon = Trans%N_out(1)
             nlat = Trans%N_out(2)
             call cube2latlon(npx, npy, nlon, nlat, qin, qout)
#else
             print *,'MAPL is compiled without Cubed Sphere support'
             ASSERT_(.false.)
#endif
          end if

       else
          if(CubeLatLonIsCreated(Trans%CubeTrans)) then
             npx = size(qout,1)
             if(Trans%Transpose) then
                call CubeToLatLon(Trans%CubeTrans,qout,qin,&
                     transpose=Trans%transpose, misval=undef, rc=status)
                VERIFY_(STATUS)
             else
                call LatLonToCube(Trans%CubeTrans,qin,qout,&
                     transpose=Trans%transpose, misval=undef, rc=status)
                VERIFY_(STATUS)
             endif
          else

#ifdef USE_CUBEDSPHERE
             NEGASSERT_(.not.Trans%transpose)

             nlon = Trans%N_in (1)
             nlat = Trans%N_in (2)
             npx  = Trans%N_out(1)
             npy  = Trans%N_out(2)
             call Latlon2Cube(npx, npy, nlon, nlat, qin, qout)
#else
             print *,'MAPL is compiled without Cubed Sphere support'
             ASSERT_(.false.)
#endif
          end if
       end if
    else
       NEGASSERT_(.not.Trans%transpose)

       if(present(undef)) then
          undef_ = undef
       else
          undef_ = huge(undef_)
       end if

       ASSERT_(all(shape(qin )==Trans%N_in ))
       ASSERT_(all(shape(qout)==Trans%N_out))

       do j=1,Trans%N_out(2)
          j0 = lbound(Trans%DimMapping(2)%WeightList(j)%f,1)
          j1 = ubound(Trans%DimMapping(2)%WeightList(j)%f,1)
          fy =>Trans%DimMapping(2)%WeightList(j)%f

          do i=1,Trans%N_out(1)
             i0 = lbound(Trans%DimMapping(1)%WeightList(i)%f,1)
             i1 = ubound(Trans%DimMapping(1)%WeightList(i)%f,1)
             fx =>Trans%DimMapping(1)%WeightList(i)%f

             q = 0.0
             w = 0.0

             do jj=j0,j1
                if(jj>Trans%N_in(2)) then
                   jx = jj - Trans%N_in(2)
                else
                   jx = jj
                end if

                do ii=i0,i1
                   if(ii>Trans%N_in(1)) then
                      ix = ii - Trans%N_in(1)
                   else
                      ix = ii
                   end if

                   if(qin(ix,jx) /= undef_) then
                      f = fx(ii)*fy(jj)
                      q = q + f*qin(ix,jx)
                      w = w + f           
                   end if
                end do
             end do

             if ( w >= wc ) then
                qout(i,j) = q / w
             else
                qout(i,j) = undef_
             end if

          end do
       end do

    endif

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformRun2R8

  recursive subroutine MAPL_HorzTransformRun3R8(Trans, qin, qout, undef, rc)

!    Trans ....  Precomputed transform
!      qin ....  Input Variable
!      qout....  Output Variable
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform), intent(INout) :: Trans
    real*8,                    intent(INout) :: qin (:,:,:)
    real*8,                    intent(inOUT) :: qout(:,:,:)
    real,    optional,       intent(IN   ) :: undef
    integer, optional,       intent(  OUT) :: rc

    real*8          :: undef_
    real*8          :: q(size(qin,3))
    real*8          :: w(size(qin,3))
    real*8          :: f
    integer       :: STATUS
    integer       :: i,j,k
    integer       :: i0,i1,j0,j1
    integer       :: ii,jj,jx,ix
    integer       :: sh(3)

    logical :: doCube2Latlon, doLatlon2Cube
    integer :: npx, npy, npz
    integer :: nlon, nlat

    real, pointer :: fx(:), fy(:)

    if(present(rc)) rc = 0

    ASSERT_(size(qin,3)==size(qout,3))
    
    if(size(qin,1)/=Trans%N_in(1)  .or. size(qin,2)/=Trans%N_in(2))  then
       ASSERT_(Trans%Parallel)
       call MAPL_HorzTransformRunParallelFromArraysR8(Trans, qin, qout, undef, rc)
       return
    endif

    if(CubeLatLonIsCreated(Trans%CubeTrans)) then
       do k=1,size(qin,3)
          call MAPL_HorzTransformRun2R8(Trans, qin(:,:,k), qout(:,:,k), undef, rc=status)
          VERIFY_(STATUS)
       end do
       return
    end if

    NEGASSERT_(.not.Trans%transpose)

    doCube2Latlon = Trans%gridtypeIN =='Cubed-Sphere'
    doLatlon2Cube = Trans%gridtypeOUT=='Cubed-Sphere'

    if (Trans%N_out(2) == Trans%N_out(1)*6 ) doLatlon2Cube = .true.
    if (Trans%N_in(2)  == Trans%N_in(1)*6  ) doCube2Latlon = .true.

    if (doCube2Latlon) then 
#ifdef USE_CUBEDSPHERE

       npx  = Trans%N_in(1)
       npy  = Trans%N_in(2)
       npz  = size(qin,3)
       nlon = Trans%N_out(1)
       nlat = Trans%N_out(2)
       do k=1,npz
          call cube2latlon(npx, npy, nlon, nlat, qin(:,:,k), qout(:,:,k))
       enddo
#else
       print *,'MAPL is compiled without Cubed Sphere support'
       ASSERT_(.false.)
#endif

    elseif(doLatlon2Cube) then

#ifdef USE_CUBEDSPHERE
       nlon = Trans%N_in (1)
       nlat = Trans%N_in (2)
       npz  = size(qin,3)
       npx  = Trans%N_out(1)
       npy  = Trans%N_out(2)
       do k=1,npz
        call Latlon2Cube(npx, npy, nlon, nlat, qin(:,:,k), qout(:,:,k))
       end do
#else
       print *,'MAPL is compiled without Cubed Sphere support'
       ASSERT_(.false.)
#endif
    else

    if(present(undef)) then
       undef_ = undef
    else
       undef_ = huge(undef_)
    end if

    sh = shape(qin )
    ASSERT_(all(sh(1:2)==Trans%N_in ))
    sh = shape(qout)
    ASSERT_(all(sh(1:2)==Trans%N_out))

    do j=1,Trans%N_out(2)
       j0 = lbound(Trans%DimMapping(2)%WeightList(j)%f,1)
       j1 = ubound(Trans%DimMapping(2)%WeightList(j)%f,1)
       fy =>Trans%DimMapping(2)%WeightList(j)%f

       do i=1,Trans%N_out(1)
          i0 = lbound(Trans%DimMapping(1)%WeightList(i)%f,1)
          i1 = ubound(Trans%DimMapping(1)%WeightList(i)%f,1)
          fx =>Trans%DimMapping(1)%WeightList(i)%f

          q = 0.0
          w = 0.0

          do jj=j0,j1
             if(jj>Trans%N_in(2)) then
                jx = jj - Trans%N_in(2)
             else
                jx = jj
             end if

             do ii=i0,i1
                if(ii>Trans%N_in(1)) then
                   ix = ii - Trans%N_in(1)
                else
                   ix = ii
                end if

                f = fx(ii)*fy(jj)

                where(qin(ix,jx,:) /= undef_)
                   q = q + f*qin(ix,jx,:)
                   w = w + f           
                end where
             end do
          end do

          where( w >= wc )
             qout(i,j,:) = q / w
          elsewhere
             qout(i,j,:) = undef_
          end where

       end do
    end do

    endif

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformRun3R8

  subroutine MAPL_HorzTransformRunV3R8(Trans, uin, vin, uout, vout, undef, rotate, rc)

!    Trans ....  Precomputed transform
!    undef ....  UNDEF Value

    type (MAPL_HorzTransform),intent(INout) :: Trans
    real*8,                     intent(INOUT) :: uin (:,:,:),vin (:,:,:)
    real*8,                     intent(INOUT) :: uout(:,:,:),vout(:,:,:)
    real,    optional,        intent(IN   ) :: undef
    logical, optional,        intent(IN   ) :: rotate
    integer, optional,        intent(  OUT) :: rc

    integer           :: STATUS
    real*8            :: Win (size(uin ,1),size(uin ,2),size(uin ,3)*3)
    real*8            :: Wout(size(uout,1),size(uout,2),size(uout,3)*3)
    logical           :: InputIsLL

    if(present(rc)) rc = 0
   
    ASSERT_(CubeLatLonIsCreated(Trans%CubeTrans))
    ASSERT_(size(uin,3)==size(uout,3))

    InputIsLL = Trans%gridtypeOut == 'Cubed-Sphere'

    call SphericalToCartesian(Trans%CubeTrans, uin , vin , Win , &
          Trans%Transpose, SphIsLL=InputIsLL)

    call MAPL_HorzTransformRun3R8(Trans, Win, Wout, undef=undef, rc=status)
    VERIFY_(STATUS)

    call CartesianToSpherical(Trans%CubeTrans, Wout, uout, vout, &
          Trans%Transpose, SphIsLL=.not.InputIsLL, rotate=rotate)

    RETURN_(SUCCESS)
  end subroutine MAPL_HorzTransformRunV3R8

  subroutine MAPL_HorzTransformRunParallelFromArraysR8(Trans,In,Out,undef,rc)
    type (MAPL_HorzTransform),intent(INout) :: Trans
    real*8,  target,            intent(INout) :: In(:,:,:)
    real*8,  target,            intent(inOUT) :: Out(:,:,:)
    real,    optional,        intent(IN   ) :: undef
    integer, optional,        intent(  OUT) :: RC

! Locals
!-------

    integer                    :: STATUS
    character(len=ESMF_MAXSTR) :: IAm="MAPL_TransformRunParallelFromArrays"
    real*8, pointer              :: InGlob (:,:,:)
    real*8, allocatable          :: OutGlob(:,:,:)

    ASSERT_(size(In,3)==size(Out,3))

! Everybody has to participate in the collective gather.
!   If we are a root of this gather we will have one or
!   more global layers in InGlob, and the third dimension
!   InGlob (which is allocated by the gather) is exactly
!   the number of layers allotted to us.
!--------------------------------------------------------

    nullify(InGlob)

    call MAPL_CollectiveGather3D(Trans%Gridin, In, InGlob, rc=STATUS)
    VERIFY_(STATUS)

! If we are one of the Roots of the gather, allocate the 3D global
!   output and then do the serial transform on the layers allotted to us.
!   We know we are root because the Gather call allocated a non-trivial
!   InGlob at the Roots, elsewhere it allocated a trivial (1,1,1) 3D array.
!--------------------------------------------------------------------------

    if(size(InGlob)>1) then
       allocate(OutGlob(Trans%N_out(1),Trans%N_out(2),size(InGlob,3)), stat=STATUS)
       VERIFY_(STATUS)

       call MAPL_HorzTransformRun3R8(Trans, InGlob, OutGlob, undef, rc=STATUS)
       VERIFY_(STATUS)
    endif

    deallocate(InGlob)

! Everybody has to participate in the collective scatter
!-------------------------------------------------------

    call MAPL_CollectiveScatter3D(Trans%Gridout, OutGlob, Out, rc=STATUS)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_HorzTransformRunParallelFromArraysR8
#endif

!JK patch for conservative interp---------------
  subroutine Get_conservative_transform(name)
    character(len=120)  :: name
    call get_conservative_weights(name)
  end subroutine Get_conservative_transform
!JK patch for conservative interp---------------


  subroutine MAPL_HorzTransformCreateConservative(Trans, FILENAME, &
       GridIn, GridOut, RootOnly, VM, RC)
! args
    type (MAPL_HorzTransform), target, intent(INOUT) :: Trans
    character (len=*), intent(IN   ) :: FILENAME
    character (len=*), intent(IN   ) :: GridIn
    character (len=*), optional, intent(IN   ) :: GridOut
    logical,           optional, intent(IN   ) :: RootOnly
    type(ESMF_VM), optional          :: VM
    integer, optional, intent(  OUT) :: RC

! local args
    integer :: status
    integer :: I
    type(MAPL_RegridConserv), pointer  :: ConsrvTrans
    type(GlobalTileTrans),    pointer  :: GlobalTrans
    type(ESMF_VM)                      :: usableVM

    if (present(VM)) then
       usableVM = VM
    else
       call ESMF_VmGetCurrent(usableVM, RC=STATUS)
       VERIFY_(STATUS)
    end if

! first check if this transform was already created
    do i=1,MAX_AvailableTransforms
       if(AvailableTransforms(i)%created) then
          Trans = AvailableTransforms(i)
          ConsrvTrans => Trans%ConsrvTrans
          GlobalTrans => ConsrvTrans%GlobalTrans
          if (Trans%runTile) then
             if (ConsrvTrans%created .and. &
                 ConsrvTrans%tilefile == filename .and. &
                 GlobalTrans%GInfo(1)%GridName == GridIn ) then
                if (present(GridOut)) then
                   ASSERT_(GlobalTrans%GInfo(2)%GridName == GridOut)
                end if
                ! found it!
                if (.not. ConsrvTrans%tileDataGlobal) then
                   call MAPL_TileTransGather(usableVM, ConsrvTrans%GlobalTrans, RC=status)
                   VERIFY_(STATUS)
                end if
                return
             end if
          end if
       end if
    enddo


! not found, do a full create

    Trans%ConsrvTrans%tileDataGlobal=.true.
!    if (present(VM)) then
!       Trans%ConsrvTrans%tileDataGlobal=.false.
!    else
!       Trans%ConsrvTrans%tileDataGlobal=.true.
!    end if

    call MAPL_RegridConservativeCreate(FILENAME, GridIn, GridOut, RootOnly, &
                                       ConsrvTrans=Trans%ConsrvTrans, &
                                       VM=usableVM, RC=status)
    VERIFY_(STATUS)
    Trans%created = .true.
    Trans%runTile = .true.
    Trans%transpose = .false.
    Trans%order = 0
    Trans%ConsrvTrans%runDistributed=.false.

    Trans%N_in(1)=Trans%ConsrvTrans%GlobalTrans%GInfo(1)%IM
    Trans%N_in(2)=Trans%ConsrvTrans%GlobalTrans%GInfo(1)%JM
    Trans%N_out(1)=Trans%ConsrvTrans%GlobalTrans%GInfo(2)%IM
    Trans%N_out(2)=Trans%ConsrvTrans%GlobalTrans%GInfo(2)%JM

! put it in the AvailTrans
    do i=1,MAX_AvailableTransforms
       if(AvailableTransforms(i)%created) then
          if(i==MAX_AvailableTransforms) then
             print *, "Error in HorzTransform: Out Of Space for new transform"
             ASSERT_(.false.)
          else
             cycle
          end if
       else
          AvailableTransforms(i)=Trans
          if (.not. Trans%ConsrvTrans%tileDataGlobal) then
             call MAPL_TileInfoMarkDestroyed( &
                  TI=AvailableTransforms(i)%ConsrvTrans%GlobalTrans%Global%IN)
             call MAPL_TileInfoMarkDestroyed( &
                  TI=AvailableTransforms(i)%ConsrvTrans%GlobalTrans%Global%OUT)
          end if
          exit
       end if
    end do

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_HorzTransformCreateConservative

  subroutine MAPL_RegridConservativeCreate(FILENAME, GridIn, GridOut, RootOnly, ConsrvTrans, VM, RC)
! args
    character (len=*), intent(IN   ) :: FILENAME
    character (len=*), intent(IN   ) :: GridIn
    character (len=*), optional, intent(IN   ) :: GridOut
    logical,           optional, intent(IN   ) :: RootOnly
    type(MAPL_RegridConserv), target :: ConsrvTrans
    type(ESMF_VM)                    :: VM
    integer, optional, intent(  OUT) :: RC

! local vars
    integer :: status
    integer :: NT, UNIT, NGRIDS, NPES
    integer :: I, IM, JM
    integer :: LT, deId
    logical :: amIRoot
    character(len=ESMF_MAXSTR) :: STRING
    real, pointer :: buffer(:) => NULL()
    type(GlobalTileTrans),  pointer  :: GT
    logical :: found
    logical :: RootOnly_
    logical :: TransRoot
    integer :: gridInIndex

! convenient alias
    GT => ConsrvTrans%GlobalTrans

! do not allow to create again
!    NEGASSERT_(.not. ConsrvTrans%created)

    NEGASSERT_(.not. ConsrvTrans%runDistributed)

    if (present(RootOnly)) then
       RootOnly_ = RootOnly
    else
       RootOnly_ = .true.
    end if

    ConsrvTrans%created = .true.

    amIRoot = MAPL_AM_I_Root(VM)

    if (RootOnly_) then
       TransRoot = amIRoot
    else
       TransRoot = .true.
    end if

    ConsrvTrans%tilefile = filename

    UNIT = GETFILE(FILENAME, form='UNFORMATTED', RC=status)
    VERIFY_(STATUS)

    if(amIRoot) read(UNIT) NT
    call MAPL_CommsBcast(vm, DATA=NT, N=1, ROOT=0, RC=status)
    VERIFY_(STATUS)

    if(amIRoot) then
       read(unit) NGRIDS
       ASSERT_(NGRIDS == 2)
    end if
    NGRIDS = 2
       
    GT%NT = NT

    DO I = 1,NGRIDS
       if(amIRoot) then
          read(unit) STRING
          read(unit) IM
          read(unit) JM
       end if
       call MAPL_CommsBcast(vm, DATA=STRING, N=ESMF_MAXSTR, ROOT=0, RC=status)
       VERIFY_(STATUS)
       call MAPL_CommsBcast(vm, DATA=IM, N=1, ROOT=0, RC=status)
       VERIFY_(STATUS)
       call MAPL_CommsBcast(vm, DATA=JM, N=1, ROOT=0, RC=status)
       VERIFY_(STATUS)
       GT%GInfo(I)%GridName = STRING
       GT%GInfo(I)%IM = IM
       GT%GInfo(I)%JM = JM
    end DO

    ! find "complement" of attached grid
    found = .false.
    do I = 1, NGRIDS
       if ( GT%GInfo(I)%GridName == GridIn) then
          found = .true.
          exit
       endif
    enddo
! if not found we assume the tile file has new-style gridnames    
    if (.not. found) then
       do I = 1, NGRIDS
          ! convert to GEOS-5 convention grid names
          call GenOldGridName_(GT%GInfo(I)%GridName)
       enddo

       ! search again
       do I = 1, NGRIDS
          if ( GT%GInfo(I)%GridName == GridIn) then
             found = .true.
             exit
          endif
       enddo
    end if

    ASSERT_(found)
    gridInIndex = I
!    NG = 3-I

    if (gridInIndex /= 1) then
       ! the input grid is the second grid in the tile file.
       ! Reorder the table (for convenience)
        string = GT%GInfo(2)%GridName
        GT%GInfo(2)%GridName =  GT%GInfo(1)%GridName 
        GT%GInfo(1)%GridName = string 

        IM = GT%GInfo(2)%IM
        GT%GInfo(2)%IM =  GT%GInfo(1)%IM 
        GT%GInfo(1)%IM = IM

        JM = GT%GInfo(2)%JM
        GT%GInfo(2)%JM =  GT%GInfo(1)%JM 
        GT%GInfo(1)%JM = JM
     end if

!ALT for simplicity (FOR NOW) assume first grid is IN, second is OUT
    ASSERT_(GT%GInfo(1)%GridName == GridIn)
    if (present(GridOut)) then
       ASSERT_(GT%GInfo(2)%GridName == GridOut)
    end if

    call MAPL_AllocateShared(GT%Global%IN%II, (/NT/), TransRoot=TransRoot, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_AllocateShared(GT%Global%IN%JJ, (/NT/), TransRoot=TransRoot, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_AllocateShared(GT%Global%OUT%II, (/NT/), TransRoot=TransRoot, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_AllocateShared(GT%Global%OUT%JJ, (/NT/), TransRoot=TransRoot, RC=STATUS)
    VERIFY_(STATUS)
    call MAPL_AllocateShared(GT%Global%OUT%W, (/NT/), TransRoot=TransRoot, RC=STATUS)
    VERIFY_(STATUS)

    ! we need a buffer (REAL) for real to INT conversion
    ! to save memory, we temporary "borrow" W
    call MAPL_AllocateShared(buffer, (/NT/), TransRoot=TransRoot, RC=STATUS)
    VERIFY_(STATUS)

    call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
    if (amIRoot) then
       read(unit) ! skip Type
       read(unit) ! skip X
       read(unit) ! skip Y

       read(unit) buffer
    end if
    call MAPL_BcastShared(vm, DATA=buffer, N=NT, ROOT=0, RootOnly=RootOnly_, RC=status)
    if (gridInIndex == 1) then
       GT%Global%IN%II = NINT(buffer)
    else
       GT%Global%OUT%II = NINT(buffer)
    end if

    call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
    if (amIRoot) then
       read(unit) buffer
    end if
    call MAPL_BcastShared(vm, DATA=buffer, N=NT, ROOT=0, RootOnly=RootOnly_, RC=status)
    call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
    if (gridInIndex == 1) then
       GT%Global%IN%JJ = NINT(buffer)
    else
       GT%Global%OUT%JJ = NINT(buffer)
    end if

    call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)

    if (gridInIndex == 1) then
       if (amIRoot) then
          read(unit) ! skip W_IN
       end if
    else
       if (amIRoot) then
          read(unit) buffer
       end if
       call MAPL_BcastShared(vm, DATA=buffer, N=NT, ROOT=0, RootOnly=RootOnly_, RC=status)
       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
       GT%Global%OUT%W = buffer
    endif

    if (amIRoot) then
       read(unit) buffer
    end if
    call MAPL_BcastShared(vm, DATA=buffer, N=NT, ROOT=0, RootOnly=RootOnly_, RC=status)
    call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
    if (gridInIndex == 1) then
       GT%Global%OUT%II = NINT(buffer)
    else
       GT%Global%IN%II = NINT(buffer)
    end if
       
    call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)

    if (amIRoot) then
       read(unit) buffer
    end if
    call MAPL_BcastShared(vm, DATA=buffer, N=NT, ROOT=0, RootOnly=RootOnly_, RC=status)
    call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
    if (gridInIndex == 1) then
       GT%Global%OUT%JJ = NINT(buffer)
    else
       GT%Global%IN%JJ = NINT(buffer)
    end if

    call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)

    if (gridInIndex == 1) then
       if (amIRoot) then
          read(unit) buffer
       end if
       call MAPL_BcastShared(vm, DATA=buffer, N=NT, ROOT=0, RootOnly=RootOnly_, RC=status)
       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
       GT%Global%OUT%W = buffer
       call MAPL_SyncSharedMemory(RC=STATUS); VERIFY_(STATUS)
    end if
    DEALOCS_(buffer)

    call FREE_FILE(UNIT)

      !print*, 'Read(1): ', trim(FILENAME), GT%GInfo(1)%IM, GT%GInfo(1)%JM 
      !print*, 'Read(2): ', trim(FILENAME), GT%GInfo(2)%IM, GT%GInfo(2)%JM


    if (.not. ConsrvTrans%tileDataGlobal) then
       ! bcast/scatter
       call MAPL_CommsBcast(vm, GT%NT, &
            N=1, ROOT=MAPL_Root, RC=STATUS)
       VERIFY_(STATUS)

       DO I=1,2
          call MAPL_CommsBcast(vm, GT%GInfo(I)%IM, &
               N=1, ROOT=MAPL_Root, RC=STATUS)
          VERIFY_(STATUS)

          call MAPL_CommsBcast(vm, GT%GInfo(I)%JM, &
               N=1, ROOT=MAPL_Root, RC=STATUS)
          VERIFY_(STATUS)
       END DO

       NT = GT%NT

       call ESMF_VMGet(vm, localPet=deId, petCount=npes, rc=status)
       VERIFY_(STATUS)

       allocate(GT%tilesPerDE(0:npes-1), stat=status)
       VERIFY_(STATUS)

!       compute partition (equal amount on every PE)
       call MAPL_DecomposeDim (NT, GT%tilesPerDE, npes )

       LT = GT%tilesPerDE(deId)

!       allocate LOCAL%IN and LOCAL%OUT
       allocate( GT%Local%IN%II(LT), &
                 GT%Local%IN%JJ(LT), &
                 GT%Local%OUT%II(LT), &
                 GT%Local%OUT%JJ(LT), &
                 GT%Local%OUT%W(LT), stat=status)
       VERIFY_(STATUS)

!       scatter IN(ii,jj), OUT(ii,jj,ww)

       call ArrayScatter(local_array=GT%Local%IN%II, &
                         global_array=GT%Global%IN%II, &
                         sendCounts=GT%tilesPerDE, &
                         vm=vm, srcPe=MAPL_Root, rc=status)
       VERIFY_(STATUS)
       call ArrayScatter(local_array=GT%Local%IN%JJ, &
                         global_array=GT%Global%IN%JJ, &
                         sendCounts=GT%tilesPerDE, &
                         vm=vm, srcPe=MAPL_Root, rc=status)
       VERIFY_(STATUS)

       call ArrayScatter(local_array=GT%Local%OUT%II, &
                         global_array=GT%Global%OUT%II, &
                         sendCounts=GT%tilesPerDE, &
                         vm=vm, srcPe=MAPL_Root, rc=status)
       VERIFY_(STATUS)
       call ArrayScatter(local_array=GT%Local%OUT%JJ, &
                         global_array=GT%Global%OUT%JJ, &
                         sendCounts=GT%tilesPerDE, &
                         vm=vm, srcPe=MAPL_Root, rc=status)
       VERIFY_(STATUS)
       call ArrayScatter(local_array=GT%Local%OUT%W, &
                         global_array=GT%Global%OUT%W, &
                         sendCounts=GT%tilesPerDE, &
                         vm=vm, srcPe=MAPL_Root, rc=status)
       VERIFY_(STATUS)

!      the global data will be deallocated in destroy,
!      otherwise we need a gather (and we already got the data)

    end if

    RETURN_(ESMF_SUCCESS)

  contains
   subroutine GenOldGridName_(name)
     character(len=*) :: name

     integer :: im, jm
     integer :: nn, xpos
     character(len=128) :: gridname
     character(len=2)   :: dateline, pole
     character(len=8)   :: imsz, jmsz
     character(len=128) :: imstr, jmstr
     

     ! Parse name for grid info 
     !-------------------------

     Gridname = AdjustL(name)
     nn   = len_trim(Gridname)
     xpos = index(Gridname,'x')
     imsz = Gridname(3:xpos-1)
     dateline = Gridname(1:2)
     pole = Gridname(xpos+1:xpos+2)


     if (pole=='6C') then ! cubed-sphere
        dateline='CF'
        pole='PE'

        read(IMSZ,*) IM
        jm = 6*im
     else
        jmsz = Gridname(xpos+3:nn)
        read(IMSZ,*) IM
        read(JMSZ,*) JM
     endif
    
     write(imstr,*) im
     write(jmstr,*) jm
     gridname =  pole // trim(adjustl(imstr))//'x'//&
                 trim(adjustl(jmstr))//'-'//dateline

     name = gridname

   end subroutine GenOldGridName_


  end subroutine MAPL_RegridConservativeCreate

  subroutine MAPL_RegridConservativeDestroy(ConsrvTrans, RC)
! args
    type(MAPL_RegridConserv)         :: ConsrvTrans
    integer, optional, intent(  OUT) :: RC

! local args
    integer :: status


    NEGASSERT_(.not. ConsrvTrans%runDistributed)

    if (ConsrvTrans%created) then

       call MAPL_TileTransDestroy(ConsrvTrans%GlobalTrans%Global)
       call MAPL_TileTransDestroy(ConsrvTrans%GlobalTrans%Local)
    end if

    ConsrvTrans%created = .false.

    RETURN_(ESMF_SUCCESS)

  end subroutine MAPL_RegridConservativeDestroy

  subroutine MAPL_RegridConservativeRun(ConsrvTrans, INPUT, OUTPUT,SAMPLE, RC)
! args
    type(MAPL_RegridConserv), TARGET :: ConsrvTrans
    real,              intent(IN   ) :: INPUT(:,:)
    real,              intent(  OUT) :: OUTPUT(:,:)
    logical, optional, intent(IN   ) :: SAMPLE
    integer, optional, intent(  OUT) :: RC

! local args
    integer :: status
    real, allocatable :: tile(:)
    type(GlobalTileTrans), pointer  :: GlobalTrans
    type(TileTrans),       pointer  :: TT

! Aliases
    GlobalTrans => ConsrvTrans%GlobalTrans
    TT => GlobalTrans%Global

! Sanity checks: make sure sizes match 
    ASSERT_(size(input,1)  == GlobalTrans%GInfo(1)%IM)
    ASSERT_(size(input,2)  == GlobalTrans%GInfo(1)%JM)
    ASSERT_(size(output,1) == GlobalTrans%GInfo(2)%IM)
    ASSERT_(size(output,2) == GlobalTrans%GInfo(2)%JM)

    allocate(tile(GlobalTrans%NT), stat=status)
    VERIFY_(STATUS)

    call G2T_(tile, input, TT%IN%II, TT%IN%JJ)

!ALT: T2T is not needed since we are doing global transform on single PE

    call T2G_(output, tile, TT%Out%II, TT%Out%JJ, TT%Out%W, SAMPLE)

    deallocate(tile)

    RETURN_(ESMF_SUCCESS)

  contains
    subroutine G2T_ ( OUTPUT, INPUT, II, JJ )

      !ARGUMENTS:
      real,                      intent(OUT) :: OUTPUT(:)
      real,                      intent(IN) :: INPUT(:,:)
      integer,                   intent(IN) :: II(:)
      integer,                   intent(IN) :: JJ(:)
      !EOPI
      
      ! Local variables

      integer :: N

      do N = 1, size(OUTPUT)
         OUTPUT(N) = INPUT(II(N),JJ(N))
      end do

    end subroutine G2T_

    subroutine T2G_ (OUTPUT, INPUT, I, J, W, SAMPLE )
  
      !ARGUMENTS:
      real,                      intent(OUT) :: OUTPUT(:,:)
      real,                      intent(IN) :: INPUT(:)
      integer,                   intent(IN) :: I(:)
      integer,                   intent(IN) :: J(:)
      real,                      intent(IN) :: W(:)
      logical, optional,         intent(IN) :: SAMPLE

      !EOPI
  
      ! Local variables

      integer :: N, II, JJ
      logical :: uSAMPLE 
      real, allocatable  :: FF(:,:)

      if (present(SAMPLE)) then
         uSAMPLE = SAMPLE
      else
         uSAMPLE = .false.
      end if

      OUTPUT = 0.0
      if(uSample) then
         OUTPUT = MAPL_Undef
      end if

      allocate(FF(size(OUTPUT,1),size(OUTPUT,2)), stat=STATUS)
      VERIFY_(STATUS)
      FF = 0.0

      do N = 1, size(INPUT)
         if(INPUT(N)/=MAPL_UNDEF) then
            II = I(N)
            JJ = J(N) 

            if(uSample) then
               if(W(N) > FF(II,JJ)) then
                  OUTPUT(II,JJ) = INPUT(N)
                  FF    (II,JJ) = W(N)
               end if
            else
               OUTPUT(II,JJ) = OUTPUT(II,JJ) + W(N) * INPUT(N)
               FF    (II,JJ) = FF    (II,JJ) + W(N)
            endif
          endif
       end do

       if(.not.uSample) then
          where(FF>0.0)
             OUTPUT = OUTPUT / FF
          elsewhere 
             OUTPUT = MAPL_Undef
          end where
       end if

       deallocate(FF)

       RETURN_(ESMF_SUCCESS)

     end subroutine T2G_

  end subroutine MAPL_RegridConservativeRun

  subroutine MAPL_TileInfoDestroy(TI)
    type(TileInfo) :: TI

    integer :: status

    DEALOCS_(TI%II)
    DEALOCS_(TI%JJ)
    DEALOCS_(TI%W)

    call MAPL_TileInfoMarkDestroyed(TI)

  end subroutine MAPL_TileInfoDestroy
  
  subroutine MAPL_TileInfoMarkDestroyed(TI)
    type(TileInfo) :: TI

    NULLIFY(TI%II)
    NULLIFY(TI%JJ)
    NULLIFY(TI%W)

  end subroutine MAPL_TileInfoMarkDestroyed
  
  subroutine MAPL_TileTransDestroy(TR)
    type(TileTrans) :: TR

    call MAPL_TileInfoDestroy(TR%IN)
    call MAPL_TileInfoDestroy(TR%OUT)

  end subroutine MAPL_TileTransDestroy

  logical function  MAPL_ConsrvTransMatch(T1,T2)
    type(MAPL_RegridConserv) :: T1, T2

    MAPL_ConsrvTransMatch = .false. !default

    if (.not. T1%created .or. .not. T2%created) return
    if (T1%tileFile /= T2%tileFile) return
    if (T1%GlobalTrans%GInfo(1)%GridName /= &
        T2%GlobalTrans%GInfo(1)%GridName ) return
    if (T1%GlobalTrans%GInfo(2)%GridName /= &
        T2%GlobalTrans%GInfo(2)%GridName ) return

    MAPL_ConsrvTransMatch = .true. ! real match found
    return

  end function MAPL_ConsrvTransMatch

  subroutine MAPL_TileTransGather(vm, GT, RC)
    type(ESMF_VM)                    :: VM
    type(GlobalTileTrans)            :: GT
    integer, optional, intent(  OUT) :: RC

! local args
    integer :: status
    integer :: NT

! check if allocation is needed (should be needed all the time)
    NT = GT%NT
! we check only II 
    if (.not. associated(GT%Global%IN%II)) then
       allocate( GT%Global%IN%II(NT), &
                 GT%Global%IN%JJ(NT), &
                 GT%Global%OUT%II(NT), &
                 GT%Global%OUT%JJ(NT), &
                 GT%Global%OUT%W(NT), stat=status)
       VERIFY_(STATUS)
    end if

! Gather

    call ArrayGather(local_array=GT%Local%IN%II, &
                     global_array=GT%Global%IN%II, &
                     recvCounts=GT%tilesPerDE, &
                     vm=vm, dstPe=MAPL_Root, rc=status)
    VERIFY_(STATUS)

    call ArrayGather(local_array=GT%Local%IN%JJ, &
                     global_array=GT%Global%IN%JJ, &
                     recvCounts=GT%tilesPerDE, &
                     vm=vm, dstPe=MAPL_Root, rc=status)
    VERIFY_(STATUS)

    call ArrayGather(local_array=GT%Local%OUT%II, &
                     global_array=GT%Global%OUT%II, &
                     recvCounts=GT%tilesPerDE, &
                     vm=vm, dstPe=MAPL_Root, rc=status)
    VERIFY_(STATUS)

    call ArrayGather(local_array=GT%Local%OUT%JJ, &
                     global_array=GT%Global%OUT%JJ, &
                     recvCounts=GT%tilesPerDE, &
                     vm=vm, dstPe=MAPL_Root, rc=status)
    VERIFY_(STATUS)

    call ArrayGather(local_array=GT%Local%OUT%W, &
                     global_array=GT%Global%OUT%W, &
                     recvCounts=GT%tilesPerDE, &
                     vm=vm, dstPe=MAPL_Root, rc=status)
    VERIFY_(STATUS)

    RETURN_(ESMF_SUCCESS)
  end subroutine MAPL_TileTransGather

end module MAPL_HorzTransformMod
