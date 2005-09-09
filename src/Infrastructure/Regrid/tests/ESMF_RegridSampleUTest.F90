module options_table
public

integer, parameter :: nOptions=9
integer, parameter, dimension(1:nOptions) :: nChoices=(/ 2,2,4,2,3,3,2,4,4 /)

character(len=25),parameter, dimension(1:nOptions) :: Option_name= &
       (/ 'SRCDELAYOUT'  &   ! (1)
        , 'DSTDELAYOUT'  &   ! (2)
        , 'FUNCTION   '  &   ! (3)
        , 'REGSCHEME  '  &   ! (4)
        , 'SRCGRID    '  &   ! (5)
        , 'DSTGRID    '  &   ! (6)
        , 'DOMAIN     '  &   ! (7)
        , 'SRCHALO    '  &   ! (8)
        , 'DSTHALO    '  &   ! (9)
       /)
character(len=25),  dimension(1:4,1:nOptions) :: Option_choice


contains

subroutine get_table

!Option_name(1) = 'SRCDELAYOUT'
  Option_choice(1,1) = '1DX'
  Option_choice(2,1) = '1DY'
  Option_choice(3,1) = '2D'

!Option_name(2) = 'DSTDELAYOUT'
  Option_choice(1,2) = '1DX'
  Option_choice(2,2) = '1DY'
  Option_choice(3,2) = '2D'

!Option_name(3) = 'FUNCTION'
  Option_choice(1,3) = 'A'
  Option_choice(2,3) = 'B'
  Option_choice(3,3) = 'C'
  Option_choice(4,3) = 'D'

!Option_name(4) = 'REGSCHEME'
  Option_choice(1,4) = 'BILINEAR'
  Option_choice(2,4) = '1CONSERV'

!Option_name(5) = 'SRCGRID'
  Option_choice(1,5)='A'
  Option_choice(2,5)='D_NE'
  Option_choice(3,5)='C_NE'

!Option_name(6) = 'DSTGRID'
  Option_choice(1,6)='A'
  Option_choice(2,6)='D_NE'
  Option_choice(3,6)='C_NE'

!Option_name(7) = 'DOMAIN'
  Option_choice(1,7)='WHOLEGLOBE'
  Option_choice(2,7)='REGIONAL'

!Option_name(8) = 'SRCHALO'
   Option_choice(1,8)='0'
   Option_choice(2,8)='1'
   Option_choice(3,8)='2'
   Option_choice(4,8)='3'

!Option_name(9) = 'DSTHALO'
   Option_choice(1,9)='0'
   Option_choice(2,9)='1'
   Option_choice(3,9)='2'
   Option_choice(4,9)='3'

  return
end subroutine get_table

!==============================

subroutine search_table(name_element, choice_element, i_name, i_choice, &
                       found_name, found_choice )
character(len=*), intent(in) :: name_element, choice_element
integer :: i_name, i_choice
logical :: found_name, found_choice

!initialize
found_name=.false.
found_choice =.false.
i_name   =-1
i_choice =-1

name_loop: do j=1,nOptions
              if (name_element .eq. Option_name(j) ) then
                i_name=j
                found_name=.true.
                do i=1,nChoices(j)
                  if (choice_element .eq. Option_choice(i,j) ) then
                    i_choice=i
                    found_choice=.true.
                    exit name_loop
                  end if
                end do
              end if
            end do name_loop

end subroutine search_table
                  
end module options_table

!=============================================================================
module Unit_Test
implicit none

PRIVATE
PUBLIC AssertEqual

!Assert that 2 scalars are identical
interface AssertEqual
 module procedure AssertEqual_char
 module procedure AssertEqual_int
 module procedure AssertEqual_log
end interface

contains

function AssertEqual_char( varA, varB,comment, line, file )
logical :: AssertEqual_char
character, intent(in) :: varA, varB
character(len=*), optional, intent(in) :: comment
integer, optional :: line
CHARACTER(Len=*), optional :: file

character(len=35) :: comento

comento='                        '
if (present(comment)) comento=comment

if (varA .eq. varB) then
  AssertEqual_char = .true.
 !print*, '     >>>>PASS<<<<  AssertEqual: ', trim(comento)
 if (present(line))  print*, '     >>>>PASS<<<< ', line, file, comento
else
  AssertEqual_char = .false.
  print*, '     >>>>FAIL<<<<  AssertEqual: Comparison of ', trim(comento )
 if (present(line))  print*, '     >>>>FAIL<<<<  ', line, file, comento
end if

return
end function AssertEqual_char


function AssertEqual_int( varA, varB, comment, line, file )
logical :: AssertEqual_int
integer, intent(in) :: varA, varB
character(len=*), optional, intent(in) :: comment
integer, optional :: line
character(len=*), optional :: file

character(len=35) :: comento
comento=' '
if (present(comment)) comento=comment

if (varA .eq. varB) then
  AssertEqual_int = .true.
 !print*, '     >>>>PASS<<<< ', trim(comento)
 if (present(line))  print*, '     >>>>PASS<<<<  ', line, file, trim(comento)
else
  AssertEqual_int = .false.
 !print*, '     >>>>FAIL<<<<  ', trim(comento)  
 if (present(line))  print*, '     >>>>FAIL<<<<  ', line, file, trim(comento)
end if

return
end function AssertEqual_int


function AssertEqual_log( varA, varB, comment, line, file)
logical :: AssertEqual_log
logical, intent(in) :: varA, varB
character(len=*), optional, intent(in) :: comment
integer, optional :: line
character(len=*), optional :: file

character(len=35) :: comento

comento=' '
if (present(comment)) comento=comment

AssertEqual_log = (varA .and. varB) .or..not. (varA .or. varB) 

if (AssertEqual_log ) then
  if (present(line)) print*, '     >>>>PASS<<<<  ', line, trim(file),  trim(comento) 
else
  if (present(line)) print*, '     >>>>FAIL<<<<  ', line, trim(file), trim(comento) 
end if

return
end function AssertEqual_log


end module Unit_Test
    module RegridArgs
!------------------------------------------------------------------------------

#include <ESMF_Macros.inc>

    !--- USES:Framework module
    use ESMF_TestMod  ! test methods
    use ESMF_Mod      ! Framework module
    integer :: lrc,iFunction,iRegrid,ig
    integer ::  npets, localPet
    type(ESMF_VM) :: vm
    type(ESMF_RegridMethod) :: RegridChoice(2)

    integer, parameter :: n_grid_pairs=4
    type(ESMF_GridHorzStagger) :: SrcGridHorzChoice(n_grid_pairs), &
                                  DstGridHorzChoice(n_grid_pairs)
    type(ESMF_RelLoc) :: SrcRelLocChoice(n_grid_pairs), &
                         DstRelLocChoice(n_grid_pairs)

    !--- cumulative result: count failures; no failures equals "all pass"
    integer :: result=0
    integer :: regrid_rc    !single test error indicator

    integer :: iSrcDistr, iDstDistr, nXY(3,2)
    integer :: TwoOrOne

    integer, parameter :: nHalo=4
    integer, dimension(nHalo) :: SrcHaloChoice, DstHaloChoice

    !--- individual test failure message
    character(ESMF_MAXSTR) :: failMsg
    character(ESMF_MAXSTR) :: name

    contains


    subroutine initTables

    integer :: i
   !--- Domain Decompositions used
    nXY(1,:)=(/ npets, 1 /)                       !1DX
    nXY(2,:)=(/ 1, npets /)                       !1DY
    TwoOrOne= 1 + mod(npets+1,2)
    nXY(3,:)=(/ npets/TwoOrOne, TwoOrOne /)       !2D

    regrid_rc=ESMF_SUCCESS

   !--- Regrid methods being tested
    RegridChoice(1)=ESMF_REGRID_METHOD_BILINEAR
    RegridChoice(2)=ESMF_REGRID_METHOD_CONSERV1

   !--- SOURCE Grid types and corresponding field relative_cell_location
    SrcGridHorzChoice(1) = ESMF_GRID_HORZ_STAGGER_A
    SrcRelLocChoice(1) = ESMF_CELL_CENTER

    SrcGridHorzChoice(2) = ESMF_GRID_HORZ_STAGGER_D_NE
    SrcRelLocChoice(2) = ESMF_CELL_EFACE

    SrcGridHorzChoice(3) = ESMF_GRID_HORZ_STAGGER_B_NE
    SrcRelLocChoice(3) = ESMF_CELL_NECORNER

    SrcGridHorzChoice(4) = ESMF_GRID_HORZ_STAGGER_C_NE
    SrcRelLocChoice(4) = ESMF_CELL_NFACE

   !--- DESTINATION Grid types and corresponding field relative_cell_location
    DstGridHorzChoice(1) = ESMF_GRID_HORZ_STAGGER_A
    DstRelLocChoice(1) = ESMF_CELL_CENTER

    DstGridHorzChoice(2) = ESMF_GRID_HORZ_STAGGER_D_NE
    DstRelLocChoice(2) = ESMF_CELL_EFACE

    DstGridHorzChoice(3) = ESMF_GRID_HORZ_STAGGER_B_NE
    DstRelLocChoice(3) = ESMF_CELL_NECORNER

    DstGridHorzChoice(4) = ESMF_GRID_HORZ_STAGGER_C_NE
    DstRelLocChoice(4) = ESMF_CELL_NFACE

   !---SOURCE  and DESTINATION  halo size 
    do i=1,4
      SrcHaloChoice(i) = i-1
      DstHaloChoice(i) = i-1
    end do

  !TODO: Print documentation for what 1)the 1-4 field test functions are
  !TODO:                              2)source and dest. grid choices are
  !TODO:                              3)formula is used for relative error calc

    end subroutine initTables
    end module RegridArgs
!================================================================================


    program ESMF_FieldRegridUTest

    use RegridArgs

    character(len=100           ) :: longString    ! [128]
   !character(len=ESMF_MAXSTR-13) :: longString    ! [128]

    call ESMF_TestStart(ESMF_SRCLINE, rc=lrc)
    call ESMF_VMGetGlobal(vm, rc=lrc)
    !--- Get number of PETs we are running with
    call ESMF_VMGet(vm, petCount=npets, localPET=localPet,  rc=lrc)

  !--LongString specifies all the adjustable parameters of the regrid test.
  !  Possible options are as follows (Ref: Options_Table subroutine):
  !   FUNCTION: C, D    (Note, A and B are not appropriate for a sphere)
  !   REGSCHEME: BILINEAR, 1CONSERV
  !   SRCGRID  : A, D_NE, C_NE
  !   DSTGRID  : A, D_NE, C_NE
  !   SRCDELAYOUT: 1DX, 1DY, 2D
  !   DSTDELAYOUT: 1DX, 1DY, 2D

    longString='FUNCTION:C:REGSCHEME:1CONSERV' &
             ! //':SRCGRID:D_NE:DSTGRID:A:SRCDELAYOUT:1DY:DSTDELAYOUT:1DX' &
               //':DOMAIN:WHOLEGLOBE:SRCHALO:3:DSTHALO:0'

    call setupRegridUTest(longString,ier)

#ifdef ESMF_EXHAUSTIVE
   !Test for "success" of regridding
   !--------------------------------
   !EX_UTest
    write(failMsg, *) "Error in regrid"
    write(name, *) "Regrid test: "//longString
    call ESMF_Test((regrid_rc.eq.ESMF_SUCCESS),name, failMsg, result, &
                    ESMF_SRCLINE)
#endif

    call ESMF_TestEnd(result, ESMF_SRCLINE)

    stop
    end program ESMF_FieldRegridUTest

!============================================================================
    subroutine setupRegridUTest(longString,ier)

    use RegridArgs
    use options_table, only : nOptions

    implicit none

interface
  subroutine find_indices( longString, maskName, choiceIndex, nOptions, &
                           nSelected, ier)
   integer, intent(in) :: nOptions
   character(len=*), intent(in) :: longString
   integer, intent(out), dimension(:) ::  choiceIndex
   logical, intent(out), dimension(:) ::  maskName
   integer, intent(out) :: ier, nSelected
  end subroutine find_indices
end interface

interface
    subroutine Regrid(FieldChoice, nSrcPetsXY, npetsXY, MethodChoice, &
                           SrcGridChoice,DstGridChoice, &
                           SrcLocChoice, DstLocChoice, &
                           SrcHalo, DstHalo, &
                           domainType, error_threshold, ier )
    use RegridArgs
    implicit none
      integer, intent(in) :: FieldChoice
      type(ESMF_RegridMethod), intent(in) :: MethodChoice
      type(ESMF_GridHorzStagger), intent(in) :: SrcGridChoice, DstGridChoice
      integer, intent(in) :: SrcHalo, DstHalo
      type(ESMF_RelLoc), intent(in) :: SrcLocChoice, DstLocChoice
      real(ESMF_KIND_R8), optional :: error_threshold
      integer, intent(in) :: npetsXY(2)
      integer, intent(in) :: nSrcPetsXY(2)
      integer, intent(in) :: domainType
      integer, optional, intent(out) :: ier
    end subroutine Regrid
end interface

    character(len=*), intent(in) :: longString

    integer :: ier
    integer, dimension(nOptions) ::  choiceIndex
    logical, dimension(nOptions) ::  maskName
    integer :: nChoices
    integer :: nSelected
    integer :: iSrcRelLoc, iDstRelLoc
    integer :: iSrcHalo, iDstHalo
    integer :: iDomain
    real(ESMF_KIND_R8) :: err_threshold=0.01
    

    !--- Initialize the error flag
    regrid_rc=ESMF_SUCCESS


    !--Access the tables of indices corresponding to the test options available.
    call initTables

    !--Find the indices associated with the test options selected in longString.
    call find_indices(longString, maskName, choiceIndex, nOptions, nSelected, &
                      ier)

    !DEBUG......
    if (localPET == 0) then
      print*,'longString=',trim(longString)
      print*,' maskName=',maskName
      print*,' choiceIndex=',choiceIndex
      print*,' nSelected=',nSelected 
    end if

    !--- Default settings for testing:
     iDstDistr =1
     iSrcDistr =2
     iFunction =1
     iRegrid   =2
     iSrcRelLoc=2
     iDstRelLoc=3
     iDomain   =1
     iSrcHalo  =4
     iDstHalo  =1
    !---Modify according to the choices in longString...
     if (MaskName(1)) iSrcDistr=ChoiceIndex(1)
     if (MaskName(2)) iDstDistr=ChoiceIndex(2)
     if (MaskName(3)) iFunction=CHoiceIndex(3)
     if (MaskName(4)) iRegrid=ChoiceIndex(4)
     if (MaskName(5)) iSrcRelLoc=ChoiceIndex(5)
     if (MaskName(6)) iDstRelLoc=ChoiceIndex(6)
     if (MaskName(7)) iDomain=ChoiceIndex(7)
     if (MaskName(8)) iSrcHalo=ChoiceIndex(8)
     if (MaskName(9)) iSrcHalo=ChoiceIndex(9)

     err_threshold=0.2

    !--Start the test..
          call Regrid(FieldChoice = iFunction,nSrcPetsXY= nXY(iSrcDistr,:), &
                           npetsXY=nXY(iDstDistr,:), &
                           MethodChoice = RegridChoice(iRegrid), &
                           SrcGridChoice = SrcGridHorzChoice(iSrcRelLoc), &
                           DstGridChoice = DstGridHorzChoice(iDstRelLoc), &
                           SrcLocChoice = SrcRelLocChoice(iSrcRelLoc), &
                           DstLocChoice = DstRelLocChoice(iDstRelLoc), &
                           SrcHalo = SrcHaloChoice(iSrcHalo), &
                           DstHalo = DstHaloChoice(iDstHalo), &
                           domainType   = iDomain, &
                           error_threshold= err_threshold )
          write(*,'(a,i2,a,i2,a,i2,a,i2,a,i2,a,i2)') &
                  'iSrcDistr=',iSrcDistr, &
                  ' iDstDistr=',iDstDistr, 'FieldChoice=',iFunction, &
                  ' RegridChoice=',iRegrid,' iSrcRelLoc=',iSrcRelLoc, &
                  ' iDstRelLoc=',iDstRelLoc,' iDomain=',iDomain, &
                  ' COMPLETED at process ', localPet

  end subroutine setupRegridUTest
    !-------------------------------------------------------------------
    subroutine Regrid(FieldChoice, nSrcPetsXY, npetsXY, MethodChoice, &
                           SrcGridChoice,DstGridChoice, &
                           SrcLocChoice, DstLocChoice, &
                           SrcHalo, DstHalo, &
                           domainType, error_threshold, ier )

  !--Execute a Regrid Unit test for a single set of testing parameter choices.

  !  FieldChoice -- Choice of test function (ESMF_RegridMethod).
  !  nSrcPetsXY -- Choice of geom. decomposition on the source grid (int).
  !  npetsXY -- Choice of geom. decomposition on destination grid (int).
  !  MethodCHoice -- Regridding Method Choice (ESMF_RegridMethod).
  !  Src(Dst)GridChoice -- Type of (Arakawa) grid (ESMF_GridHorzStagger).
  !  Src(Dst)LocChoice -- In cell relative location of data (ESMF_RelLoc).
  !  error_threshold -- Normalized error threshold (int).

    use RegridArgs
    implicit none
  
  
interface
  subroutine functionValues(Choice, xCoord, yCoord, Phi, Theta, &
                            lb, ub, halo, maxCoor, f90ptr, ier)

  use ESMF_Mod
  integer, intent(in)                             :: Choice
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: xCoord, yCoord
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: Theta, Phi
  integer, dimension(2), intent(in)               :: lb, ub
  integer, intent(in)                             :: halo
  real(ESMF_KIND_R8), dimension(2), intent(in)    :: maxCoor
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: f90ptr
  integer :: ier
  end subroutine functionValues
end interface

      ! Choice of test function for the field to be regridded
      ! 1 -->   f=x+y
      ! 2 -->   f=2+cos(pi*r/L)
      ! 3 -->   f=2+(cos(theta))**2 * cos(2*phi)
      ! 4-->    f=2+ (sin(2*theta))**16 * cos(16*phi)

      integer, intent(in) :: FieldChoice
      type(ESMF_RegridMethod), intent(in) :: MethodChoice
      type(ESMF_GridHorzStagger), intent(in) :: SrcGridChoice, DstGridChoice
      type(ESMF_RelLoc), intent(in) :: SrcLocChoice, DstLocChoice 
      integer, intent(in) :: SrcHalo, DstHalo
      real(ESMF_KIND_R8), optional :: error_threshold
      integer, intent(in) :: npetsXY(2)
      integer, intent(in) :: nSrcPetsXY(2)
      integer, intent(in) :: domainType
      integer, optional, intent(out) :: ier

    !--- Local variables
    type(ESMF_Field) :: field1, field2
    type(ESMF_Grid) :: srcgrid, dstgrid
    type(ESMF_RouteHandle) :: regrid_rh
    type(ESMF_DELayout) :: layout1, layout2
    integer :: rc

    integer :: i, j
    integer :: lbSrc(2), ubSrc(2)
    integer :: lbDst(2), ubDst(2)
    integer :: nx_domain, ny_domain
    integer ::  n_cells(2)
    type(ESMF_ArraySpec) :: arrayspec
    real(ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1, f90ptr2
    type(ESMF_Array), dimension(2) :: ESMF_coords, ESMF_coords2
    real(ESMF_KIND_R8), dimension(:,:), pointer :: x_coords,y_coords
    real(ESMF_KIND_R8), dimension(:,:), pointer :: x_coords2,y_coords2
    real(ESMF_KIND_R8), dimension(:,:), pointer :: Phi, Theta
    real(ESMF_KIND_R8), dimension(:,:), pointer     :: SolnOnTarget
    real(ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords
    real(ESMF_KIND_R8) ::  length_scale, radius, RelativeError
    real(ESMF_KIND_R8) :: epsil,max_error, avg_error
    real(ESMF_KIND_R8) :: xmin, ymin, xmax, ymax
    real(ESMF_KIND_R8) :: crop_factor
    real(ESMF_KIND_R8), parameter ::  pi            = 3.1416d0


!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination grid with data on it, to use
!   !  in the Regrid calls below.
 
    layout1 = ESMF_DELayoutCreate(vm, nSrcPetsXY    , rc=rc)
    layout2 = ESMF_DELayoutCreate(vm, npetsXY, rc=rc)

   !--Create and distribute the source and destination grids
   !=========================================================
!   domainType=2
   !...tab for choice of DOMAIN type:
    if ( domainType == 1 )  then         !WholeGlobe
      call createWholeGlobeGrids
    else if ( domainType == 2 ) then     !Regional
      call createRegionalGrids
    else                          !ERROR
      print*,'ERROR! domainType=', domainType,'  valid values= 1,2 '
      stop
    end if

   !Specify settings for the fields' arrays
   !=======================================
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_DATA_REAL, ESMF_R8, rc)


   !Create the source field (with halo width of 3)
   !==============================================
    call createField(grid=srcgrid,               &
                     LocChoice = SrcLocChoice,   &
                     halo      = SrcHalo,        &
                     fieldName = "src pressure", &
                     field     = field1,         &
                     f90ptr    = f90ptr1,        &
                     xCoor     = x_coords,       &
                     yCoor     = y_coords         )


    !--- Assign values to the source field data (4 case choices) via pointer
    !=======================================================================
                            ! -->Array bounds<-- !
    lbSrc(:) = lbound(f90ptr1)
    ubSrc(:) = ubound(f90ptr1)
                           ! -->Define Phi, Theta values<-- !
    if (domainType == 1) then
      !--Whole_globe domain.
      Phi => x_coords
      Theta => y_coords
    else if (domainType == 2) then
      !--Regional domain
      allocate( Phi( size(x_coords,1), size(x_coords,2) ) )
      allocate( Theta( size(y_coords,1) , size(y_coords,2) ) )
      Phi   = 2.  * pi * (x_coords+10.) / xmax
      Theta = 0.5 * pi * (y_coords+10.) / ymax
    else
      print*,'ERROR! domainType=',domainType,' valid values=1,2'
    end if
                    ! -->Assign values to test function<-- !             
    call functionValues(FieldChoice, x_coords, y_coords, Phi, Theta, &
                        lbSrc, ubSrc, Srchalo, maxcoords, f90ptr1, ier)

   !print*,'Source grid: x_coords(1,1)=',x_coords(1,1)

   !Create the destination field (with halo width of 0)
   !===================================================
    call createField(grid=Dstgrid,               &
                     LocChoice = DstLocChoice,   &
                     halo      = DstHalo,        &
                     fieldName = "Dst pressure", &
                     field     = field2,         &
                     f90ptr    = f90ptr2,        &
                     xCoor     = x_coords2,      &
                     yCoor     = y_coords2        )

    ! fields all ready to go

!\subsubsection{Precomputing and Executing a Regrid}
      
!  The user has already created an {\tt ESMF\_Grid}, an
!  {\tt ESMF\_Array} with data, and put them together in an {\tt ESMF\_Field}.
!  An {\tt ESMF\_RouteHandle} is created and the data movement needed to
!  execute the regrid is stored with that handle by the store method. 
!  To actually execute the operation, the source and destination data
!  objects must be supplied, along with the same {\tt ESMF\_RouteHandle}.
      

   !Create a Route Handle
   !=====================
    regrid_rh = ESMF_RouteHandleCreate(rc)


   !Do all the calculations in preparation for the actual re-gridding
   !=================================================================
    call ESMF_FieldRegridStore(field1, field2, vm, &
                               routehandle=regrid_rh, &
                               regridmethod=MethodChoice, rc=rc)

 
   !Regrid
   !======
    call ESMF_FieldRegrid(field1, field2, regrid_rh, rc=rc)


!===============================================================
!Verification: Compare to the "exact solution" on the dest. grid
!===============================================================

    !Array bounds in the destination grid (local indexing)
    !=====================================================
     lbDst(:) = lbound(f90ptr2)
     ubDst(:) = ubound(f90ptr2)

    !Allocate the array pointer for the "exact solution at the dest. grid
    !====================================================================
     allocate( SolnOnTarget( lbDst(1):ubDst(1) , lbDst(2):ubDst(2) ) )


   !--Re-associate the Phi and Theta pointers to the field2 coordinates
   !===================================================================
    if (domainType == 1) then
      !--Whole_globe domain:
      Phi => x_coords2
      Theta => y_coords2
    else if (domainType ==2) then
      !--Regional domain:
      allocate( Phi( size(x_coords2,1), size(x_coords2,2) ) )
      allocate( Theta( size(y_coords2,1) , size(y_coords2,2) ) )
      Phi   = 2.  * pi * (x_coords2+10.) / xmax
      Theta = 0.5 * pi * (y_coords2+10.) / ymax
    else
      print*,'ERROR! domainType=',domainType,' valid values=1,2'
    end if
   !print*,'Destination grid: x_coords2(1,1)=',x_coords2(1,1)


   !--Compute exact fcn. values at the Destination Grid 
   !===================================================
    call functionValues(FieldChoice, x_coords2, y_coords2, Phi, Theta, &
                        lbDst, ubDst, DstHalo, maxcoords, SolnOnTarget, ier)

 
   !Verify success in regridding. Compute maximum and average normalized error
   !==========================================================================
   max_error=0.
   avg_error=0.

   !--set the threshold for the normalized error..
    if ( present(error_threshold) ) then
      epsil=error_threshold
    else
      epsil=0.5 !threshold for normalized error
    end if

   do j=lbDst(2)+1,ubDst(2)
     do i=lbDst(1),ubDst(1)
       RelativeError=abs( (SolnOnTarget(i,j)-f90ptr2(i,j)) / SolnOnTarget(i,j) )
      !write(*,'(a,i2,a,2i3,1x,e11.4,1x,e11.4)') &
      !     'localPET=',localPet,' i,j=',i,j,SolnOnTarget(i,j), &
      !     f90ptr2(i,j)
       if (RelativeError .gt. epsil ) then
         regrid_rc=ESMF_FAILURE
     !TODO compute the global values of i,j for diagnostic printing
         write( *, '(i3,a,i2,a,i2,a,2i2,a,2i4,a,3(1pe11.4))' )                &
                 localPet,' ERROR! Regr= ',iRegrid,' Field=',FieldChoice, &
                 ' npetsXY=',npetsXY, ' i,j=',i,j,             &
                 '  exact/actual/Err',SolnOnTarget(i,j), f90ptr2(i,j),    &
                 RelativeError
       end if
       avg_error=avg_error+RelativeError
       if(RelativeError > max_error) max_error=RelativeError
     end do
   end do
  !TODO: compute the GLOBAL average and maximum normalized errors
   avg_error=avg_error/( (ubDst(1)-lbDst(1)+1) * (ubDst(2)-lbDst(2)+1) )
   write(*,'(a,i3,a,i1,a,1pe12.4,a,1pe12.4)' ) &
         'localPet=',localPet,' FieldChoice=',FieldChoice, &
         '  local max norm error=',max_error, &
         '  local avg norm error=',avg_error

    call ESMF_FieldRegridRelease(regrid_rh, rc=rc)

    call ESMF_RouteHandleDestroy(regrid_rh)


!-------------------------------------------------------------------------
!    ! Cleanup

    call ESMF_FieldDestroy(field1, rc=rc)

    call ESMF_FieldDestroy(field2, rc=rc)

    call ESMF_GridDestroy(srcgrid, rc=rc)

    call ESMF_GridDestroy(dstgrid, rc=rc)
!----------------------------------------------------------------
    return

contains

!==================================================================
    subroutine createWholeGlobeGrids

 ! Create grids covering the whole globe. Note that crop factor was introduced
 ! for the benefit of regional grids originally. It is not needed here.

    !--- Full physical domain dimension
    xmin = 0.0
    ymin = -0.5*pi
    xmax = 2.*pi
    ymax = 0.5*pi

    crop_factor=1.  !portion of the domain to be covered by the grid

    !--- Grid dimension to cover full physical domain.
    nx_domain=100
    ny_domain=150

    !--Coordinate ranges of the "test grids"
    mincoords = (/ xmin*crop_factor,  ymin*crop_factor /)
    maxcoords = (/ xmax*crop_factor,  ymax*crop_factor /)

    !--- Number of cells in the current grid.
    n_cells = (/real(nx_domain)*crop_factor, real(ny_domain)*crop_factor /)

    if (localPET == 0) then
      print*,'n_cells=',n_cells
      print*,'mincoords=',mincoords
      print*,'maxcoords=',maxcoords
    end if


    !Create the source grid
   !===========================
    srcgrid = ESMF_GridCreateHorzLatLonUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=SrcGridChoice, &
                   dimUnits= (/ "radians" , "radians" /), &
                   periodic=(/ ESMF_true , ESMF_false /), &
                   name="srcgrid", rc=rc)

   !Distribute the source grid
   !===========================
    call ESMF_GridDistribute(srcgrid, delayout=layout1, rc=rc)

   !Create the destination grid
   !===========================
    dstgrid = ESMF_GridCreateHorzLatLonUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=DstGridChoice, &
                   dimUnits= (/ "radians" , "radians" /), &
                   periodic=(/ ESMF_true , ESMF_false /), &
                   name="dstgrid", rc=rc)

   !Distribute destination grid
   !===========================
    call ESMF_GridDistribute(dstgrid, delayout=layout2, rc=rc)

    return
    end subroutine createWholeGlobeGrids

!===========================================================================

    subroutine createRegionalGrids

!--Create a Regional (not whole-globe) rectangular grid.
   
    !---Maximum range of physical dimensions of the grid
    xmin = 0.0
    ymin = 0.0
    xmax = 20.
    ymax = 30.

    crop_factor=.2   !Fraction of the maximum range covered by the actual grid

    !--- Maximum size of the grid (# of grid cells).
    nx_domain=100
    ny_domain=150

    !--- Number of cells in the current grid.
    n_cells = (/real(nx_domain)*crop_factor, real(ny_domain)*crop_factor /)
    mincoords = (/ xmin*crop_factor,  ymin*crop_factor /)
    maxcoords = (/ xmax*crop_factor,  ymax*crop_factor /)

    if (localPET == 0) then
      print*,'n_cells=',n_cells
      print*,'mincoords=',mincoords
      print*,'maxcoords=',maxcoords
    end if

    !Create the source grid
   !===========================
    srcgrid = ESMF_GridCreateHorzXYUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=SrcGridChoice, &
                   name="srcgrid", rc=rc)

   !Distribute the source grid
   !===========================
    call ESMF_GridDistribute(srcgrid, delayout=layout1, rc=rc)

   !Create the destination grid
   !===========================
    dstgrid = ESMF_GridCreateHorzXYUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=DstGridChoice, &
                   name="srcgrid", rc=rc)

   !Distribute destination grid
   !===========================
    call ESMF_GridDistribute(dstgrid, delayout=layout2, rc=rc)

    return
    end subroutine createRegionalGrids

!=========================================================================

    subroutine createField(grid, LocChoice, halo, fieldName, field, &
                              f90ptr, xCoor,  yCoor )
    type(ESMF_Grid), intent(in)                 :: grid
    type(ESMF_RelLoc), intent(in)               :: LocChoice
    integer, intent(in)                         :: halo
    character (len=*), intent(in)               :: fieldName
    type(ESMF_Field)                            :: field
    real(ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    real(ESMF_KIND_R8), dimension(:,:), pointer :: xCoor, yCoor
    

 !-------------------------------------------------------------------------
 !--Create a field and return a pointer to its data array, and the arrays 
 !  of coordinates --- x and y.
 !--------------------------------------------------------------------------

   !Create the field 
   !================
    field = ESMF_FieldCreate(grid, arrayspec, &
                              horzRelloc=LocChoice, &
                              haloWidth=halo, name=fieldName, rc=rc)


   !Create a pointer to the field data space
   !========================================
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=rc)


   !Get the coordinates of the grid
   !===============================
    call ESMF_GridGetCoord(grid,horzRelLoc=LocChoice,  &
           centercoord=ESMF_coords,rc=rc)


   !Get the actual values of the x and y coordinate arrays
   !======================================================
    call ESMF_ArrayGetData(ESMF_coords(1), xCoor, ESMF_DATA_COPY, rc=rc)
    call ESMF_ArrayGetData(ESMF_coords(2), yCoor, ESMF_DATA_COPY, rc=rc)

    return
    end subroutine createField

    end subroutine Regrid
!=============================================
  subroutine functionValues(Choice, xCoord, yCoord, Phi, Theta, &
                            lb, ub, halo, maxCoor, f90ptr, ier)

 !--Compute the values of the test function and store in the f90ptr.
 !  The parameter Choice determines which function is returned.
 !  Choice 3 and 4 are the only ones appropriate for WHOLE_GLOBE tests.

  use ESMF_Mod

  integer, intent(in)                             :: Choice
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: xCoord, yCoord
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: Theta, Phi
  integer, dimension(2), intent(in)               :: lb, ub
  integer, intent(in)                             :: halo
  real(ESMF_KIND_R8), dimension(2), intent(in)    :: maxCoor
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: f90ptr
  integer :: ier

 !--Local Variables
  integer :: i,j
  real(ESMF_KIND_R8) :: length_scale, radius
  real(ESMF_KIND_R8), parameter ::  pi            = 3.1416d0

    select case(Choice)
    case(1) !** f=x+y
      f90ptr(:,:) = 0.0
      do j=lb(2)+halo, ub(2)-halo
        do i=lb(1)+halo, ub(1)-halo
        f90ptr(i, j) = xCoord(i-halo,j-halo) + yCoord(i-halo,j-halo)
        enddo
      enddo
    case(2) !**f=2+cos(pi*r/L)
      f90ptr(:,:) = 0.0
      length_scale=sqrt( maxCoor(1)**2 + maxCoor(2)**2 )
      do j=lb(2)+halo, ub(2)-halo
        do i=lb(1)+halo, ub(1)-halo
          radius=sqrt( xCoord(i-halo,j-halo)**2 +   &
                       yCoord(i-halo,j-halo)**2 )
          f90ptr(i, j) = 2. + cos( pi * radius / length_scale )
        end do
      end do

    case(3) !**f=2+cos(y)**2*cos(2*x)
      do j=lb(2)+halo, ub(2)-halo
        do i=lb(1)+halo, ub(1)-halo
         !Phi = xCoord(i-halo,j-halo)
         !Theta = yCoord(i-halo,j-halo)
          f90ptr(i, j) = 2. + &
                        cos(Theta(i-halo,j-halo))**2 * &
                        cos( 2.*Phi(i-halo,j-halo))
        end do
      end do
    case(4) !**f=2+sin(2*y)**16 * cos(16*x)
      do j=lb(2)+halo, ub(2)-halo
        do i=lb(1)+halo, ub(1)-halo
         !Phi = xCoord(i-halo,j-halo)
         !Theta = yCoord(i-halo,j-halo)
          f90ptr(i, j) = 2. + &
                        sin( 2*Theta(i-halo,j-halo) )**16 * &
                        cos( 16.* Phi(i-halo,j-halo) )
        end do
      end do
    case default
      print*,'ERROR! invalid iFunction value (=',Choice, &
             ') valid range is [ 1 -> 4 ]'
      ier=1
    end select

    return
    end subroutine functionValues


!   end subroutine setupRegridUTest

!=============================================================================
subroutine find_indices( long_string, maskName, choiceIndex, nOptions, nSelected, ier)

!Find the indices in the table that correspond to the option names and choices
! specified in the arguments concatenated in the long_string argument.
!Fill up a mask array marking the testing options selected
!List the indices of the test options selected (non-selected ones will have
! index 0).

! maskName -- is a mask for which options are to be tested
! choiceIndex represents which of the available choices has been selected to 
!             be used for a given test option.
! nOptions -- is the total number of test options available
! nSelected-- is the number of options selected to be tested.


use options_table, only : option_name, Option_choice, get_table, search_table
use options_table, only : nChoices
!use Unit_Test, only : AssertEqual

interface
  subroutine parse( long_string, stringArray, ns, ier)
    character(len=*), intent(in) :: long_String
    character(len=25), dimension(15,2), intent(out) :: stringArray
    integer, intent(out) :: ns, ier
  end subroutine parse
end interface

integer, intent(in) :: nOptions
character(len=*), intent(in) :: long_String
integer, intent(out), dimension(:) ::  choiceIndex
logical, intent(out), dimension(:) :: maskName
integer, intent(out) :: nSelected, ier

character(len=25), dimension(15,2) :: StringPairs
logical :: iguales
integer :: i, j, nChoicesJ
integer :: i_name, i_choice
logical :: found_name, found_choice

!Access table
call get_table

!Parse string containing test choices into a 2D character array: StringPairs
call parse(long_string, stringPairs, nSelected, ier)

!--Represent the test options to use in the maskName array. The choice
!  index for each options selected in ChoiceIndex.
!Initialize
maskName=.false.
choiceIndex=0
selected: do i=1,nSelected
            call search_table(stringPairs(i,1), stringPairs(i,2), &
                              i_name,           i_choice,         &
                              found_name,       found_choice       )
            if (found_name) then 
              maskName(i_name)=.true.
              if (found_choice) then
                choiceIndex(i_name)=i_choice
              else
                print*,'ERROR!! ', trim(StringPairs(i,2)),' is not a ', &
                    ' valid choice for option test: ',                  &
                    trim(StringPairs(i,1)),                             &
                    '. Valid choices are: ',                            &
                    (trim(Option_choice(m,i_name)),'  ',                &
                    m=1,nChoices(i_name) )
               end if
            else
               print*,'ERROR!! Invalid option test name:',         &
                       trim( StringPairs(i,1)),                    &
                      '.  Valid names are: ',                      &
                       (trim(Option_Name(m)),'  ', m=1,nOptions)

            end if
           end do selected  !(i)
ier=0

return

end subroutine find_indices

!==============================================================================
subroutine parse( long_string, stringArray, ns, ier)

    implicit none

   character(len=*), intent(in) :: long_string
   character(len=25), dimension(15,2), intent(out) :: stringArray
   integer, intent(out) :: ns, ier

   integer :: colon_location,next_colon_location
   logical :: found_colon

!--initialize
ns=0
found_colon=.true.
colon_location=0

do while (found_colon)
  !Is there a delimiter ':' corresponding to another pair?
  if ( scan(long_string(colon_location+1:),':') .ne. 0) then  !..pair found
    ns=ns+1
    found_colon=.true.
    next_colon_location=colon_location + &
                       index(long_string(colon_location+1:),':')
   !print*,'next_colon_location=',next_colon_location
    if (next_colon_location-1 .le.colon_location) then
        print*,'ERROR in parse!! Null parameter name selected (::)'
        stop
    end if
    stringArray(ns,1)=long_string(colon_location+1 : next_colon_location-1)
      colon_location=next_colon_location
    if ( scan(long_string(colon_location+1:),':') .ne. 0) then  !..not last
      found_colon=.true.
      next_colon_location=colon_location + &
                          index(long_string(colon_location+1:),':')
      if (next_colon_location-1 .le.colon_location) then
        print*,'ERROR in parse!! no choice_value selected for ',trim(stringArray(ns,1)) 
        stop
      end if
       
      !print*,'next_colon_location=',next_colon_location
       stringArray(ns,2)=long_string(colon_location+1 : next_colon_location-1)
       colon_location=next_colon_location
     else
       found_colon=.false.
      !print*,'last parameter in the list'
       if (colon_location == len_trim(long_string)) then
         print*,'ERROR in parse!! no choice_value selected for ', &
                 trim(stringArray(ns,1))
        stop
      end if

       stringArray(ns,2)=long_string(colon_location+1 :)
       ier=0
     end if

   else                                       !...ERROR: single item at the end
     print*,' Pair mismatched for ',long_string(colon_location+1 :)
     found_colon=.false.
     ier=1
   end if
end do
end subroutine parse

!===============================================================================
