! $Id: ESMF_RegridSubroutines.F90,v 1.27.2.1 2008/11/07 23:52:08 theurich Exp $
!-------------------------------------------------------------------------
!-------------------------------------------------------------------------

#define ESMF_FILENAME "ESMF_RegridSubroutines.F90"

module ESMF_RegridSubroutines
#include "ESMF_Macros.inc"
#include "ESMF.h"
	use ESMF_Mod
	use ESMF_TestMod  ! test methods

    type testArguments
    	integer :: function
        type(ESMF_RegridMethod) :: regscheme
        type(ESMF_IGridHorzStagger) :: srcigrid, dstigrid
        type(ESMF_RelLoc) :: srcrelloc, dstrelloc
        integer :: srcdelayout(2), dstdelayout(2)
	integer :: domain, srchalo, dsthalo
        character(ESMF_MAXSTR) :: functionString, regridString
        character(ESMF_MAXSTR) :: srcdelayoutString, dstdelayoutString
        character(ESMF_MAXSTR) :: srcigridStaggerString, dstigridStaggerString
        character(ESMF_MAXSTR) :: srcHaloString, dstHaloString
    end type testArguments
    type(ESMF_VM),save :: vm
    integer ::  npets, localPet
    integer :: regrid_rc    !single test error indicator
    integer :: lrc,iFunction,iRegrid,ig

!------------------------------------------------------------------------------

public  readUnitTestList

contains
#undef  ESMF_METHOD
#define ESMF_METHOD "readUnitTestList"

!============================================================================

    subroutine readTestList(testListFile, npets, rc)
    
    character(len=*), intent(in) :: testListFile
    integer, intent(in) :: npets
    integer, intent(out) :: rc

    character(ESMF_MAXSTR) :: testString

    integer :: openStatus, readStatus, startTesting, regrid_rc
    integer :: testCount
    character(ESMF_MAXSTR) :: failMsg, name, testMsg
    ! cumulative result: count failures; no failures equals "all pass"
    integer :: result = 0
    real(ESMF_KIND_R8) :: err_threshold

    type(testArguments) :: testArgs

    rc = ESMF_SUCCESS ! assume success
    openStatus = 0
    startTesting = 1 ! Don't start reading the test list strings until the start of test string is found.
    testCount = 0

   ! Open test list file
    open (unit=1, file = testListFile, action = "read", iostat=openStatus)
     if (openStatus > 0 ) then
     	print *, "Cannot open file: ", testListFile
        rc = ESMF_FAILURE
	return
     endif

   ! Read test list until end of tests string to count the number of tests
   do 
	read (unit=1, fmt = * , iostat=readStatus) testString 
     	if (readStatus.ne.0 ) then
     		print *, "Cannot read file: ", testListFile
        	rc = ESMF_FAILURE
	        return
        	exit
     	endif
        if (testString.eq."end_of_tests") then
		print *, "TEST_COUNT:",testCount
		rewind (1)
                startTesting = 1 ! Reset flag 
        	exit
        endif
        if ( startTesting.eq.0 ) then
		testCount = testCount + 1

        endif
        if (testString.eq."start_of_tests") then
                startTesting = 0
        endif 
    enddo

   ! Read test list until end of tests string
   do 
	read (unit=1, fmt = * , iostat=readStatus) testString 
     	if (readStatus.ne.0 ) then
     		print *, "Cannot read file: ", testListFile
        	rc = ESMF_FAILURE
	        return
        	exit
     	endif
        if (testString.eq."end_of_tests") then
        	exit
        endif
        if ( startTesting.eq.0 ) then
		call getTestingArguments(string=testString, testArgs=testArgs, npets=npets, rc=rc)
		if ( rc.ne.ESMF_SUCCESS) then
                	rc = ESMF_FAILURE
                	return
		endif
		! Run the Regrid unit test here: srcigrid to dstigrid1
		err_threshold=0.5
		write(failMsg, *) "Error in Regrid"
                write(name, *) "Regrid test: ", trim(testString)
		call Regrid(FieldChoice = testArgs%function, &
			   nSrcPetsXY= testArgs%srcdelayout, &
                           npetsXY=testArgs%dstdelayout, &
                           MethodChoice = testArgs%regscheme, &
                           SrcIGridChoice = testArgs%srcigrid, &
                           DstIGridChoice = testArgs%dstigrid, &
                           SrcLocChoice = testArgs%srcrelloc, &
                           DstLocChoice = testArgs%dstrelloc, &
                           SrcHalo = testArgs%srchalo, &
                           DstHalo = testArgs%dsthalo, &
                           domainType   = testArgs%domain, &
                           error_threshold=err_threshold, &
			   ier=regrid_rc)

		call ESMF_Test((regrid_rc.eq.ESMF_SUCCESS),name, failMsg, result, &
                    ESMF_SRCLINE)
		testMsg = trim(" Function                  : " // testArgs%FunctionString)
		print *, testMsg
		call ESMF_LogWrite(testMsg, ESMF_LOG_INFO)
		testMsg = trim(" Regrid Method             : " // testArgs%regridString)
		print *, testMsg
		call ESMF_LogWrite(testMsg, ESMF_LOG_INFO)
		testMsg = trim(" Source Horiz. Stagger Pair: " // testArgs%srcigridStaggerString)
		print *, testMsg
		call ESMF_LogWrite(testMsg, ESMF_LOG_INFO)
		testMsg = trim(" Dest. Horiz. Stagger Pair : " // testArgs%dstigridStaggerString)
		print *, testMsg
		call ESMF_LogWrite(testMsg, ESMF_LOG_INFO)
		testMsg = trim(" Source Delayout           : " // testArgs%srcdelayoutString)
		print *, testMsg
		call ESMF_LogWrite(testMsg, ESMF_LOG_INFO)
		testMsg = trim(" Destination Delayout      : " // testArgs%dstdelayoutString)
		print *, testMsg
		call ESMF_LogWrite(testMsg, ESMF_LOG_INFO)
		testMsg = trim(" Source Halo               : " // testArgs%srcHaloString)
		print *, testMsg
		call ESMF_LogWrite(testMsg, ESMF_LOG_INFO)
		testMsg = trim(" Destination  Halo         : " // testArgs%dstHaloString)
		call ESMF_LogWrite(testMsg, ESMF_LOG_INFO)
		print *, testMsg
		print *, ""
		call ESMF_LogWrite("", ESMF_LOG_INFO)

        endif
        if (testString.eq."start_of_tests") then
                startTesting = 0
        endif 
    enddo

    
   close (1)

  end subroutine readTestList
!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "getTestingArguments"

!============================================================================

    subroutine getTestingArguments(string, npets, testArgs, rc)
    
    character(len=*), intent(in) :: string
    integer, intent(in) :: npets
    type(testArguments), intent(out) :: testArgs
    integer, intent(out) :: rc

    ! Local variables
    character(ESMF_MAXSTR) :: index, letter, delayoutconfig, functionString
    character(ESMF_MAXSTR) :: regridString, igridStaggerString
    character(ESMF_MAXSTR) :: delayoutString, haloString
    type(ESMF_RegridMethod) :: regscheme
    integer :: i, value, openStatus, readStatus, domain
    integer :: relloc, igrid
    namelist /igridMethod/ index, regscheme, regridString
    namelist /igridHStagger/ index, igrid, relloc, igridStaggerString
    namelist /delayout/ index, delayoutconfig, delayoutString
    namelist /function/ index, value, domain, functionString
    namelist /halo/ index, value, haloString

    rc = ESMF_SUCCESS ! assume success
    !Read each letter of the test list and get test arguments.
    i = 1
    letter = string (i:i)
    open (unit=(npets+20), file = "ESMF_RegridFunction.rc", action = "read", iostat=openStatus)
    if (openStatus.ne.0) then
	print *, " Unable to open file: ESMF_RegridFunction.rc"
	rc=ESMF_FAILURE
        return
    endif
    do
	read (unit=(npets+20), nml = function , iostat=readStatus)
    	if (readStatus.ne.0) then
		print *, " Unable to read file: ESMF_RegridFunction.rc"
		rc=ESMF_FAILURE
        	return
    	endif
        if (index.eq.letter) then
             testArgs%function = value
             testArgs%domain = domain
             testArgs%functionString = trim(functionString)
	     close ((npets+20))
             exit
        endif
    enddo

    i = 2
    letter = string (i:i)
    open (unit=(npets+20), file = "ESMF_RegridMethod.rc", action = "read", iostat=openStatus)
    if (openStatus.ne.0) then
	print *, " Unable to open file: ESMF_RegridMethod.rc"
	rc=ESMF_FAILURE
        return
    endif
    do
	read (unit=(npets+20), nml = igridMethod, iostat=readStatus) 
    	if (readStatus.ne.0) then
		print *, " Unable to read file: ESMF_RegridMethod.rc"
		rc=ESMF_FAILURE
        	return
    	endif
        if (index.eq.letter) then
             testArgs%regscheme = regscheme
             testArgs%regridString = trim(regridString)
	     close ((npets+20))
             exit
        endif
    enddo

    i = 3
    letter = string (i:i)
    open (unit=(npets+20), file = "ESMF_IGridHorzStagger.rc", action = "read", iostat=openStatus)
    if (openStatus.ne.0) then
        print *, " Unable to open file: ESMF_IGridHorzStagger.rc"
        rc=ESMF_FAILURE
        return
    endif
    do
	read (unit=(npets+20), nml = igridHStagger, iostat=readStatus) 
        if (readStatus.ne.0) then
                print *, " Unable to read file: ESMF_IGridHorzStagger.rc"
                rc=ESMF_FAILURE
                return
        endif
        if (index.eq.letter) then
            testArgs%srcigrid = ESMF_IGridHorzStagger(igrid)
            testArgs%srcrelloc = ESMF_Relloc(relloc)
            testArgs%srcigridStaggerString = trim(igridStaggerString)
	    close ((npets+20))
            exit
        endif
    enddo


    i = 4
    letter = string (i:i)
    open (unit=(npets+20), file = "ESMF_IGridHorzStagger.rc", action = "read", iostat=openStatus)
    if (openStatus.ne.0) then
        print *, " Unable to open file: ESMF_IGridHorzStagger.rc"
        rc=ESMF_FAILURE
        return
    endif
    do
	read (unit=(npets+20), nml = igridHStagger, iostat=readStatus) 
        if (readStatus.ne.0) then
                print *, " Unable to read file: ESMF_IGridHorzStagger.rc"
                rc=ESMF_FAILURE
                return
        endif
        if (index.eq.letter) then
            testArgs%dstigrid = ESMF_IGridHorzStagger(igrid)
            testArgs%dstrelloc = ESMF_RelLoc(relloc)
            testArgs%dstigridStaggerString = trim(igridStaggerString)
	    close ((npets+20))
            exit
        endif
    enddo


    i = 5
    letter = string (i:i)
    open (unit=(npets+20), file = "ESMF_RegridDELayout.rc", action = "read", iostat=openStatus)
    if (openStatus.ne.0) then
        print *, " Unable to open file: ESMF_RegridDELayout.rc"
        rc=ESMF_FAILURE
        return
    endif
    do
	read (unit=(npets+20), nml = delayout, iostat=readStatus) 
        if (readStatus.ne.0) then
                print *, " Unable to read file: ESMF_RegridDELayout.rc"
                rc=ESMF_FAILURE
                return
        endif
        if (index.eq.letter) then
	    testArgs%srcdelayoutString = trim(delayoutString)
            select case (trim(delayoutconfig))
		case ("1DX")
                   testArgs%srcdelayout = (/ npets, 1 /)
		case ("1DY")
                   testArgs%srcdelayout = (/ 1, npets /)
		case ("2D")
                   testArgs%srcdelayout = (/ npets/(1 + mod(npets+1,2)), (1 + mod(npets+1,2)) /)
	    end select
	    close ((npets+20))
            exit
        endif
    enddo


    i = 6
    letter = string (i:i)
    open (unit=(npets+20), file = "ESMF_RegridDELayout.rc", action = "read", iostat=openStatus)
    if (openStatus.ne.0) then
        print *, " Unable to open file: ESMF_RegridDELayout.rc"
        rc=ESMF_FAILURE
        return
    endif
    do
	read (unit=(npets+20), nml = delayout, iostat=readStatus) 
        if (readStatus.ne.0) then
                print *, " Unable to read file: ESMF_RegridDELayout.rc"
                rc=ESMF_FAILURE
                return
        endif
        if (index.eq.letter) then
	    testArgs%dstdelayoutString = trim(delayoutString)
            select case (trim(delayoutconfig))
		case ("1DX")
                   testArgs%dstdelayout = (/ npets, 1 /)
		case ("1DY")
                   testArgs%dstdelayout = (/ 1, npets /)
		case ("2D")
                   testArgs%dstdelayout = (/ npets/(1 + mod(npets+1,2)), (1 + mod(npets+1,2)) /)
	    end select
	    close ((npets+20))
            exit
        endif
    enddo

    i = 7
    letter = string (i:i)
    open (unit=(npets+20), file = "ESMF_RegridHalo.rc", action = "read", iostat=openStatus)
    if (openStatus.ne.0) then
        print *, " Unable to open file: ESMF_RegridHalo.rc"
        rc=ESMF_FAILURE
        return
    endif
    do
	read (unit=(npets+20), nml = halo, iostat=readStatus)
        if (readStatus.ne.0) then
                print *, " Unable to read file: ESMF_RegridHalo.rc"
                rc=ESMF_FAILURE
                return
        endif
        if (index.eq.letter) then
             testArgs%srchalo = value
             testArgs%srcHaloString = haloString
	     close ((npets+20))
             exit
        endif
    enddo


    i = 8
    letter = string (i:i)
    open (unit=(npets+20), file = "ESMF_RegridHalo.rc", action = "read", iostat=openStatus)
    if (openStatus.ne.0) then
        print *, " Unable to open file: ESMF_RegridHalo.rc"
        rc=ESMF_FAILURE
        return
    endif
    do
	read (unit=(npets+20), nml = halo, iostat=readStatus)
        if (readStatus.ne.0) then
                print *, " Unable to read file: ESMF_RegridHalo.rc"
                rc=ESMF_FAILURE
                return
        endif
        if (index.eq.letter) then
             testArgs%dsthalo = value
             testArgs%dstHaloString = haloString
	     close ((npets+20))
             exit
        endif
    enddo

  end subroutine getTestingArguments


!------------------------------------------------------------------------------

#undef  ESMF_METHOD
#define ESMF_METHOD "Regrid"

!-------------------------------------------------------------------
    subroutine Regrid(FieldChoice, nSrcPetsXY, npetsXY, MethodChoice, &
                           SrcIGridChoice,DstIGridChoice, &
                           SrcLocChoice, DstLocChoice, &
                           SrcHalo, DstHalo, &
                           domainType, error_threshold, ier )

  !--Execute a Regrid Unit test for a single set of testing parameter choices.

  !  FieldChoice -- Choice of test function (ESMF_RegridMethod).
  !  nSrcPetsXY -- Choice of geom. decomposition on the source igrid (int).
  !  npetsXY -- Choice of geom. decomposition on destination igrid (int).
  !  MethodCHoice -- Regridding Method Choice (ESMF_RegridMethod).
  !  Src(Dst)IGridChoice -- Type of (Arakawa) igrid (ESMF_IGridHorzStagger).
  !  Src(Dst)LocChoice -- In cell relative location of data (ESMF_RelLoc).
  !  error_threshold -- Normalized error threshold (int).

    implicit none
  
      ! Choice of test function for the field to be regridded
      ! 1 -->   f=x+y
      ! 2 -->   f=2+cos(pi*r/L)
      ! 3 -->   f=2+(cos(theta))**2 * cos(2*phi)
      ! 4-->    f=2+ (sin(2*theta))**16 * cos(16*phi)

      integer, intent(in) :: FieldChoice
      type(ESMF_RegridMethod), intent(in) :: MethodChoice
      type(ESMF_IGridHorzStagger), intent(in) :: SrcIGridChoice, DstIGridChoice
      type(ESMF_RelLoc), intent(in) :: SrcLocChoice, DstLocChoice 
      integer, intent(in) :: SrcHalo, DstHalo
      real(ESMF_KIND_R8), optional, intent(in) :: error_threshold
      integer, intent(in) :: npetsXY(2)
      integer, intent(in) :: nSrcPetsXY(2)
      integer, intent(in) :: domainType
      integer, optional, intent(out) :: ier

    !--- Local variables
    type(ESMF_Field) :: field1, field2, field3
    type(ESMF_IGrid) :: srcigrid, dstigrid1, dstigrid2
    type(ESMF_RouteHandle) :: regrid_rh1, regrid_rh2
    type(ESMF_DELayout) :: layout1, layout2
    integer :: rc

    integer :: i, j
    integer :: lbSrc(2), ubSrc(2)
    integer :: lbDst(2), ubDst(2)
    integer :: nx_domain, ny_domain
    integer ::  n_cells(2), sub_rc
    type(ESMF_ArraySpec) :: arrayspec
    real(ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr1, f90ptr2, f90ptr3
    real(ESMF_KIND_R8), dimension(:,:), pointer :: x_coords,y_coords
    real(ESMF_KIND_R8), dimension(:,:), pointer :: x_coords2,y_coords2
    real(ESMF_KIND_R8), dimension(:,:), pointer :: x_coords3,y_coords3
    real(ESMF_KIND_R8), dimension(:,:), pointer :: Phi, Theta
    real(ESMF_KIND_R8), dimension(:,:), pointer     :: SolnOnTarget
    real(ESMF_KIND_R8), dimension(2) :: mincoords, maxcoords
    real(ESMF_KIND_R8) :: RelativeError
    ! real(ESMF_KIND_R8) ::  length_scale, radius
    real(ESMF_KIND_R8) :: epsil, max_error, avg_error
    real(ESMF_KIND_R8) :: xmin=0.0, ymin=0.0, xmax=1.0, ymax=1.0
    real(ESMF_KIND_R8) :: crop_factor = 1.0
    real(ESMF_KIND_R8), parameter ::  pi            = 3.1416d0


    ier=ESMF_SUCCESS	! Assume success
!-------------------------------------------------------------------------
!   ! Setup:
!   !
!   !  Create a source and destination igrid with data on it, to use
!   !  in the Regrid calls below.
 
    layout1 = ESMF_DELayoutCreate(vm, nSrcPetsXY    , rc=rc)
    if (rc.ne.ESMF_SUCCESS) then
        ier=ESMF_FAILURE
        print *, "layout 1 failed"
    end if
    layout2 = ESMF_DELayoutCreate(vm, npetsXY, rc=rc)
    if (rc.ne.ESMF_SUCCESS) then
        ier=ESMF_FAILURE
        print *, "layout 2 failed"
    end if

   !--Create and distribute the source and destination igrids
   !=========================================================
!   domainType=2
   !...tab for choice of DOMAIN type:
    if ( domainType == 1 )  then         !WholeGlobe
      call createWholeGlobeIGrids
    else if ( domainType == 2 ) then     !Regional
      call createRegionalIGrids
    else                          !ERROR
      print*,'ERROR! domainType=', domainType,'  valid values= 1,2 '
      ier=ESMF_FAILURE
      stop
    end if

   !Specify settings for the fields' arrays
   !=======================================
    call ESMF_ArraySpecSet(arrayspec, 2, ESMF_TYPEKIND_R8, rc)
    if (rc.ne.ESMF_SUCCESS) then
        ier=ESMF_FAILURE
        print *, "ESMF_ArraySpecSet failed"
    end if

   !Create the source field (with halo width of 3)
   !==============================================
    call createField(igrid=srcigrid,               &
                     LocChoice = SrcLocChoice,   &
                     halo      = SrcHalo,        &
                     fieldName = "src pressure", &
                     field     = field1,         &
                     f90ptr    = f90ptr1,        &
                     xCoor     = x_coords,       &
                     yCoor     = y_coords,       &
                     arrayspec = arrayspec,      &
                     rc        = sub_rc	)

    if (sub_rc.ne.ESMF_SUCCESS) then 
	ier = ESMF_FAILURE 
        print *, "createField failed"
    endif

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


   !Create the destination field for dstigrid1(with halo width of 0)
   !===================================================
    call createField(igrid=Dstigrid1,               &
                     LocChoice = DstLocChoice,   &
                     halo      = DstHalo,        &
                     fieldName = "Dst pressure", &
                     field     = field2,         &
                     f90ptr    = f90ptr2,        &
                     xCoor     = x_coords2,      &
                     yCoor     = y_coords2,      &
                     arrayspec = arrayspec,      &
                     rc        = sub_rc )

   !Create the destination field for distgrid2(with halo width of 0)
   !===================================================
    call createField(igrid=Dstigrid2,               &
                     LocChoice = DstLocChoice,   &
                     halo      = DstHalo,        &
                     fieldName = "Dst pressure", &
                     field     = field3,         &
                     f90ptr    = f90ptr3,        &
                     xCoor     = x_coords3,      &
                     yCoor     = y_coords3,      &
                     arrayspec = arrayspec,      &
                     rc        = sub_rc )

    if (sub_rc.ne.ESMF_SUCCESS) then 
	ier = ESMF_FAILURE
        print *, "createField failed"
    end if
    ! fields all ready to go

!\subsubsection{Precomputing and Executing a Regrid}
      
!  The user has already created an {\tt ESMF\_IGrid}, an
!  {\tt ESMF\_Array} with data, and put them together in an {\tt ESMF\_Field}.
!  An {\tt ESMF\_RouteHandle} is created and the data movement needed to
!  execute the regrid is stored with that handle by the store method. 
!  To actually execute the operation, the source and destination data
!  objects must be supplied, along with the same {\tt ESMF\_RouteHandle}.
      


   !Do all the calculations in preparation for the actual re-igridding
   !=================================================================
    call ESMF_FieldRegridStore(field1, field2, vm, &
                               routehandle=regrid_rh1, &
                               regridmethod=MethodChoice, rc=rc)
    if (rc.ne.ESMF_SUCCESS) then 
	ier = ESMF_FAILURE
        print *, "ESMF_FieldRegridStore failed"
    end if

   !Do all the calculations in preparation for the actual re-igridding
   !=================================================================
    call ESMF_FieldRegridStore(field1, field3, vm, &
                               routehandle=regrid_rh2, &
                               regridmethod=MethodChoice, rc=rc)
    if (rc.ne.ESMF_SUCCESS) then 
	ier = ESMF_FAILURE
        print *, "ESMF_FieldRegridStore failed"
    end if

 
   !Regrid
   !======
    call ESMF_FieldRegrid(field1, field2, regrid_rh1, rc=rc)
    if (rc.ne.ESMF_SUCCESS) then 
	ier = ESMF_FAILURE
        print *, "ESMF_FieldRegrid failed"
    end if

    call ESMF_FieldRegrid(field1, field3, regrid_rh2, rc=rc)
    if (rc.ne.ESMF_SUCCESS) then 
	ier = ESMF_FAILURE
        print *, "ESMF_FieldRegrid failed"
    end if


!===============================================================
!Verification: Compare to the "exact solution" on the dest. igrid
!===============================================================

    !Array bounds in the destination igrid (local indexing)
    !=====================================================
     lbDst(:) = lbound(f90ptr2)
     ubDst(:) = ubound(f90ptr2)

    !Allocate the array pointer for the "exact solution at the dest. igrid
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
      ier=ESMF_FAILURE
    end if


   !--Compute exact fcn. values at the Destination IGrid 
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
         ier=ESMF_FAILURE
        print *, "RelativeError .gt. epsil"
     !TODO compute the global values of i,j for diagnostic printing
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

   deallocate(SolnOnTarget)

   !Verify the second regrid with a different igrid size 
   !=====================================================

    !Array bounds in the destination igrid (local indexing)
    !=====================================================
     lbDst(:) = lbound(f90ptr3)
     ubDst(:) = ubound(f90ptr3)

    !Allocate the array pointer for the "exact solution at the dest. igrid
    !====================================================================
     allocate( SolnOnTarget( lbDst(1):ubDst(1) , lbDst(2):ubDst(2) ) )


   !--Re-associate the Phi and Theta pointers to the field2 coordinates
   !===================================================================
    if (domainType == 1) then
      !--Whole_globe domain:
      Phi => x_coords3
      Theta => y_coords3
    else if (domainType ==2) then
      !--Regional domain:
      allocate( Phi( size(x_coords3,1), size(x_coords3,2) ) )
      allocate( Theta( size(y_coords3,1) , size(y_coords3,2) ) )
      Phi   = 2.  * pi * (x_coords3+10.) / xmax
      Theta = 0.5 * pi * (y_coords3+10.) / ymax
    else
      print*,'ERROR! domainType=',domainType,' valid values=1,2'
      ier=ESMF_FAILURE
    end if


   !--Compute exact fcn. values at the Destination IGrid 
   !===================================================
    call functionValues(FieldChoice, x_coords3, y_coords3, Phi, Theta, &
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
       RelativeError=abs( (SolnOnTarget(i,j)-f90ptr3(i,j)) / SolnOnTarget(i,j) )
      !write(*,'(a,i2,a,2i3,1x,e11.4,1x,e11.4)') &
      !     'localPET=',localPet,' i,j=',i,j,SolnOnTarget(i,j), &
      !     f90ptr3(i,j)
       if (RelativeError .gt. epsil ) then
         regrid_rc=ESMF_FAILURE
         ier=ESMF_FAILURE
        print *, "RelativeError .gt. epsil"
     !TODO compute the global values of i,j for diagnostic printing
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


    call ESMF_FieldRegridRelease(regrid_rh1, rc=rc)
    if (rc.ne.ESMF_SUCCESS) then 
	ier = ESMF_FAILURE
        print *, "ESMF_FieldRegridRelease failed"
    end if

    call ESMF_FieldRegridRelease(regrid_rh2, rc=rc)
    if (rc.ne.ESMF_SUCCESS) then 
	ier = ESMF_FAILURE
        print *, "ESMF_FieldRegridRelease failed"
    end if

    deallocate(SolnOnTarget)
!-------------------------------------------------------------------------
!    ! Cleanup

    call ESMF_FieldDestroy(field1, rc=rc)

    call ESMF_FieldDestroy(field2, rc=rc)

    call ESMF_FieldDestroy(field3, rc=rc)

    call ESMF_IGridDestroy(srcigrid, rc=rc)

    call ESMF_IGridDestroy(dstigrid1, rc=rc)

    call ESMF_IGridDestroy(dstigrid2, rc=rc)
!----------------------------------------------------------------
    return

contains

!==================================================================
    subroutine createWholeGlobeIGrids

 ! Create igrids covering the whole globe. Note that crop factor was introduced
 ! for the benefit of regional igrids originally. It is not needed here.

    !--- Full physical domain dimension
    xmin = 0.0
    ymin = -0.5*pi
    xmax = 2.*pi
    ymax = 0.5*pi

    crop_factor=1.0 !portion of the domain to be covered by the igrid

    !--- IGrid dimension to cover full physical domain.
    nx_domain=100
    ny_domain=150
	
    !--Coordinate ranges of the "test igrids"
    ! The following line is replaced with actual values to avoid an
    ! internal compiler error on jazz pgi.
    ! PGF90-F-0000-Internal compiler error. Errors in Lowering
    !mincoords = (/ xmin*crop_factor,  ymin*crop_factor /)
    mincoords = (/ 0.0*1.0d0,  -0.5d0*pi*1.0d0 /)
    maxcoords = (/ xmax*crop_factor,  ymax*crop_factor /)

    !--- Number of cells in the current igrid.
    n_cells = (/ int(real(nx_domain)*crop_factor), &
                 int(real(ny_domain)*crop_factor) /)

    if (localPET == 0) then
      print*,'n_cells=',n_cells
      print*,'mincoords=',mincoords
      print*,'maxcoords=',maxcoords
    end if


    !Create the source igrid
   !===========================
    srcigrid = ESMF_IGridCreateHorzLatLonUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=SrcIGridChoice, &
                   dimUnits= (/ "radians" , "radians" /), &
                   periodic=(/ ESMF_true , ESMF_false /), &
                   name="srcigrid", rc=rc)

   !Distribute the source igrid
   !===========================
    call ESMF_IGridDistribute(srcigrid, delayout=layout1, rc=rc)

   !Create the destination igrid1 (the same size as srcigrid)
   !=======================================================
    dstigrid1 = ESMF_IGridCreateHorzLatLonUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=DstIGridChoice, &
                   dimUnits= (/ "radians" , "radians" /), &
                   periodic=(/ ESMF_true , ESMF_false /), &
                   name="dstigrid1", rc=rc)

   !Distribute destination igrid
   !===========================
    call ESMF_IGridDistribute(dstigrid1, delayout=layout2, rc=rc)

    !--- IGrid dimension to cover full physical domain.
    nx_domain=240
    ny_domain=120

    !--- Number of cells in the current igrid.
    n_cells = (/ int(real(nx_domain)*crop_factor), &
                 int(real(ny_domain)*crop_factor) /)

   !Create the destination igrid2 (different size from srcigrid)
   !==========================================================
    dstigrid2 = ESMF_IGridCreateHorzLatLonUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=DstIGridChoice, &
                   dimUnits= (/ "radians" , "radians" /), &
                   periodic=(/ ESMF_true , ESMF_false /), &
                   name="dstigrid2", rc=rc)

   !Distribute destination igrid
   !===========================
    call ESMF_IGridDistribute(dstigrid2, delayout=layout2, rc=rc)

    return
    end subroutine createWholeGlobeIGrids

!===========================================================================

    subroutine createRegionalIGrids

!--Create a Regional (not whole-globe) rectangular igrid.
   
    !---Maximum range of physical dimensions of the igrid
    xmin = 0.0
    ymin = 0.0
    xmax = 20.
    ymax = 30.

    crop_factor=.2   !Fraction of the maximum range covered by the actual igrid

    !--- Maximum size of the igrid (# of igrid cells).
    nx_domain=100
    ny_domain=150

    !--- Number of cells in the current igrid.
    n_cells = (/real(nx_domain)*crop_factor, real(ny_domain)*crop_factor /)
    mincoords = (/ xmin*crop_factor,  ymin*crop_factor /)
    maxcoords = (/ xmax*crop_factor,  ymax*crop_factor /)

    if (localPET == 0) then
      print*,'n_cells=',n_cells
      print*,'mincoords=',mincoords
      print*,'maxcoords=',maxcoords
    end if

    !Create the source igrid
   !===========================
    srcigrid = ESMF_IGridCreateHorzXYUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=SrcIGridChoice, &
                   name="srcigrid", rc=rc)

   !Distribute the source igrid
   !===========================
    call ESMF_IGridDistribute(srcigrid, delayout=layout1, rc=rc)

   !Create the destination igrid1
   !===========================
    dstigrid1 = ESMF_IGridCreateHorzXYUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=DstIGridChoice, &
                   name="dstigrid1", rc=rc)

   !Distribute destination igrid1
   !===========================
    call ESMF_IGridDistribute(dstigrid1, delayout=layout2, rc=rc)

    !---Maximum range of physical dimensions of the igrid
    !xmin = 0.0
    !ymin = 0.0
    !xmax = 48.
    !ymax = 24.

    !--- Maximum size of the igrid (# of igrid cells).
    nx_domain=240
    ny_domain=120
    n_cells = (/real(nx_domain)*crop_factor, real(ny_domain)*crop_factor /)
    !mincoords = (/ xmin*crop_factor,  ymin*crop_factor /)
    !maxcoords = (/ xmax*crop_factor,  ymax*crop_factor /)

   !Create the destination igrid2
   !===========================
    dstigrid2 = ESMF_IGridCreateHorzXYUni( n_cells, &
                   mincoords, maxcoords, &
                   horzStagger=DstIGridChoice, &
                   name="dstigrid2", rc=rc)

   !Distribute destination igrid2
   !===========================
    call ESMF_IGridDistribute(dstigrid2, delayout=layout2, rc=rc)


    return
    end subroutine createRegionalIGrids

!=========================================================================

    subroutine createField(igrid, LocChoice, halo, fieldName, field, &
                              f90ptr, xCoor,  yCoor, arrayspec, rc )
    type(ESMF_IGrid), intent(inout)              :: igrid
    type(ESMF_RelLoc), intent(in)               :: LocChoice
    integer, intent(in)                         :: halo
    character (len=*), intent(in)               :: fieldName
    type(ESMF_Field), intent(out)               :: field
    real(ESMF_KIND_R8), dimension(:,:), pointer :: f90ptr
    real(ESMF_KIND_R8), dimension(:,:), pointer :: xCoor, yCoor
    type(ESMF_ArraySpec), intent(inout)         :: arrayspec
    integer, intent(out)			:: rc   

 !-------------------------------------------------------------------------
 !--Create a field and return a pointer to its data array, and the arrays 
 !  of coordinates --- x and y.
 !--------------------------------------------------------------------------
  integer :: local_rc
   rc = ESMF_SUCCESS

   !Create the field 
   !================
    field = ESMF_FieldCreate(igrid, arrayspec, &
                              horzRelloc=LocChoice, &
                              haloWidth=halo, name=fieldName, rc=local_rc)
   if (local_rc.ne.ESMF_SUCCESS) then 
	rc = ESMF_FAILURE
        print *, "ESMF_FieldCreate failed"
   end if


   !Create a pointer to the field data space
   !========================================
    call ESMF_FieldGetDataPointer(field, f90ptr, ESMF_DATA_REF, rc=local_rc)

   if (local_rc.ne.ESMF_SUCCESS) then 
	rc = ESMF_FAILURE
        print *, "ESMF_FieldGetDataPointer failed"
   end if

   !Get the coordinates of the igrid
   !===============================
    call ESMF_IGridGetCoord(igrid,dim=1,horzRelLoc=LocChoice,  &
           centercoord=xCoor,docopy=ESMF_DATA_COPY,rc=local_rc)

   if (local_rc.ne.ESMF_SUCCESS) then 
	rc = ESMF_FAILURE
        print *, "ESMF_IGridGetCoord xCoor failed"
   end if

    call ESMF_IGridGetCoord(igrid,dim=2,horzRelLoc=LocChoice,  &
           centercoord=yCoor,docopy=ESMF_DATA_COPY,rc=local_rc)
                                                                                                                      
   if (local_rc.ne.ESMF_SUCCESS) then
        rc = ESMF_FAILURE
        print *, "ESMF_IGridGetCoord yCoor failed"
   end if

    return
    end subroutine createField

    end subroutine Regrid
#undef  ESMF_METHOD
#define ESMF_METHOD "functionValues"

!=============================================
  subroutine functionValues(Choice, xCoord, yCoord, Phi, Theta, &
                            lb, ub, halo, maxCoor, f90ptr, ier)

 !--Compute the values of the test function and store in the f90ptr.
 !  The parameter Choice determines which function is returned.
 !  Choice 3 and 4 are the only ones appropriate for WHOLE_GLOBE tests.

  integer, intent(in)                             :: Choice
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: xCoord, yCoord
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: Theta, Phi
  integer, dimension(2), intent(inout)            :: lb, ub
  integer, intent(in)                             :: halo
  real(ESMF_KIND_R8), dimension(2), intent(in)    :: maxCoor
  real(ESMF_KIND_R8), dimension(:,:), pointer     :: f90ptr
  integer, intent (out) :: ier

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
#undef  ESMF_METHOD


end module ESMF_RegridSubroutines

