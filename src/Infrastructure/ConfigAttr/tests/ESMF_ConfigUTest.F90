!-------------------------------------------------------------------------
!           NASA Earth System Modeling Framework Project (ESMF)          !
!            NASA/GSFC, Data Assimilation Office, Code 910.3             !
!-------------------------------------------------------------------------
!
!
! !TITLE: ESMF Congiguration Management Test File \\ Version 1.01
!
! !AUTHORS: Leonid Zaslavsky and Arlindo da Silva
!
! !AFFILIATION: Data Assimilation Office, NASA/GSFC, Greenbelt, MD 20771
!
! !DATE: April 7, 2003
!
!
! This test file provides tests for ESMF Configuration Management System
! implemented in ESMF\_ConfigMod.F90.
!
! !REVISION HISTORY:
!
!       7apr2003 Leonid Zaslavsky Created.
!      14apr2003 Leonid Zaslavsky Corrected.
!      27apr2003 Leonid Zaslavsky Further corrected and debugged.
!------------------------------------------------------------------------

    program ESMF_Config_Test

      use  ESMF_DELayoutMod
      use ESMF_ConfigMod   

      type(ESMF_DELayout) :: layout  
      type (ESMF_Config) cf 
      
      character, parameter :: fname ='ESMF_Resource_File_Sample.rc'
      integer, parameter   :: nDE_0 = 32      
      real, parameter      :: tau_0 = 14.0
      character, parameter :: restart_file_0 = 'RestartFile123'
      character, parameter   :: answer_0 = 'y'

      character, parameter :: u_dataType_0 = 'u-wind_error'
      character, parameter :: v_dataType_0 = 'v-wind_error'
      integer, parameter   :: nu_0 = 6
      integer, parameter   :: nv_0 = 6
      real, dimension(nu_0), parameter :: sigU_0 = &
           (/ 2.0, 2.0, 2.2, 2.3, 2.7, 3.2 /)
      real, dimension(nv_0), parameter :: sigV_0 = &
           (/ 2.2, 2.2, 2.3, 2.7, 3.2, 3.4 /)


      character, parameter :: u_dataType_1 = 'u_UprAir.u'
      character, parameter :: v_dataType_1 = 'v_UprAir.u'
      integer, parameter   :: nu_1 = 6
      integer, parameter   :: nv_1 = 6
      real, dimension(nu_1), parameter :: sigU_1 = &
           (/ 2.0, 2.0, 2.2, 2.3, 2.7, 3.2 /)
      real, dimension(nv_1), parameter :: sigV_1 = &
           (/ 2.2, 2.2, 2.3, 2.7, 3.2, 3.2 /)  

      integer, parameter   :: nlines_0 = 11
      integer, dimension(nlines_0), parameter :: ncol_0 = &
           (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11/)
      real, dimension(nlines_0), parameter ::plev_0 = &
           (/1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100/)
      real, dimension(nlines_0, nlines_0) :: vCorr_0

!!!  Should be initialized ========== PARAMETER

      character(len=255) :: restart_file
      integer :: rc
      logical :: unique
      integer   :: nDE
      real      :: tau
      logical   :: Doing_QC
      character(len=1)   :: answer
      character(len=10) :: u_dataType, v_dataType
      integer           :: nu, nv
      real              :: sigU(6), sigV(6)
      integer, parameter :: MAXLEV = 100, EOL = 111
      real    :: plev(MAXLEV), vCorr(MAXLEV, MAXLEV)
      integer :: nlev
      integer :: line, col, nlines
      integer, allocatable, dimension(:) :: ncol
      logical :: end
      real temp
      
      integer :: counter_total, counter_success
      integer :: rc_opening
      real :: success_rate

      counter_total = 0
      counter_success = 0
 
      rc = 0

! Initialization:
!----------------

      cf = ESMF_ConfigCreate( rc )
      
      if ( rc /= 0 ) then 
         print *,'ESMF_ConfigCreate: catastrophic error, rc =', rc
         STOP 'CANNOT CREAT CONFIGURATION'
      endif

      call ESMF_ConfigLoadFile( cf, fname, rc = rc)

      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigLoadFile:  loaded file ', fname, &
              ' catastrophic error, rc = ', rc
         STOP 'CANNOT OPEN FILE'
      else
         counter_total =counter_total + 1
         counter_success =counter_success + 1         
      endif
      
!!      if ( .NOT. unique ) then
!!         print *,' File contains multiple copies of a label' 
!!      end if

! Retrieval of single parameters
!--------------------------------
! Integer

      rc = 0

!*********************************************************************
      nDE = ESMF_ConfigGetInt( cf, label ='Number_of_DEs:', & 
           default=7, rc = rc )
!*********************************************************************      
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetInt got nDE =', nDE,' rc =', rc
      else
         if (nDE == nDE_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetInt ERROR: got nDE =', nDE, &
              ' should be', nDE_0
         endif
      endif


! Floating point

      rc = 0
!*********************************************************************   
      tau = ESMF_ConfigGetFloat(cf, &
           label = 'Relaxation_time_scale_in_days:', rc = rc)
!*********************************************************************   
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetFloat got tau =', tau,' rc =', rc
      else
         if (tau == tau_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetFloat ERROR: got tau =', tau, &
              ' should be', tau_0
         endif
      endif


! Character

      rc = 0
!*********************************************************************
      answer = ESMF_ConfigGetChar ( cf, 'Do_you_want_quality_control:', &
                                    rc = rc )
!*********************************************************************
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetChar got answer =', answer,' rc =', rc
      else
         if (answer == answer_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetChar ERROR: got answer =', answer, &
              ' should be', answer_0
         endif
      endif

! String

     rc = 0
!*********************************************************************
      call ESMF_ConfigGetString ( cf, restart_file ,'restart_file_name:', &
           rc = rc )
!*********************************************************************
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetString got =', restart_file,' rc =', rc
      else
         if (answer == answer_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetString ERROR: got  =', restart_file, &
              ' should be', restart_file_0
         endif
      endif


      print *,'ESMF_ConfigGetString: restart_file =',  restart_file, &
           ' rc =', rc

! NOTE: A non-zero rc is returned when an attribute is not found - unless
!      the function is called with an optional default.



! Retrieval of a group of parameters on a single line
! ----------------------------------------------------


      rc = 0
 
!*********************************************************************
      call ESMF_ConfigFindLabel ( cf, 'u-wind-error:', rc ) ! identifies label
!*********************************************************************
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, rc =', rc 
         go to 100        
      endif

!*********************************************************************
      call ESMF_ConfigGetString ( cf, u_dataType, rc =rc )  ! first token   
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetString failed, rc =', rc
         go to 100
      endif

      if(u_dataType ==  u_dataType_0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetString ERROR: got  =', u_dataType, &
              ' should be', u_dataType_0
         go to 100
      endif


!*********************************************************************
      nu = ESMF_ConfigGetInt ( cf, rc = rc )                 ! second token
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetInt failed, rc =', rc
         go to 100
      endif

      if( nu == nu_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetInt ERROR: got  =', nu, &
              ' should be', nu_0 
         go to 100
      endif

!*********************************************************************
      call ESMF_ConfigGetFloats ( cf, sigU, nu,  rc=rc )     ! tokens 3 thru 8
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetFloats failed, rc =', rc
         go to 100
      endif

      if( any(sigU /= sigU_0) ) then
         print *,'ESMF_ConfigGetInt ERROR: got sigU =', sigU(1:nu), &
              ' should be sigU =', sigU_0(1:nu) 
         go to 100
      else
         counter_success =counter_success + 1
      endif

!----------------
100   continue
!----------------

!*********************************************************************
      call ESMF_ConfigFindLabel ( cf, 'v-wind-error:', rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, rc =', rc 
         go to 200        
      endif

!*********************************************************************
      call ESMF_ConfigGetString ( cf, v_dataType, rc = rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetString failed, rc =', rc
         go to 200
      endif

      if(v_dataType ==  v_dataType_0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetString ERROR: got  =', v_dataType, &
              ' should be', v_dataType_0
         go to 200
      endif

!*********************************************************************
      nv = ESMF_ConfigGetInt ( cf, rc = rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetInt failed, rc =', rc
         go to 200
      endif

      if( nv == nv_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetInt ERROR: got  =', nv, &
              ' should be', nv_0 
         go to 200
      endif

!*********************************************************************
      call ESMF_ConfigGetFloats ( cf, sigV, nsize=nv, rc=rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetFloats failed, rc =', rc
         go to 200
      endif

      if( any(sigV /= sigV_0) ) then
         print *,'ESMF_ConfigGetInt ERROR: got sigV =', sigV(1:nv), &
              ' should be sigV =', sigV_0(1:nv) 
         go to 200
      else
        counter_success =counter_success + 1
      endif

!----------------
200   continue
!----------------
  
! NOTE: Order is not relevant; first label found is returned



! Retrieval of a group of parameters on multiple lines
!   ----------------------------------------------------

      rc = 0

!*********************************************************************
      call ESMF_ConfigFindLabel ( cf, 'ObsErr*QSCAT::', rc ) ! identify label
!*********************************************************************
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, rc =', rc 
         go to 300        
      endif

!*********************************************************************
      call ESMF_ConfigNextLine ( cf, rc=rc )               ! move down 1 line
!*********************************************************************
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigNextLine failed, rc =', rc 
         go to 300        
      endif

!*********************************************************************
      call ESMF_ConfigGetString ( cf, u_dataType, rc=rc )  ! first token
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetString failed, rc =', rc
         go to 300
      endif

      if(u_dataType ==  u_dataType_1) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetString ERROR: got  =', u_dataType, &
              ' should be', u_dataType_1
         go to 300
      endif

!*********************************************************************
      nu = ESMF_ConfigGetInt ( cf, rc=rc )                 ! second token
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetInt failed, rc =', rc
         go to 300
      endif

      if( nu == nu_1 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetInt ERROR: got  =', nu, &
              ' should be', nu_1 
         go to 300
      endif

!*********************************************************************
      call ESMF_ConfigGetFloats ( cf, sigU, nsize=6, rc=rc ) ! tokens 3 thru 8
!*********************************************************************
     counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetFloats failed, rc =', rc
         go to 300
      endif

      if( any(sigU /= sigU_1) ) then
         print *,'ESMF_ConfigGetInt ERROR: got sigU =', sigU(1:nu), &
              ' should be sigU =', sigU_1(1:nu_1) 
         go to 300
      else
        counter_success =counter_success + 1
      endif



!      Similarly for v
!*********************************************************************
      call ESMF_ConfigNextLine ( cf, rc=rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigNextLine failed, rc =', rc 
         go to 300        
      endif

!*********************************************************************
      call ESMF_ConfigGetString ( cf, v_dataType, rc=rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetString failed, rc =', rc
         go to 300
      endif

      if(v_dataType ==  v_dataType_1) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetString ERROR: got  =', v_dataType, &
              ' should be', v_dataType_1
         go to 300
      endif

!*********************************************************************
      nv = ESMF_ConfigGetInt ( cf, rc=rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetInt failed, rc =', rc
         go to 300
      endif

      if( nv == nv_1 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetInt ERROR: got  =', nv, &
              ' should be', nv_1 
         go to 300
      endif

!*********************************************************************
      call ESMF_ConfigGetFloats ( cf, sigV, nsize=6,rc=rc )
!*********************************************************************
     counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetFloats failed, rc =', rc
         go to 300
      endif

      if( any(sigV /= sigV_1) ) then
         print *,'ESMF_ConfigGetInt ERROR: got sigV =', sigV(1:nv), &
              ' should be sigV =', sigV_1(1:nv_1) 
         go to 300
      else
        counter_success =counter_success + 1
      endif


!----------------
300   continue
!----------------


! Retrieval of Tables of unknown length
! ---------------------------------------


!            Get dimension, label and start getting lines

      rc = 0

!*********************************************************************
      call ESMF_ConfigGetDim(cf,'ObsErr*vCor_HH-7::', nlines, col, rc)
!*********************************************************************
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetDim failed, rc =', rc
         go to 400
      endif

      if( nlines == nlines_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetDim ERROR: got  =', nlines, &
              ' should be', nlines_0 
         go to 400
      endif

      if( any (ncol /= ncol_0) ) then
         print *,'ESMF_ConfigGetDim ERROR: got  =', ncol, &
              ' should be', ncol_0 
         go to 400
      else
         counter_success =counter_success + 1
      endif

!!              if (line >= MAXLEV) exit
!!              if (rc /= 0) exit
      
!*********************************************************************         
         call ESMF_ConfigFindLabel ( cf,'ObsErr*vCor_HH-7::', rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, rc =', rc 
         go to 400        
      endif

!*********************************************************************     
         allocate(ncol(1:nlines), STAT= rc)
!*********************************************************************
      if (rc /= 0) then
         print *,'array allocation failed, rc =', rc
         go to 400
      endif
      
      counter_total =counter_total + 1
!********************************************************************* 
      do line = 1, nlines
         call ESMF_ConfigNextLine(cf, rc = rc)
!*********************************************************************
         if (rc /= 0) then
            print *,'ESMF_ConfigNextLine failed, rc =', rc 
            go to 400        
         endif
!*********************************************************************    
      ncol(line) = ESMF_ConfigGetLen(cf,'ObsErr*vCor_HH-7::', rc)
!*********************************************************************  
      if (rc /= 0) then
         print *,'ESMF_ConfigGetLen failed, rc =', rc
         go to 400
      endif
!*********************************************************************  
      enddo
!*********************************************************************

      if( any(ncol /= ncol_0 )) then
         print *,'ESMF_ConfigGetInt ERROR: got ncol =', ncol(1:nlines), &
              ' should be ncol =', ncol_0(1:nlines_0) 
         go to 300
      else
         counter_success =counter_success + 1
      endif


!            Looping over lines

!*********************************************************************   
         call ESMF_ConfigFindLabel ( cf,'ObsErr*vCor_HH-7::', rc )
!*********************************************************************
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, rc =', rc 
         go to 400        
      endif


!*********************************************************************
      do line = 1, nlines
         call ESMF_ConfigNextLine( cf, end, rc )
!*********************************************************************
         if (rc /= 0) then
            print *,'ESMF_ConfigNextLine failed, rc =', rc 
            go to 400        
         endif
            
!               Retrieve pressure level
!               -----------------------
         counter_total =counter_total + 1
!*********************************************************************
            plev(line) = ESMF_ConfigGetFloat ( cf, rc=rc )
!*********************************************************************
         if (rc /= 0) then
            print *,'ESMF_ConfigNextLine failed, rc =', rc 
            go to 400
         endif

         if( any(plev(1:nlines) /= plev_0(1:nlines)) ) then
            print *,'ESMF_ConfigGetFloat ERROR: got plev =', &
                 plev(1:nlines), ' should be sigV =',        &
                 plev_0(1:nlines_0) 
            go to 400
         else
            counter_success =counter_success + 1
         endif
            
!               Looping over columns
!               --------------------
         counter_total =counter_total + 1
!*********************************************************************
         do col =1, ncol(line) - 1
            temp = ESMF_ConfigGetFloat ( cf, rc=rc)
            if (rc == 0) then 
               vCorr(line,col) = temp 
            end if
         end do
!*********************************************************************
         if (rc /= 0) then
            print *,'ESMF_ConfigGetFloat failed, rc =', rc 
            go to 400        
         endif

         
         do col =1, ncol(line) - 1
            if (vCorr(line, col) /= vCorr_0(line, col)) then
               print *,'ESMF_ConfigGetFloat:  Wrong value in vCorr '
               go to 400
            endif
         end do
         counter_success =counter_success + 1

!*********************************************************************    
      end do
!*********************************************************************

!----------------
400   continue
!----------------

! Finalization
! ------------

      rc = 0

!*********************************************************************
      call ESMF_ConfigDestroy ( cf, rc ) 
!*********************************************************************
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigDestroy failed, rc =', rc 
      endif     


! REPORTING
! ------------
      if  (counter_total > 0) then
         if( counter_success == counter_total ) then 
            print *,'ESMF_Config_Test: All tests were successful'
         else
            success_rate = 100.0 * counter_success / counter_total 
            print *,'ESMF_Config_Test: Success rate: ', success_rate,'%' 
         endif
      endif

    end program ESMF_Config_Test
