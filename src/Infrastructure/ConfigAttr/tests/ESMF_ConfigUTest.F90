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
      character, parameter ::restart_file_0 = 'RestartFile123'
      character(len=1)   :: answer_0 = 'y'
 
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

      call ESMF_ConfigFindLabel ( cf, 'u-wind-error:', rc ) ! identifies label
      call ESMF_ConfigGetString ( cf, u_dataType, rc =rc )  ! first token

      print *,'ESMF_ConfigGetString: u_dataType= ', u_dataType, &
           ' rc =', rc

      nu = ESMF_ConfigGetInt ( cf, rc = rc )                 ! second token
      print *,'ESMF_ConfigGetInts: nu =', nu, ' rc =', rc

      call ESMF_ConfigGetFloats ( cf, sigU, nu,  rc=rc )     ! tokens 3 thru 8 
      print *,'ESMF_ConfigGetFloats: sigU(1,', nu,') =', &
           sigU(1:nu), ' rc =', rc 

      call ESMF_ConfigFindLabel ( cf, 'v-wind-error:', rc )

      call ESMF_ConfigGetString ( cf, v_dataType, rc = rc )
      print *,'ESMF_ConfigGetString: v_dataType= ', v_dataType, &
           ' rc =', rc

      nv = ESMF_ConfigGetInt ( cf, rc = rc )
      print *,'ESMF_ConfigGetInts: nv =', nv, ' rc =', rc

      call ESMF_ConfigGetFloats ( cf, sigV, nsize=nv, rc=rc )
      print *,'ESMF_ConfigGetFloats: sigV(1,', nv,') =', &
           sigV(1:nv), ' rc =', rc 

  
! NOTE: Order is not relevant; first label found is returned



! Retrieval of a group of parameters on multiple lines
!   ----------------------------------------------------

      call ESMF_ConfigFindLabel ( cf, 'ObsErr*QSCAT::', rc ) ! identify label
      print *,'ESMF_ConfigFindLabel: Finding label ObsErr*QSCAT::  rc =', rc


      call ESMF_ConfigNextLine ( cf, rc=rc )               ! move down 1 line
      print *,'ESMF_ConfigNextLine: rc =', rc

      call ESMF_ConfigGetString ( cf, u_dataType, rc=rc )  ! first token
      print *,'ESMF_ConfigGetString: u_dataType= ', u_dataType, ' rc =', rc

      nu = ESMF_ConfigGetInt ( cf, rc=rc )                 ! second token
      print *,'ESMF_ConfigGetInts: nu =', nu, ' rc =', rc

      call ESMF_ConfigGetFloats ( cf, sigU, nsize=6, rc=rc ) ! tokens 3 thru 8 
      print *,'ESMF_ConfigGetFloats: sigU(1,6) =', sigU(1:6), ' rc =', rc

!      Similarly for v

      call ESMF_ConfigNextLine ( cf, rc=rc )
      print *,'ESMF_ConfigNextLine: rc =', rc
      
      call ESMF_ConfigGetString ( cf, v_dataType, rc=rc )
      print *,'ESMF_ConfigGetString: v_dataType= ', v_dataType, ' rc =', rc

      nv = ESMF_ConfigGetInt ( cf, rc=rc )
      print *,'ESMF_ConfigGetInts: nv =', nv, ' rc =', rc

      call ESMF_ConfigGetFloats ( cf, sigV, nsize=6,rc=rc )
      print *,'ESMF_ConfigGetFloats: sigV(1,6) =', sigV(1:6), ' rc =', rc 


! Retrieval of Tables of unknown length
! ---------------------------------------


!            Get label and start getting lines




      call ESMF_ConfigGetDim(cf,'ObsErr*vCor_HH-7::', nlines, col, rc)
      print *,'ESMF_ConfigGetLen: nlines =', nlines,' rc = ', rc 

!!              if (line >= MAXLEV) exit
!!              if (rc /= 0) exit
      
      if ( (rc ==0) .and. (nlines > 0)) then
         
         call ESMF_ConfigFindLabel ( cf,'ObsErr*vCor_HH-7::', rc )
         print *,'ESMF_ConfigFindLabel: Finding ObsErr*vCor_HH-7:: rc =', rc
         
         allocate(ncol(1:nlines))
         
         do line = 1, nlines
            call ESMF_ConfigNextLine(cf, rc = rc)
            ncol(line) = ESMF_ConfigGetLen(cf,'ObsErr*vCor_HH-7::', rc)
         enddo
         
!            Looping over lines
         call ESMF_ConfigFindLabel ( cf,'ObsErr*vCor_HH-7::', rc )
         do line = 1, nlines
            call ESMF_ConfigNextLine( cf, end, rc )
            
!               Retrieve pressure level
!               -----------------------
            plev(line) = ESMF_ConfigGetFloat ( cf, rc=rc )
            if (rc /= 0) exit
            
            print *,' Line ', line, 'plev =', plev
            
!               Looping over columns
!               --------------------
            do col =1, ncol(line) - 1
               temp = ESMF_ConfigGetFloat ( cf, rc=rc)
               if (rc == 0) then 
                  vCorr(line,col) = temp 
               end if
            end do

            if (rc == 0) then
               print *,(vCorr(line,1:ncol(line)))    
            else
               print *,' Error in reading this line'
            endif
         end do
      end if

! Finalization
! ------------
      call ESMF_ConfigDestroy ( cf, rc ) 
      print *,'ESMF_ConfigDestroy: rc =', rc
      


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
