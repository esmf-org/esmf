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
      
      character(len=255) :: fname, restart_file
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

      fname ='ESMF_Resource_File_Sample.rc'

! Initialization:
!----------------

      cf = ESMF_ConfigCreate( rc )
      print *,'ESMF_ConfigCreate completed, rc =', rc

      call ESMF_ConfigLoadFile( cf, fname, rc = rc)
      print *,'ESMF_ConfigLoadFile loaded file ', fname,' rc = ', rc

!!      if ( .NOT. unique ) then
!!         print *,' File contains multiple copies of a label' 
!!      end if

! Retrieval of single parameters
!--------------------------------

      call ESMF_ConfigGetInt ( cf, nDE, label ='Number_of_DEs:', default=7, &
           rc = rc )
      print *,'ESMF_ConfigGetInt got nDE =', nDE,' rc =', rc


      call ESMF_ConfigGetFloat ( cf, tau, &
           label ='Relaxation_time_scale_in_days:', rc = rc)
      answer = ESMF_ConfigGetChar ( cf, 'Do_you_want_quality_control:', &
                                    rc = rc )
      print *,'ESMF_ConfigGetChar: answer =', answer,' rc =', rc

      call ESMF_ConfigGetString ( cf, restart_file ,'restart_file_name:', &
           rc = rc )
      print *,'ESMF_ConfigGetString: restart_file =',  restart_file, &
           ' rc =', rc

! NOTE: A non-zero rc is returned when an attribute is not found - unless
!      the function is called with an optional default.



! Retrieval of a group of parameters on a single line
! ----------------------------------------------------

      call ESMF_ConfigFindLabel ( cf, 'u-wind-error:', rc ) ! identifies label
      call ESMF_ConfigGetString ( cf, u_dataType, rc =rc )  ! first token

      print *,'ESMF_ConfigGetString: u_dataType= ', u_dataType, &
           ' rc =', rc

      call ESMF_ConfigGetInt ( cf, nu, rc = rc )            ! second token
      print *,'ESMF_ConfigGetInt: nu =', nu, ' rc =', rc

      call ESMF_ConfigGetFloat ( cf, sigU, nu,  rc=rc )     ! tokens 3 thru 8 
      print *,'ESMF_ConfigGetFloat: sigU(1,', nu,') =', &
           sigU(1:nu), ' rc =', rc 

      call ESMF_ConfigFindLabel ( cf, 'v-wind-error:', rc )

      call ESMF_ConfigGetString ( cf, v_dataType, rc = rc )
      print *,'ESMF_ConfigGetString: v_dataType= ', v_dataType, &
           ' rc =', rc

      call ESMF_ConfigGetInt ( cf, nv, rc = rc )
      print *,'ESMF_ConfigGetInt: nv =', nv, ' rc =', rc

      call ESMF_ConfigGetFloat ( cf, sigV, nsize=nv, rc=rc )
      print *,'ESMF_ConfigGetFloat: sigV(1,', nv,') =', &
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

      call ESMF_ConfigGetInt ( cf, nu, rc=rc )             ! second token
      print *,'ESMF_ConfigGetInt: nu =', nu, ' rc =', rc

      call ESMF_ConfigGetFloat ( cf, sigU, nsize=6, rc=rc ) ! tokens 3 thru 8 
      print *,'ESMF_ConfigGetFloat: sigU(1,6) =', sigU(1:6), ' rc =', rc

!      Similarly for v

      call ESMF_ConfigNextLine ( cf, rc=rc )
      print *,'ESMF_ConfigNextLine: rc =', rc
      
      call ESMF_ConfigGetString ( cf, v_dataType, rc=rc )
      print *,'ESMF_ConfigGetString: v_dataType= ', v_dataType, ' rc =', rc

      call ESMF_ConfigGetInt ( cf, nv, rc=rc )
      print *,'ESMF_ConfigGetInt: nv =', nv, ' rc =', rc

      call ESMF_ConfigGetFloat ( cf, sigV, nsize=6,rc=rc )
      print *,'ESMF_ConfigGetFloat: sigV(1,6) =', sigV(1:6), ' rc =', rc 


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
            call ESMF_ConfigGetFloat ( cf, plev(line), rc=rc )
            if (rc /= 0) exit
            
            print *,' Line ', line, 'plev =', plev
            
!               Looping over columns
!               --------------------
            do col =1, ncol(line) - 1
               call ESMF_ConfigGetFloat ( cf, temp, rc=rc)
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
!   ------------
      call ESMF_ConfigDestroy ( cf, rc ) 
      print *,'ESMF_ConfigDestroy: rc =', rc
      

    end program ESMF_Config_Test
