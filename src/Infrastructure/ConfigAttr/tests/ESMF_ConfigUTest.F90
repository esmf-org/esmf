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
! implemented in ESMF\_ConfigMod.F90..
!
! !REVISION HISTORY:
!
!       7apr2003 Leonid Zaslavsky Created. 
!------------------------------------------------------------------------

    program ESMF_Config_Test

      use  ESMF_DELayoutMod
      use ESMF_ConfigMod
!!!      use  ESMF_temp   

      type(ESMF_DELayout) :: layout  
      type (ESMF_Config) cf 
      
      character(len=255) :: fname
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
      integer :: line, column
      logical :: end
      real temp


! Initialization:
!----------------

      cf = ESMF_ConfigCreate( rc )
      call ESMF_ConfigLoadFile( cf, fname, rc = rc)

!!      if ( .NOT. unique ) then
!!         print *,' File contains multiple copies of a label' 
!!      end if

! Retrieval of single parameters
!--------------------------------

      nDE = ESMF_ConfigGetInt ( cf, label ='Number_of_DEs:', default=7, &
           rc = rc )
      tau = ESMF_ConfigGetFloat ( cf, &
           label ='Relaxation_time_scale_in_days:', rc = rc)
      answer = ESMF_ConfigGetChar ( cf, 'Do_you_want_quality_control:', &
                                    rc = rc )
      call ESMF_ConfigGetString ( cf, fname ,'restart_file_name:', rc = rc )


! NOTE: A non-zero rc is returned when an attribute is not found - unless
!      the function is called with an optional default.



! Retrieval of a group of parameters on a single line
! ----------------------------------------------------

      call ESMF_ConfigFindLabel ( cf, 'u-wind-error:', rc ) ! identifies label
      call ESMF_ConfigGetString ( cf, u_dataType, rc =rc )  ! first token
      nu = ESMF_ConfigGetInt ( cf, rc = rc )                ! seconf token
      call ESMF_ConfigGetFloat ( cf,  array=sigU, nsize=nu,  rc=rc )  
                                                            ! tokens 3 thru 8 

      call ESMF_ConfigFindLabel ( cf, 'v-wind-error:', rc )
      call ESMF_ConfigGetString ( cf, v_dataType, rc = rc )
      nv = ESMF_ConfigGetInt ( cf, rc = rc )
      call ESMF_ConfigGetFloat ( cf, sigV, nsize=nv, rc=rc )
  
! NOTE: Order is not relevant; first label found is returned



! Retrieval of a group of parameters on multiple lines
!   ----------------------------------------------------

      call ESMF_ConfigFindLabel ( cf, 'ObsErr*QSCAT::', rc ) ! identify label

      call ESMF_ConfigNextLine ( cf, rc=rc )               ! move down 1 line
      call ESMF_ConfigGetString ( cf, u_dataType, rc=rc )  ! first token
      nu = ESMF_ConfigGetInt ( cf, rc=rc )                 ! second token
      call ESMF_ConfigGetFloat ( cf, sigU, nsize=6, rc=rc ) ! tokens 3 thru 8 

!      Similarly for v

      call ESMF_ConfigNextLine ( cf, rc=rc )
      call ESMF_ConfigGetString ( cf, v_dataType, rc=rc )
      nv = ESMF_ConfigGetInt ( cf, rc=rc )
      call ESMF_ConfigGetFloat ( cf, sigV, nsize=6,rc=rc )



! Retrieval of Tables of unknown length
! ---------------------------------------


!            Get label and start getting lines

      call ESMF_ConfigFindLabel ( cf,'ObsErr*vCor_HH-7::', rc )

!            Looping over lines

      end = .false.
      line = 0
      do
        call ESMF_ConfigNextLine( cf, end, rc )
        if (.NOT. end ) then
           line = line + 1 

!               Retrieve pressure level
!               -----------------------
           plev(line) = ESMF_ConfigGetFloat ( cf, rc=rc )
           if (rc /= 0) exit
           
!               Looping over columns
!               --------------------
           column = 0
           do while ((column < MAXLEV) .and. ( rc == 0) )
              temp = ESMF_ConfigGetFloat ( cf, rc=rc)
              if (rc == 0) then 
                 column = column + 1
                 vCorr(line,column) = temp 
              end if
!!              if (line >= MAXLEV) exit
!!              if (rc /= 0) exit
           end do
           if (rc == EOL) rc = 0
                

!       ------------------------------------`
        else
           exit       ! End of table reached (that is, "::")
        end if
     end do


! Finalization
!   ------------

     call ESMF_ConfigDestroy ( cf, rc ) 

   end program ESMF_Config_Test
