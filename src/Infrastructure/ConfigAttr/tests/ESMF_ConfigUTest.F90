!-------------------------------------------------------------------------
!           NASA Earth System Modeling Framework Project (ESMF)          !
!            NASA/GSFC, Data Assimilation Office, Code 910.3             !
!-------------------------------------------------------------------------
!BOI
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
! implemented in ESMF_ConfigMod.F90.
!
! !REVISION HISTORY:
!
!       7apr2003 Leonid Zaslavsky Created. 
!EOI_______________________________________________________________________

    program ESMF_Config_Test

      use ESMF_ConfigMod
      use  ESMF_temp   

      type(ESMF_Layout) :: layout  
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

      cf = ESMF_ConfigCreate( layout, rc )
      call ESMF_ConfigLoadFile( cf, fname, rc, unique )

      if ( .NOT. unique ) then
         print *,' File contains multiple copies of a label' 
      end if

! Retrieval of single parameters
!--------------------------------

      nDE = ESMF_ConfigGetInt ( cf, rc, label ='Number_of_DEs:', default=7 )
      tau = ESMF_ConfigGetFloat ( cf, rc, &
           label ='Relaxation_time_scale_in_days:')
      answer = ESMF_ConfigGetChar ( cf, 'Do_you_want_quality_control:', rc )
      call ESMF_ConfigGetString ( cf, 'restart_file_name:', fname, rc )


! NOTE: A non-zero rc is returned when an attribute is not found - unless
!      the function is called with an optional default.



! Retrieval of a group of parameters on a single line
! ----------------------------------------------------

      call ESMF_ConfigFindLabel ( cf, 'u-wind-error:', rc ) ! identifies label
      call ESMF_ConfigGetString ( cf, u_dataType, rc )      ! first token
      nu = ESMF_ConfigGetInt ( cf, rc )                     ! seconf token
      sigU(1:nu) = ESMF_ConfigGetFloat ( cf, rc, size=nu )  ! tokens 3 thru 8 

      call ESMF_ConfigFindLabel ( cf, 'v-wind-error:', rc )
      call ESMF_ConfigGetString ( cf, v_dataType, rc )
      nv = ESMF_ConfigGetInt ( cf, rc )
      sigV = ESMF_ConfigGetFloat ( cf, rc, size=nv )
  
! NOTE: Order is not relevant; first label found is returned



! Retrieval of a group of parameters on multiple lines
!   ----------------------------------------------------

      call ESMF_ConfigFindLabel ( cf, 'ObsErr*QSCAT::', rc ) ! identify label

      call ESMF_ConfigNextLine ( cf, rc )                ! move down 1 line
      call ESMF_ConfigGetString ( cf, u_dataType, rc )   ! first token
      nu = ESMF_ConfigGetInt ( cf, rc )                  ! second token
      sigU = ESMF_ConfigGetFloat ( cf, rc, size=6 )      ! tokens 3 thru 8 

!      Similarly for v

      call ESMF_ConfigNextLine ( cf, rc )
      call ESMF_ConfigGetString ( cf, v_dataType, rc )
      nv = ESMF_ConfigGetInt ( cf, rc )
      sigV = ESMF_ConfigGetFloat ( cf, rc, size=6 )



! Retrieval of Tables of unknown length
! ---------------------------------------


!            Get label and start getting lines

      call ESMF_ConfigFindLabel ( cf,'ObsErr*vCor_HH-7::', rc )

!            Looping over lines

      end = .false.
      line = 0
      do while ( (line < MAXLEV) . and. (rc == 0) )
        call ESMF_ConfigNextLine( cf, rc, end )
        if (.NOT. end ) then
           line = line + 1 

!               Retrieve pressure level
!               -----------------------
           plev(line) = ESMF_ConfigGetFloat ( cf, rc )
           if (rc /= 0) exit
           
!               Looping over columns
!               --------------------
           column = 0
           do while ((column < MAXLEV) .and. ( rc == 0) )
              temp = ESMF_ConfigGetFloat ( cf, rc)
              if (rc == 0) then 
                 column = column + 1
                 vCorr(line,column) = temp 
              end if
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
