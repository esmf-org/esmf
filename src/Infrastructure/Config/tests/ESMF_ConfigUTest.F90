! $ Id: $
!==============================================================================
! Earth System Modeling Framework
!
! Copyright 2002-2003, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!==============================================================================
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

#include <ESMF_Macros.inc>

!USES
      use ESMF_TestMod     ! test methods
      use ESMF_Mod

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ConfigUTest.F90,v 1.8 2004/06/11 17:07:12 svasquez Exp $'
!------------------------------------------------------------------------------

      type (ESMF_Config) cf 
      
      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0


      character(len=255) :: fname = 'ESMF_Resource_File_Sample.rc'
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
 
!-------------------------------------------------------------------------------
! The unit tests are divided into Sanity and Exhaustive. The Sanity tests are
! always run. When the environment variable, EXHAUSTIVE, is set to ON then
! the EXHAUSTIVE and sanity tests both run. If the EXHAUSTIVE variable is set
! to OFF, then only the sanity unit tests.
! Special strings (Non-exhaustive and exhaustive) have been
! added to allow a script to count the number and types of unit tests.
!-------------------------------------------------------------------------------


      call ESMF_Initialize()

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Config Create
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Create Test"
      cf = ESMF_ConfigCreate( rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Config Destroy
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Destroy Test"
      call ESMF_ConfigDestroy( cf, rc ) 
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_EXHAUSTIVE


! Initialization:
!----------------
      call Initialization()
      if (rc /=0) STOP            ! Catastropic Error


! Retrieval of single parameters
!--------------------------------
      call SinglePar()


! Retrieval of a group of parameters on a single line
! ----------------------------------------------------

      call  MultPar_SingleLine_U()

      call  MultPar_SingleLine_v()


! Retrieval of a group of parameters on multiple lines
!   ----------------------------------------------------
      call MultPar_MultLines()


! Retrieval of Tables of unknown length
! ---------------------------------------

      call Table()


! Finalization
! ------------
 
      call Finalization()

! REPORTING
! ------------
      if  (counter_total > 0) then
         if( counter_success == counter_total ) then 
            print *,'ESMF_Config: All tests were successful'
         else
            success_rate = 100.0 * counter_success / counter_total 
            print *,'ESMF_Config: Success rate: ', nint(success_rate),'%' 
         endif
      endif

#endif

      call ESMF_Finalize()



    CONTAINS

!--------------------------------------------------------------------
      subroutine Initialization()
!--------------------------------------------------------------------

        character(ESMF_MAXSTR) :: failMsg
        character(ESMF_MAXSTR) :: name
        integer :: result = 0
        rc = 0

        
        !------------------------------------------------------------------------
        !EX_UTest
        ! Create Config Test
        write(failMsg, *) "Did not return ESMF_SUCCESS"
        write(name, *) "Create Config Test"
        cf = ESMF_ConfigCreate( rc )
	call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
      
        if ( rc /= 0 ) then 
           print *,'ESMF_ConfigCreate: catastrophic error, rc =', rc
           return
        endif

        !------------------------------------------------------------------------
        !EX_UTest
        ! Config Load File Test
        write(failMsg, *) "Did not return ESMF_SUCCESS"
        write(name, *) "Config Load File Test"
        call ESMF_ConfigLoadFile( cf, fname, rc = rc)
	call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

        if (rc == -99) then 
           print *,' ESMF_ConfigLoadFile: Out of memory: exceeded NBUF_MAX'
        endif
        if ( rc /= 0 ) then      
           print *,' ESMF_ConfigLoadFile:  loaded file ', fname, &
                ' catastrophic error, rc = ', rc
           return
        else
           counter_total =counter_total + 1
           counter_success =counter_success + 1         
        endif
      
!!      if ( .NOT. unique ) then
!!         print *,' File contains multiple copies of a label' 
!!      end if

        return
        
      end subroutine Initialization
      
!--------------------------------------------------------------------
     subroutine SinglePar()
!--------------------------------------------------------------------
      integer, parameter   :: nDE_0 = 32      
      real, parameter      :: tau_0 = 14.0
      character, parameter :: restart_file_0 = 'RestartFile123'
      character, parameter   :: answer_0 = 'y'
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0

      rc = 0

!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Int Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Get Attribute Int Test"
     call ESMF_ConfigGetAttribute( cf, nDE, label ='Number_of_DEs:', & 
           default=7, rc = rc )
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''
      
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetInt got nDE =', nDE,' rc =', rc
      else
         if (nDE == nDE_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetInt ERROR: got nDE =', nDE, &
              ' should be ', nDE_0
         endif
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Verification Test
     write(failMsg, *) "Attribute integer value is incorrect"
     write(name, *) "Verify Attribute Value Test"
     call ESMF_Test((nDE.eq.NDE_0), name, failMsg, result, ESMF_SRCLINE)


! Floating point

      rc = 0
!''''''''''''''''''''''''''''
   
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Float Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Get Attribute Float Test"
      call ESMF_ConfigGetAttribute(cf, tau, &
           label = 'Relaxation_time_scale_in_days:', rc = rc)
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''
   
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetFloat got tau =', tau,' rc =', rc
      else
         if (tau == tau_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetFloat ERROR: got tau =', tau, &
              ' should be ', tau_0
         endif
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute FLoat Verification Test
     write(failMsg, *) "Attribute float value is incorrect"
     write(name, *) "Verify Attribute Float Value Test"
     call ESMF_Test((tau.eq.tau_0), name, failMsg, result, ESMF_SRCLINE)


! Character

      rc = 0
!''''''''''''''''''''''''''''
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Char Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Get Attribute Char Test"
     call ESMF_ConfigGetChar( cf, answer, 'Do_you_want_quality_control:', &
                                    rc = rc )
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetChar got answer =', answer,' rc =', rc
      else
         if (answer == answer_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetChar ERROR: got answer =', answer, &
              ' should be ', answer_0
         endif
      endif
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Char Verification Test
     write(failMsg, *) "Attribute char value is incorrect"
     write(name, *) "Verify Attribute Char Value Test"
     call ESMF_Test((answer.eq.answer_0), name, failMsg, result, ESMF_SRCLINE)


! String

     rc = 0
!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Get Attribute String Test"
     call ESMF_ConfigGetAttribute( cf, restart_file ,'restart_file_name:', &
           rc = rc )
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetString got =', restart_file,' rc =', rc
      else
         if (answer == answer_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetString ERROR: got  =', restart_file, &
              ' should be ', restart_file_0
         endif
      endif
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Verification Test
     write(failMsg, *) "Attribute char value is incorrect"
     write(name, *) "Verify Attribute String Value Test"
     call ESMF_Test((answer.eq.answer_0), name, failMsg, result, ESMF_SRCLINE)

    end subroutine SinglePar




!--------------------------------------------------------------------
    subroutine MultPar_SingleLine_U()
!--------------------------------------------------------------------
      character(len=12), parameter :: u_dataType_0 = 'u_UprAir'
      integer, parameter   :: nu_0 = 6
      real, dimension(nu_0), parameter :: sigU_0 = &
           (/ 2.0, 2.0, 2.2, 2.3, 2.7, 3.2 /)
 
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0

      rc = 0
 
!''''''''''''''''''''''''''''
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Find Label Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Find Label Test"
     call ESMF_ConfigFindLabel( cf, 'u-wind_error:', rc ) ! identifies label
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, label = u-wind_error:, rc =', rc 
         return
      endif

!''''''''''''''''''''''''''''
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Get String Test"
     call ESMF_ConfigGetAttribute( cf, u_dataType, rc =rc )  ! first token   
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetString failed, rc =', rc
         return
      endif

      if(u_dataType ==  u_dataType_0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetString ERROR: got  =', u_dataType, &
              ' should be ', u_dataType_0
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Verification Test
     write(failMsg, *) "Attribute String value is incorrect"
     write(name, *) "Verify Attribute String Value Test"
     call ESMF_Test((u_dataType.eq.u_dataType_0), name, failMsg, result, ESMF_SRCLINE)


!''''''''''''''''''''''''''''
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Int Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Get Int Test"
     call ESMF_ConfigGetAttribute( cf, nu, rc = rc )            ! second token
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetInt failed, rc =', rc
         return
      endif

      if( nu == nu_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetInt ERROR: got  =', nu, &
              ' should be ', nu_0 
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Int Verification Test
     write(failMsg, *) "Attribute String value is incorrect"
     write(name, *) "Verify Attribute Int Value Test"
     call ESMF_Test((nu.eq.nu_0), name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Floats Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Get Floats Test"
     call ESMF_ConfigGetAttribute( cf, sigU, nu,  rc=rc )     ! tokens 3 thru 8
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetFloats failed, rc =', rc
         return
      endif

      if( any(sigU /= sigU_0) ) then
         print *,'ESMF_ConfigGetInt ERROR: got sigU =', sigU(1:nu), &
              ' should be sigU =', sigU_0(1:nu) 
         return
      else
         counter_success =counter_success + 1
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Floats Verification Test
     write(failMsg, *) "Attribute Floats values are incorrect"
     write(name, *) "Verify Attribute Floats Values Test"
     call ESMF_Test((all(sigU.eq.sigU_0)), name, failMsg, result, ESMF_SRCLINE)

    end subroutine MultPar_SingleLine_U


!--------------------------------------------------------------------
subroutine MultPar_SingleLine_V
!--------------------------------------------------------------------
      character(len=12), parameter :: v_dataType_0 = 'v_UprAir'
      integer, parameter   :: nv_0 = 6
      real, dimension(nv_0), parameter :: sigV_0 = &
           (/ 2.2, 2.2, 2.3, 2.7, 3.2, 3.4 /)
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0

      rc = 0

!''''''''''''''''''''''''''''
      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Find Label Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Find Label Test"
      call ESMF_ConfigFindLabel( cf, 'v-wind_error:', rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, label = v-wind_error:, rc =', rc
         return        
      endif

!''''''''''''''''''''''''''''
      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get String Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get String Test"
      call ESMF_ConfigGetAttribute( cf, v_dataType, rc = rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetString failed, rc =', rc
         return
      endif

      if(v_dataType ==  v_dataType_0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetString ERROR: got  =', v_dataType, &
              ' should be ', v_dataType_0
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Verification Test
     write(failMsg, *) "Attribute String values are incorrect"
     write(name, *) "Verify Attribute String Values Test"
     call ESMF_Test((v_dataType.eq.v_dataType_0), name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''

      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Int Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Int Test"
      call ESMF_ConfigGetAttribute( cf, nv, rc = rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetInt failed, rc =', rc
         return
      endif

      if( nv == nv_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetInt ERROR: got  =', nv, &
              ' should be ', nv_0 
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Int Verification Test
     write(failMsg, *) "Attribute Int values are incorrect"
     write(name, *) "Verify Attribute Int Values Test"
     call ESMF_Test((nv.eq.nv_0), name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''

      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Floats Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Floats Test"
      call ESMF_ConfigGetAttribute( cf, sigV, nsize=nv, rc=rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetFloats failed, rc =', rc
         return
      endif

      if( any(sigV /= sigV_0) ) then
         print *,'ESMF_ConfigGetInt ERROR: got sigV =', sigV(1:nv), &
              ' should be sigV =', sigV_0(1:nv) 
         return
      else
        counter_success =counter_success + 1
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Floats Verification Test
     write(failMsg, *) "Attribute Floats values are incorrect"
     write(name, *) "Verify Attribute Floats Values Test"
     call ESMF_Test((all(sigV.eq.sigV_0)), name, failMsg, result, ESMF_SRCLINE)

    end subroutine MultPar_SingleLine_V

!--------------------------------------------------------------------
    subroutine MultPar_MultLines()
!--------------------------------------------------------------------
      character(len=10), parameter :: u_dataType_1 = 'u_UprAir.u'
      character(len=10), parameter :: v_dataType_1 = 'v_UprAir.u'
      integer, parameter   :: nu_1 = 6
      integer, parameter   :: nv_1 = 6
      real, dimension(nu_1), parameter :: sigU_1 = &
           (/ 2.0, 2.0, 2.2, 2.3, 2.7, 3.2 /)
      real, dimension(nv_1), parameter :: sigV_1 = &
           (/ 2.0, 2.0, 2.2, 2.3, 2.7, 3.2 /)  
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0

      rc = 0

!''''''''''''''''''''''''''''

      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Find Label Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Find Label Test"
      call ESMF_ConfigFindLabel( cf, 'ObsErr*QSCAT::', rc ) ! identify label
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, label = ObsErr*QSCAT::, rc =', rc 
         return        
      endif

!''''''''''''''''''''''''''''

      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Next Line Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Next Line Test"
      call ESMF_ConfigNextLine( cf, rc=rc )               ! move down 1 line
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigNextLine failed, rc =', rc 
         return        
      endif

!''''''''''''''''''''''''''''

      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get String Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get String Test"
      call ESMF_ConfigGetAttribute( cf, u_dataType, rc=rc )  ! first token
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetString failed, rc =', rc
         return
      endif

      if(u_dataType ==  u_dataType_1) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetString ERROR: got  =', u_dataType, &
              ' should be ', u_dataType_1
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Verification Test
     write(failMsg, *) "Attribute String values are incorrect"
     write(name, *) "Verify Attribute String Values Test"
     call ESMF_Test((u_dataType.eq.u_dataType_1), name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Int Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Int Test"
      call ESMF_ConfigGetAttribute( cf, nu, rc=rc )              ! second token
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetInt failed, rc =', rc
         return
      endif

      if( nu == nu_1 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetInt ERROR: got  =', nu, &
              ' should be ', nu_1 
         return
      endif

!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Floats Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Floats Test"
      call ESMF_ConfigGetAttribute( cf, sigU, nsize=6, rc=rc ) ! tokens 3 thru 8
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

     counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetFloats failed, rc =', rc
         return
      endif

      if( any(sigU /= sigU_1) ) then
         print *,'ESMF_ConfigGetInt ERROR: got sigU =', sigU(1:nu), &
              ' should be sigU =', sigU_1(1:nu_1) 
         return
      else
        counter_success =counter_success + 1
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Floats Verification Test
     write(failMsg, *) "Attribute Floats values are incorrect"
     write(name, *) "Verify Attribute Floats Values Test"
     call ESMF_Test((all(sigU.eq.sigU_1)), name, failMsg, result, ESMF_SRCLINE)


!      Similarly for v
!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Next LIne Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Next Line Test"
      call ESMF_ConfigNextLine( cf, rc=rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigNextLine failed, rc =', rc 
         return        
      endif

!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get STring Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get String Test"
      call ESMF_ConfigGetAttribute( cf, v_dataType, rc=rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetString failed, rc =', rc
         return
      endif

      if(v_dataType ==  v_dataType_1) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetString ERROR: got  =', v_dataType, &
              ' should be ', v_dataType_1
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Verification Test
     write(failMsg, *) "Attribute String values are incorrect"
     write(name, *) "Verify Attribute String Values Test"
     call ESMF_Test((v_dataType.eq.v_dataType_1), name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''
     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Int Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Int Test"
      call ESMF_ConfigGetAttribute( cf, nv, rc=rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetInt failed, rc =', rc
         return
      endif

      if( nv == nv_1 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetInt ERROR: got  =', nv, &
              ' should be ', nv_1 
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Int Verification Test
     write(failMsg, *) "Attribute Int values are incorrect"
     write(name, *) "Verify Attribute Int Values Test"
     call ESMF_Test((nv.eq.nv_1), name, failMsg, result, ESMF_SRCLINE)


!''''''''''''''''''''''''''''
     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Floats Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Floats Test"
      call ESMF_ConfigGetAttribute( cf, sigV, nsize=6,rc=rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

     counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetFloats failed, rc =', rc
         return
      endif

      if( any(sigV /= sigV_1) ) then
         print *,'ESMF_ConfigGetInt ERROR: got sigV =', sigV(1:nv), &
              ' should be sigV =', sigV_1(1:nv_1) 
         return
      else
        counter_success =counter_success + 1
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Floats Verification Test
     write(failMsg, *) "Attribute Floats values are incorrect"
     write(name, *) "Verify Attribute Floats Values Test"
     call ESMF_Test((all(sigV.eq.sigV_1)), name, failMsg, result, ESMF_SRCLINE)

    end subroutine MultPar_MultLines


!--------------------------------------------------------------------
    subroutine Table()
!--------------------------------------------------------------------
      integer, parameter   :: nlines_0 = 11
      integer, dimension(nlines_0), parameter :: ncol_0 = &
           (/ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11/)
      real, dimension(nlines_0), parameter ::plev_0 = &
           (/1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100/)
      real, dimension(nlines_0, nlines_0) :: vCorr_0
!!!      real :: vCorr_aux(121)
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0
     
      vCorr_0 = RESHAPE (  (/ &
           1.00, 0., 0., 0., 0., 0., 0.,0., 0., 0., 0., &
           0.84, 1.00, 0., 0., 0., 0.,0., 0.,0., 0., 0., &
           0.68, 0.84, 1.00, 0., 0., 0., 0.,0., 0.,0., 0., & 
           0.53, 0.67, 0.81, 1.00, 0., 0., 0., 0.,0., 0.,0., & 
           0.35, 0.46, 0.56, 0.81, 1.00, 0., 0., 0., 0.,0., 0., & 
           0.27, 0.35, 0.44, 0.64, 0.79, 1.00, 0., 0., 0., 0.,0., & 
           0.18, 0.25, 0.32, 0.46, 0.58, 0.75, 1.00, 0., 0., 0., 0.,& 
           0.13, 0.19, 0.25, 0.38, 0.48, 0.62, 0.83, 1.00,  0., 0., 0., & 
           0.09, 0.14, 0.19, 0.29, 0.38, 0.49, 0.66, 0.80, 1.00, 0., 0., & 
           0.06, 0.09, 0.13, 0.20, 0.28, 0.36, 0.49, 0.59, 0.75, 1.00, 0., & 
           0.00, 0.03, 0.06, 0.10, 0.17, 0.23, 0.32, 0.39, 0.50, 0.75, 1.00  &
           /),  (/11,11/)  )


      
!            Get dimension, label and start getting lines

      rc = 0

!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Dim Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Dim Test"
      call ESMF_ConfigGetDim(cf,'ObsErr*vCor_HH-7::', nlines, col, rc)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''
      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetDim failed, rc =', rc
         return
      endif

      if( nlines == nlines_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetDim ERROR: got  =', nlines, &
              ' should be ', nlines_0 
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Dim Verification Test
     write(failMsg, *) "Attribute Dim values are incorrect"
     write(name, *) "Verify Attribute Dim Values Test"
     call ESMF_Test((nlines.eq.nlines_0), name, failMsg, result, ESMF_SRCLINE)

      
!''''''''''''''''''''''''''''         

     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Find Label Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Find Label Test"
      call ESMF_ConfigFindLabel( cf,'ObsErr*vCor_HH-7::', rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
          print *,'ESMF_ConfigFindLabel failed, label ObsErr*vCor_HH-7::, = rc =', rc 
         return        
      endif

!''''''''''''''''''''''''''''     
         allocate(ncol(1:nlines), STAT= rc)
!''''''''''''''''''''''''''''
      if (rc /= 0) then
         print *,'array allocation failed, rc =', rc
         return
      endif
      
      counter_total =counter_total + 1
!'''''''''''''''''''''''''''' 
      do line = 1, nlines

      call ESMF_ConfigNextLine(cf, rc = rc)
!''''''''''''''''''''''''''''
         if (rc /= 0) then
            print *,'ESMF_ConfigNextLine failed, rc =', rc 
            exit        
         endif
!''''''''''''''''''''''''''''    
      ncol(line) = ESMF_ConfigGetLen(cf, rc = rc) - 1
!''''''''''''''''''''''''''''  
      if (rc /= 0) then
         print *,'ESMF_ConfigGetLen failed, rc =', rc
         exit
      endif
!''''''''''''''''''''''''''''  
      enddo
!''''''''''''''''''''''''''''


     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Next Line and Get Len Verification Test
     write(failMsg, *) "Attribute Line values are incorrect"
     write(name, *) "Verify Attribute Line Values Test"
     call ESMF_Test((all(ncol.eq.ncol_0)), name, failMsg, result, ESMF_SRCLINE)

      if( any(ncol /= ncol_0 )) then
         print *,'ESMF_ConfigGetInt ERROR: got ncol =', ncol(1:nlines), &
              ' should be ncol =', ncol_0(1:nlines_0) 
         return
      else
         counter_success =counter_success + 1
      endif


!            Looping over lines

!''''''''''''''''''''''''''''   
     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Find Label Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Find Label Test"
      call ESMF_ConfigFindLabel( cf,'ObsErr*vCor_HH-7::', rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''
      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, label = ObsErr*vCor_HH-7::, rc =', rc 
         return        
      endif


!''''''''''''''''''''''''''''
      do line = 1, nlines
         call ESMF_ConfigNextLine( cf, end, rc )
!''''''''''''''''''''''''''''

         if (rc /= 0) then
            print *,'ESMF_ConfigNextLine failed, rc =', rc 
            exit        
         endif
            
!               Retrieve pressure level
!               -----------------------
         counter_total =counter_total + 1
!''''''''''''''''''''''''''''
            call ESMF_ConfigGetAttribute( cf, plev(line), rc=rc )
!''''''''''''''''''''''''''''
         if (rc /= 0) then
            print *,'ESMF_ConfigNextLine failed, rc =', rc 
            exit
         endif

         if( plev(line) /= plev_0(line) ) then
            print *,'ESMF_ConfigGetFloat ERROR: got plev =', &
                 plev(line), ' should be plev =',        &
                 plev_0(line) 
            exit
         else
            counter_success =counter_success + 1
         endif
            
!               Looping over columns
!               --------------------
         counter_total =counter_total + 1
!''''''''''''''''''''''''''''
         do col =1, ncol(line)
            call ESMF_ConfigGetAttribute( cf, temp, rc=rc)
            if (rc == 0) then 
               vCorr(line,col) = temp 
            end if
         end do
!''''''''''''''''''''''''''''
         if (rc /= 0) then
            print *,'ESMF_ConfigGetFloat failed, rc =', rc 
            exit        
         endif

         
         do col =1, ncol(line)
            if (vCorr(line, col) /= vCorr_0(col, line)) then
               print *,'ESMF_ConfigGetFloat:  Wrong value in vCorr line =', &
                    line,' col =', col,' VCorr = ', vCorr(col, line), &
               ' should be ', vCorr_0(line, col)
               exit
            endif
         end do
         counter_success = counter_success + 1

!''''''''''''''''''''''''''''    
      end do
!''''''''''''''''''''''''''''
!''''''''''''''''''''''''''''     
      deallocate(ncol, STAT= rc)
!''''''''''''''''''''''''''''
      if (rc /= 0) then
         print *,'array deallocation failed, rc =', rc
      endif


      return
    end subroutine Table


!--------------------------------------------------------------------
    subroutine Finalization()
!--------------------------------------------------------------------

      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0
      rc = 0
!''''''''''''''''''''''''''''
      !------------------------------------------------------------------------
      !NEX_UTest
      ! Test Config Destroy
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Destroy Test"
      call ESMF_ConfigDestroy( cf, rc ) 
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigDestroy failed, rc =', rc 
      endif
      
    end subroutine Finalization

  end program ESMF_Config_Test

