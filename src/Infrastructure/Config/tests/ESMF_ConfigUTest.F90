! $Id: ESMF_ConfigUTest.F90,v 1.31.2.4 2009/01/21 21:25:20 cdeluca Exp $
!==============================================================================
! Earth System Modeling Framework
!
! Copyright 2002-2009, University Corporation for Atmospheric Research, 
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics 
! Laboratory, University of Michigan, National Centers for Environmental 
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory, 
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
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

#include "ESMF.h"


module config_subrs

        use ESMF_TestMod
        use ESMF_Mod
        implicit none

        public

      type (ESMF_Config), save :: cf, cf1
      
      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0
   


      character(len=255) :: fname = 'ESMF_Resource_File_Sample.rc'
      character(len=255) :: restart_file
      integer :: rc, npets
      logical :: unique
      integer   :: nDE
      real(ESMF_KIND_R4) :: tau
      logical   :: optimize
      logical   :: Doing_QC
      character(len=1)   :: answer
      character(len=10) :: u_dataType, v_dataType, vf_dataType
      integer           :: nu, nv
      real(ESMF_KIND_R4) :: sigU(6), sigV(6)
      logical           :: sigVf(6)
      integer, parameter :: MAXLEV = 100, EOL = 111
      real(ESMF_KIND_R4) :: plev(MAXLEV), vCorr(MAXLEV, MAXLEV)
      integer :: nlev
      integer :: line, col, nlines
      integer, allocatable, dimension(:) :: ncol
      logical :: end
      real(ESMF_KIND_R4) temp
      type(ESMF_VM),save:: vm
      
      integer :: counter_total, counter_success
      integer :: rc_opening
      real(ESMF_KIND_R4) :: success_rate

        contains
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
        write(failMsg, *) "Did not return ESMF_RC_DUP_NAME"
        write(name, *) "Config Load File Test"
        call ESMF_ConfigLoadFile( cf, fname, unique = .true., rc = rc)
	call ESMF_Test((rc.eq.ESMF_RC_DUP_NAME), name, failMsg, result, ESMF_SRCLINE)

        if (rc == -99) then 
           print *,' ESMF_ConfigLoadFile: Out of memory: exceeded NBUF_MAX'
        endif
        if ( rc /= ESMF_RC_DUP_NAME ) then      
           print *,' ESMF_ConfigLoadFile:  loaded file ', fname, &
                ' catastrophic error, rc = ', rc
           return
        else
           counter_total =counter_total + 1
           counter_success =counter_success + 1         
           print *,' File contains duplicate labels - check logfile.' 
        endif


        return
        
      end subroutine Initialization
      
!--------------------------------------------------------------------
     subroutine SinglePar()
!--------------------------------------------------------------------
      integer, parameter   :: nDE_0 = 32      
      real(ESMF_KIND_R4), parameter      :: tau_0 = 14.0
      character, parameter :: restart_file_0 = 'RestartFile123'
      character, parameter   :: answer_0 = 'y'
      logical, parameter     :: optimize_0 = .false.
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0

      rc = 0

!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
     !EX_UTest
     ! Non initialized Config Get Attribute Int Test
     write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
     write(name, *) "Non initialized Config Get Attribute Int Test"
     call ESMF_ConfigGetAttribute( cf1, nDE, label ='Number_of_DEs:', & 
           default=7, rc = rc )
     call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

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
         print *,'ESMF_ConfigGetAttribute(int) got nDE =', nDE,' rc =', rc
      else
         if (nDE == nDE_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetAttribute(int) ERROR: got nDE =', nDE, &
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
         print *,'ESMF_ConfigGetAttribute(float) got tau =', tau,' rc =', rc
      else
         if (tau == tau_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetAttribute(float) ERROR: got tau =', tau, &
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
     ! Non-created Config Get Attribute Char Test
     write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
     write(name, *) "Config Get Attribute Char Test"
     call ESMF_ConfigGetChar( cf1, answer, 'Do_you_want_quality_control:', &
                                    rc = rc )
     call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

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
         print *,'ESMF_ConfigGetAttribute(char) got answer =', &
                 answer,' rc =', rc
      else
         if (answer == answer_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetAttribute(char) ERROR: got answer =', &
                     answer, ' should be ', answer_0
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
         print *,'ESMF_ConfigGetAttribute(string) got =', &
                  restart_file,' rc =', rc
      else
         if (answer == answer_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetAttribute(string) ERROR: got  =', &
                     restart_file, ' should be ', restart_file_0
         endif
      endif
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Verification Test
     write(failMsg, *) "Attribute char value is incorrect"
     write(name, *) "Verify Attribute String Value Test"
     call ESMF_Test((answer.eq.answer_0), name, failMsg, result, ESMF_SRCLINE)

! Logical

      rc = 0
!''''''''''''''''''''''''''''
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Logical Test
     write(failMsg, *) "Did not return ESMF_SUCCESS"
     write(name, *) "Config Get Attribute Logical Test"
     call ESMF_ConfigGetAttribute( cf, optimize, 'Optimization:', rc = rc )
     call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigGetAttribute(logical) got optimize = ', &
                 optimize,' rc =', rc
      else
         if (optimize .eqv. optimize_0) then
            counter_success =counter_success + 1
         else
            print *,'ESMF_ConfigGetAttribute(logical) ERROR: got optimize =', &
                     optimize, ' should be ', optimize_0
         endif
      endif
     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Logical Verification Test
     write(failMsg, *) "Attribute logical value is incorrect"
     write(name, *) "Verify Attribute Logical Value Test"
     call ESMF_Test((optimize.eqv.optimize_0), name, failMsg, result, &
                     ESMF_SRCLINE)


    end subroutine SinglePar



!--------------------------------------------------------------------
    subroutine MultPar_SingleLine_U()
!--------------------------------------------------------------------
      character(len=12), parameter :: u_dataType_0 = 'u_UprAir'
      integer, parameter   :: nu_0 = 6
      real(ESMF_KIND_R4), dimension(nu_0), parameter :: sigU_0 = &
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
         print *,'ESMF_ConfigGetAttribute(string) failed, rc =', rc
         return
      endif

      if(u_dataType ==  u_dataType_0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(string) ERROR: got  =', &
                 u_dataType, ' should be ', u_dataType_0
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
         print *,'ESMF_ConfigGetAttribute(int) failed, rc =', rc
         return
      endif

      if( nu == nu_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(int) ERROR: got  =', nu, &
              ' should be ', nu_0 
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Int Verification Test
     write(failMsg, *) "Attribute Int value is incorrect"
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
         print *,'ESMF_ConfigGetAttribute(floats) failed, rc =', rc
         return
      endif

      if( any(sigU /= sigU_0) ) then
         print *,'ESMF_ConfigGetAttribute(floats) ERROR: got sigU =', &
                  sigU(1:nu), ' should be sigU =', sigU_0(1:nu) 
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
      real(ESMF_KIND_R4), dimension(nv_0), parameter :: sigV_0 = &
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
         print *,'ESMF_ConfigGetAttribute(string) failed, rc =', rc
         return
      endif

      if(v_dataType ==  v_dataType_0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(string) ERROR: got  =', v_dataType, &
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
         print *,'ESMF_ConfigGetAttribute(int) failed, rc =', rc
         return
      endif

      if( nv == nv_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(int) ERROR: got  =', nv, &
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
      call ESMF_ConfigGetAttribute( cf, sigV, count=nv, rc=rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetAttribute(floats) failed, rc =', rc
         return
      endif

      if( any(sigV /= sigV_0) ) then
         print *,'ESMF_ConfigGetAttribute(floats) ERROR: got sigV =', &
                 sigV(1:nv), ' should be sigV =', sigV_0(1:nv) 
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
subroutine MultPar_SingleLine_Vf
!--------------------------------------------------------------------
!  array of logicals
      character(len=6), parameter :: vf_dataType_0 = 'v_Flag'
      integer, parameter   :: nv_0 = 6
      logical, dimension(nv_0), parameter :: sigVf_0 = &
           (/ .true., .false., .true., .true., .false., .false. /)
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0

      rc = 0

!''''''''''''''''''''''''''''
      !------------------------------------------------------------------------
      !EX_UTest
      ! Non-created Config Find Label Test
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      write(name, *) "Non-create Config Find Label Test"
      call ESMF_ConfigFindLabel( cf1, 'v-wind_flag:', rc )
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Find Label Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Find Label Test"
      call ESMF_ConfigFindLabel( cf, 'v-wind_flag:', rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc == 0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigFindLabel failed, label = v-wind_flag:, rc =', rc
         return        
      endif

!''''''''''''''''''''''''''''
      !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get String Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get String Test"
      call ESMF_ConfigGetAttribute( cf, vf_dataType, rc = rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetAttribute(string) failed, rc =', rc
         return
      endif

      if(vf_dataType ==  vf_dataType_0) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(string) ERROR: got  =', vf_dataType, &
              ' should be ', vf_dataType_0
         return
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute String Verification Test
     write(failMsg, *) "Attribute String values are incorrect"
     write(name, *) "Verify Attribute String Values Test"
     call ESMF_Test((vf_dataType.eq.vf_dataType_0), name, failMsg, result, ESMF_SRCLINE)

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
         print *,'ESMF_ConfigGetAttribute(int) failed, rc =', rc
         return
      endif

      if( nv == nv_0 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(int) ERROR: got  =', nv, &
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
      ! Config Get Logicals Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Logicals Test"
      call ESMF_ConfigGetAttribute( cf, sigVf, count=nv_0, rc=rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetAttribute(logicals) failed, rc =', rc
         return
      endif

      if( any(sigVf .neqv. sigVf_0) ) then
         print *,'ESMF_ConfigGetAttribute(logicals) ERROR: got sigVf =', &
                 sigVf(1:nv), ' should be sigVf =', sigVf_0(1:nv) 
         return
      else
        counter_success =counter_success + 1
      endif

     !------------------------------------------------------------------------
     !EX_UTest
     ! Config Get Attribute Logicals Verification Test
     write(failMsg, *) "Attribute Logicals values are incorrect"
     write(name, *) "Verify Attribute Logicals Values Test"
     call ESMF_Test((all(sigVf.eqv.sigVf_0)), name, failMsg, result, &
                     ESMF_SRCLINE)

    end subroutine MultPar_SingleLine_Vf

!--------------------------------------------------------------------
    subroutine MultPar_MultLines()
!--------------------------------------------------------------------
      character(len=10), parameter :: u_dataType_1 = 'u_UprAir.u'
      character(len=10), parameter :: v_dataType_1 = 'v_UprAir.u'
      integer, parameter   :: nu_1 = 6
      integer, parameter   :: nv_1 = 6
      real(ESMF_KIND_R4), dimension(nu_1), parameter :: sigU_1 = &
           (/ 2.0, 2.0, 2.2, 2.3, 2.7, 3.2 /)
      real(ESMF_KIND_R4), dimension(nv_1), parameter :: sigV_1 = &
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
         print *,'ESMF_ConfigGetAttribute(string) failed, rc =', rc
         return
      endif

      if(u_dataType ==  u_dataType_1) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(string) ERROR: got  =', &
                 u_dataType, ' should be ', u_dataType_1
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
         print *,'ESMF_ConfigGetAttribute(int) failed, rc =', rc
         return
      endif

      if( nu == nu_1 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(int) ERROR: got  =', nu, &
              ' should be ', nu_1 
         return
      endif

!''''''''''''''''''''''''''''

     !------------------------------------------------------------------------
      !EX_UTest
      ! Config Get Floats Test
      write(failMsg, *) "Did not return ESMF_SUCCESS"
      write(name, *) "Config Get Floats Test"
      call ESMF_ConfigGetAttribute( cf, sigU, count=6, rc=rc ) ! tokens 3 thru 8
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

     counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetAttribute(floats) failed, rc =', rc
         return
      endif

      if( any(sigU /= sigU_1) ) then
         print *,'ESMF_ConfigGetAttribute(floats) ERROR: got sigU =', &
                  sigU(1:nu), ' should be sigU =', sigU_1(1:nu_1) 
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
         print *,'ESMF_ConfigGetAttribute(string) failed, rc =', rc
         return
      endif

      if(v_dataType ==  v_dataType_1) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(string) ERROR: got  =', &
                  v_dataType, ' should be ', v_dataType_1
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
         print *,'ESMF_ConfigGetAttribute(int) failed, rc =', rc
         return
      endif

      if( nv == nv_1 ) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigGetAttribute(int) ERROR: got  =', nv, &
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
      call ESMF_ConfigGetAttribute( cf, sigV, count=6,rc=rc )
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

     counter_total =counter_total + 1
      if (rc /= 0) then
         print *,'ESMF_ConfigGetAttribute(floats) failed, rc =', rc
         return
      endif

      if( any(sigV /= sigV_1) ) then
         print *,'ESMF_ConfigGetAttribute(floats) ERROR: got sigV =', &
                 sigV(1:nv), ' should be sigV =', sigV_1(1:nv_1) 
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
      real(ESMF_KIND_R4), dimension(nlines_0), parameter ::plev_0 = &
           (/1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100/)
      real(ESMF_KIND_R4), dimension(nlines_0, nlines_0) :: vCorr_0
!!!      real(ESMF_KIND_R4) :: vCorr_aux(121)
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
      call ESMF_ConfigGetDim(cf, nlines, col, label='ObsErr*vCor_HH-7::', rc=rc)
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
            print *,'ESMF_ConfigAttribute failed, rc =', rc 
            exit
         endif

         if( plev(line) /= plev_0(line) ) then
            print *,'ESMF_ConfigGetAttribute(float) ERROR: got plev =', &
                 plev(line), ' should be plev =', plev_0(line) 
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
            print *,'ESMF_ConfigGetAttribute(float) failed, rc =', rc 
            exit        
         endif

         
         do col =1, ncol(line)
            if (vCorr(line, col) /= vCorr_0(col, line)) then
               print *,'ESMF_ConfigGetAttribute(float):  Wrong value in vCorr line =', &
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
    subroutine SingleParSet()
!--------------------------------------------------------------------
      integer :: memberNum, numMembers, numConstituents, numDelegates
      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0

      rc = 0

!''''''''''''''''''''''''''''

     !-----------------------------------------------------------------------
     !EX_UTest
     ! Config Set Attribute Int Test 1: Append to end of config object
     write(failMsg, *) "Did not return Member_Number 20 and ESMF_SUCCESS"
     write(name, *) "Config Set Attribute IntI4 Test 1"
     call ESMF_ConfigSetAttribute(cf, 20, label = 'Member_Number:', rc = rc)
     call ESMF_ConfigGetAttribute(cf, memberNum, &
                                  label = 'Member_Number:', rc = rc)
     call ESMF_Test((memberNum.eq.20 .and. rc.eq.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''
      
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigSetAttribute(intI4) got Member_Num =', memberNum, &
                 ' rc =', rc
      else
        if (memberNum == 20) then
          counter_success =counter_success + 1
        else
          print *,'ESMF_ConfigSetAttribute(intI4) ERROR: got Member_Number =', &
                    memberNum, ' should be 20'
        endif
      endif

     !-----------------------------------------------------------------------
     !EX_UTest
     ! Config Set Attribute Int Test 2:  Overwrite; same number of characters
     write(failMsg, *) "Did not return Number_of_Members 40 and ESMF_SUCCESS"
     write(name, *) "Config Set Attribute IntI4 Test 2"
     call ESMF_ConfigSetAttribute(cf, 40, label = 'Number_of_Members:', rc = rc)
     call ESMF_ConfigGetAttribute(cf, numMembers, &
                                  label = 'Number_of_Members:', rc = rc)
     call ESMF_Test((numMembers.eq.40 .and. rc.eq.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''
      
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigSetAttribute(intI4) got numMembers=', numMembers, &
                 ' rc =', rc
      else
        if (numMembers == 40) then
          counter_success =counter_success + 1
        else
          print *,'ESMF_ConfigSetAttribute(intI4) ERROR: got numMembers= ', &
                    numMembers, ' should be 40'
        endif
      endif

     !-----------------------------------------------------------------------
     !EX_UTest
     ! Config Set Attribute Int Test 3: Overwrite; insert 1 extra character
     write(failMsg, *) &
           "Did not return Number_of_Constituents 123 and ESMF_SUCCESS"
     write(name, *) "Config Set Attribute IntI4 Test 3"
     call ESMF_ConfigSetAttribute(cf, 123, label = 'Number_of_Constituents:', &
                                  rc = rc)
     call ESMF_ConfigGetAttribute(cf, numConstituents, &
                                  label = 'Number_of_Constituents:', rc = rc)
     call ESMF_Test((numConstituents.eq.123 .and. rc.eq.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''
      
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *,'ESMF_ConfigSetAttribute(intI4) got numConstituents=', &
                  numConstituents, ' rc =', rc
      else
        if (numConstituents == 123) then
          counter_success =counter_success + 1
        else
          print *, &
              'ESMF_ConfigSetAttribute(intI4) ERROR: got numConstituents= ', &
                    numConstituents, ' should be 123'
        endif
      endif

     !-----------------------------------------------------------------------
     !EX_UTest
     ! Config Set Attribute Int Test 4:  Overwrite; delete 1 extra character
     write(failMsg, *) "Did not return Number_of_Delegates 5 and ESMF_SUCCESS"
     write(name, *) "Config Set Attribute IntI4 Test 4"
     call ESMF_ConfigSetAttribute(cf, 5, label = 'Number_of_Delegates:', &
                                  rc = rc)
     call ESMF_ConfigGetAttribute(cf, numDelegates, &
                                  label = 'Number_of_Delegates:', rc = rc)
     call ESMF_Test((numDelegates.eq.5 .and. rc.eq.ESMF_SUCCESS), &
                     name, failMsg, result, ESMF_SRCLINE)

!''''''''''''''''''''''''''''
      
      counter_total =counter_total + 1
      if ( rc /= 0 ) then      
         print *, 'ESMF_ConfigSetAttribute(intI4) got numDelegates=', &
                   numDelegates, ' rc =', rc
      else
        if (numDelegates == 5) then
          counter_success =counter_success + 1
        else
          print *,'ESMF_ConfigSetAttribute(intI4) ERROR: got numDelegates= ', &
                    numDelegates, ' should be 5'
        endif
      endif

    end subroutine SingleParSet

!--------------------------------------------------------------------
    subroutine Finalization()
!--------------------------------------------------------------------

      character(ESMF_MAXSTR) :: failMsg
      character(ESMF_MAXSTR) :: name
      integer :: result = 0
      rc = 0
!''''''''''''''''''''''''''''
      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Config Validate
      write(failMsg, *) "Did not return ESMF_RC_ATTR_UNUSED"
      write(name, *) "Config Validate Test"
      call ESMF_ConfigValidate( cf, "unusedAttributes", rc)
      call ESMF_Test((rc.eq.ESMF_RC_ATTR_UNUSED), name, failMsg, result, ESMF_SRCLINE)
!''''''''''''''''''''''''''''

      counter_total =counter_total + 1
      if (rc == ESMF_RC_ATTR_UNUSED) then
         counter_success =counter_success + 1
      else
         print *,'ESMF_ConfigValidate failed, rc =', rc 
      endif

!''''''''''''''''''''''''''''
      !------------------------------------------------------------------------
      !EX_UTest
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

end module config_subrs



    program ESMF_Config_Test


!USES
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      use config_subrs

      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_ConfigUTest.F90,v 1.31.2.4 2009/01/21 21:25:20 cdeluca Exp $'
!------------------------------------------------------------------------------

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


      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

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

#ifdef ESMF_TESTEXHAUSTIVE

      !------------------------------------------------------------------------
      !EX_UTest
      ! Test Config Destroy of a destroyed Config
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Destroy a destroyed Config Test"
      call ESMF_ConfigDestroy( cf, rc ) 
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

     !------------------------------------------------------------------------
     !EX_UTest
     ! Destroyed Config Get Attribute Int Test
     write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
     write(name, *) "Destroyed Config Get Attribute Int Test"
     call ESMF_ConfigGetAttribute( cf, nDE, label ='Number_of_DEs:', &
           default=7, rc = rc )
     call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

     !------------------------------------------------------------------------
     !EX_UTest
     ! Destroyed Config Get Attribute Char Test
     write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
     write(name, *) "Destroyed Config Get Attribute Char Test"
     call ESMF_ConfigGetChar( cf, answer, 'Do_you_want_quality_control:', &
                                    rc = rc )
     call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

     !------------------------------------------------------------------------
     !EX_UTest
     ! Destroyed Config Find Label Test
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      write(name, *) "Non-create Config Find Label Test"
      call ESMF_ConfigFindLabel( cf, 'v-wind_flag:', rc )
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)




! Initialization:
!----------------
      call Initialization()
      if (rc /= ESMF_RC_DUP_NAME) then
        call ESMF_TestEnd(result, ESMF_SRCLINE)
        STOP            ! Catastropic Error
      endif

! Retrieval of single parameters
!--------------------------------
      call SinglePar()


! Retrieval of a group of parameters on a single line
! ----------------------------------------------------

      call  MultPar_SingleLine_U()
      call  MultPar_SingleLine_V()
      call  MultPar_SingleLine_Vf()


! Retrieval of a group of parameters on multiple lines
!   ----------------------------------------------------
      call MultPar_MultLines()


! Retrieval of Tables of unknown length
! ---------------------------------------

      call Table()

!
! Setting of single parameters
! ------------------------------
      call SingleParSet()


! Finalization
! ------------
 
      call Finalization()

! REPORTING
! ------------
      
      !EX_UTest
      write(failMsg, *) "Config Unit test failed"
      write(name, *) "Config Unit Test"
      call ESMF_Test((counter_success.eq.counter_total), name, failMsg, result, ESMF_SRCLINE)
      if  (counter_total > 0) then
         if( counter_success == counter_total ) then 
            print *,'ESMF_Config: All tests were successful'
         else
            success_rate = 100.0 * counter_success / counter_total 
            print *,'ESMF_Config: Success rate: ', nint(success_rate),'%' 
         endif
      endif

#endif

      call ESMF_TestEnd(result, ESMF_SRCLINE)


  end program ESMF_Config_Test

