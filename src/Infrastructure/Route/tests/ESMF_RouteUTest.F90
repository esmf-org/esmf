! $Id: ESMF_RouteUTest.F90,v 1.4 2004/06/02 11:54:40 nscollins Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2003, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the GPL.
!
!==============================================================================
!
      program ESMF_RouteTest

#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_RouteTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 Route unit tests.
! The companion file ESMF\_Route.F90 contains the definitions for the
! Route methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_RouteUTest.F90,v 1.4 2004/06/02 11:54:40 nscollins Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR*2) :: failMsg
      character(ESMF_MAXSTR) :: validate_options = "full"
      character(ESMF_MAXSTR) :: print_options = "all"

      ! instantiate a Route 
      type(ESMF_Route) :: route
 
      ! local args needed to create/construct objects
      type(ESMF_DELayout) :: mylayout
      type(ESMF_XPacket) :: sendxp, recvxp
      integer :: i, mydeid, deidcount, otherdeid
      type(ESMF_LocalArray) :: srcarr, dstarr
      integer, dimension(:,:), pointer :: srcptr, dstptr


      ! -------- all variable declarations above here ----------
!------------------------------------------------------------------------------
      ! -------- beginning of executable code below here -------

      ! make a layout for what's needed below
      mylayout = ESMF_DELayoutCreate(rc=rc)

      call ESMF_DELayoutGetDEid(mylayout, mydeid, rc=rc)
      call ESMF_DELayoutGetNumDEs(mylayout, deidcount, rc=rc)

      allocate(srcptr(10,20))
      srcptr = reshape( (/ (i, i=1,200) /), (/ 10, 20 /))
      allocate(dstptr(10,20))
      dstptr = reshape( (/ (i, i=201,400) /), (/ 10, 20 /))
      srcarr = ESMF_LocalArrayCreate(srcptr, ESMF_DATA_REF, rc)
      dstarr = ESMF_LocalArrayCreate(dstptr, ESMF_DATA_REF, rc)

      ! test dynamic allocation of ESMF_Route
      route = ESMF_RouteCreate(mylayout, rc)
      write(name, *) "ESMF_RouteCreate"
      write(failMsg, *) "rc =", rc, ", args ="
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated ESMF_Route
      !call ESMF_RouteConstruct(route, mylayout, rc)
      !write(name, *) "ESMF_RouteConstruct"
      !write(failMsg, *) "rc =", rc, ", args ="
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test validate method via option string
      call ESMF_RouteValidate(route, validate_options, rc)
      write(name, *) "ESMF_RouteValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", &
                                   trim(validate_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_RoutePrint(route, print_options, rc)
      write(name, *) "ESMF_RoutePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", &
                                  trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! add a send route
      call ESMF_XPacketSetDefault(sendxp, 2, 0, 10, (/ 1, 10 /), (/ 3, 4 /) )
      otherdeid = MODULO(mydeid+1, deidcount)
      call ESMF_RouteSetSend(route, otherdeid, sendxp, rc);
      write(name, *) "ESMF_RouteSetSend"
      write(failMsg, *) "rc =", rc, ", args =", otherdeid
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_RoutePrint(route, print_options, rc)
      write(name, *) "ESMF_RoutePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", &
                                  trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! add a recv route
      call ESMF_XPacketSetDefault(recvxp, 2, 0, 10, (/ 1, 10 /), (/ 3, 4 /) )
      otherdeid = MODULO(mydeid-1, deidcount)
      call ESMF_RouteSetRecv(route, otherdeid, recvxp, rc);
      write(name, *) "ESMF_RouteSetRecv"
      write(failMsg, *) "rc =", rc, ", args =", otherdeid
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      call ESMF_RoutePrint(route, print_options, rc)
      write(name, *) "ESMF_RoutePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", &
                                  trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      ! execute the existing route
      call ESMF_RouteRun(route, srcarr, dstarr, rc)
      write(name, *) "ESMF_RouteRun"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)


      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_Route
      !call ESMF_RouteDestruct(route, rc)
      !write(name, *) "ESMF_RouteDestruct"
      !write(failMsg, *) "rc =", rc
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_Route also tests destructor
      call ESMF_RouteDestroy(route, rc)
      write(name, *) "ESMF_RouteDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
      write(*,*) "===================================="
      write(*,*) "========= end of test =============="
      write(*,*) "===================================="
  
      end program ESMF_RouteTest
