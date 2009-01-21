! $Id: ESMF_RouteUTest.F90,v 1.12.2.4 2009/01/21 21:25:23 cdeluca Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2009, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
      program ESMF_RouteTest

#include "ESMF_Macros.inc"
#include "ESMF.h"

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
      '$Id: ESMF_RouteUTest.F90,v 1.12.2.4 2009/01/21 21:25:23 cdeluca Exp $'
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
      type(ESMF_Route) :: route, route1
 
      ! local args needed to create/construct objects
      type(ESMF_VM) :: vm
      type(ESMF_DELayout) :: mylayout
      type(ESMF_XPacket) :: sendxp, recvxp
      integer :: i, myvmid, vmidcount, othervmid
      type(ESMF_LocalArray) :: srcarr, dstarr
      integer, dimension(:,:), pointer :: srcptr, dstptr


      ! -------- all variable declarations above here ----------
!------------------------------------------------------------------------------
      ! -------- beginning of executable code below here -------

      call ESMF_TestStart(ESMF_SRCLINE, rc=rc)

      if (.not. ESMF_TestMinPETs(4, ESMF_SRCLINE)) goto 10

      ! make a layout for what's needed below
      call ESMF_VMGetGlobal(vm, rc=rc)
      mylayout = ESMF_DELayoutCreate(vm, rc=rc)

      call ESMF_VMGet(vm, petcount=vmidcount, localpet=myvmid, rc=rc)


      allocate(srcptr(10,20))
      srcptr = reshape( (/ (i, i=1,200) /), (/ 10, 20 /))
      allocate(dstptr(10,20))
      dstptr = reshape( (/ (i, i=201,400) /), (/ 10, 20 /))
      srcarr = ESMF_LocalArrayCreate(srcptr, ESMF_DATA_REF, rc)
      dstarr = ESMF_LocalArrayCreate(dstptr, ESMF_DATA_REF, rc)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! test dynamic allocation of ESMF_Route
      route = ESMF_RouteCreate(vm, rc)
      write(name, *) "ESMF_RouteCreate"
      write(failMsg, *) "rc =", rc, ", args ="
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)
    
      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! test validate method via option string
      call ESMF_RouteValidate(route, options=validate_options, rc=rc)
      write(name, *) "ESMF_RouteValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", &
                                   trim(validate_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! test print method via option string
      call ESMF_RoutePrint(route, print_options, rc)
      write(name, *) "ESMF_RoutePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", &
                                  trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! add a send route
      call ESMF_XPacketSetDefault(sendxp, 2, 0, 10, (/ 1, 10 /), (/ 3, 4 /) )
      othervmid = MODULO(myvmid+1, vmidcount)
      call ESMF_RouteSetSend(route, othervmid, sendxp, rc);
      write(name, *) "ESMF_RouteSetSend"
      write(failMsg, *) "rc =", rc, ", args =", othervmid
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! test print method via option string
      call ESMF_RoutePrint(route, print_options, rc)
      write(name, *) "ESMF_RoutePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", &
                                  trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! add a recv route
      call ESMF_XPacketSetDefault(recvxp, 2, 0, 10, (/ 1, 10 /), (/ 3, 4 /) )
      othervmid = MODULO(myvmid-1, vmidcount)
      call ESMF_RouteSetRecv(route, othervmid, recvxp, rc);
      write(name, *) "ESMF_RouteSetRecv"
      write(failMsg, *) "rc =", rc, ", args =", othervmid
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! test print method via option string
      call ESMF_RoutePrint(route, print_options, rc)
      write(name, *) "ESMF_RoutePrint"
      write(failMsg, *) "rc =", rc, ", print_options =", &
                                  trim(print_options)
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !
      ! execute the existing route
      !call ESMF_RouteRun(route, srcarr, dstarr, rc=rc)
      !write(name, *) "ESMF_RouteRun"
      !write(failMsg, *) "rc =", rc
      !call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !NEX_removeUTest_Multi_Proc_Only
      ! test dynamic deallocation of ESMF_Route also tests destructor
      call ESMF_RouteDestroy(route, rc)
      write(name, *) "ESMF_RouteDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), name, failMsg, result, ESMF_SRCLINE)

#ifdef ESMF_TESTEXHAUSTIVE
      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! destroy a destroyed Route
      call ESMF_RouteDestroy(route, rc)
      write(name, *) "Destroy a destroyed Route"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! destroy a non-created Route
      call ESMF_RouteDestroy(route1, rc)
      write(name, *) "Destroy a non-created Route"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! Print a deleted Route
      call ESMF_RoutePrint(route, print_options, rc)
      write(name, *) "Print a destroyed Route"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! Print a non-created Route
      call ESMF_RoutePrint(route1, print_options, rc)
      write(name, *) "Print a non-created Route"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! Validate a destroyed Route
      call ESMF_RouteValidate(route, options=validate_options, rc=rc)
      write(name, *) "Validate a destroyed Route Test"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! Validate a non-created Route
      call ESMF_RouteValidate(route1, options=validate_options, rc=rc)
      write(name, *) "Validate a non-created Route Test"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! add a recv route to a deleted Route
      call ESMF_XPacketSetDefault(recvxp, 2, 0, 10, (/ 1, 10 /), (/ 3, 4 /) )
      othervmid = MODULO(myvmid-1, vmidcount)
      call ESMF_RouteSetRecv(route, othervmid, recvxp, rc);
      write(name, *) "RouteSetRecv to a destroyed Route"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_DELETED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_DELETED), name, failMsg, result, ESMF_SRCLINE)

      !------------------------------------------------------------------------
      !EX_removeUTest_Multi_Proc_Only
      ! add a recv route to a non-created Route
      call ESMF_XPacketSetDefault(recvxp, 2, 0, 10, (/ 1, 10 /), (/ 3, 4 /) )
      othervmid = MODULO(myvmid-1, vmidcount)
      call ESMF_RouteSetRecv(route1, othervmid, recvxp, rc);
      write(name, *) "RouteSetRecv to a non-created Route"
      write(failMsg, *) "Did not return ESMF_RC_OBJ_NOT_CREATED"
      call ESMF_Test((rc.eq.ESMF_RC_OBJ_NOT_CREATED), name, failMsg, result, ESMF_SRCLINE)

#endif

10    continue

      call ESMF_TestEnd(result, ESMF_SRCLINE)
  
      end program ESMF_RouteTest
