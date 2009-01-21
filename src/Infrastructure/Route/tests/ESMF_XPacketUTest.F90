! $Id: ESMF_XPacketUTest.F90,v 1.4.2.2 2009/01/21 21:25:23 cdeluca Exp $
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
      program ESMF_XPacketTest

#include "ESMF_Macros.inc"

!------------------------------------------------------------------------------
!
!==============================================================================
!BOP
! !PROGRAM: ESMF_XPacketTest - One line general statement about this test
!
! !DESCRIPTION:
!
! The code in this file drives F90 XPacket unit tests.
! The companion file ESMF\_XPacket.F90 contains the definitions for the
! XPacket methods.
!
!-----------------------------------------------------------------------------
! !USES:
      use ESMF_TestMod     ! test methods
      use ESMF_Mod         ! rest of esmf
      implicit none

!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
      character(*), parameter :: version = &
      '$Id: ESMF_XPacketUTest.F90,v 1.4.2.2 2009/01/21 21:25:23 cdeluca Exp $'
!------------------------------------------------------------------------------

      ! cumulative result: count failures; no failures equals "all pass"
      integer :: result = 0

      ! individual test result code
      integer :: rc

      ! individual test name
      character(ESMF_MAXSTR) :: name

      ! individual test failure message
      character(ESMF_MAXSTR) :: failMsg

      ! instantiate a XPacket 
      type(ESMF_XPacket) :: rtable
 
      ! local args needed to create/construct objects
      integer :: mydeid
      integer :: decount

      mydeid = 1
      decount = 4

#if 0
      ! test dynamic allocation of ESMF_XPacket
      rtable = ESMF_RTableInit(mydeid, decount, rc)
      write(name, *) "ESMF_XPacketCreate"
      write(failMsg, *) "rc =", rc, ", args =", mydeid, decount
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)
    
      ! test internal dynamic allocation within statically allocated
      !   ESMF_XPacket
      call ESMF_XPacketConstruct(rtable, mydeid, decount, rc)
      write(name, *) "ESMF_XPacketConstruct"
      write(failMsg, *) "rc =", rc, ", args =", mydeid, decount
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test validate method via option string
      character(ESMF_MAXSTR) :: validate_options
      call ESMF_XPacketValidate(rtable, validate_options, rc)
      write(name, *) "ESMF_XPacketValidate"
      write(failMsg, *) "rc =", rc, ", validate_options =", validate_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test print method via option string
      character(ESMF_MAXSTR) :: print_options
      call ESMF_XPacketPrint(rtable, print_options, rc)
      write(name, *) "ESMF_XPacketPrint"
      write(failMsg, *) "rc =", rc, ", print_options =", print_options
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! test internal dynamic deallocation within statically allocated 
      !   ESMF_XPacket
      call ESMF_XPacketDestruct(rtable, rc)
      write(name, *) "ESMF_XPacketDestruct"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      failMsg, result, ESMF_SRCLINE)

      ! test dynamic deallocation of ESMF_XPacket
      !   also tests destructor
      call ESMF_XPacketDestroy(rtable, rc)
      write(name, *) "ESMF_XPacketDestroy"
      write(failMsg, *) "rc =", rc
      call ESMF_Test((rc.eq.ESMF_SUCCESS), &
                      name, failMsg, result, ESMF_SRCLINE)

      ! return number of failures to environment; 0 = success (all pass)
      ! return result  ! TODO: no way to do this in F90 ?
#endif
  
      end program ESMF_XPacketTest
