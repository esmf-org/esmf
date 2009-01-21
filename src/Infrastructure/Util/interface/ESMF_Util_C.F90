!  $Id: ESMF_Util_C.F90,v 1.8.2.2 2009/01/21 21:25:24 cdeluca Exp $
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
! F77 interface files for C++ layer calling into F90 implementation layer.
!  This cannot use any F90 syntax, including modules, or allocatable 
!   arrays, or ...
!
!==============================================================================
!
!------------------------------------------------------------------------------
! INCLUDES
#include "ESMF.h"
!==============================================================================
!------------------------------------------------------------------------------
! The following line turns the CVS identifier string into a printable variable.
!      character(*), parameter, private :: version = &
!      '$Id: ESMF_Util_C.F90,v 1.8.2.2 2009/01/21 21:25:24 cdeluca Exp $'
!==============================================================================
!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
! 
! The code in this file implements the interface code between C++ and F90
!  for the {\tt Util} entry points.
! 
!EOP
!------------------------------------------------------------------------------




   subroutine f_esmf_domainlistgetde(domlist, dnum, DE, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_UtilMod    ! ESMF base class
       use ESMF_InitMacrosMod

       type(ESMF_DomainList) :: domlist
       !type(ESMF_DomainList) :: domlist_t
       !type(ESMF_Domain) :: dd
       integer :: dnum     
       !integer :: dnum_t
       integer :: DE
       !integer :: DE_t
       integer :: rc     
       !integer :: rc_t

       ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,domlist)

       !domlist_t = domlist
       !dnum_t = dnum
       !DE_t = DE
       !rc_t = rc

       !dd = domlist%domains(dnum+1)
       !DE_t = dd%DE
       
       DE = domlist%domains(dnum+1)%DE
       rc = ESMF_SUCCESS

   end subroutine f_esmf_domainlistgetde

   subroutine f_esmf_domainlistgetai(domlist, dnum, ainum, AI, rc)
       use ESMF_UtilTypesMod    ! ESMF base class
       use ESMF_UtilMod    ! ESMF base class
       use ESMF_InitMacrosMod

       type(ESMF_DomainList) :: domlist
       integer :: dnum     
       integer :: ainum
       type(ESMF_AxisIndex) :: AI
       integer :: rc

       ESMF_INIT_CHECK_SHALLOW(ESMF_DomainListGetInit,ESMF_DomainListInit,domlist)
       ESMF_INIT_CHECK_SHALLOW(ESMF_AxisIndexGetInit,ESMF_AxisIndexInit,AI)

       AI = domlist%domains(dnum+1)%ai(ainum+1)
       rc = ESMF_SUCCESS

   end subroutine f_esmf_domainlistgetai



