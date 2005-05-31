!  $Id: ESMF_Util_C.F90,v 1.1 2005/05/31 17:27:20 nscollins Exp $
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
!      '$Id: ESMF_Util_C.F90,v 1.1 2005/05/31 17:27:20 nscollins Exp $'
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

       type(ESMF_DomainList) :: domlist
       !type(ESMF_DomainList) :: domlist_t
       !type(ESMF_Domain) :: dd
       integer :: dnum     
       !integer :: dnum_t
       integer :: DE
       !integer :: DE_t
       integer :: rc     
       !integer :: rc_t

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

       type(ESMF_DomainList) :: domlist
       integer :: dnum     
       integer :: ainum
       type(ESMF_AxisIndex) :: AI
       integer :: rc     

       AI = domlist%domains(dnum+1)%ai(ainum+1)

   end subroutine f_esmf_domainlistgetai



