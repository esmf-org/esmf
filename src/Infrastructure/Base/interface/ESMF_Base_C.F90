!  $Id: ESMF_Base_C.F90,v 1.1 2003/10/21 23:14:24 nscollins Exp $
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
!      '$Id: ESMF_Base_C.F90,v 1.1 2003/10/21 23:14:24 nscollins Exp $'
!==============================================================================

!------------------------------------------------------------------------------
!BOP
!  !DESCRIPTION:
! 
! The code in this file implements the interface code between C++ and F90
!  for the {\tt Base} entry points.
! 
!EOP
!------------------------------------------------------------------------------




   subroutine f_esmf_domainlistgetde(domlist, dnum, DE, rc)
       use ESMF_BaseMod    ! ESMF base class

       type(ESMF_DomainList), pointer :: domlist
       integer :: dnum     
       integer :: DE
       integer :: rc     

       DE = domlist%domains(dnum)%DE
       rc = ESMF_SUCCESS

   end subroutine f_esmf_domainlistgetde

   subroutine f_esmf_domainlistgetai(this, dnum, ainum, AI, rc);
       use ESMF_BaseMod    ! ESMF base class

       type(ESMF_DomainList), pointer :: domlist
       type(ESMF_AxisIndex) :: AI
       integer :: dnum     
       integer :: ainum
       integer :: rc     

       AI = domlist%domains(dnum)%ai(ainum)

   end subroutine f_esmf_domainlistgetai



