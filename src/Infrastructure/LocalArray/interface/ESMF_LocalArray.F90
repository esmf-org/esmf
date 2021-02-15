! $Id$
!
! Earth System Modeling Framework
! Copyright 2002-2021, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
#define ESMF_FILENAME "ESMF_LocalArray.F90"
!==============================================================================
!
! ESMF LocalArray module
module ESMF_LocalArrayMod
!
!==============================================================================
!
! This file contains the sub modules for LocalArray class definition and methods
!
!------------------------------------------------------------------------------
  use iso_c_binding
  
  use ESMF_LocalArrayCreateMod
  use ESMF_LocalArrayGetMod
  
#ifndef ESMF_NO_F2018ASSUMEDTYPE
  public c_esmf_f90ptrsizeprint
#endif
  
!------------------------------------------------------------------------------
! ! Interoperability interfaces

#ifndef ESMF_NO_F2018ASSUMEDTYPE

  interface

    subroutine c_esmf_f90ptrsizeprint(p1, p2, rank, rc)
      type(*)       :: p1(*)
      type(*)       :: p2(*)
      integer       :: rank
      integer       :: rc
    end subroutine

  end interface

#endif

!------------------------------------------------------------------------------

end module ESMF_LocalArrayMod
