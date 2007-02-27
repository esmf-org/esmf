#if 0
! $Id: ESMF_FortranWordsizeMacros.h,v 1.1 2007/02/27 22:41:36 theurich Exp $
!
! Earth System Modeling Framework
! Copyright 2002-2008, University Corporation for Atmospheric Research,
! Massachusetts Institute of Technology, Geophysical Fluid Dynamics
! Laboratory, University of Michigan, National Centers for Environmental
! Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
! NASA Goddard Space Flight Center.
! Licensed under the University of Illinois-NCSA License.
!
!==============================================================================
!
#endif

#if 0
!------------------------------------------------------------------------------
! Declare a wrapper for a Fortran array pointer which is expanded later to
! one of every type, and a local variable of every type.
!------------------------------------------------------------------------------
#endif


#define ESMF_FortranWordsizeMacro(mname, mtypekind) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!------------------------------------------------------------------------------ @\
@\
^undef  ESMF_METHOD @\
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" @\
^define ESMF_METHOD "ESMF_FortranWordsize" @\
    integer function ESMF_FortranWordsize##mtypekind(var, rc) @\
@\
     mname(ESMF_KIND_##mtypekind), intent(in) :: var @\
     integer, intent(out), optional :: rc @\
@\
     ! local data @\
     integer :: size @\
     mname(ESMF_KIND_##mtypekind) :: varTK(2)   ! varTK is same TK as var @\
     logical :: rcpresent                    ! Return code present @\
@\
     ! Initialize return code; assume failure until success is certain @\
     rcpresent = .FALSE. @\
     if (present(rc)) then @\
         rcpresent = .TRUE. @\
         rc = ESMF_FAILURE @\
     endif @\
@\
     call ESMF_PointerDifference(ESMC_POINTER_SIZE, varTK(1), varTK(2), size) @\
     ESMF_FortranWordsize##mtypekind = size @\
@\
     if (rcpresent) rc = ESMF_SUCCESS @\
@\
     end function ESMF_FortranWordsize##mtypekind @\
@\
! < end macro - do not edit directly >  @\
!------------------------------------------------------------------------------ @
\


