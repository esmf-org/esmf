#if 0
! $Id: ESMF_FortranWordsizeMacros.h,v 1.3 2007/02/27 23:53:19 theurich Exp $
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


#define ESMF_FortranWordsizeDoc() \
!TODO: add interface documentation @\


#define ESMF_FortranWordsizeMacro(mtypename, mtypekind) \
!------------------------------------------------------------------------------ @\
! <Created by macro - do not edit directly > @\
!------------------------------------------------------------------------------ @\
@\
^undef  ESMF_METHOD @\
!define ESMF_METHOD to "ESMF_FortranWordsize##mtypekind" @\
^define ESMF_METHOD "ESMF_FortranWordsize" @\
    integer function ESMF_FortranWordsize##mtypekind(var, rc) @\
@\
     mtypename(ESMF_KIND_##mtypekind), intent(in) :: var @\
     integer, intent(out), optional :: rc @\
@\
     ! local data @\
     integer :: size @\
     mtypename(ESMF_KIND_##mtypekind) :: varTK(2) ! varTK is same TK as var @\
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


