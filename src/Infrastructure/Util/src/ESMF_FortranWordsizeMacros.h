#if 0
! $Id: ESMF_FortranWordsizeMacros.h,v 1.4 2007/02/28 20:36:28 rosalind Exp $
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
! <Created by macro - do not edit directly > @\
!BOP @\
! !IROUTINE: ESMF_FortranWordsize - Return the size in byte units of a scalar @\
 a scalar @\
! @\
! !INTERFACE: @\
!   ! Private name; call using ESMF_FortranWordsize() @\
!   function ESMF_FortranWordsize<typekind>(var, rc) @\
! @\
! !RETURN VALUE: @\
!      integer :: ESMF_FortranWordsize<typekind> @\
! @\
! !ARGUMENTS: @\
!     <type>(ESMF_KIND_<typekind>), intent(in) :: var @\
!     integer, intent(out), optional :: rc @\
! @\
! !DESCRIPTION: @\
!   Return the size in units of bytes of a scalar (var) argument. @\
!   Valid types and kinds supported by the framework are: @\
!   integers of 1-byte, 2-byte, 4-byte, and 8-byte size, and @\
!   reals of 4-byte and 8-bytes size.  @\
! @\
!   The arguments are: @\
!   \begin{description} @\
!   \item [var] @\
!      Scalar of any supported type and kind
!   \item [rc] @\
!     Return code; equals {\tt ESMF\_SUCCESS} if there are no errors. @\
!   \end{description} @\
! @\
!EOP @\

#if 0
!------------------------------------------------------------------------------
! Return the size in bytes of a scalar
!------------------------------------------------------------------------------
#endif



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


