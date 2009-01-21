#if 0
! $Id: ESMF_AllocMacros.h,v 1.4.2.2 2009/01/21 21:25:21 cdeluca Exp $
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
#endif
#if 0
!------------------------------------------------------------------------------
! Macros for the Alloc class.
!------------------------------------------------------------------------------
#endif

#if 0
!------------------------------------------------------------------------------
! Allocate the contents of the array.
!------------------------------------------------------------------------------
#endif
#define AllocAllocateMacro(mtypekind, mrank, mrng, mloc) \
! <Created by macro - do not edit directly >  @\
        allocate(l##mrank##D##mtypekind % ptr##mrank##D##mtypekind( mrng ), stat=status) @\
        if (ESMF_LogMsgFoundAllocError(status, "Local pointer", & @\
                                       ESMF_CONTEXT, rc)) return @\
 @\
        ! Set all the new accumulated information about the array - the @\
        ! F90 pointer, the base addr, the counts, etc. @\
        ! (Since I am not sure if these are used, set offsets to 0 for now.) @\
        offsets = 0 @\
 @\
        call c_ESMC_IArraySetInfo(array, l##mrank##D##mtypekind, & @\
                        ESMF_DATA_ADDRESS(l##mrank##D##mtypekind % ptr##mrank##D##mtypekind (mloc) ), & @\
                        counts, lb, ub, offsets, & @\
                        ESMF_TRUE, ESMF_TRUE, hwidth, status) @\
        if (ESMF_LogMsgFoundError(status, & @\
                                  ESMF_ERR_PASSTHRU, & @\
                                  ESMF_CONTEXT, rc)) return @\
! < End macro - do not edit directly >  @\

#if 0
!------------------------------------------------------------------------------
! Deallocate the contents of the array.
!------------------------------------------------------------------------------
#endif
#define AllocDeallocateMacro(mtypekind, mrank) \
! <Created by macro - do not edit directly >  @\
        call c_ESMC_IArrayGetF90Ptr(array, l##mrank##D##mtypekind, status) @\
        deallocate(l##mrank##D##mtypekind % ptr##mrank##D##mtypekind, stat=status)  @\
        nullify(l##mrank##D##mtypekind % ptr##mrank##D##mtypekind) @\
! < End macro - do not edit directly >  @\

