#if 0
! $Id: ESMF_AllocMacros.h,v 1.2 2003/05/01 16:10:54 nscollins Exp $
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
#define AllocAllocateMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
! <Created by macro - do not edit directly >  @\
        allocate(local##mtypekind##mrank##D%mtypekind##mrank##Dptr( mlen ), stat=status) @\
        if (status .ne. 0) then @\
          print *, "ESMC_ArrayCreate: Allocation error" @\
          return @\
        endif @\
 @\
        ! Set all the new accumulated information about the array - the @\
        ! F90 pointer, the base addr, the counts, etc. @\
 @\
        ! TODO: query the ptr for strides/lbounds/ubounds/offsets/whatever @\
        !  and set them in the array object.  For now, used fixed values. @\
        lbounds = 1 @\
        ubounds = 1 @\
        ubounds(1:mrank) = counts(1:mrank) @\
        strides = 0 @\
        offsets = 0 @\
 @\
        call c_ESMC_ArraySetInfo(array, local##mtypekind##mrank##D, & @\
                        local##mtypekind##mrank##D%mtypekind##mrank##Dptr( mloc ), & @\
                        counts, lbounds, ubounds, strides, offsets, & @\
                        ESMF_TF_TRUE, ESMF_TF_TRUE) @\
 @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "Array internal set info error" @\
          return @\
        endif @\
! < End macro - do not edit directly >  @\

#if 0
!------------------------------------------------------------------------------
! Deallocate the contents of the array.
!------------------------------------------------------------------------------
#endif
#define AllocDeallocateMacro(mname, mtypekind, mrank, mdim, mlen, mloc) \
! <Created by macro - do not edit directly >  @\
        call c_ESMC_ArrayGetF90Ptr(array, local##mtypekind##mrank##D, status) @\
        deallocate(local##mtypekind##mrank##D%mtypekind##mrank##Dptr, stat=status)  @\
        nullify(local##mtypekind##mrank##D%mtypekind##mrank##Dptr) @\
! < End macro - do not edit directly >  @\

