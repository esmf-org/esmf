#if 0
! $Id: ESMF_LocalAllocMacros.h,v 1.4 2004/03/04 16:33:39 nscollins Exp $
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
#define AllocAllocateMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
! <Created by macro - do not edit directly >  @\
        allocate(local##mtypekind##mrank##D%mtypekind##mrank##Dptr( mrng ), stat=status) @\
        if (status .ne. 0) then @\
          print *, "ESMC_LocalArrayCreate: Allocation error" @\
          return @\
        endif @\
 @\
        ! Set all the new accumulated information about the array - the @\
        ! F90 pointer, the base addr, the counts, etc. @\
 @\
        ! Set offsets for now to 0, since this is apparently unused. @\
        offsets = 0 @\
 @\
        call c_ESMC_LocalArraySetInternal(array, local##mtypekind##mrank##D, & @\
                        ESMF_DATA_ADDRESS(local##mtypekind##mrank##D%mtypekind##mrank##Dptr ), & @\
                        counts, lbounds, ubounds, offsets, & @\
                        ESMF_TRUE, ESMF_TRUE) @\
 @\
        if (status .ne. ESMF_SUCCESS) then @\
          print *, "LocalArray internal set info error" @\
          return @\
        endif @\
! < End macro - do not edit directly >  @\

#if 0
!------------------------------------------------------------------------------
! Deallocate the contents of the array.
!------------------------------------------------------------------------------
#endif
#define AllocDeallocateMacro(mname, mtypekind, mrank, mdim, mlen, mrng, mloc) \
! <Created by macro - do not edit directly >  @\
        call c_ESMC_LocalArrayGetF90Ptr(array, local##mtypekind##mrank##D, status) @\
        deallocate(local##mtypekind##mrank##D%mtypekind##mrank##Dptr, stat=status)  @\
        nullify(local##mtypekind##mrank##D%mtypekind##mrank##Dptr) @\
! < End macro - do not edit directly >  @\

