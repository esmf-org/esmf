#if 0
! $Id: ESMF_AllocMacros.h,v 1.8 2004/03/11 16:16:35 nscollins Exp $
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
#define AllocAllocateMacro(mtypekind, mrank, mrng, mloc) \
! <Created by macro - do not edit directly >  @\
        allocate(local##mrank##D##mtypekind % ptr##mrank##D##mtypekind( mrng ), stat=status) @\
        if (status .ne. 0) then @\
          print *, "ESMC_ArrayCreate: Allocation error" @\
          return @\
        endif @\
 @\
        ! Set all the new accumulated information about the array - the @\
        ! F90 pointer, the base addr, the counts, etc. @\
 @\
        ! Since I am not sure what these are used for, leave them 0 for now. @\
        offsets = 0 @\
 @\
        call c_ESMC_ArraySetInfo(array, local##mrank##D##mtypekind, & @\
                        ESMF_DATA_ADDRESS(local##mrank##D##mtypekind % ptr##mrank##D##mtypekind (mloc) ), & @\
                        counts, lb, ub, offsets, & @\
                        ESMF_TRUE, ESMF_TRUE, hwidth, status) @\
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
#define AllocDeallocateMacro(mtypekind, mrank) \
! <Created by macro - do not edit directly >  @\
        call c_ESMC_ArrayGetF90Ptr(array, local##mrank##D##mtypekind, status) @\
        deallocate(local##mrank##D##mtypekind % ptr##mrank##D##mtypekind, stat=status)  @\
        nullify(local##mrank##D##mtypekind % ptr##mrank##D##mtypekind) @\
! < End macro - do not edit directly >  @\

